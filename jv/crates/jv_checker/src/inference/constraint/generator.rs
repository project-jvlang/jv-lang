//! AST を走査して型制約を生成するビジター実装。
//!
//! `ConstraintGenerator` はスコープ管理を `TypeEnvironment` に委ねつつ、
//! val/var 宣言や代表的な式から型整合性と null 許容性の制約を収集する。

use crate::diagnostics::messages;
use crate::inference::compatibility::CompatibilityChecker;
use crate::inference::constraint::{Constraint, ConstraintKind, ConstraintSet};
use crate::inference::context_adaptation;
use crate::inference::conversions::ConversionOutcome;
use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::extensions::ExtensionRegistry;
use crate::inference::imports::ImportRegistry;
use crate::inference::iteration::{
    LoopClassification, classify_loop, expression_can_yield_iterable,
};
use crate::inference::regex::{
    RegexCommandIssue, RegexCommandTyping, RegexMatchTyping, RegexMatchWarning,
};
use crate::inference::type_factory::TypeFactory;
use crate::inference::types::{PrimitiveType, TypeError, TypeId, TypeKind};
use crate::pattern::{
    NarrowedBinding, NarrowedNullability, NarrowingSnapshot, PatternMatchService, PatternTarget,
    expression_span,
};
use jv_ast::{
    Argument, BinaryMetadata, BinaryOp, Expression, ForInStatement, IsTestKind, IsTestMetadata,
    Literal, Parameter, Program, RegexCommand, RegexCommandMode, RegexCommandModeOrigin, RegexFlag,
    RegexGuardStrategy, RegexReplacement, Span, Statement, TypeAnnotation, UnaryOp,
};
use jv_inference::types::NullabilityFlag;
use std::collections::{HashMap, HashSet};

/// AST から制約を抽出するジェネレータ。
#[derive(Debug)]
pub struct ConstraintGenerator<'env, 'ext, 'imp> {
    env: &'env mut TypeEnvironment,
    constraints: ConstraintSet,
    extensions: &'ext ExtensionRegistry,
    imports: Option<&'imp mut ImportRegistry>,
    pattern_service: PatternMatchService,
    type_var_usage: HashMap<TypeId, usize>,
}

const DIAG_RANGE_BOUNDS: &str = "E_LOOP_002: numeric range bounds must resolve to the same type";
const DIAG_RANGE_BINDING: &str =
    "E_LOOP_002: loop binding must align with the numeric range element type";
const DIAG_ITERABLE_PROTOCOL: &str =
    "E_LOOP_003: loop target expression does not expose iterable semantics";
const DIAG_AMBIGUOUS_EXTENSION: &str = "E_EXT_001: ambiguous extension method resolution";

impl<'env, 'ext, 'imp> ConstraintGenerator<'env, 'ext, 'imp> {
    /// 環境への可変参照を受け取ってジェネレータを初期化する。
    pub fn new(
        env: &'env mut TypeEnvironment,
        extensions: &'ext ExtensionRegistry,
        imports: Option<&'imp mut ImportRegistry>,
    ) -> Self {
        Self {
            env,
            constraints: ConstraintSet::new(),
            extensions,
            imports,
            pattern_service: PatternMatchService::new(),
            type_var_usage: HashMap::new(),
        }
    }

    fn push_constraint(&mut self, kind: ConstraintKind, note: Option<&str>) {
        self.push_constraint_with_span(kind, note, None);
    }

    fn push_constraint_with_span(
        &mut self,
        kind: ConstraintKind,
        note: Option<&str>,
        span: Option<&Span>,
    ) {
        self.record_type_vars_in_constraint(&kind);
        let mut constraint = Constraint::new(kind).with_span(span);
        if let Some(text) = note {
            constraint = constraint.with_note(text);
        }
        self.constraints.push(constraint);
    }

    fn push_assignability_constraint(
        &mut self,
        from: TypeKind,
        to: TypeKind,
        note: Option<&str>,
        span: Option<&Span>,
    ) {
        match CompatibilityChecker::analyze(self.env, &from, &to) {
            ConversionOutcome::Identity => {
                self.push_constraint_with_span(ConstraintKind::Equal(to, from), note, span);
            }
            ConversionOutcome::Allowed(_) => {
                self.push_constraint_with_span(
                    ConstraintKind::Convertible { from, to },
                    note,
                    span,
                );
            }
            ConversionOutcome::Rejected(error) => self.report_type_error(error),
        }
    }
    /// プログラム全体を走査して制約集合を構築する。
    pub fn generate(mut self, program: &Program) -> ConstraintSet {
        for stmt in &program.statements {
            self.visit_statement(stmt);
        }
        self.constraints
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::ValDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                let init_ty = self.infer_expression(initializer);
                self.bind_symbol(
                    name,
                    type_annotation.as_ref(),
                    Some(initializer),
                    Some(init_ty),
                );
            }
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                let init_ty = initializer.as_ref().map(|expr| self.infer_expression(expr));
                self.bind_symbol(
                    name,
                    type_annotation.as_ref(),
                    initializer.as_ref(),
                    init_ty,
                );
            }
            Statement::Expression { expr, .. } => {
                self.infer_expression(expr);
            }
            Statement::Assignment { target, value, .. } => {
                let target_ty = self.infer_expression(target);
                let value_ty = self.infer_expression(value);
                self.push_assignability_constraint(
                    value_ty,
                    target_ty,
                    Some("assignment target and value must align"),
                    expression_span(value),
                );
            }
            Statement::ForIn(for_in) => {
                self.handle_for_in(for_in);
            }
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.infer_expression(expr);
                }
            }
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
                ..
            } => {
                let mut param_types = Vec::with_capacity(parameters.len());
                for param in parameters {
                    let ty = param
                        .type_annotation
                        .as_ref()
                        .map(|ann| self.type_from_annotation(ann))
                        .unwrap_or_else(|| self.env.fresh_type_variable());
                    param_types.push(ty);
                }

                let return_ty = return_type
                    .as_ref()
                    .map(|ann| self.type_from_annotation(ann))
                    .unwrap_or_else(|| self.env.fresh_type_variable());

                let function_type = TypeKind::function(param_types.clone(), return_ty.clone());
                self.env
                    .define_scheme(name.clone(), TypeScheme::monotype(function_type));

                self.env.enter_scope();
                for (param, param_ty) in parameters.iter().zip(param_types.iter()) {
                    self.env
                        .define_scheme(&param.name, TypeScheme::monotype(param_ty.clone()));

                    if let Some(default_expr) = &param.default_value {
                        let default_ty = self.infer_expression(default_expr);
                        self.push_assignability_constraint(
                            default_ty,
                            param_ty.clone(),
                            Some("parameter default must match declared type"),
                            expression_span(default_expr),
                        );
                    }
                }

                let body_ty = self.infer_expression(body);
                self.push_assignability_constraint(
                    body_ty,
                    return_ty.clone(),
                    Some("function body must satisfy return type"),
                    expression_span(body),
                );

                self.env.leave_scope();
            }
            _ => {
                // そのほかの文でも子ノードを可能な範囲で評価しておく。
            }
        }
    }

    fn handle_for_in(&mut self, for_in: &ForInStatement) {
        self.env.enter_scope();

        let binding_ty = if let Some(annotation) = &for_in.binding.type_annotation {
            self.type_from_annotation(annotation)
        } else {
            self.env.fresh_type_variable()
        };

        self.env.define_scheme(
            for_in.binding.name.clone(),
            TypeScheme::monotype(binding_ty.clone()),
        );

        match classify_loop(for_in) {
            LoopClassification::NumericRange { range } => {
                let start_ty = self.infer_expression(&range.start);
                let end_ty = self.infer_expression(&range.end);
                self.push_constraint(
                    ConstraintKind::Equal(start_ty.clone(), end_ty.clone()),
                    Some(DIAG_RANGE_BOUNDS),
                );
                self.push_constraint(
                    ConstraintKind::Equal(binding_ty.clone(), start_ty),
                    Some(DIAG_RANGE_BINDING),
                );
            }
            LoopClassification::Iterable => {
                self.infer_expression(&for_in.iterable);
                if !expression_can_yield_iterable(&for_in.iterable) {
                    self.constraints
                        .push(Constraint::new(ConstraintKind::Placeholder(
                            DIAG_ITERABLE_PROTOCOL,
                        )));
                }
            }
            LoopClassification::LazySequence { .. } => {
                self.infer_expression(&for_in.iterable);
                if !expression_can_yield_iterable(&for_in.iterable) {
                    self.constraints
                        .push(Constraint::new(ConstraintKind::Placeholder(
                            DIAG_ITERABLE_PROTOCOL,
                        )));
                }
            }
        }

        self.infer_expression(&for_in.body);
        self.env.leave_scope();
    }

    fn bind_symbol(
        &mut self,
        name: &str,
        annotation: Option<&TypeAnnotation>,
        initializer_expr: Option<&Expression>,
        initializer_ty: Option<TypeKind>,
    ) {
        let symbol_ty = self.env.fresh_type_variable();
        let mut representative = initializer_ty.clone().unwrap_or_else(|| TypeKind::Unknown);
        let mut annotated_ty: Option<TypeKind> = None;

        if let Some(ann) = annotation {
            let annotated = self.type_from_annotation(ann);
            if !matches!(annotated, TypeKind::Unknown) {
                self.push_constraint(
                    ConstraintKind::Equal(symbol_ty.clone(), annotated.clone()),
                    Some("declaration annotation must match symbol"),
                );
                representative = annotated.clone();
                annotated_ty = Some(annotated);
            }
        }

        if let Some(init) = initializer_ty {
            let span = initializer_expr.and_then(expression_span);
            let target_ty = annotated_ty.clone().unwrap_or_else(|| symbol_ty.clone());
            self.push_assignability_constraint(
                init.clone(),
                target_ty,
                Some("initializer must satisfy declaration"),
                span,
            );
            if matches!(representative, TypeKind::Unknown) {
                representative = init;
            }
        }

        let scheme = self.env.generalize(representative);
        self.env.define_scheme(name.to_string(), scheme);
    }

    fn infer_expression(&mut self, expr: &Expression) -> TypeKind {
        match expr {
            Expression::RegexLiteral(_) => TypeKind::reference("java.util.regex.Pattern"),
            Expression::RegexCommand(command) => self.infer_regex_command(command),
            Expression::Literal(literal, _) => self.type_from_literal(literal),
            Expression::Identifier(name, _) => {
                if let Some(scheme) = self.env.lookup(name).cloned() {
                    self.env.instantiate(&scheme)
                } else {
                    self.imports
                        .as_deref_mut()
                        .and_then(|imports| imports.resolve_identifier(self.env, name))
                        .unwrap_or(TypeKind::Unknown)
                }
            }
            Expression::Binary {
                left,
                op,
                right,
                metadata,
                ..
            } => self.infer_binary_expression(op, left, right, metadata),
            Expression::Unary { op, operand, .. } => self.infer_unary_expression(op, operand),
            Expression::Call { function, args, .. } => {
                let fn_ty = self.infer_expression(function);
                let (concrete_params, concrete_result) = match fn_ty.clone() {
                    TypeKind::Function(params, result) => (Some(params), Some(*result)),
                    _ => (None, None),
                };
                let mut parameter_types = Vec::with_capacity(args.len());
                let mut pending_arguments = Vec::with_capacity(args.len());
                for (index, arg) in args.iter().enumerate() {
                    let (arg_expr, arg_ty) = match arg {
                        Argument::Positional(expr) => (expr, self.infer_expression(expr)),
                        Argument::Named { value, .. } => (value, self.infer_expression(value)),
                    };
                    let target_ty = concrete_params
                        .as_ref()
                        .and_then(|params| params.get(index))
                        .cloned();
                    let param_ty = target_ty
                        .clone()
                        .unwrap_or_else(|| self.env.fresh_type_variable());
                    let span = expression_span(arg_expr).cloned();
                    if target_ty
                        .as_ref()
                        .is_some_and(|ty| !matches!(ty, TypeKind::Function(_, _)))
                    {
                        pending_arguments.push((arg_ty, param_ty.clone(), span));
                    }
                    parameter_types.push(param_ty);
                }

                let result_ty = concrete_result.unwrap_or_else(|| self.env.fresh_type_variable());
                let expected_fn = TypeKind::function(parameter_types, result_ty.clone());
                self.push_constraint(
                    ConstraintKind::Equal(fn_ty, expected_fn),
                    Some("call target must be compatible with argument list"),
                );

                for (arg_ty, param_ty, span) in pending_arguments {
                    self.push_assignability_constraint(
                        arg_ty,
                        param_ty,
                        Some("call argument must satisfy parameter type"),
                        span.as_ref(),
                    );
                }
                result_ty
            }
            Expression::TypeCast { expr, target, .. } => {
                let _ = self.infer_expression(expr);
                self.type_from_annotation(target)
            }
            Expression::Block { statements, .. } => self.infer_block(statements),
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_ty = self.infer_expression(condition);
                self.push_constraint_with_span(
                    ConstraintKind::Convertible {
                        from: cond_ty,
                        to: TypeKind::primitive(PrimitiveType::Boolean),
                    },
                    Some("if condition requires boolean"),
                    expression_span(condition),
                );
                let then_ty = self.infer_expression(then_branch);
                if let Some(else_expr) = else_branch {
                    let else_ty = self.infer_expression(else_expr);
                    self.push_constraint(
                        ConstraintKind::Equal(then_ty.clone(), else_ty.clone()),
                        Some("if branches must agree"),
                    );
                    then_ty
                } else {
                    TypeKind::Unknown
                }
            }
            Expression::MemberAccess {
                object,
                property,
                span,
            } => self.infer_member_access(object, property, span),
            Expression::NullSafeMemberAccess {
                object,
                property,
                span,
            } => self.infer_member_access(object, property, span),
            Expression::IndexAccess { object, .. }
            | Expression::NullSafeIndexAccess { object, .. } => {
                self.infer_expression(object);
                TypeKind::Unknown
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.infer_expression(element);
                }
                TypeKind::Unknown
            }
            Expression::Lambda {
                parameters, body, ..
            } => self.infer_lambda(parameters, body),
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                let body_ty = self.infer_expression(body);
                for clause in catch_clauses {
                    let catch_ty = self.infer_expression(&clause.body);
                    self.push_constraint(
                        ConstraintKind::Equal(body_ty.clone(), catch_ty),
                        Some("try/catch branches must agree"),
                    );
                }
                if let Some(finally_expr) = finally_block.as_deref() {
                    self.infer_expression(finally_expr);
                }
                body_ty
            }
            Expression::When {
                expr: subject,
                arms,
                else_arm,
                implicit_end: _,
                ..
            } => {
                if let Some(scrutinee) = subject {
                    self.infer_expression(scrutinee);
                }
                let facts = self.pattern_service.analyze(expr, PatternTarget::Java25);
                let narrowing = facts.narrowing();
                let mut branch_types = Vec::with_capacity(arms.len());
                for (index, arm) in arms.iter().enumerate() {
                    self.env.enter_scope();
                    if let Some((_, snapshot)) =
                        narrowing.arms().find(|(arm_id, _)| **arm_id == index)
                    {
                        self.apply_branch_narrowing(snapshot);
                    }
                    if let Some(guard_expr) = &arm.guard {
                        let guard_ty = self.infer_expression(guard_expr);
                        self.push_constraint_with_span(
                            ConstraintKind::Convertible {
                                from: guard_ty,
                                to: TypeKind::primitive(PrimitiveType::Boolean),
                            },
                            Some("when guard must evaluate to boolean"),
                            expression_span(guard_expr),
                        );
                    }
                    let body_ty = self.infer_expression(&arm.body);
                    self.env.leave_scope();
                    branch_types.push(body_ty);
                }
                let else_ty = else_arm.as_ref().map(|else_body| {
                    self.env.enter_scope();
                    if let Some(snapshot) = narrowing.fallback() {
                        self.apply_branch_narrowing(snapshot);
                    }
                    let ty = self.infer_expression(else_body);
                    self.env.leave_scope();
                    ty
                });

                if let Some(first_ty) = branch_types.first() {
                    for other in branch_types.iter().skip(1) {
                        self.push_constraint(
                            ConstraintKind::Equal(first_ty.clone(), other.clone()),
                            Some("when branches must agree"),
                        );
                    }
                    if let Some(else_ty_val) = &else_ty {
                        self.push_constraint(
                            ConstraintKind::Equal(first_ty.clone(), else_ty_val.clone()),
                            Some("when branches must agree"),
                        );
                    }
                }

                let mut aggregated_flag = if branch_types.is_empty() && else_ty.is_none() {
                    NullabilityFlag::Unknown
                } else {
                    NullabilityFlag::NonNull
                };
                for ty in &branch_types {
                    aggregated_flag = aggregated_flag.combine(nullability_from_type(ty));
                }
                match &else_ty {
                    Some(ty) => {
                        aggregated_flag = aggregated_flag.combine(nullability_from_type(ty));
                    }
                    None if !branch_types.is_empty() => {
                        aggregated_flag = aggregated_flag.combine(NullabilityFlag::Unknown);
                    }
                    None => {}
                }

                let mut result_ty = if let Some(ty) = &else_ty {
                    ty.clone()
                } else if let Some(first) = branch_types.first() {
                    first.clone()
                } else {
                    TypeKind::Unknown
                };

                if matches!(aggregated_flag, NullabilityFlag::Nullable) {
                    result_ty = ensure_optional_type(result_ty);
                }

                result_ty
            }
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let jv_ast::StringPart::Expression(expr) = part {
                        self.infer_expression(expr);
                    }
                }
                TypeKind::reference("java.lang.String")
            }
            Expression::MultilineString(literal) => {
                context_adaptation::infer_multiline_literal_type(literal)
            }
            Expression::JsonLiteral(_) => TypeKind::Unknown,
            Expression::This(_) | Expression::Super(_) => TypeKind::Unknown,
        }
    }

    fn infer_member_access(
        &mut self,
        object: &Expression,
        property: &str,
        span: &Span,
    ) -> TypeKind {
        let receiver_ty = self.infer_expression(object);
        match self.resolve_extension_call(&receiver_ty, property, span) {
            Some(resolved) => resolved,
            None => TypeKind::Unknown,
        }
    }

    fn resolve_extension_call(
        &mut self,
        receiver_ty: &TypeKind,
        property: &str,
        span: &Span,
    ) -> Option<TypeKind> {
        if let TypeKind::Optional(inner) = receiver_ty {
            return self.resolve_extension_call(inner, property, span);
        }

        if let Some(receiver) = Self::receiver_name(receiver_ty) {
            let scheme = self.extensions.lookup(receiver, property)?;
            return Some(self.env.instantiate(scheme));
        }

        if let TypeKind::Variable(id) = receiver_ty {
            let candidates = self.extensions.candidates_for_method(property);
            if candidates.is_empty() {
                return None;
            }

            if candidates.len() == 1 {
                let (receiver, scheme) = candidates[0];
                self.enqueue_receiver_constraint(*id, receiver, property);
                return Some(self.env.instantiate(scheme));
            }

            let mut usage_count = *self.type_var_usage.get(id).unwrap_or(&0);
            if usage_count == 0 {
                if let Some(origin) = self.env.type_origin(*id) {
                    usage_count = *self.type_var_usage.get(&origin).unwrap_or(&0);
                }
            }
            if usage_count == 0 {
                let candidate_names: Vec<&'static str> =
                    candidates.iter().map(|(recv, _)| *recv).collect();
                let message = format!(
                    "ambiguous extension method '{}' at {}:{}; matching receivers: {}",
                    property,
                    span.start_line,
                    span.start_column,
                    candidate_names.join(", ")
                );
                let constraint =
                    Constraint::new(ConstraintKind::Placeholder(DIAG_AMBIGUOUS_EXTENSION))
                        .with_note(message);
                self.constraints.push(constraint);
                return None;
            }

            let (receiver, scheme) = candidates[0];
            self.enqueue_receiver_constraint(*id, receiver, property);
            return Some(self.env.instantiate(scheme));
        }

        None
    }

    fn infer_lambda(&mut self, parameters: &[Parameter], body: &Expression) -> TypeKind {
        self.env.enter_scope();
        let mut param_types = Vec::with_capacity(parameters.len());
        for param in parameters {
            let ty = param
                .type_annotation
                .as_ref()
                .map(|ann| self.type_from_annotation(ann))
                .unwrap_or_else(|| self.env.fresh_type_variable());

            self.env
                .define_scheme(param.name.clone(), TypeScheme::monotype(ty.clone()));

            if let Some(default_expr) = &param.default_value {
                let default_ty = self.infer_expression(default_expr);
                self.push_constraint(
                    ConstraintKind::Equal(ty.clone(), default_ty),
                    Some("lambda default must match parameter type"),
                );
            }

            param_types.push(ty);
        }

        let body_ty = self.infer_expression(body);
        self.env.leave_scope();
        TypeKind::function(param_types, body_ty)
    }

    fn receiver_name<'a>(ty: &'a TypeKind) -> Option<&'a str> {
        match ty {
            TypeKind::Primitive(primitive) => Some(primitive.java_name()),
            TypeKind::Boxed(primitive) => Some(primitive.boxed_fqcn()),
            TypeKind::Reference(name) => Some(name.as_str()),
            _ => None,
        }
    }

    fn infer_block(&mut self, statements: &[Statement]) -> TypeKind {
        self.env.enter_scope();
        let mut last = TypeKind::Unknown;
        for stmt in statements {
            match stmt {
                Statement::Expression { expr, .. } => {
                    last = self.infer_expression(expr);
                }
                Statement::Return { value, .. } => {
                    if let Some(expr) = value {
                        last = self.infer_expression(expr);
                    }
                }
                _ => {
                    self.visit_statement(stmt);
                    last = TypeKind::Unknown;
                }
            }
        }
        self.env.leave_scope();
        last
    }

    fn infer_binary_expression(
        &mut self,
        op: &BinaryOp,
        left: &Expression,
        right: &Expression,
        metadata: &BinaryMetadata,
    ) -> TypeKind {
        use BinaryOp::*;

        let left_ty = self.infer_expression(left);
        let right_ty = self.infer_expression(right);

        match op {
            Add | PlusAssign => {
                if self.should_use_string_concatenation(&left_ty, &right_ty) {
                    let string_ty = TypeKind::reference("java.lang.String");
                    self.push_constraint_with_span(
                        ConstraintKind::Convertible {
                            from: left_ty.clone(),
                            to: string_ty.clone(),
                        },
                        Some("string concatenation requires operands convertible to java.lang.String"),
                        expression_span(left),
                    );
                    self.push_constraint_with_span(
                        ConstraintKind::Convertible {
                            from: right_ty.clone(),
                            to: string_ty.clone(),
                        },
                        Some("string concatenation requires operands convertible to java.lang.String"),
                        expression_span(right),
                    );
                    string_ty
                } else {
                    self.push_constraint(
                        ConstraintKind::Equal(left_ty.clone(), right_ty.clone()),
                        Some("arithmetic operands must share the same type"),
                    );
                    left_ty
                }
            }
            Subtract | Multiply | Divide | Modulo | MinusAssign | MultiplyAssign | DivideAssign => {
                self.push_constraint(
                    ConstraintKind::Equal(left_ty.clone(), right_ty.clone()),
                    Some("arithmetic operands must share the same type"),
                );
                left_ty
            }
            BitAnd | BitOr | BitXor => {
                let expected = TypeKind::primitive(PrimitiveType::Int);
                self.push_constraint(
                    ConstraintKind::Equal(left_ty.clone(), expected.clone()),
                    Some("bitwise operations require Int operands"),
                );
                self.push_constraint(
                    ConstraintKind::Equal(right_ty.clone(), expected.clone()),
                    Some("bitwise operations require Int operands"),
                );
                expected
            }
            And | Or => {
                let expected = TypeKind::primitive(PrimitiveType::Boolean);
                self.push_constraint_with_span(
                    ConstraintKind::Convertible {
                        from: left_ty.clone(),
                        to: expected.clone(),
                    },
                    Some("logical operations require boolean operands"),
                    expression_span(left),
                );
                self.push_constraint_with_span(
                    ConstraintKind::Convertible {
                        from: right_ty.clone(),
                        to: expected.clone(),
                    },
                    Some("logical operations require boolean operands"),
                    expression_span(right),
                );
                expected
            }
            Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual => {
                self.push_constraint(
                    ConstraintKind::Equal(left_ty.clone(), right_ty.clone()),
                    Some("comparison operands must share the same type"),
                );
                TypeKind::primitive(PrimitiveType::Boolean)
            }
            Is => {
                if let Some(is_test) = metadata.is_test.as_ref() {
                    match is_test.kind {
                        IsTestKind::RegexLiteral | IsTestKind::PatternExpression => {
                            self.infer_regex_is(left, &left_ty, right, &right_ty, is_test);
                        }
                        IsTestKind::Type => {}
                    }
                }
                TypeKind::primitive(PrimitiveType::Boolean)
            }
            RangeExclusive | RangeInclusive => {
                self.push_constraint(
                    ConstraintKind::Equal(left_ty.clone(), right_ty.clone()),
                    Some(DIAG_RANGE_BOUNDS),
                );
                TypeKind::Unknown
            }
            Elvis => {
                let result_inner = self.env.fresh_type_variable();
                let optional_result = TypeKind::Optional(Box::new(result_inner.clone()));
                self.push_constraint(
                    ConstraintKind::Equal(left_ty.clone(), optional_result),
                    Some("elvis left-hand side is optional"),
                );
                self.push_constraint(
                    ConstraintKind::Equal(result_inner.clone(), right_ty.clone()),
                    Some("elvis fallback must match unwrapped type"),
                );
                result_inner
            }
        }
    }

    fn infer_regex_is(
        &mut self,
        left_expr: &Expression,
        left_ty: &TypeKind,
        right_expr: &Expression,
        right_ty: &TypeKind,
        metadata: &IsTestMetadata,
    ) {
        let subject_span = expression_span(left_expr);
        let pattern_span = expression_span(right_expr);
        let subject_error = self.regex_subject_error_message(left_ty);

        let char_sequence_ty = TypeKind::reference("java.lang.CharSequence");
        let optional_char_sequence_ty = TypeKind::optional(char_sequence_ty.clone());

        let mut typing = RegexMatchTyping::new(metadata.span.clone(), RegexGuardStrategy::None);

        let subject_ok = if matches!(left_ty, TypeKind::Optional(_)) {
            typing.guard_strategy = RegexGuardStrategy::CaptureAndGuard { temp_name: None };
            typing.push_warning(RegexMatchWarning::new(
                "JV_REGEX_W001",
                self.regex_optional_warning_message(),
            ));
            self.push_regex_constraint(
                left_ty.clone(),
                optional_char_sequence_ty,
                subject_span,
                subject_error,
                true,
            )
        } else {
            self.push_regex_constraint(
                left_ty.clone(),
                char_sequence_ty,
                subject_span,
                subject_error,
                false,
            )
        };

        if !subject_ok {
            return;
        }

        if matches!(metadata.kind, IsTestKind::PatternExpression) {
            let pattern_error = self.regex_pattern_error_message(right_ty);
            let pattern_ok = self.push_regex_constraint(
                right_ty.clone(),
                TypeKind::reference("java.util.regex.Pattern"),
                pattern_span,
                pattern_error,
                false,
            );
            if !pattern_ok {
                return;
            }
        }

        self.env.push_regex_typing(typing);
    }

    fn infer_regex_command(&mut self, command: &RegexCommand) -> TypeKind {
        let subject_ty = self.infer_expression(&command.subject);
        let subject_span = expression_span(&command.subject);
        let mut guard_strategy = RegexGuardStrategy::None;
        let mut diagnostics: Vec<RegexCommandIssue> = Vec::new();

        let char_sequence_ty = TypeKind::reference("java.lang.CharSequence");
        let optional_char_sequence_ty = TypeKind::optional(char_sequence_ty.clone());
        let subject_message = self.regex_subject_error_message(&subject_ty);

        let subject_ok = if matches!(subject_ty, TypeKind::Optional(_)) {
            guard_strategy = RegexGuardStrategy::CaptureAndGuard { temp_name: None };
            self.push_regex_constraint(
                subject_ty.clone(),
                optional_char_sequence_ty,
                subject_span,
                subject_message,
                true,
            )
        } else {
            self.push_regex_constraint(
                subject_ty.clone(),
                char_sequence_ty,
                subject_span,
                subject_message,
                false,
            )
        };

        if !subject_ok {
            return TypeKind::Unknown;
        }

        let return_type = self.resolve_regex_command_return_type(command);

        if let Some(replacement) = &command.replacement {
            self.verify_regex_replacement(replacement, &mut diagnostics);
        }

        self.collect_flag_diagnostics(command, &mut diagnostics);

        if self.detect_mode_flag_confusion(command) {
            diagnostics.push(RegexCommandIssue::new(
                "JV_REGEX_I001",
                messages::regex_mode_flag_confusion_message(),
            ));
        }

        if matches!(command.mode_origin, RegexCommandModeOrigin::DefaultMatch) {
            diagnostics.push(RegexCommandIssue::new(
                "JV_REGEX_I002",
                messages::regex_ambiguous_mode_message(),
            ));
        }

        let mut typing = RegexCommandTyping::new(
            command.span.clone(),
            command.mode,
            subject_ty.clone(),
            return_type.clone(),
            guard_strategy,
        );

        typing.requires_stream_materialization =
            matches!(command.mode, RegexCommandMode::Iterate) && command.replacement.is_none();

        for diag in diagnostics {
            typing.push_diagnostic(diag);
        }

        self.env.push_regex_command_typing(typing);
        return_type
    }

    fn resolve_regex_command_return_type(&self, command: &RegexCommand) -> TypeKind {
        match command.mode {
            RegexCommandMode::All | RegexCommandMode::First => {
                TypeKind::reference("java.lang.String")
            }
            RegexCommandMode::Match => TypeKind::boxed(PrimitiveType::Boolean),
            RegexCommandMode::Split => TypeKind::reference("java.lang.String[]"),
            RegexCommandMode::Iterate => {
                if command.replacement.is_some() {
                    TypeKind::reference("java.lang.String")
                } else {
                    TypeKind::reference("java.util.stream.Stream")
                }
            }
        }
    }

    fn verify_regex_replacement(
        &mut self,
        replacement: &RegexReplacement,
        diagnostics: &mut Vec<RegexCommandIssue>,
    ) {
        match replacement {
            RegexReplacement::Literal(_) => {}
            RegexReplacement::Expression(expr) => {
                let expr_ty = self.infer_expression(expr);
                let span = expression_span(expr);
                self.ensure_string_output(expr_ty, span, diagnostics, None);
            }
            RegexReplacement::Lambda(lambda) => {
                self.env.enter_scope();
                for param in &lambda.params {
                    let ty = param
                        .type_annotation
                        .as_ref()
                        .map(|ann| self.type_from_annotation(ann))
                        .unwrap_or_else(|| TypeKind::reference("java.util.regex.MatchResult"));

                    self.env
                        .define_scheme(param.name.clone(), TypeScheme::monotype(ty.clone()));

                    if let Some(default_expr) = &param.default_value {
                        let default_ty = self.infer_expression(default_expr);
                        self.push_constraint(
                            ConstraintKind::Equal(ty.clone(), default_ty),
                            Some("ラムダ引数の既定値は宣言された型と一致する必要があります"),
                        );
                    }
                }

                let body_ty = self.infer_expression(&lambda.body);
                let body_span = expression_span(&lambda.body);
                self.env.leave_scope();

                self.ensure_string_output(body_ty, body_span, diagnostics, Some("JV_REGEX_E102"));
            }
        }
    }

    fn ensure_string_output(
        &mut self,
        actual: TypeKind,
        span: Option<&Span>,
        diagnostics: &mut Vec<RegexCommandIssue>,
        diag_code: Option<&'static str>,
    ) {
        let expected = TypeKind::reference("java.lang.String");
        let note = "正規表現の置換結果は String 型である必要があります";
        let actual_desc = actual.describe();

        match CompatibilityChecker::analyze(self.env, &actual, &expected) {
            ConversionOutcome::Identity => {
                self.push_constraint_with_span(
                    ConstraintKind::Equal(actual, expected),
                    Some(note),
                    span,
                );
            }
            ConversionOutcome::Allowed(_) => {
                if let Some(code) = diag_code {
                    let message = self.regex_replacement_error_message(code, &actual_desc);
                    diagnostics.push(RegexCommandIssue::new(code, message.clone()));
                    self.report_type_error(TypeError::custom(message));
                } else {
                    self.push_constraint_with_span(
                        ConstraintKind::Convertible {
                            from: actual,
                            to: expected,
                        },
                        Some(note),
                        span,
                    );
                }
            }
            ConversionOutcome::Rejected(_) => {
                if let Some(code) = diag_code {
                    let message = self.regex_replacement_error_message(code, &actual_desc);
                    diagnostics.push(RegexCommandIssue::new(code, message.clone()));
                    self.report_type_error(TypeError::custom(message));
                } else {
                    let message = format!(
                        "正規表現の置換結果に String へ変換できない型 `{}` が使用されています。",
                        actual_desc
                    );
                    self.report_type_error(TypeError::custom(message));
                }
            }
        }
    }

    fn should_use_string_concatenation(&self, left: &TypeKind, right: &TypeKind) -> bool {
        self.is_explicit_string(left) || self.is_explicit_string(right)
    }

    fn is_explicit_string(&self, ty: &TypeKind) -> bool {
        match ty {
            TypeKind::Reference(name) => {
                matches!(name.as_str(), "java.lang.String" | "String")
            }
            TypeKind::Optional(inner) => self.is_explicit_string(inner),
            _ => false,
        }
    }
    fn regex_replacement_error_message(&self, code: &str, actual_desc: &str) -> String {
        match code {
            "JV_REGEX_E102" => messages::regex_lambda_return_mismatch_message(actual_desc),
            _ => format!(
                "{}: 正規表現の置換で想定外の型 `{}` が検出されました。",
                code, actual_desc
            ),
        }
    }

    fn collect_flag_diagnostics(
        &mut self,
        command: &RegexCommand,
        diagnostics: &mut Vec<RegexCommandIssue>,
    ) {
        if let Some(raw_flags) = &command.raw_flags {
            let mut reported = HashSet::new();
            for ch in raw_flags.chars() {
                let normalized = ch.to_ascii_lowercase();
                if !self.is_known_regex_flag(normalized) && reported.insert(normalized) {
                    let message = messages::regex_unknown_flag_message(ch);
                    diagnostics.push(RegexCommandIssue::new("JV_REGEX_E101", message.clone()));
                    self.report_type_error(TypeError::custom(message));
                }
            }
        }

        if let Some((primary, secondary)) = self.detect_literal_flag_conflict(&command.flags) {
            let message = messages::regex_flag_conflict_message(primary, secondary);
            diagnostics.push(RegexCommandIssue::new("JV_REGEX_E103", message.clone()));
            self.report_type_error(TypeError::custom(message));
        }
    }

    fn detect_mode_flag_confusion(&self, command: &RegexCommand) -> bool {
        if !matches!(command.mode, RegexCommandMode::Match) {
            return false;
        }

        if !matches!(command.mode_origin, RegexCommandModeOrigin::ShortMode) {
            return false;
        }

        if command
            .flags
            .iter()
            .any(|flag| matches!(flag, RegexFlag::Multiline))
        {
            return true;
        }

        command
            .raw_flags
            .as_ref()
            .is_some_and(|raw| raw.chars().any(|ch| matches!(ch, 'm' | 'M')))
    }

    fn is_known_regex_flag(&self, ch: char) -> bool {
        matches!(ch, 'i' | 'm' | 's' | 'u' | 'd' | 'x' | 'l' | 'c')
    }

    fn detect_literal_flag_conflict(&self, flags: &[RegexFlag]) -> Option<(char, char)> {
        if !flags.iter().any(|flag| matches!(flag, RegexFlag::Literal)) {
            return None;
        }

        let conflicting = flags
            .iter()
            .find(|flag| !matches!(flag, RegexFlag::Literal))?;

        Some(('l', self.flag_to_char(conflicting)))
    }

    fn flag_to_char(&self, flag: &RegexFlag) -> char {
        match flag {
            RegexFlag::CaseInsensitive => 'i',
            RegexFlag::Multiline => 'm',
            RegexFlag::DotAll => 's',
            RegexFlag::UnicodeCase => 'u',
            RegexFlag::UnixLines => 'd',
            RegexFlag::Comments => 'x',
            RegexFlag::Literal => 'l',
            RegexFlag::CanonEq => 'c',
        }
    }

    fn push_regex_constraint(
        &mut self,
        from: TypeKind,
        to: TypeKind,
        span: Option<&Span>,
        note: String,
        warn: bool,
    ) -> bool {
        match CompatibilityChecker::analyze(self.env, &from, &to) {
            ConversionOutcome::Rejected(_) => {
                self.report_type_error(TypeError::custom(note));
                false
            }
            _ => {
                let kind = if warn {
                    ConstraintKind::ConvertibleWithWarning { from, to }
                } else {
                    ConstraintKind::Convertible { from, to }
                };
                self.push_constraint_with_span(kind, Some(note.as_str()), span);
                true
            }
        }
    }

    fn regex_subject_error_message(&self, ty: &TypeKind) -> String {
        let ty_desc = ty.describe();
        messages::regex_subject_error_message(&ty_desc)
    }

    fn regex_optional_warning_message(&self) -> String {
        messages::regex_optional_warning_message()
    }

    fn regex_pattern_error_message(&self, ty: &TypeKind) -> String {
        let ty_desc = ty.describe();
        messages::regex_pattern_error_message(&ty_desc)
    }

    fn infer_unary_expression(&mut self, op: &UnaryOp, operand: &Expression) -> TypeKind {
        use UnaryOp::*;

        let operand_ty = self.infer_expression(operand);

        match op {
            Not => {
                let expected = TypeKind::primitive(PrimitiveType::Boolean);
                self.push_constraint_with_span(
                    ConstraintKind::Convertible {
                        from: operand_ty,
                        to: expected.clone(),
                    },
                    Some("logical not requires boolean"),
                    expression_span(operand),
                );
                expected
            }
            Minus | Plus | BitNot => operand_ty,
        }
    }

    fn type_from_literal(&mut self, literal: &Literal) -> TypeKind {
        match literal {
            Literal::String(_) => TypeKind::reference("java.lang.String"),
            Literal::Number(n) => {
                let lower = n.to_ascii_lowercase();
                let suffix = lower
                    .chars()
                    .last()
                    .filter(|ch| matches!(ch, 'f' | 'd' | 'l'));
                let core = suffix
                    .map(|_| &lower[..lower.len() - 1])
                    .unwrap_or(lower.as_str());

                match suffix {
                    Some('f') => TypeKind::primitive(PrimitiveType::Float),
                    Some('d') => TypeKind::primitive(PrimitiveType::Double),
                    Some('l') => TypeKind::primitive(PrimitiveType::Long),
                    _ => {
                        if core.contains('.') || core.contains('e') {
                            TypeKind::primitive(PrimitiveType::Double)
                        } else {
                            TypeKind::primitive(PrimitiveType::Int)
                        }
                    }
                }
            }
            Literal::Boolean(_) => TypeKind::primitive(PrimitiveType::Boolean),
            Literal::Character(_) => TypeKind::primitive(PrimitiveType::Char),
            Literal::Regex(_) => TypeKind::reference("java.util.regex.Pattern"),
            Literal::Null => TypeKind::Optional(Box::new(TypeKind::Unknown)),
        }
    }

    fn type_from_annotation(&mut self, annotation: &TypeAnnotation) -> TypeKind {
        match annotation {
            TypeAnnotation::Simple(name) => self.type_from_identifier(name),
            TypeAnnotation::Generic { name, .. } => self.type_from_identifier(name),
            TypeAnnotation::Nullable(inner) => {
                let inner_ty = self.type_from_annotation(inner);
                TypeKind::optional(inner_ty)
            }
            TypeAnnotation::Function {
                params,
                return_type,
            } => {
                let param_types = params
                    .iter()
                    .map(|ann| self.type_from_annotation(ann))
                    .collect();
                let ret_ty = self.type_from_annotation(return_type.as_ref());
                TypeKind::function(param_types, ret_ty)
            }
            _ => TypeKind::Unknown,
        }
    }

    fn type_from_identifier(&mut self, identifier: &str) -> TypeKind {
        match TypeFactory::from_annotation(identifier) {
            Ok(kind) => kind,
            Err(error) => {
                self.report_type_error(error);
                TypeKind::Unknown
            }
        }
    }

    fn enqueue_receiver_constraint(&mut self, id: TypeId, receiver: &str, property: &str) {
        let receiver_ty = match TypeFactory::from_annotation(receiver) {
            Ok(kind) => kind,
            Err(error) => {
                self.report_type_error(error);
                TypeKind::Unknown
            }
        };
        let note = format!(
            "extension receiver must be {} to call {}",
            receiver, property
        );
        let kind = ConstraintKind::Equal(TypeKind::Variable(id), receiver_ty);
        self.record_type_vars_in_constraint(&kind);
        let constraint = Constraint::new(kind).with_note(note);
        self.constraints.push(constraint);
    }

    fn report_type_error(&mut self, error: TypeError) {
        self.constraints
            .push(Constraint::new(ConstraintKind::ReportError(error)));
    }

    fn record_type_vars_in_constraint(&mut self, kind: &ConstraintKind) {
        match kind {
            ConstraintKind::Equal(left, right) => {
                self.record_type_vars(left);
                self.record_type_vars(right);
            }
            ConstraintKind::Assign(id, ty) => {
                *self.type_var_usage.entry(*id).or_default() += 1;
                self.record_type_vars(ty);
            }
            ConstraintKind::Convertible { from, to }
            | ConstraintKind::ConvertibleWithWarning { from, to } => {
                self.record_type_vars(from);
                self.record_type_vars(to);
            }
            ConstraintKind::Placeholder(_) | ConstraintKind::ReportError(_) => {}
        }
    }

    fn record_type_vars(&mut self, ty: &TypeKind) {
        match ty {
            TypeKind::Variable(id) => {
                *self.type_var_usage.entry(*id).or_default() += 1;
            }
            TypeKind::Optional(inner) => self.record_type_vars(inner),
            TypeKind::Function(params, result) => {
                for param in params {
                    self.record_type_vars(param);
                }
                self.record_type_vars(result);
            }
            TypeKind::Primitive(_)
            | TypeKind::Boxed(_)
            | TypeKind::Reference(_)
            | TypeKind::Unknown => {}
        }
    }

    fn apply_branch_narrowing(&mut self, snapshot: &NarrowingSnapshot) {
        for binding in snapshot.on_match() {
            self.apply_binding_narrowing(binding);
        }
    }

    fn apply_binding_narrowing(&mut self, binding: &NarrowedBinding) {
        match binding.nullability {
            NarrowedNullability::NonNull => {
                self.shadow_binding_as_non_null(&binding.variable);
            }
            NarrowedNullability::Nullable | NarrowedNullability::Unknown => {}
        }
    }

    fn shadow_binding_as_non_null(&mut self, name: &str) {
        if let Some(original) = self.env.lookup(name).cloned() {
            let narrowed_type = match original.ty {
                TypeKind::Optional(inner) => *inner,
                other => other,
            };
            self.env
                .define_scheme(name.to_string(), TypeScheme::monotype(narrowed_type));
        }
    }
}

fn nullability_from_type(ty: &TypeKind) -> NullabilityFlag {
    match ty {
        TypeKind::Optional(_) => NullabilityFlag::Nullable,
        TypeKind::Unknown | TypeKind::Variable(_) => NullabilityFlag::Unknown,
        TypeKind::Primitive(_)
        | TypeKind::Boxed(_)
        | TypeKind::Reference(_)
        | TypeKind::Function(_, _) => NullabilityFlag::NonNull,
    }
}

fn ensure_optional_type(ty: TypeKind) -> TypeKind {
    match ty {
        TypeKind::Optional(_) => ty,
        other => TypeKind::Optional(Box::new(other)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::BinaryMetadata;
    use jv_ast::{Modifiers, Pattern, Span, ValBindingOrigin, WhenArm};
    use jv_parser_frontend::ParserPipeline;
    use jv_parser_rowan::frontend::RowanPipeline;

    fn dummy_span() -> Span {
        Span::dummy()
    }

    fn default_modifiers() -> Modifiers {
        Modifiers::default()
    }

    fn collect_constraints(mut set: ConstraintSet) -> Vec<Constraint> {
        let mut items = Vec::new();
        while let Some(constraint) = set.pop() {
            items.push(constraint);
        }
        items
    }

    fn parse_program(source: &str) -> Program {
        RowanPipeline::default()
            .parse(source)
            .expect("source should parse for constraint generator tests")
            .into_program()
    }

    #[test]
    fn generates_constraints_for_val_literal() {
        let span = dummy_span();
        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![Statement::ValDeclaration {
                name: "x".into(),
                binding: None,

                type_annotation: None,
                initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
                modifiers: default_modifiers(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            }],
            span,
        };

        let mut env = TypeEnvironment::new();
        let extensions = ExtensionRegistry::new();
        let constraints = ConstraintGenerator::new(&mut env, &extensions, None).generate(&program);
        let collected = collect_constraints(constraints);

        assert!(collected.iter().any(|constraint| matches!(
            &constraint.kind,
            ConstraintKind::Equal(TypeKind::Variable(_), TypeKind::Primitive(primitive))
                if *primitive == PrimitiveType::Int
        )));

        let scheme = env.lookup("x").expect("symbol x must exist");
        assert_eq!(scheme.ty, TypeKind::primitive(PrimitiveType::Int));
    }

    #[test]
    fn arithmetic_binary_expression_enforces_operand_alignment() {
        let span = dummy_span();
        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![
                Statement::ValDeclaration {
                    name: "x".into(),
                    binding: None,

                    type_annotation: None,
                    initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
                    modifiers: default_modifiers(),
                    origin: ValBindingOrigin::ExplicitKeyword,
                    span: span.clone(),
                },
                Statement::ValDeclaration {
                    name: "sum".into(),
                    binding: None,

                    type_annotation: None,
                    initializer: Expression::Binary {
                        left: Box::new(Expression::Identifier("x".into(), span.clone())),
                        op: BinaryOp::Add,
                        right: Box::new(Expression::Literal(
                            Literal::Number("2".into()),
                            span.clone(),
                        )),
                        span: span.clone(),
                        metadata: BinaryMetadata::default(),
                    },
                    modifiers: default_modifiers(),
                    origin: ValBindingOrigin::ExplicitKeyword,
                    span: span.clone(),
                },
            ],
            span,
        };

        let mut env = TypeEnvironment::new();
        let extensions = ExtensionRegistry::new();
        let constraints = ConstraintGenerator::new(&mut env, &extensions, None).generate(&program);
        let collected = collect_constraints(constraints);

        assert!(collected.iter().any(|constraint| matches!(
            &constraint.kind,
            ConstraintKind::Equal(
                TypeKind::Primitive(PrimitiveType::Int),
                TypeKind::Primitive(PrimitiveType::Int)
            )
        )));

        let count_int_var = collected
            .iter()
            .filter(|constraint| {
                matches!(
                    &constraint.kind,
                    ConstraintKind::Equal(
                        TypeKind::Variable(_),
                        TypeKind::Primitive(PrimitiveType::Int)
                    )
                )
            })
            .count();
        assert!(count_int_var >= 2);

        let scheme = env.lookup("sum").expect("symbol sum must exist");
        assert_eq!(scheme.ty, TypeKind::primitive(PrimitiveType::Int));
    }

    #[test]
    fn when_expression_with_nullable_branch_infers_optional_result() {
        let span = dummy_span();
        let when_expr = Expression::When {
            expr: None,
            arms: vec![WhenArm {
                pattern: Pattern::Wildcard(span.clone()),
                guard: None,
                body: Expression::Literal(Literal::Null, span.clone()),
                span: span.clone(),
            }],
            else_arm: Some(Box::new(Expression::Literal(
                Literal::String("fallback".into()),
                span.clone(),
            ))),
            implicit_end: None,
            span: span.clone(),
        };

        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![Statement::ValDeclaration {
                name: "value".into(),
                binding: None,

                type_annotation: None,
                initializer: when_expr,
                modifiers: default_modifiers(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            }],
            span,
        };

        let mut env = TypeEnvironment::new();
        let extensions = ExtensionRegistry::new();
        let _constraints = ConstraintGenerator::new(&mut env, &extensions, None).generate(&program);

        let scheme = env.lookup("value").expect("value binding must exist");
        assert_eq!(
            scheme.ty,
            TypeKind::Optional(Box::new(TypeKind::reference("java.lang.String")))
        );
    }

    #[test]
    fn when_branch_narrowing_unwraps_optional_subject() {
        let program = parse_program(
            r#"
fun provide(): String? = null

maybe = provide()
val result = when (maybe) {
    is String -> maybe
    else -> ""
}
"#,
        );

        let mut env = TypeEnvironment::new();
        let extensions = ExtensionRegistry::new();
        let _constraints = ConstraintGenerator::new(&mut env, &extensions, None).generate(&program);

        let scheme = env.lookup("result").expect("result binding must exist");
        assert_eq!(scheme.ty, TypeKind::reference("java.lang.String"));
    }

    #[test]
    fn respects_shadowing_and_optional_null_literal() {
        let span = dummy_span();
        let inner_span = dummy_span();
        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![
                Statement::ValDeclaration {
                    name: "value".into(),
                    binding: None,

                    type_annotation: None,
                    initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
                    modifiers: default_modifiers(),
                    origin: ValBindingOrigin::ExplicitKeyword,
                    span: span.clone(),
                },
                Statement::Expression {
                    expr: Expression::Block {
                        statements: vec![
                            Statement::ValDeclaration {
                                name: "value".into(),
                                binding: None,

                                type_annotation: None,
                                initializer: Expression::Literal(
                                    Literal::Number("1.5".into()),
                                    inner_span.clone(),
                                ),
                                modifiers: default_modifiers(),
                                origin: ValBindingOrigin::ExplicitKeyword,
                                span: inner_span.clone(),
                            },
                            Statement::Expression {
                                expr: Expression::Identifier("value".into(), inner_span.clone()),
                                span: inner_span.clone(),
                            },
                        ],
                        span: inner_span.clone(),
                    },
                    span: inner_span.clone(),
                },
                Statement::ValDeclaration {
                    name: "maybe".into(),
                    binding: None,

                    type_annotation: None,
                    initializer: Expression::Literal(Literal::Null, span.clone()),
                    modifiers: default_modifiers(),
                    origin: ValBindingOrigin::ExplicitKeyword,
                    span: span.clone(),
                },
            ],
            span,
        };

        let mut env = TypeEnvironment::new();
        let extensions = ExtensionRegistry::new();
        let constraints = ConstraintGenerator::new(&mut env, &extensions, None).generate(&program);
        let collected = collect_constraints(constraints);

        assert!(collected.iter().any(|constraint| {
            if let ConstraintKind::Equal(TypeKind::Variable(_), TypeKind::Optional(inner)) =
                &constraint.kind
            {
                matches!(**inner, TypeKind::Unknown)
            } else {
                false
            }
        }));

        let outer = env.lookup("value").expect("outer value must exist");
        assert_eq!(outer.ty, TypeKind::primitive(PrimitiveType::Int));

        let maybe = env.lookup("maybe").expect("maybe binding must exist");
        match &maybe.ty {
            TypeKind::Optional(inner) => assert!(matches!(**inner, TypeKind::Unknown)),
            other => panic!("expected optional type, found {:?}", other),
        }
    }
}
