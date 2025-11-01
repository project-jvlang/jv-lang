//! AST を走査して型制約を生成するビジター実装。
//!
//! `ConstraintGenerator` はスコープ管理を `TypeEnvironment` に委ねつつ、
//! val/var 宣言や代表的な式から型整合性と null 許容性の制約を収集する。

use crate::inference::compatibility::CompatibilityChecker;
use crate::inference::constraint::{Constraint, ConstraintKind, ConstraintSet};
use crate::inference::conversions::ConversionOutcome;
use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::extensions::ExtensionRegistry;
use crate::inference::imports::ImportRegistry;
use crate::inference::iteration::{
    LoopClassification, classify_loop, expression_can_yield_iterable,
};
use crate::inference::type_factory::TypeFactory;
use crate::inference::types::{PrimitiveType, TypeError, TypeId, TypeKind};
use crate::pattern::{
    NarrowedBinding, NarrowedNullability, NarrowingSnapshot, PatternMatchService, PatternTarget,
};
use jv_ast::{
    statement::{UnitTypeDefinition, UnitTypeMember},
    Argument, BinaryOp, Expression, ForInStatement, Literal, Parameter, Program, Span, Statement,
    TypeAnnotation, UnaryOp,
};
use jv_inference::types::NullabilityFlag;
use std::collections::HashMap;

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
        self.record_type_vars_in_constraint(&kind);
        let constraint = if let Some(text) = note {
            Constraint::new(kind).with_note(text)
        } else {
            Constraint::new(kind)
        };
        self.constraints.push(constraint);
    }

    fn push_assignability_constraint(&mut self, from: TypeKind, to: TypeKind, note: Option<&str>) {
        match CompatibilityChecker::analyze(self.env, &from, &to) {
            ConversionOutcome::Identity => {
                self.push_constraint(ConstraintKind::Equal(to, from), note);
            }
            ConversionOutcome::Allowed(_) => {
                self.push_constraint(ConstraintKind::Convertible { from, to }, note);
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
                self.bind_symbol(name, type_annotation.as_ref(), Some(init_ty));
            }
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                let init_ty = initializer.as_ref().map(|expr| self.infer_expression(expr));
                self.bind_symbol(name, type_annotation.as_ref(), init_ty);
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
                        );
                    }
                }

                let body_ty = self.infer_expression(body);
                self.push_assignability_constraint(
                    body_ty,
                    return_ty.clone(),
                    Some("function body must satisfy return type"),
                );

                self.env.leave_scope();
            }
            Statement::UnitTypeDefinition(definition) => {
                self.visit_unit_definition(definition);
            }
            _ => {
                // そのほかの文でも子ノードを可能な範囲で評価しておく。
            }
        }
    }

    fn visit_unit_definition(&mut self, definition: &UnitTypeDefinition) {
        for member in &definition.members {
            match member {
                UnitTypeMember::Dependency(dependency) => {
                    if let Some(expr) = dependency.value.as_ref() {
                        self.infer_expression(expr);
                    }
                }
                UnitTypeMember::Conversion(block) => {
                    for statement in &block.body {
                        self.visit_statement(statement);
                    }
                }
                UnitTypeMember::NestedStatement(statement) => {
                    self.visit_statement(statement);
                }
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
        initializer_ty: Option<TypeKind>,
    ) {
        let symbol_ty = self.env.fresh_type_variable();
        let mut representative = initializer_ty.clone().unwrap_or_else(|| TypeKind::Unknown);

        if let Some(ann) = annotation {
            let annotated = self.type_from_annotation(ann);
            if !matches!(annotated, TypeKind::Unknown) {
                self.push_constraint(
                    ConstraintKind::Equal(symbol_ty.clone(), annotated.clone()),
                    Some("declaration annotation must match symbol"),
                );
                representative = annotated;
            }
        }

        if let Some(init) = initializer_ty {
            self.push_assignability_constraint(
                init.clone(),
                symbol_ty.clone(),
                Some("initializer must satisfy declaration"),
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
                left, op, right, ..
            } => self.infer_binary_expression(op, left, right),
            Expression::Unary { op, operand, .. } => self.infer_unary_expression(op, operand),
            Expression::Call { function, args, .. } => {
                let fn_ty = self.infer_expression(function);
                let mut argument_types = Vec::with_capacity(args.len());
                for arg in args {
                    let ty = match arg {
                        Argument::Positional(expr) => self.infer_expression(expr),
                        Argument::Named { value, .. } => self.infer_expression(value),
                    };
                    argument_types.push(ty);
                }

                let result_ty = self.env.fresh_type_variable();
                let expected_fn = TypeKind::function(argument_types, result_ty.clone());
                self.push_constraint(
                    ConstraintKind::Equal(fn_ty, expected_fn),
                    Some("call target must be compatible with argument list"),
                );
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
                self.push_constraint(
                    ConstraintKind::Convertible {
                        from: cond_ty,
                        to: TypeKind::primitive(PrimitiveType::Boolean),
                    },
                    Some("if condition requires boolean"),
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
            Expression::UnitLiteral { value, .. } => self.infer_expression(value),
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
                        self.push_constraint(
                            ConstraintKind::Convertible {
                                from: guard_ty,
                                to: TypeKind::primitive(PrimitiveType::Boolean),
                            },
                            Some("when guard must evaluate to boolean"),
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
            Expression::MultilineString(_) => TypeKind::reference("java.lang.String"),
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
    ) -> TypeKind {
        use BinaryOp::*;

        let left_ty = self.infer_expression(left);
        let right_ty = self.infer_expression(right);

        match op {
            Add | Subtract | Multiply | Divide | Modulo | PlusAssign | MinusAssign
            | MultiplyAssign | DivideAssign => {
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
                self.push_constraint(
                    ConstraintKind::Convertible {
                        from: left_ty.clone(),
                        to: expected.clone(),
                    },
                    Some("logical operations require boolean operands"),
                );
                self.push_constraint(
                    ConstraintKind::Convertible {
                        from: right_ty.clone(),
                        to: expected.clone(),
                    },
                    Some("logical operations require boolean operands"),
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
            Is => TypeKind::primitive(PrimitiveType::Boolean),
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

    fn infer_unary_expression(&mut self, op: &UnaryOp, operand: &Expression) -> TypeKind {
        use UnaryOp::*;

        let operand_ty = self.infer_expression(operand);

        match op {
            Not => {
                let expected = TypeKind::primitive(PrimitiveType::Boolean);
                self.push_constraint(
                    ConstraintKind::Convertible {
                        from: operand_ty,
                        to: expected.clone(),
                    },
                    Some("logical not requires boolean"),
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
