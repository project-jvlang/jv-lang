//! Doublebrace 初期化式のプランニング補助。

use super::{DoublebracePlan, DoublebracePlanError, plan_doublebrace_application};
use crate::CheckError;
use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::type_factory::TypeFactory;
use crate::inference::types::{PrimitiveType, TypeKind};
use jv_ast::expression::{CallKind, DoublebraceInit};
use jv_ast::{Expression, Program, Statement, TypeAnnotation};
use jv_build::metadata::SymbolIndex;
use jv_inference::doublebrace::{DoublebraceContext, DoublebraceHeuristics, infer_doublebrace};
use jv_inference::session::InferenceSession;
use jv_inference::registry::default_impl::DefaultImplementationRegistry;
use std::collections::HashMap;

/// Doublebrace プランのマップでキーとして利用する `Span` の正規化文字列。
pub fn span_key(span: &jv_ast::Span) -> String {
    format!(
        "{}:{}-{}:{}",
        span.start_line, span.start_column, span.end_line, span.end_column
    )
}

fn annotation_to_type_kind(annotation: &TypeAnnotation) -> Option<TypeKind> {
    match annotation {
        TypeAnnotation::Simple(name) => TypeFactory::from_annotation(name).ok(),
        TypeAnnotation::Nullable(inner) => {
            annotation_to_type_kind(inner).map(TypeKind::optional)
        }
        TypeAnnotation::Generic { name, type_args } => {
            let base = TypeFactory::from_annotation(name).ok()?.describe();
            let args: Option<Vec<String>> = type_args
                .iter()
                .map(|arg| annotation_to_type_kind(arg).map(|ty| ty.describe()))
                .collect();
            let args = args?;
            Some(TypeKind::reference(format!("{base}<{}>", args.join(", "))))
        }
        TypeAnnotation::Function { .. } => Some(TypeKind::Unknown),
        TypeAnnotation::Array(inner) => annotation_to_type_kind(inner)
            .map(|element| TypeKind::reference(format!("Array<{}>", element.describe()))),
    }
}

fn resolve_expression_type_in_env(
    environment: &TypeEnvironment,
    expr: &Expression,
) -> Option<TypeKind> {
    match expr {
        Expression::Identifier(name, _) => environment
            .lookup(name)
            .map(|scheme| scheme.ty.clone()),
        Expression::This(_) => environment
            .lookup("this")
            .map(|scheme| scheme.ty.clone()),
        Expression::Call {
            call_kind,
            type_arguments,
            ..
        } => match call_kind {
            CallKind::Constructor { type_name, fqcn } => {
                let mut base = fqcn.clone().unwrap_or_else(|| type_name.clone());
                if !type_arguments.is_empty() {
                    let generics: Option<Vec<String>> = type_arguments
                        .iter()
                        .map(|ann| annotation_to_type_kind(ann).map(|ty| ty.describe()))
                        .collect();
                    if let Some(args) = generics {
                        base = format!("{base}<{}>", args.join(", "));
                    }
                }
                Some(TypeKind::reference(base))
            }
            CallKind::Function => None,
        },
        Expression::TypeCast { target, .. } => annotation_to_type_kind(target),
        _ => None,
    }
}

fn type_kind_to_reference_name(ty: &TypeKind) -> Option<String> {
    match ty {
        TypeKind::Reference(name) => Some(name.clone()),
        TypeKind::Optional(inner) => type_kind_to_reference_name(inner),
        TypeKind::Primitive(primitive) => Some(primitive.boxed_fqcn().to_string()),
        TypeKind::Boxed(primitive) => Some(primitive.boxed_fqcn().to_string()),
        _ => None,
    }
}

fn split_type_name(candidate: &str) -> (&str, Option<&str>) {
    if let Some(start) = candidate.find('<') {
        let (base, generics) = candidate.split_at(start);
        (base.trim(), Some(generics.trim()))
    } else {
        (candidate.trim(), None)
    }
}

fn resolve_default_reference(candidate: &str, symbol_index: Option<&SymbolIndex>) -> String {
    let registry = DefaultImplementationRegistry::shared();
    if let Some(interface) = registry.resolve_interface(candidate) {
        interface.target().to_string()
    } else if let Some(abstract_impl) = registry.resolve_abstract(candidate, symbol_index) {
        abstract_impl.target().to_string()
    } else {
        candidate.to_string()
    }
}

fn apply_registry_to_reference(candidate: &str, symbol_index: Option<&SymbolIndex>) -> String {
    let (base, generics) = split_type_name(candidate);
    let resolved = resolve_default_reference(base, symbol_index);
    if let Some(args) = generics {
        if args.is_empty() || resolved.contains('<') {
            resolved
        } else {
            format!("{resolved}{args}")
        }
    } else {
        resolved
    }
}

fn apply_registry_to_typekind(ty: &TypeKind, symbol_index: Option<&SymbolIndex>) -> TypeKind {
    match ty {
        TypeKind::Reference(name) => {
            TypeKind::reference(apply_registry_to_reference(name, symbol_index))
        }
        TypeKind::Optional(inner) => {
            TypeKind::optional(apply_registry_to_typekind(inner, symbol_index))
        }
        TypeKind::Function(params, ret) => {
            let mapped_params: Vec<TypeKind> = params
                .iter()
                .map(|param| apply_registry_to_typekind(param, symbol_index))
                .collect();
            let mapped_ret = apply_registry_to_typekind(ret, symbol_index);
            TypeKind::function(mapped_params, mapped_ret)
        }
        TypeKind::Primitive(_) | TypeKind::Boxed(_) | TypeKind::Variable(_) | TypeKind::Unknown => {
            ty.clone()
        }
    }
}

fn apply_registry_to_environment(env: &mut TypeEnvironment, symbol_index: Option<&SymbolIndex>) {
    let bindings = env.flattened_bindings();
    for (name, scheme) in bindings {
        let mapped = apply_registry_to_typekind(&scheme.ty, symbol_index);
        if mapped != scheme.ty {
            let new_scheme = TypeScheme::new(scheme.quantifiers.clone(), mapped);
            env.redefine_scheme(name, new_scheme);
        }
    }
}

fn infer_binding_type_for_doublebrace(
    environment: &TypeEnvironment,
    init: &DoublebraceInit,
    annotation: Option<&TypeAnnotation>,
    symbol_index: Option<&SymbolIndex>,
) -> Option<TypeKind> {
    let base_ty = init
        .base
        .as_ref()
        .and_then(|expr| resolve_expression_type_in_env(environment, expr));
    let hint_ty = init
        .receiver_hint
        .as_ref()
        .and_then(annotation_to_type_kind);
    let expected_ty = annotation.and_then(annotation_to_type_kind);

    let base_name = base_ty
        .as_ref()
        .and_then(|ty| type_kind_to_reference_name(ty));
    let expected_name = expected_ty
        .as_ref()
        .and_then(|ty| type_kind_to_reference_name(ty));
    let hint_name = hint_ty
        .as_ref()
        .and_then(|ty| type_kind_to_reference_name(ty));

    let context = DoublebraceContext {
        base_type: base_name.as_deref(),
        expected_type: expected_name.as_deref(),
        receiver_hint: hint_name.as_deref(),
    };

    let session = InferenceSession::new();
    let inference = infer_doublebrace(init, context, &session);

    if let Some(resolved) = inference.resolved_type {
        return Some(TypeKind::reference(resolved));
    }

    if let Some(hint) = &hint_ty {
        return Some(apply_registry_to_typekind(hint, symbol_index));
    }

    if let Some(expected) = &expected_ty {
        return Some(apply_registry_to_typekind(expected, symbol_index));
    }

    base_ty.map(|ty| apply_registry_to_typekind(&ty, symbol_index))
}

fn prepopulate_doublebrace_bindings(
    environment: &mut TypeEnvironment,
    program: &Program,
    symbol_index: Option<&SymbolIndex>,
) {
    for statement in &program.statements {
        match statement {
            Statement::ValDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                if environment.lookup(name).is_none() {
                    if let Expression::DoublebraceInit(init) = initializer {
                        if let Some(ty) = infer_binding_type_for_doublebrace(
                            environment,
                            init,
                            type_annotation.as_ref(),
                            symbol_index,
                        ) {
                            environment.define_monotype(name.clone(), ty);
                        }
                    }
                }
            }
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer: Some(initializer),
                ..
            } => {
                if environment.lookup(name).is_none() {
                    if let Expression::DoublebraceInit(init) = initializer {
                        if let Some(ty) = infer_binding_type_for_doublebrace(
                            environment,
                            init,
                            type_annotation.as_ref(),
                            symbol_index,
                        ) {
                            environment.define_monotype(name.clone(), ty);
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// プログラム全体の Doublebrace 初期化式をプランニングする。
pub fn plan_doublebrace_in_program(
    program: &Program,
    environment: &TypeEnvironment,
    symbol_index: Option<&SymbolIndex>,
) -> Result<HashMap<String, DoublebracePlan>, Vec<CheckError>> {
    let mut prepared_env = environment.clone();
    apply_registry_to_environment(&mut prepared_env, symbol_index);
    prepopulate_doublebrace_bindings(&mut prepared_env, program, symbol_index);

    let mut planner = DoublebracePlanner::new(&prepared_env, symbol_index);
    planner.visit_program(program);
    if planner.errors.is_empty() {
        Ok(planner.plans)
    } else {
        Err(planner.errors)
    }
}

struct DoublebracePlanner<'env> {
    environment: &'env TypeEnvironment,
    symbol_index: Option<&'env SymbolIndex>,
    plans: HashMap<String, DoublebracePlan>,
    errors: Vec<CheckError>,
}

impl<'env> DoublebracePlanner<'env> {
    fn new(environment: &'env TypeEnvironment, symbol_index: Option<&'env SymbolIndex>) -> Self {
        Self {
            environment,
            symbol_index,
            plans: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::ValDeclaration {
                name,
                type_annotation,
                initializer,
                ..
            } => {
                let expected = self.resolve_binding_type(name, type_annotation.as_ref());
                self.visit_expression(initializer, expected.as_ref());
            }
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer: Some(initializer),
                ..
            } => {
                let expected = self.resolve_binding_type(name, type_annotation.as_ref());
                self.visit_expression(initializer, expected.as_ref());
            }
            Statement::VarDeclaration {
                initializer: None, ..
            } => {}
            Statement::FunctionDeclaration {
                body, parameters, ..
            } => {
                for parameter in parameters {
                    if let Some(default) = parameter.default_value.as_ref() {
                        let expected = self
                            .annotation_to_type(parameter.type_annotation.as_ref())
                            .or_else(|| self.resolve_binding_type(&parameter.name, None));
                        self.visit_expression(default, expected.as_ref());
                    }
                }
                self.visit_expression(body, None);
            }
            Statement::ClassDeclaration {
                properties,
                methods,
                ..
            }
            | Statement::InterfaceDeclaration {
                properties,
                methods,
                ..
            } => {
                for property in properties {
                    if let Some(initializer) = property.initializer.as_ref() {
                        let expected = self.annotation_to_type(property.type_annotation.as_ref());
                        self.visit_expression(initializer, expected.as_ref());
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
            }
            Statement::DataClassDeclaration { parameters, .. } => {
                for parameter in parameters {
                    if let Some(default) = parameter.default_value.as_ref() {
                        let expected = self.annotation_to_type(parameter.type_annotation.as_ref());
                        self.visit_expression(default, expected.as_ref());
                    }
                }
            }
            Statement::ExtensionFunction(extension) => {
                self.visit_statement(&extension.function);
            }
            Statement::Expression { expr, .. } => self.visit_expression(expr, None),
            Statement::Return { value, .. } => {
                if let Some(expr) = value.as_ref() {
                    self.visit_expression(expr, None);
                }
            }
            Statement::Throw { expr, .. } => self.visit_expression(expr, None),
            Statement::Assignment { value, target, .. } => {
                let expected = self.resolve_expression_type(target);
                self.visit_expression(value, expected.as_ref());
            }
            Statement::ForIn(for_in) => {
                self.visit_expression(&for_in.iterable, None);
                self.visit_expression(&for_in.body, None);
            }
            Statement::Concurrency(construct) => match construct {
                jv_ast::ConcurrencyConstruct::Spawn { body, .. }
                | jv_ast::ConcurrencyConstruct::Async { body, .. } => {
                    self.visit_expression(body, None);
                }
                jv_ast::ConcurrencyConstruct::Await { expr, .. } => {
                    self.visit_expression(expr, None);
                }
            },
            Statement::ResourceManagement(resource) => match resource {
                jv_ast::ResourceManagement::Use { resource, body, .. } => {
                    self.visit_expression(resource, None);
                    self.visit_expression(body, None);
                }
                jv_ast::ResourceManagement::Defer { body, .. } => {
                    self.visit_expression(body, None);
                }
            },
            Statement::Comment(_)
            | Statement::Break(_)
            | Statement::Continue(_)
            | Statement::Import { .. }
            | Statement::Package { .. } => {}
        }
    }

    fn visit_expression(&mut self, expression: &Expression, expected: Option<&TypeKind>) {
        match expression {
            Expression::DoublebraceInit(init) => {
                self.plan_doublebrace(init, expected.cloned());
            }
            Expression::Block { statements, .. } => {
                for statement in statements {
                    self.visit_statement(statement);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.visit_expression(
                    condition,
                    Some(&TypeKind::primitive(PrimitiveType::Boolean)),
                );
                self.visit_expression(then_branch, expected);
                if let Some(expr) = else_branch.as_deref() {
                    self.visit_expression(expr, expected);
                }
            }
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                if let Some(subject) = expr.as_deref() {
                    self.visit_expression(subject, None);
                }
                for arm in arms {
                    self.visit_expression(&arm.body, expected);
                }
                if let Some(expr) = else_arm.as_deref() {
                    self.visit_expression(expr, expected);
                }
            }
            Expression::Call { function, args, .. } => {
                self.visit_expression(function, None);
                for arg in args {
                    match arg {
                        jv_ast::Argument::Positional(expr) => self.visit_expression(expr, None),
                        jv_ast::Argument::Named { value, .. } => {
                            self.visit_expression(value, None);
                        }
                    }
                }
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                for parameter in parameters {
                    if let Some(default) = parameter.default_value.as_ref() {
                        let expected = self.annotation_to_type(parameter.type_annotation.as_ref());
                        self.visit_expression(default, expected.as_ref());
                    }
                }
                self.visit_expression(body, None);
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                self.visit_expression(body, expected);
                for clause in catch_clauses {
                    self.visit_expression(&clause.body, expected);
                }
                if let Some(finally_expr) = finally_block.as_deref() {
                    self.visit_expression(finally_expr, None);
                }
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.visit_expression(element, None);
                }
            }
            Expression::Binary { left, right, .. } => {
                self.visit_expression(left, None);
                self.visit_expression(right, None);
            }
            Expression::Unary { operand, .. } => self.visit_expression(operand, None),
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. }
            | Expression::IndexAccess { object, .. }
            | Expression::NullSafeIndexAccess { object, .. } => self.visit_expression(object, None),
            Expression::TypeCast { expr, .. } => self.visit_expression(expr, expected),
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let jv_ast::StringPart::Expression(inner) = part {
                        self.visit_expression(inner, None);
                    }
                }
            }
            Expression::Literal(_, _)
            | Expression::RegexLiteral(_)
            | Expression::Identifier(_, _)
            | Expression::MultilineString(_)
            | Expression::JsonLiteral(_)
            | Expression::This(_)
            | Expression::Super(_) => {}
        }
    }

    fn plan_doublebrace(&mut self, init: &DoublebraceInit, expected: Option<TypeKind>) {
        let target_type = expected
            .or_else(|| {
                init.receiver_hint
                    .as_ref()
                    .and_then(|hint| self.annotation_to_type(Some(hint)))
            })
            .or_else(|| self.heuristic_receiver(init));

        let Some(target_type) = target_type else {
            self.errors.push(CheckError::ValidationError {
                message: "Doublebrace 初期化ブロックのレシーバー型を特定できません。型注釈を追加してください。".into(),
                span: Some(init.span.clone()),
            });
            return;
        };

        let base_type = init
            .base
            .as_ref()
            .and_then(|expr| self.resolve_expression_type(expr));

        match plan_doublebrace_application(
            base_type.as_ref(),
            &target_type,
            init,
            self.symbol_index,
        ) {
            Ok(plan) => {
                self.plans.insert(span_key(&init.span), plan);
            }
            Err(error) => {
                self.errors.push(self.convert_plan_error(error, init));
            }
        }
    }

    fn resolve_binding_type(
        &self,
        name: &str,
        annotation: Option<&TypeAnnotation>,
    ) -> Option<TypeKind> {
        self.environment
            .lookup(name)
            .map(|scheme| scheme.ty.clone())
            .or_else(|| annotation.and_then(|ann| self.annotation_to_type(Some(ann))))
    }

    fn resolve_expression_type(&self, expr: &Expression) -> Option<TypeKind> {
        resolve_expression_type_in_env(self.environment, expr)
    }

    fn annotation_to_type(&self, annotation: Option<&TypeAnnotation>) -> Option<TypeKind> {
        annotation.and_then(annotation_to_type_kind)
    }

    fn heuristic_receiver(&self, init: &DoublebraceInit) -> Option<TypeKind> {
        let interface = DoublebraceHeuristics::infer_interface(&init.statements)?;
        if let Some(default_impl) =
            DoublebraceHeuristics::resolve_default_implementation(&interface)
        {
            return Some(TypeKind::reference(default_impl));
        }
        Some(TypeKind::reference(interface))
    }

    fn convert_plan_error(
        &self,
        error: DoublebracePlanError,
        init: &DoublebraceInit,
    ) -> CheckError {
        match error {
            DoublebracePlanError::UnsupportedReceiverType { ty } => CheckError::ValidationError {
                message: format!(
                    "Doublebrace 初期化ブロックのレシーバー型 `{ty}` を計画できません。"
                ),
                span: Some(init.span.clone()),
            },
            DoublebracePlanError::CopyUnavailable { receiver, reason } => {
                CheckError::ValidationError {
                    message: format!(
                        "Doublebrace のコピー計画を構築できません（型: `{receiver}`）: {reason}"
                    ),
                    span: Some(init.span.clone()),
                }
            }
        }
    }
}
