//! Null safety analysis built on top of the AST.
//!
//! 型推論エンジンと併用することを想定し、Optional 伝播と null セーフ演算子の
//! 振る舞いをモデル化して代入の安全性を検証する。現時点では Nullability の
//! 判定に特化し、将来的に詳細な型情報へ拡張できる構造を維持している。

use crate::CheckError;
use jv_ast::types::Span;
use jv_ast::{
    expression::Argument, BinaryOp, Expression, Literal, Program, Statement, TypeAnnotation,
    UnaryOp,
};
use std::collections::HashMap;

/// Null 許容性を三段階で表す。
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Nullability {
    NonNull,
    Optional,
    Unknown,
}

impl Nullability {
    fn from_annotation(annotation: &TypeAnnotation) -> Self {
        match annotation {
            TypeAnnotation::Nullable(_) => Nullability::Optional,
            _ => Nullability::NonNull,
        }
    }

    fn from_literal(literal: &Literal) -> Self {
        match literal {
            Literal::Null => Nullability::Optional,
            _ => Nullability::NonNull,
        }
    }

    fn combine(a: Self, b: Self) -> Self {
        use Nullability::*;
        match (a, b) {
            (Optional, _) | (_, Optional) => Optional,
            (Unknown, _) | (_, Unknown) => Unknown,
            _ => NonNull,
        }
    }
}

/// Nullability アナライザ本体。
#[derive(Default)]
pub struct NullabilityAnalyzer {
    scopes: Vec<HashMap<String, Nullability>>,
    errors: Vec<CheckError>,
}

impl NullabilityAnalyzer {
    /// プログラム全体の null 安全解析を実行する。
    pub fn analyze(program: &Program) -> Vec<CheckError> {
        let mut analyzer = Self::new();
        analyzer.visit_program(program);
        analyzer.errors
    }

    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
        }
    }

    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) -> Option<Nullability> {
        match statement {
            Statement::ValDeclaration {
                name,
                type_annotation,
                initializer,
                span,
                ..
            } => {
                let value_null = self.evaluate_expression(initializer);
                let annotated = type_annotation
                    .as_ref()
                    .map_or(Nullability::Unknown, Nullability::from_annotation);
                let final_null = self.enforce_assignment(name, annotated, value_null, span);
                self.define(name.clone(), final_null);
                None
            }
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer,
                span,
                ..
            } => {
                let value_null = initializer
                    .as_ref()
                    .map(|expr| self.evaluate_expression(expr))
                    .unwrap_or(Nullability::Unknown);
                let annotated = type_annotation
                    .as_ref()
                    .map_or(Nullability::Unknown, Nullability::from_annotation);
                let final_null = if annotated == Nullability::Unknown {
                    value_null
                } else {
                    self.enforce_assignment(name, annotated, value_null, span)
                };
                self.define(name.clone(), final_null);
                None
            }
            Statement::Assignment {
                target,
                value,
                span,
            } => {
                let target_name = self.assignment_target_name(target);
                let target_null = target_name
                    .as_ref()
                    .map(|name| self.lookup(name))
                    .unwrap_or_else(|| self.evaluate_expression(target));
                let value_null = self.evaluate_expression(value);

                if let Some(name) = target_name {
                    let final_null = self.enforce_assignment(&name, target_null, value_null, span);
                    self.update(&name, final_null);
                } else {
                    self.enforce_assignment("assignment target", target_null, value_null, span);
                }
                None
            }
            Statement::Expression { expr, .. } => Some(self.evaluate_expression(expr)),
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    Some(self.evaluate_expression(expr))
                } else {
                    None
                }
            }
            Statement::ForIn(for_in) => {
                self.evaluate_expression(&for_in.iterable);
                self.enter_scope();
                let annotated = for_in
                    .binding
                    .type_annotation
                    .as_ref()
                    .map_or(Nullability::Unknown, Nullability::from_annotation);
                self.define(for_in.binding.name.clone(), annotated);
                self.evaluate_expression(&for_in.body);
                self.leave_scope();
                None
            }
            Statement::FunctionDeclaration {
                parameters,
                body,
                span: _,
                ..
            } => {
                self.enter_scope();
                for param in parameters {
                    let annotated = param
                        .type_annotation
                        .as_ref()
                        .map_or(Nullability::Unknown, Nullability::from_annotation);
                    let default_null = param
                        .default_value
                        .as_ref()
                        .map(|expr| self.evaluate_expression(expr))
                        .unwrap_or(Nullability::Unknown);
                    let final_null = if annotated == Nullability::Unknown {
                        default_null
                    } else {
                        self.enforce_assignment(&param.name, annotated, default_null, &param.span)
                    };
                    self.define(param.name.clone(), final_null);
                }
                self.evaluate_expression(body);
                self.leave_scope();
                // Signal misuse if any parameter default caused an error.
                Some(Nullability::Unknown).map(|_| Nullability::Unknown)
            }
            Statement::Concurrency(construct) => {
                match construct {
                    jv_ast::statement::ConcurrencyConstruct::Spawn { body, .. }
                    | jv_ast::statement::ConcurrencyConstruct::Async { body, .. }
                    | jv_ast::statement::ConcurrencyConstruct::Await { expr: body, .. } => {
                        self.evaluate_expression(body);
                    }
                }
                None
            }
            Statement::ResourceManagement(resource) => {
                match resource {
                    jv_ast::statement::ResourceManagement::Use { resource, body, .. } => {
                        self.evaluate_expression(resource);
                        self.evaluate_expression(body);
                    }
                    jv_ast::statement::ResourceManagement::Defer { body, .. } => {
                        self.evaluate_expression(body);
                    }
                }
                None
            }
            Statement::ClassDeclaration {
                methods,
                properties,
                ..
            }
            | Statement::InterfaceDeclaration {
                methods,
                properties,
                ..
            } => {
                for prop in properties {
                    if let Some(initializer) = &prop.initializer {
                        let property_null = self.evaluate_expression(initializer);
                        let annotated = prop
                            .type_annotation
                            .as_ref()
                            .map_or(Nullability::Unknown, Nullability::from_annotation);
                        self.enforce_assignment(&prop.name, annotated, property_null, &prop.span);
                    }
                }
                for method in methods {
                    self.visit_statement(method);
                }
                None
            }
            Statement::DataClassDeclaration { .. }
            | Statement::Import { .. }
            | Statement::Package { .. }
            | Statement::ExtensionFunction(_)
            | Statement::Break(_)
            | Statement::Continue(_) => None,
        }
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> Nullability {
        match expr {
            Expression::Literal(literal, _) => Nullability::from_literal(literal),
            Expression::Identifier(name, _) => self.lookup(name),
            Expression::Binary {
                left, op, right, ..
            } => self.evaluate_binary(op, left, right),
            Expression::Unary { op, operand, .. } => {
                let operand_null = self.evaluate_expression(operand);
                match op {
                    UnaryOp::Not => Nullability::NonNull,
                    UnaryOp::Minus | UnaryOp::Plus | UnaryOp::BitNot => operand_null,
                }
            }
            Expression::Call { function, args, .. } => {
                self.evaluate_expression(function);
                for arg in args {
                    match arg {
                        Argument::Positional(expr) => {
                            self.evaluate_expression(expr);
                        }
                        Argument::Named { value, .. } => {
                            self.evaluate_expression(value);
                        }
                    }
                }
                Nullability::Unknown
            }
            Expression::MemberAccess { object, .. } => {
                self.evaluate_expression(object);
                Nullability::Unknown
            }
            Expression::NullSafeMemberAccess { object, .. } => {
                self.evaluate_expression(object);
                Nullability::Optional
            }
            Expression::IndexAccess { object, index, .. } => {
                self.evaluate_expression(object);
                self.evaluate_expression(index);
                Nullability::Unknown
            }
            Expression::NullSafeIndexAccess { object, index, .. } => {
                self.evaluate_expression(object);
                self.evaluate_expression(index);
                Nullability::Optional
            }
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let jv_ast::expression::StringPart::Expression(expr) = part {
                        self.evaluate_expression(expr);
                    }
                }
                Nullability::NonNull
            }
            Expression::When {
                expr: scrutinee,
                arms,
                else_arm,
                implicit_end: _,
                ..
            } => {
                if let Some(condition) = scrutinee {
                    self.evaluate_expression(condition);
                }
                let mut result = Nullability::Unknown;
                for arm in arms {
                    if let Some(guard_expr) = &arm.guard {
                        self.evaluate_expression(guard_expr);
                    }
                    let body_null = self.evaluate_expression(&arm.body);
                    result = Nullability::combine(result, body_null);
                }
                if let Some(else_body) = else_arm {
                    result = Nullability::combine(result, self.evaluate_expression(else_body));
                }
                result
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.evaluate_expression(condition);
                let then_null = self.evaluate_expression(then_branch);
                if let Some(else_expr) = else_branch {
                    let else_null = self.evaluate_expression(else_expr);
                    Nullability::combine(then_null, else_null)
                } else {
                    Nullability::Unknown
                }
            }
            Expression::Block { statements, .. } => {
                self.enter_scope();
                let mut last = Nullability::Unknown;
                for stmt in statements {
                    if let Some(result) = self.visit_statement(stmt) {
                        last = result;
                    }
                }
                self.leave_scope();
                last
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.evaluate_expression(element);
                }
                Nullability::NonNull
            }
            Expression::Lambda {
                body, parameters, ..
            } => {
                self.enter_scope();
                for param in parameters {
                    let annotated = param
                        .type_annotation
                        .as_ref()
                        .map_or(Nullability::Unknown, Nullability::from_annotation);
                    self.define(param.name.clone(), annotated);
                }
                self.evaluate_expression(body);
                self.leave_scope();
                Nullability::NonNull
            }
            Expression::Try {
                body,
                catch_clauses,
                finally_block,
                ..
            } => {
                let mut state = self.evaluate_expression(body);
                for clause in catch_clauses {
                    let catch_state = self.evaluate_expression(&clause.body);
                    state = Nullability::combine(state, catch_state);
                }
                if let Some(finally_expr) = finally_block.as_deref() {
                    let finally_state = self.evaluate_expression(finally_expr);
                    state = Nullability::combine(state, finally_state);
                }
                state
            }
            Expression::MultilineString(_) => Nullability::NonNull,
            Expression::JsonLiteral(_) => Nullability::NonNull,
            Expression::This(_) | Expression::Super(_) => Nullability::NonNull,
        }
    }

    fn evaluate_binary(
        &mut self,
        op: &BinaryOp,
        left: &Expression,
        right: &Expression,
    ) -> Nullability {
        use BinaryOp::*;

        let left_null = self.evaluate_expression(left);
        let right_null = self.evaluate_expression(right);

        match op {
            Elvis => match left_null {
                Nullability::NonNull => Nullability::NonNull,
                Nullability::Optional => match right_null {
                    Nullability::NonNull => Nullability::NonNull,
                    Nullability::Optional | Nullability::Unknown => Nullability::Optional,
                },
                Nullability::Unknown => right_null,
            },
            _ => Nullability::NonNull,
        }
    }

    fn enforce_assignment(
        &mut self,
        subject: &str,
        expected: Nullability,
        value: Nullability,
        span: &Span,
    ) -> Nullability {
        if matches!(expected, Nullability::NonNull) && matches!(value, Nullability::Optional) {
            let message = format!(
                "{}:{} `{}` は非 null として宣言されていますが Optional の値が代入されました。`?.` や `!!` で明示的に処理してください。",
                span.start_line, span.start_column, subject
            );
            self.errors.push(CheckError::NullSafetyError(message));
            Nullability::NonNull
        } else if matches!(expected, Nullability::Unknown) {
            value
        } else {
            expected
        }
    }

    fn define(&mut self, name: String, nullability: Nullability) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, nullability);
        }
    }

    fn update(&mut self, name: &str, nullability: Nullability) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), nullability);
                return;
            }
        }
    }

    fn lookup(&self, name: &str) -> Nullability {
        for scope in self.scopes.iter().rev() {
            if let Some(nullability) = scope.get(name) {
                return *nullability;
            }
        }
        Nullability::Unknown
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn leave_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    fn assignment_target_name(&self, expr: &Expression) -> Option<String> {
        match expr {
            Expression::Identifier(name, _) => Some(name.clone()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::types::{Modifiers, Visibility};

    fn span() -> Span {
        Span::new(1, 1, 1, 5)
    }

    fn modifiers() -> Modifiers {
        Modifiers {
            visibility: Visibility::Private,
            ..Modifiers::default()
        }
    }

    fn val_decl(
        name: &str,
        annotation: Option<TypeAnnotation>,
        initializer: Expression,
    ) -> Statement {
        Statement::ValDeclaration {
            name: name.into(),
            type_annotation: annotation,
            initializer,
            modifiers: modifiers(),
            span: span(),
        }
    }

    fn program_with(statements: Vec<Statement>) -> Program {
        Program {
            package: None,
            imports: Vec::new(),
            statements,
            span: span(),
        }
    }

    #[test]
    fn reports_optional_assignment_to_non_null_binding() {
        let nullable_int = TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple("Int".into())));
        let program = program_with(vec![
            val_decl(
                "maybe",
                Some(nullable_int.clone()),
                Expression::Literal(Literal::Null, span()),
            ),
            val_decl(
                "strict",
                Some(TypeAnnotation::Simple("Int".into())),
                Expression::Identifier("maybe".into(), span()),
            ),
        ]);

        let errors = NullabilityAnalyzer::analyze(&program);
        assert!(
            matches!(errors.as_slice(), [CheckError::NullSafetyError(message)] if message.contains("strict"))
        );
    }

    #[test]
    fn elvis_operator_provides_non_null_fallback() {
        let nullable_int = TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple("Int".into())));
        let elvis_expr = Expression::Binary {
            left: Box::new(Expression::Identifier("maybe".into(), span())),
            op: BinaryOp::Elvis,
            right: Box::new(Expression::Literal(Literal::Number("0".into()), span())),
            span: span(),
        };

        let program = program_with(vec![
            val_decl(
                "maybe",
                Some(nullable_int),
                Expression::Literal(Literal::Null, span()),
            ),
            val_decl(
                "safe",
                Some(TypeAnnotation::Simple("Int".into())),
                elvis_expr,
            ),
        ]);

        let errors = NullabilityAnalyzer::analyze(&program);
        assert!(errors.is_empty());
    }

    #[test]
    fn detects_null_safe_member_access_to_non_null_target() {
        let nullable_string =
            TypeAnnotation::Nullable(Box::new(TypeAnnotation::Simple("String".into())));
        let access_expr = Expression::NullSafeMemberAccess {
            object: Box::new(Expression::Identifier("maybe".into(), span())),
            property: "length".into(),
            span: span(),
        };

        let program = program_with(vec![
            val_decl(
                "maybe",
                Some(nullable_string),
                Expression::Literal(Literal::Null, span()),
            ),
            val_decl(
                "len",
                Some(TypeAnnotation::Simple("Int".into())),
                access_expr,
            ),
        ]);

        let errors = NullabilityAnalyzer::analyze(&program);
        assert!(
            matches!(errors.as_slice(), [CheckError::NullSafetyError(message)] if message.contains("len"))
        );
    }
}
