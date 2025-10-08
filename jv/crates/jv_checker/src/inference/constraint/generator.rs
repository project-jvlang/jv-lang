//! AST を走査して型制約を生成するビジター実装。
//!
//! `ConstraintGenerator` はスコープ管理を `TypeEnvironment` に委ねつつ、
//! val/var 宣言や代表的な式から型整合性と null 許容性の制約を収集する。

use crate::inference::constraint::{Constraint, ConstraintKind, ConstraintSet};
use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::iteration::{
    classify_loop, expression_can_yield_iterable, LoopClassification,
};
use crate::inference::types::TypeKind;
use jv_ast::{
    Argument, BinaryOp, Expression, ForInStatement, Literal, Program, Statement, TypeAnnotation,
    UnaryOp,
};

/// AST から制約を抽出するジェネレータ。
#[derive(Debug)]
pub struct ConstraintGenerator<'env> {
    env: &'env mut TypeEnvironment,
    constraints: ConstraintSet,
}

const DIAG_RANGE_BOUNDS: &str = "E_LOOP_002: numeric range bounds must resolve to the same type";
const DIAG_RANGE_BINDING: &str =
    "E_LOOP_002: loop binding must align with the numeric range element type";
const DIAG_ITERABLE_PROTOCOL: &str =
    "E_LOOP_003: loop target expression does not expose iterable semantics";

impl<'env> ConstraintGenerator<'env> {
    /// 環境への可変参照を受け取ってジェネレータを初期化する。
    pub fn new(env: &'env mut TypeEnvironment) -> Self {
        Self {
            env,
            constraints: ConstraintSet::new(),
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
                self.push_constraint(
                    ConstraintKind::Equal(target_ty, value_ty),
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
                        self.push_constraint(
                            ConstraintKind::Equal(param_ty.clone(), default_ty),
                            Some("parameter default must match declared type"),
                        );
                    }
                }

                let body_ty = self.infer_expression(body);
                self.push_constraint(
                    ConstraintKind::Equal(body_ty, return_ty.clone()),
                    Some("function body must satisfy return type"),
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
            self.push_constraint(
                ConstraintKind::Equal(symbol_ty.clone(), init.clone()),
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
            Expression::RegexLiteral(_) => TypeKind::Primitive("java.util.regex.Pattern"),
            Expression::Literal(literal, _) => self.type_from_literal(literal),
            Expression::Identifier(name, _) => {
                if let Some(scheme) = self.env.lookup(name).cloned() {
                    self.env.instantiate(&scheme)
                } else {
                    TypeKind::Unknown
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
            Expression::Block { statements, .. } => self.infer_block(statements),
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let cond_ty = self.infer_expression(condition);
                self.push_constraint(
                    ConstraintKind::Equal(cond_ty, TypeKind::Primitive("Boolean")),
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
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. }
            | Expression::IndexAccess { object, .. }
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
            Expression::Lambda { body, .. } => self.infer_expression(body),
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
                expr,
                arms,
                else_arm,
                implicit_end: _,
                ..
            } => {
                if let Some(scrutinee) = expr {
                    self.infer_expression(scrutinee);
                }
                for arm in arms {
                    if let Some(guard_expr) = &arm.guard {
                        let guard_ty = self.infer_expression(guard_expr);
                        self.push_constraint(
                            ConstraintKind::Equal(guard_ty, TypeKind::Primitive("Boolean")),
                            Some("when guard must evaluate to boolean"),
                        );
                    }
                    self.infer_expression(&arm.body);
                }
                if let Some(else_body) = else_arm {
                    self.infer_expression(else_body)
                } else {
                    TypeKind::Unknown
                }
            }
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let jv_ast::StringPart::Expression(expr) = part {
                        self.infer_expression(expr);
                    }
                }
                TypeKind::Primitive("String")
            }
            Expression::MultilineString(_) => TypeKind::Primitive("String"),
            Expression::JsonLiteral(_) => TypeKind::Unknown,
            Expression::This(_) | Expression::Super(_) => TypeKind::Unknown,
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
                let expected = TypeKind::Primitive("Int");
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
                let expected = TypeKind::Primitive("Boolean");
                self.push_constraint(
                    ConstraintKind::Equal(left_ty.clone(), expected.clone()),
                    Some("logical operations require boolean operands"),
                );
                self.push_constraint(
                    ConstraintKind::Equal(right_ty.clone(), expected.clone()),
                    Some("logical operations require boolean operands"),
                );
                expected
            }
            Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual => {
                self.push_constraint(
                    ConstraintKind::Equal(left_ty.clone(), right_ty.clone()),
                    Some("comparison operands must share the same type"),
                );
                TypeKind::Primitive("Boolean")
            }
            Is => TypeKind::Primitive("Boolean"),
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
                let expected = TypeKind::Primitive("Boolean");
                self.push_constraint(
                    ConstraintKind::Equal(operand_ty, expected.clone()),
                    Some("logical not requires boolean"),
                );
                expected
            }
            Minus | Plus | BitNot => operand_ty,
        }
    }

    fn type_from_literal(&mut self, literal: &Literal) -> TypeKind {
        match literal {
            Literal::String(_) => TypeKind::Primitive("String"),
            Literal::Number(n) => {
                if n.contains('.') {
                    TypeKind::Primitive("Double")
                } else {
                    TypeKind::Primitive("Int")
                }
            }
            Literal::Boolean(_) => TypeKind::Primitive("Boolean"),
            Literal::Character(_) => TypeKind::Primitive("String"),
            Literal::Regex(_) => TypeKind::Primitive("java.util.regex.Pattern"),
            Literal::Null => TypeKind::Optional(Box::new(TypeKind::Unknown)),
        }
    }

    fn type_from_annotation(&mut self, annotation: &TypeAnnotation) -> TypeKind {
        match annotation {
            TypeAnnotation::Simple(name) => match name.as_str() {
                "Int" | "Integer" => TypeKind::Primitive("Int"),
                "Double" | "Float" => TypeKind::Primitive("Double"),
                "Boolean" | "Bool" => TypeKind::Primitive("Boolean"),
                "String" => TypeKind::Primitive("String"),
                _ => TypeKind::Unknown,
            },
            TypeAnnotation::Nullable(inner) => {
                let inner_ty = self.type_from_annotation(inner);
                TypeKind::Optional(Box::new(inner_ty))
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

    fn push_constraint(&mut self, kind: ConstraintKind, note: Option<&str>) {
        let constraint = if let Some(text) = note {
            Constraint::new(kind).with_note(text)
        } else {
            Constraint::new(kind)
        };
        self.constraints.push(constraint);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::{Modifiers, Span, ValBindingOrigin};

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

    #[test]
    fn generates_constraints_for_val_literal() {
        let span = dummy_span();
        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![Statement::ValDeclaration {
                name: "x".into(),
                type_annotation: None,
                initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
                modifiers: default_modifiers(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            }],
            span,
        };

        let mut env = TypeEnvironment::new();
        let constraints = ConstraintGenerator::new(&mut env).generate(&program);
        let collected = collect_constraints(constraints);

        assert!(collected.iter().any(|constraint| matches!(
            &constraint.kind,
            ConstraintKind::Equal(TypeKind::Variable(_), TypeKind::Primitive(name)) if *name == "Int"
        )));

        let scheme = env.lookup("x").expect("symbol x must exist");
        assert_eq!(scheme.ty, TypeKind::Primitive("Int"));
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
                    type_annotation: None,
                    initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
                    modifiers: default_modifiers(),
                    origin: ValBindingOrigin::ExplicitKeyword,
                    span: span.clone(),
                },
                Statement::ValDeclaration {
                    name: "sum".into(),
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
        let constraints = ConstraintGenerator::new(&mut env).generate(&program);
        let collected = collect_constraints(constraints);

        assert!(collected.iter().any(|constraint| matches!(
            &constraint.kind,
            ConstraintKind::Equal(TypeKind::Primitive("Int"), TypeKind::Primitive("Int"))
        )));

        let count_int_var = collected
            .iter()
            .filter(|constraint| {
                matches!(
                    &constraint.kind,
                    ConstraintKind::Equal(TypeKind::Variable(_), TypeKind::Primitive("Int"))
                )
            })
            .count();
        assert!(count_int_var >= 2);

        let scheme = env.lookup("sum").expect("symbol sum must exist");
        assert_eq!(scheme.ty, TypeKind::Primitive("Int"));
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
        let constraints = ConstraintGenerator::new(&mut env).generate(&program);
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
        assert_eq!(outer.ty, TypeKind::Primitive("Int"));

        let maybe = env.lookup("maybe").expect("maybe binding must exist");
        match &maybe.ty {
            TypeKind::Optional(inner) => assert!(matches!(**inner, TypeKind::Unknown)),
            other => panic!("expected optional type, found {:?}", other),
        }
    }
}
