use super::*;
use jv_ast::{
    BinaryOp, Expression, Literal, Modifiers, Parameter, Program, Span, Statement, TypeAnnotation,
};

fn dummy_span() -> Span {
    Span::dummy()
}

fn default_modifiers() -> Modifiers {
    Modifiers::default()
}

#[test]
fn check_program_populates_inference_snapshot() {
    let span = dummy_span();
    let function_body = Expression::Binary {
        left: Box::new(Expression::Identifier("lhs".into(), span.clone())),
        op: BinaryOp::Add,
        right: Box::new(Expression::Identifier("rhs".into(), span.clone())),
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::ValDeclaration {
                name: "lhs".into(),
                type_annotation: None,
                initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
                modifiers: default_modifiers(),
                span: span.clone(),
            },
            Statement::ValDeclaration {
                name: "rhs".into(),
                type_annotation: None,
                initializer: Expression::Literal(Literal::Number("2".into()), span.clone()),
                modifiers: default_modifiers(),
                span: span.clone(),
            },
            Statement::FunctionDeclaration {
                name: "add".into(),
                parameters: vec![
                    Parameter {
                        name: "lhs".into(),
                        type_annotation: None,
                        default_value: None,
                        span: span.clone(),
                    },
                    Parameter {
                        name: "rhs".into(),
                        type_annotation: None,
                        default_value: None,
                        span: span.clone(),
                    },
                ],
                return_type: None,
                body: Box::new(function_body),
                modifiers: default_modifiers(),
                span: span.clone(),
            },
        ],
        span,
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_ok(), "expected successful inference: {result:?}");

    let snapshot = checker
        .inference_snapshot()
        .expect("inference snapshot should be populated");
    assert!(snapshot.function_scheme("add").is_some());
    assert!(!snapshot.bindings().is_empty());
}

#[test]
fn check_program_reports_type_error_on_mismatch() {
    let span = dummy_span();
    let mismatched_expr = Expression::Binary {
        left: Box::new(Expression::Literal(Literal::Boolean(true), span.clone())),
        op: BinaryOp::Add,
        right: Box::new(Expression::Literal(
            Literal::Number("1".into()),
            span.clone(),
        )),
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "x".into(),
            type_annotation: None,
            initializer: mismatched_expr,
            modifiers: default_modifiers(),
            span: span.clone(),
        }],
        span,
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "type mismatch should yield an error");

    let errors = result.err().unwrap();
    assert!(
        matches!(errors.first(), Some(CheckError::TypeError(message)) if message.contains("type mismatch"))
    );
}

#[test]
fn null_safety_violation_is_reported() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "greeting".into(),
            type_annotation: Some(TypeAnnotation::Simple("String".into())),
            initializer: Expression::Literal(Literal::Null, span.clone()),
            modifiers: default_modifiers(),
            span: span.clone(),
        }],
        span,
    };

    let checker = TypeChecker::new();
    let diagnostics = checker.check_null_safety(&program);
    assert!(diagnostics.iter().any(
        |error| matches!(error, CheckError::NullSafetyError(message) if message.contains("null"))
    ));
}
