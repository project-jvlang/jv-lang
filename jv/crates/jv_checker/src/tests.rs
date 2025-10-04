use super::*;
use crate::pattern::{self, PatternTarget};
use jv_ast::{
    Annotation, AnnotationName, BinaryOp, Expression, Literal, Modifiers, Parameter, Pattern,
    Program, Span, Statement, TypeAnnotation, ValBindingOrigin, WhenArm,
};

fn dummy_span() -> Span {
    Span::new(1, 0, 1, 5)
}

fn default_modifiers() -> Modifiers {
    Modifiers::default()
}

fn annotation(name: &str) -> Annotation {
    Annotation {
        name: AnnotationName::new(vec![name.to_string()], dummy_span()),
        arguments: Vec::new(),
        span: dummy_span(),
    }
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
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            },
            Statement::ValDeclaration {
                name: "rhs".into(),
                type_annotation: None,
                initializer: Expression::Literal(Literal::Number("2".into()), span.clone()),
                modifiers: default_modifiers(),
                origin: ValBindingOrigin::ExplicitKeyword,
                span: span.clone(),
            },
            Statement::FunctionDeclaration {
                name: "add".into(),
                type_parameters: Vec::new(),
                where_clause: None,
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
        span: span.clone(),
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
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
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
fn override_annotation_on_method_is_allowed() {
    let span = dummy_span();
    let mut method_modifiers = Modifiers::default();
    method_modifiers.annotations.push(annotation("Override"));

    let method = Statement::FunctionDeclaration {
        name: "run".into(),
        type_parameters: Vec::new(),
        where_clause: None,
        parameters: Vec::new(),
        return_type: None,
        body: Box::new(Expression::Literal(
            Literal::Number("1".into()),
            span.clone(),
        )),
        modifiers: method_modifiers,
        span: span.clone(),
    };

    let class = Statement::ClassDeclaration {
        name: "Service".into(),
        type_parameters: Vec::new(),
        superclass: None,
        interfaces: Vec::new(),
        properties: Vec::new(),
        methods: vec![Box::new(method)],
        modifiers: Modifiers::default(),
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![class],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "override on method should be accepted: {result:?}"
    );
}

#[test]
fn override_annotation_on_field_is_rejected() {
    let span = dummy_span();
    let mut field_modifiers = Modifiers::default();
    field_modifiers.annotations.push(annotation("Override"));

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "count".into(),
            type_annotation: None,
            initializer: Expression::Literal(Literal::Number("1".into()), span.clone()),
            modifiers: field_modifiers,
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "override on field should be rejected");

    let errors = result.err().unwrap();
    assert!(matches!(
        errors.first(),
        Some(CheckError::ValidationError { message, .. }) if message.contains("@Override") && message.contains("method")
    ));
}

#[test]
fn duplicate_reserved_annotation_is_reported() {
    let span = dummy_span();
    let mut modifiers = Modifiers::default();
    modifiers.annotations.push(annotation("Sample"));
    modifiers.annotations.push(annotation("Sample"));

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "fixture".into(),
            type_annotation: None,
            initializer: Expression::Literal(Literal::String("data".into()), span.clone()),
            modifiers,
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "duplicate reserved annotation should fail");

    let errors = result.err().unwrap();
    assert!(matches!(
        errors.first(),
        Some(CheckError::ValidationError { message, .. }) if message.contains("@Sample") && message.contains("used once")
    ));
}

#[test]
fn reserved_annotation_shadowing_is_detected() {
    let span = dummy_span();
    let shadow = Annotation {
        name: AnnotationName::new(
            vec![
                "com".to_string(),
                "example".to_string(),
                "Sample".to_string(),
            ],
            span.clone(),
        ),
        arguments: Vec::new(),
        span: span.clone(),
    };

    let mut modifiers = Modifiers::default();
    modifiers.annotations.push(shadow);

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "fixture".into(),
            type_annotation: None,
            initializer: Expression::Literal(Literal::String("data".into()), span.clone()),
            modifiers,
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "shadowing reserved annotation should fail");

    let errors = result.err().unwrap();
    assert!(matches!(
        errors.first(),
        Some(CheckError::ValidationError { message, .. }) if message.contains("reserved jv annotation")
    ));
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
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let diagnostics = checker.check_null_safety(&program, None);
    assert!(diagnostics.iter().any(
        |error| matches!(error, CheckError::NullSafetyError(message) if message.contains("null"))
    ));
}

fn sample_when_arm(span: &Span) -> WhenArm {
    WhenArm {
        pattern: Pattern::Literal(Literal::Number("1".into()), span.clone()),
        guard: None,
        body: Expression::Literal(Literal::Number("1".into()), span.clone()),
        span: span.clone(),
    }
}

#[test]
fn when_without_else_in_value_position_emits_validation_error() {
    let span = dummy_span();
    let when_expr = Expression::When {
        expr: None,
        arms: vec![sample_when_arm(&span)],
        else_arm: None,
        implicit_end: None,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "result".into(),
            type_annotation: None,
            initializer: when_expr,
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let errors = checker
        .check_program(&program)
        .expect_err("when without else in value context should fail");

    assert!(
        matches!(
            errors.first(),
            Some(CheckError::ValidationError {
                message,
                span: Some(error_span),
            }) if message.contains("E_WHEN_002") && *error_span == span
        ),
        "expected E_WHEN_002 validation error with span info",
    );
}

#[test]
fn when_with_else_in_value_position_passes_validation() {
    let span = dummy_span();
    let when_expr = Expression::When {
        expr: None,
        arms: vec![sample_when_arm(&span)],
        else_arm: Some(Box::new(Expression::Literal(
            Literal::Number("0".into()),
            span.clone(),
        ))),
        implicit_end: None,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::ValDeclaration {
            name: "result".into(),
            type_annotation: None,
            initializer: when_expr,
            modifiers: default_modifiers(),
            origin: ValBindingOrigin::ExplicitKeyword,
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("when with explicit else should pass validation");

    let snapshot = checker
        .inference_snapshot()
        .expect("snapshot should include pattern facts");
    let node_id = pattern::node_identifier(&span);
    let facts = snapshot
        .pattern_fact(node_id, PatternTarget::Java25)
        .expect("pattern facts recorded for when expression");
    assert!(
        facts.is_exhaustive(),
        "else branch should render exhaustive"
    );
}

#[test]
fn when_without_else_in_statement_position_is_allowed() {
    let span = dummy_span();
    let when_expr = Expression::When {
        expr: None,
        arms: vec![sample_when_arm(&span)],
        else_arm: None,
        implicit_end: None,
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::Expression {
            expr: when_expr,
            span: span.clone(),
        }],
        span,
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "statement-position when without else should be permitted"
    );
}

#[test]
fn implicit_assignment_becomes_val_declaration() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![Statement::Assignment {
            target: Expression::Identifier("total".into(), span.clone()),
            value: Expression::Literal(Literal::Number("42".into()), span.clone()),
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "binding resolver should accept implicit assignment: {result:?}"
    );

    let normalized = checker
        .normalized_program()
        .expect("normalized program should be available");
    match normalized.statements.first() {
        Some(Statement::ValDeclaration { name, origin, .. }) => {
            assert_eq!(name, "total");
            assert_eq!(*origin, ValBindingOrigin::Implicit);
        }
        other => panic!("expected ValDeclaration, got {:?}", other),
    }

    let usage = checker.binding_usage();
    assert_eq!(usage.implicit, 1);
    assert_eq!(usage.explicit + usage.implicit_typed + usage.vars, 0);
}

#[test]
fn reassigning_immutable_binding_emits_jv4201() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::Assignment {
                target: Expression::Identifier("value".into(), span.clone()),
                value: Expression::Literal(Literal::Number("1".into()), span.clone()),
                span: span.clone(),
            },
            Statement::Assignment {
                target: Expression::Identifier("value".into(), span.clone()),
                value: Expression::Literal(Literal::Number("2".into()), span.clone()),
                span: span.clone(),
            },
        ],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(result.is_err(), "reassignment should produce JV4201 error");
    let errors = result.err().unwrap();
    assert!(errors.iter().any(|error| matches!(
        error,
        CheckError::ValidationError { message, .. } if message.contains("JV4201")
    )));
}

#[test]
fn mutable_var_allows_reassignment() {
    let span = dummy_span();
    let program = Program {
        package: None,
        imports: Vec::new(),
        statements: vec![
            Statement::VarDeclaration {
                name: "count".into(),
                type_annotation: None,
                initializer: Some(Expression::Literal(
                    Literal::Number("0".into()),
                    span.clone(),
                )),
                modifiers: default_modifiers(),
                span: span.clone(),
            },
            Statement::Assignment {
                target: Expression::Identifier("count".into(), span.clone()),
                value: Expression::Literal(Literal::Number("1".into()), span.clone()),
                span: span.clone(),
            },
        ],
        span: span.clone(),
    };

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);
    assert!(
        result.is_ok(),
        "var reassignment should be permitted: {result:?}"
    );
}
