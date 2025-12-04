use jv_checker::{CheckError, TypeChecker};
use jv_parser_frontend::{Parser2Pipeline, ParserPipeline};
use jv_ast::{Expression, Statement, Modifiers, ValBindingOrigin};

fn parse_program(source: &str) -> jv_ast::Program {
    let program = Parser2Pipeline::default()
        .parse(source)
        .expect("source snippet should parse")
        .into_program();
    normalize_implicit_assignments(program)
}

fn normalize_implicit_assignments(mut program: jv_ast::Program) -> jv_ast::Program {
    program.statements = program
        .statements
        .into_iter()
        .map(|statement| match statement {
            Statement::Assignment {
                target: Expression::Identifier(name, _),
                value,
                span,
                ..
            } => Statement::ValDeclaration {
                name,
                binding: None,
                type_annotation: None,
                initializer: value,
                modifiers: Modifiers::default(),
                origin: ValBindingOrigin::Implicit,
                span,
            },
            other => other,
        })
        .collect();
    program
}

#[test]
fn numeric_range_loop_with_matching_bounds_succeeds() {
    let program = parse_program(
        r#"
        for (index in 0..10) {
            val snapshot = index
        }
    "#,
    );

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);

    if let Err(errors) = result {
        panic!("expected loop to type-check, got {errors:?}");
    }
}

#[test]
fn inclusive_numeric_range_loop_with_matching_bounds_succeeds() {
    let program = parse_program(
        r#"
        for (index in 0..=10) {
            val snapshot = index
        }
    "#,
    );

    let mut checker = TypeChecker::new();
    if let Err(errors) = checker.check_program(&program) {
        panic!("expected inclusive loop to type-check, got {errors:?}");
    }
}

#[test]
fn numeric_range_bounds_type_mismatch_reports_e_loop_002() {
    let program = parse_program(
        r#"
        for (index in 0.."end") {
            val capture = index
        }
    "#,
    );

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);

    match result {
        Err(errors) => {
            assert!(errors.iter().any(|error| matches!(
                error,
                CheckError::TypeError(message) if message.contains("E_LOOP_002")
            )));
        }
        Ok(()) => panic!("expected E_LOOP_002 type mismatch diagnostic"),
    }
}

#[test]
fn non_iterable_loop_target_reports_e_loop_003() {
    let program = parse_program(
        r#"
        for (value in 42) {
            val echo = value
        }
    "#,
    );

    let mut checker = TypeChecker::new();
    let result = checker.check_program(&program);

    match result {
        Err(errors) => {
            assert!(errors.iter().any(|error| matches!(
                error,
                CheckError::TypeError(message) if message.contains("E_LOOP_003")
            )));
        }
        Ok(()) => panic!("expected E_LOOP_003 iterable diagnostic"),
    }
}
