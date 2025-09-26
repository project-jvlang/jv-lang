//! Integration tests for the type inference workflow.
//!
//! Run with: `cargo test --lib -p jv_checker`.

use jv_checker::{CheckError, TypeChecker, TypeInferenceService, TypeKind};
use jv_parser::Parser;

fn parse_program(source: &str) -> jv_ast::Program {
    Parser::parse(source).expect("source snippet should be valid")
}

#[test]
fn infers_local_binding_from_literal() {
    let program = parse_program("val counter = 42\n");

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("local inference from literals succeeds");

    let snapshot = checker
        .inference_snapshot()
        .expect("snapshot should be populated");
    let scheme = snapshot
        .environment()
        .lookup("counter")
        .expect("local binding should be registered");

    assert!(
        scheme.quantifiers.is_empty(),
        "literal binding stays monomorphic"
    );
    assert_eq!(scheme.ty, TypeKind::Primitive("Int"));
}

#[test]
fn generalizes_identity_function() {
    let program = parse_program("fun identity(value) = value\n");

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("function inference succeeds");

    let snapshot = checker
        .inference_snapshot()
        .expect("snapshot should be populated");
    let scheme = snapshot
        .function_scheme("identity")
        .expect("function scheme should be generalized");

    assert_eq!(
        scheme.quantifiers.len(),
        1,
        "identity should remain polymorphic"
    );
    match &scheme.ty {
        TypeKind::Function(params, return_ty) => {
            assert_eq!(params.len(), 1, "identity has a single parameter");
            match (&params[0], return_ty.as_ref()) {
                (TypeKind::Variable(a), TypeKind::Variable(b)) => assert_eq!(a, b),
                other => panic!("expected matching type variables, found {other:?}"),
            }
        }
        other => panic!("expected function type, found {other:?}"),
    }
}

#[test]
fn reports_null_safety_violation_for_non_nullable_binding() {
    let program = parse_program("val message: String = null\n");

    let checker = TypeChecker::new();
    let diagnostics = checker.check_null_safety(&program);

    assert!(diagnostics.iter().any(|error| matches!(
        error,
        CheckError::NullSafetyError(message) if message.contains("message")
    )));
}
