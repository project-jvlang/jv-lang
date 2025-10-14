//! Integration tests for the type inference workflow.
//!
//! Run with: `cargo test --lib -p jv_checker`.

use jv_checker::{CheckError, TypeChecker, TypeInferenceService, TypeKind};
use jv_inference::service::TypeFacts;
use jv_parser::Parser;
use serde_json::Value;

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

    let mut checker = TypeChecker::new();
    let diagnostics = checker.check_null_safety(&program, None);

    assert!(diagnostics.iter().any(|error| matches!(
        error,
        CheckError::NullSafetyError(message)
            if message.contains("JV3002") && message.contains("message")
    )));
}

#[test]
fn type_facts_snapshot_contains_environment() {
    let program = parse_program("val greeting = \"hello\"\n");

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("program should type-check");

    let facts = checker
        .type_facts()
        .expect("type facts snapshot should be available");

    let json = facts.to_json();
    let environment = json
        .get("environment")
        .and_then(Value::as_object)
        .expect("environment section present");
    let greeting_entry = environment
        .get("greeting")
        .and_then(Value::as_str)
        .expect("greeting binding exported");
    assert!(
        greeting_entry.contains("Primitive(\"String\")"),
        "unexpected environment entry: {greeting_entry}"
    );
    let pretty = facts.to_pretty_json().expect("serialize facts to json");
    assert!(
        pretty.contains("greeting"),
        "json output should mention binding: {pretty}"
    );
    assert!(facts.bindings().len() >= 1);
}

#[test]
fn reports_ambiguous_function_signature_error() {
    let program = parse_program("fun ambiguous(value) { null }\n");

    let mut checker = TypeChecker::new();
    let errors = checker
        .check_program(&program)
        .expect_err("ambiguous function must yield type error");

    assert!(errors.iter().any(|error| matches!(
        error,
        CheckError::TypeError(message) if message.contains("ambiguous function signature")
    )));
}

#[test]
fn reports_ambiguous_extension_method_error() {
    let program = parse_program(
        r#"
        fun ambiguousExtension(input) {
            val result = input.map { element -> element }
            result
        }
    "#,
    );

    let mut checker = TypeChecker::new();
    let errors = checker
        .check_program(&program)
        .expect_err("ambiguous extension resolution should fail");

    assert!(errors.iter().any(|error| matches!(
        error,
        CheckError::TypeError(message) if message.contains("ambiguous extension method 'map'")
    )));
}

#[test]
fn type_facts_update_after_rechecking_program() {
    let program_v1 = parse_program("val counter = 1\n");
    let mut checker = TypeChecker::new();
    checker
        .check_program(&program_v1)
        .expect("first pass should succeed");

    let first_snapshot = checker
        .type_facts()
        .expect("first snapshot available")
        .to_json();
    let first_env = first_snapshot
        .get("environment")
        .and_then(Value::as_object)
        .expect("environment exists in first snapshot");
    assert!(first_env.get("counter").is_some());
    assert!(first_env.get("incremented").is_none());

    let program_v2 = parse_program("val counter = 1\nval incremented = counter + 1\n");
    checker
        .check_program(&program_v2)
        .expect("second pass should succeed");

    let second_snapshot = checker
        .type_facts()
        .expect("second snapshot available")
        .to_json();
    let second_env = second_snapshot
        .get("environment")
        .and_then(Value::as_object)
        .expect("environment exists in second snapshot");

    assert!(second_env.get("counter").is_some());
    let incremented = second_env
        .get("incremented")
        .and_then(Value::as_str)
        .expect("incremented binding exported after second run");
    assert!(
        incremented.contains("Primitive(\"Int\")"),
        "expected incremented binding to be inferred as Int"
    );
}

#[test]
fn sequence_extension_chain_maintains_inference() {
    let program = parse_program(
        r#"
        import jv.collections.sequenceFromIterable

        val numbers = sequenceFromIterable([1 2 3 4 5])
        val doubled = numbers.map { value -> value * 2 }
        val filtered = doubled.filter { value -> value > 4 }
        val total = filtered.reduce { (acc value) -> acc + value }
    "#,
    );

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("sequence pipeline should type-check");

    let snapshot = checker
        .inference_snapshot()
        .expect("inference snapshot should be available");

    let filtered_scheme = snapshot
        .binding_scheme("filtered")
        .expect("filtered binding registered");
    assert_eq!(
        filtered_scheme.ty,
        TypeKind::Primitive("jv.collections.SequenceCore")
    );

    let total_scheme = snapshot
        .binding_scheme("total")
        .expect("total binding registered");
    assert!(
        !matches!(total_scheme.ty, TypeKind::Unknown),
        "reduce result should not remain Unknown"
    );
}

#[test]
fn lambda_extension_receiver_resolves_sequence_core() {
    let program = parse_program(
        r#"
        import jv.collections.sequenceFromIterable

        val source = sequenceFromIterable([1 2 3])
        val toStream = { seq -> seq.toStream() }
        val stream = toStream(source)
    "#,
    );

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("lambda extension call should infer receiver");

    let snapshot = checker
        .inference_snapshot()
        .expect("snapshot should exist after successful inference");
    let lambda_scheme = snapshot
        .binding_scheme("toStream")
        .expect("lambda binding should exist");
    assert!(lambda_scheme.quantifiers.is_empty());
    match &lambda_scheme.ty {
        TypeKind::Function(params, ret) => {
            assert_eq!(
                params,
                &vec![TypeKind::Primitive("jv.collections.SequenceCore")]
            );
            assert_eq!(**ret, TypeKind::Primitive("java.util.stream.Stream"));
        }
        other => panic!("expected function type, found {other:?}"),
    }

    let stream_scheme = snapshot
        .binding_scheme("stream")
        .expect("stream binding should be registered");
    assert!(
        !stream_scheme.ty.contains_unknown(),
        "stream binding should not remain Unknown"
    );
}
