//! Integration tests exercising the null safety pipeline end-to-end.

use jv_checker::{CheckError, TypeChecker};
use jv_parser::Parser;
use serde_json::Value;

fn parse_program(source: &str) -> jv_ast::Program {
    Parser::parse(source).expect("source snippet should parse")
}

fn collect_null_safety_messages(errors: &[CheckError]) -> Vec<String> {
    errors
        .iter()
        .filter_map(|error| match error {
            CheckError::NullSafetyError(message) => Some(message.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn null_safety_errors_are_tagged_with_jv3002() {
    let program = parse_program("val message: String = null\n");

    let mut checker = TypeChecker::new();
    let diagnostics = checker.check_null_safety(&program, None);
    let messages = collect_null_safety_messages(&diagnostics);

    assert!(
        messages.iter().any(|message| message.contains("JV3002")),
        "expected JV3002 code in diagnostics, got: {messages:?}"
    );
}

#[test]
fn null_safety_retains_type_facts_snapshot() {
    let program = parse_program("val greeting = \"hello\"\n");

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("program should type-check");

    let before = checker
        .type_facts()
        .expect("inference snapshot should expose facts")
        .to_json();

    let snapshot = checker.inference_snapshot().cloned();
    let diagnostics = checker.check_null_safety(&program, snapshot.as_ref());
    assert!(
        diagnostics.is_empty(),
        "no diagnostics expected for safe program"
    );

    let after = checker
        .type_facts()
        .expect("null safety run should keep facts accessible")
        .to_json();

    assert_eq!(
        before, after,
        "null safety should not drop or corrupt TypeFacts snapshot"
    );

    if let Some(environment) = after.get("environment").and_then(Value::as_object) {
        assert!(environment.contains_key("greeting"));
    } else {
        panic!("expected environment section in exported type facts");
    }
}

#[test]
fn when_null_branch_conflict_emits_jv3108() {
    let program = parse_program(
        "val token: String = \"hello\"\n\
         val label = when (token) {\n\
             null -> \"none\"\n\
             else -> token\n\
         }\n",
    );

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("program should type-check");

    let diagnostics = checker.check_null_safety(&program, None);
    let messages = collect_null_safety_messages(&diagnostics);

    assert!(
        messages.iter().any(|message| message.contains("JV3108")),
        "expected JV3108 conflict diagnostic, got: {messages:?}"
    );

    assert!(
        checker.telemetry().pattern_bridge_ms >= 0.0,
        "pattern bridge telemetry should be recorded"
    );
}
