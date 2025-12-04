//! Integration tests exercising the null safety pipeline end-to-end.

use jv_ast::{Expression, Modifiers, Statement, ValBindingOrigin};
use jv_checker::{CheckError, TypeChecker, TypeKind};
use jv_parser_frontend::{Parser2Pipeline, ParserPipeline};
use serde_json::Value;
use test_case::test_case;

fn parse_program(source: &str) -> jv_ast::Program {
    let program = Parser2Pipeline::default()
        .parse(source)
        .expect("source snippet should parse")
        .into_program();
    normalize_implicit_assignments(program)
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

struct NullSafetyCaseResult {
    messages: Vec<String>,
    telemetry_ms: f64,
}

fn run_null_safety_case(source: &str) -> NullSafetyCaseResult {
    let program = parse_program(source);

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("program should type-check");

    let diagnostics = checker.check_null_safety(&program, None);
    let messages = collect_null_safety_messages(&diagnostics);
    let telemetry_ms = checker.telemetry().pattern_bridge_ms;

    NullSafetyCaseResult {
        messages,
        telemetry_ms,
    }
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
fn null_safety_errors_are_tagged_with_jv3002() {
    let program = parse_program("val message: String = null\n");

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("program should type-check before null safety");
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

    assert_eq!(
        messages.len(),
        1,
        "expected single JV3108 conflict diagnostic, got: {messages:?}"
    );

    let expected = "JV3108: `token` は non-null と推論されていますが、when 分岐で null と比較されています。分岐を削除するか型を nullable に変更してください。\nJV3108: `token` is inferred as non-null but the when expression compares it against null. Remove the branch or update the type to be nullable.\nQuick Fix: when.remove.null-branch -> `token` の null 分岐を削除\nQuick Fix: when.remove.null-branch -> remove the null arm for `token` or declare the type as nullable";
    assert!(
        messages.contains(&expected.to_string()),
        "expected JV3108 conflict for `token`, got: {messages:?}"
    );

    assert!(
        checker.telemetry().pattern_bridge_ms >= 0.0,
        "pattern bridge telemetry should be recorded"
    );
}

#[test_case(
    r#"
fun provide(): String? = null
fun consume(value: String): Int = 1

maybe = provide()
result = when (maybe) {
    is String -> consume(maybe)
    else -> 0
}
"#, false;
    "subject_when_smart_cast"
)]
#[test_case(
    r#"
fun provide(): String? = null
fun consume(value: String): Int = 1

val maybe = provide()
val result = when {
    maybe is String -> consume(maybe)
    else -> 0
}
"#, false;
    "subjectless_when_smart_cast"
)]
#[test_case(
    r#"
fun provide(): String? = null
fun consume(value: String): Int = 1

val maybe = provide()
val result = when (maybe) {
    is String -> {
        val inner = maybe
        consume(inner)
    }
    else -> 0
}
"#, false;
    "block_expression_branch"
)]
#[test_case(
    r#"
fun provide(): String? = null

maybe = provide()
label = when (maybe) {
    is String -> maybe
    else -> "fallback"
}
"#, false;
    "smart_cast_result_assignment"
)]
#[test_case(
    r#"
fun provide(): String? = null

var maybe = provide()
when (maybe) {
    is String -> {}
    else -> {
        maybe = "fallback"
    }
}

val label = maybe
"#, false;
    "smart_cast_result_assignment_after_rebind"
)]
fn pattern_bridge_allows_smart_casts(source: &str, expect_violation: bool) {
    let result = run_null_safety_case(source);
    if expect_violation {
        assert!(
            result
                .messages
                .iter()
                .any(|message| message.contains("JV3002") && message.contains("maybe")),
            "expected JV3002 violation for implicit binding `maybe`, got: {:?}",
            result.messages
        );
    } else {
        assert!(
            result.messages.is_empty(),
            "expected no null safety diagnostics, got: {:?}",
            result.messages
        );
    }
    assert!(
        result.telemetry_ms >= 0.0,
        "pattern bridge telemetry should record elapsed time"
    );
}

#[test_case(
    r#"
val token: String = "hello"
val label = when (token) {
    null -> "none"
    else -> token
}
"#;
    "simple_null_branch"
)]
#[test_case(
    r#"
fun isPositive(value: String): Boolean = true

val token: String = "hello"
val label = when (token) {
    null -> "none"
    is String && isPositive(token) -> token
    else -> token
}
"#;
    "null_branch_with_guard"
)]
#[test_case(
    r#"
fun sizeOf(value: String): Int = 1

val token: String = "hello"
val count = when (token) {
    null -> 0
    else -> sizeOf(token)
}
"#;
    "null_branch_numeric"
)]
#[test_case(
    r#"
fun label(token: String): String = when (token) {
    null -> "none"
    else -> token
}

val status = label("input")
"#;
    "null_branch_in_function"
)]
fn pattern_bridge_reports_null_branch_conflicts(source: &str) {
    let result = run_null_safety_case(source);
    assert!(
        result
            .messages
            .iter()
            .any(|message| message.contains("JV3108")),
        "expected JV3108 conflict diagnostic, got: {:?}",
        result.messages
    );
    assert!(
        result.telemetry_ms >= 0.0,
        "pattern bridge telemetry should record elapsed time"
    );
}

#[test_case(
    r#"
fun provide(): String? = null
fun accept(value: String): Int = 1
fun longEnough(value: String): Boolean = true

val maybe = provide()
val result = when (maybe) {
    is String && longEnough(maybe) -> accept(maybe)
    else -> 0
}
"#;
    "single_guard_propagation"
)]
#[test_case(
    r#"
fun provide(): String? = null
fun accept(value: String): Int = 1
fun longEnough(value: String): Boolean = true
fun hasCapital(value: String): Boolean = true

val maybe = provide()
val result = when (maybe) {
    is String && longEnough(maybe) && hasCapital(maybe) -> accept(maybe)
    is String -> accept(maybe)
    else -> 0
}
"#;
    "chained_guard_propagation"
)]
#[test_case(
    r#"
fun provide(): String? = null
fun accept(value: String): Int = 1
fun stable(value: String): Boolean = true

val maybe = provide()
val result = when {
    maybe is String && stable(maybe) -> accept(maybe)
    else -> 0
}
"#;
    "subjectless_guard_propagation"
)]
fn pattern_bridge_propagates_guard_narrowing(source: &str) {
    let result = run_null_safety_case(source);
    assert!(
        result.messages.is_empty(),
        "expected guard propagation to avoid diagnostics, got: {:?}",
        result.messages
    );
    assert!(
        result.telemetry_ms >= 0.0,
        "pattern bridge telemetry should record elapsed time"
    );
}

#[test_case(
    r#"
fun provide(): String? = null
fun consume(value: String): Int = 1

val maybe = provide()
when (maybe) {
    is String -> {}
    else -> {}
}

consume(maybe)
"#,
    None;
    "post_when_nullable_usage"
)]
#[test_case(
    r#"
fun provide(): String? = null
fun consume(value: String): Int = 1

var maybe = provide()
when (maybe) {
    is String -> {}
    else -> {
        maybe = "fallback"
    }
}

consume(maybe)
"#,
    None;
    "else_branch_initialises_non_null"
)]
#[test_case(
    r#"
fun provide(): String? = null

var maybe = provide()
when (maybe) {
    is String -> {}
    else -> {
        maybe = "fallback"
        maybe = provide()
    }
}

consume(maybe)
"#,
    None;
    "else_branch_reintroduces_nullable"
)]
#[test_case(
    r#"
fun provide(): String? = null
fun consume(value: String): Int = 1

var maybe = provide()
val value = when (maybe) {
    is String -> {
        consume(maybe)
        maybe
    }
    else -> {
        maybe = null
        "fallback"
    }
}

consume(maybe)
"#,
    None;
    "else_branch_forces_null"
)]
#[test_case(
    r#"
fun provide(): String? = null
fun consume(value: String): Int = 1

var maybe = provide()
when (maybe) {
    is String -> {}
    else -> {
        maybe = "fallback"
    }
}

consume(maybe)
"#,
    None;
    "all_paths_initialise_non_null"
)]
fn pattern_bridge_merges_flow_states(source: &str, expected_code: Option<&str>) {
    let result = run_null_safety_case(source);
    match expected_code {
        Some(code) => assert!(
            result.messages.iter().any(|message| message.contains(code)),
            "expected diagnostic {code}, got: {:?}",
            result.messages
        ),
        None => assert!(
            result.messages.is_empty(),
            "expected no diagnostics, got: {:?}",
            result.messages
        ),
    }
    assert!(
        result.telemetry_ms >= 0.0,
        "pattern bridge telemetry should record elapsed time"
    );
}

#[test]
fn implicit_declarations_mix_with_explicit_val() {
    let program = parse_program(
        r#"
fun provide(): String? = null

maybe = provide()
val fallback = "fallback"
label = when (maybe) {
    is String -> maybe
    else -> fallback
}

label
"#,
    );

    let mut checker = TypeChecker::new();
    checker
        .check_program(&program)
        .expect("program should type-check with mixed explicit/implicit declarations");

    let maybe_scheme = checker
        .inference_snapshot()
        .and_then(|snapshot| snapshot.binding_scheme("maybe"))
        .cloned()
        .expect("implicit binding `maybe` should have a type scheme");
    assert!(
        matches!(maybe_scheme.ty, TypeKind::Optional(_)),
        "expected `maybe` to be inferred as optional, got {:?}",
        maybe_scheme.ty
    );

    let diagnostics = checker.check_null_safety(&program, None);
    assert!(
        diagnostics.is_empty(),
        "expected no null safety errors for exhaustive when expression with non-null branches, got: {:?}",
        diagnostics
    );
}
