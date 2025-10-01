use jv_ast::{Expression, Literal, Pattern, Span, Statement, WhenArm};
use jv_checker::pattern::{NarrowedNullability, PatternMatchService, PatternTarget};
use jv_parser::Parser;

fn parse_when_expression(source: &str) -> Expression {
    let program = Parser::parse(source).expect("snippet should parse");
    let statement = program
        .statements
        .first()
        .expect("expected at least one statement");
    match statement {
        Statement::ValDeclaration { initializer, .. }
        | Statement::Expression {
            expr: initializer, ..
        } => initializer.clone(),
        Statement::VarDeclaration {
            initializer: Some(initializer),
            ..
        } => initializer.clone(),
        other => panic!("expected value-bearing statement, got {other:?}"),
    }
}

fn analyze(source: &str) -> jv_checker::pattern::PatternMatchFacts {
    let expr = parse_when_expression(source);
    let mut service = PatternMatchService::new();
    service.analyze(&expr, PatternTarget::Java25)
}

#[test]
fn missing_false_case_is_reported() {
    let facts = analyze("val label = when (flag) { true -> \"yes\" }\n");
    assert!(
        !facts.is_exhaustive(),
        "expected missing cases for boolean when"
    );
    assert!(facts.missing_cases().iter().any(|case| matches!(
        case,
        jv_checker::pattern::MissingCase::Boolean {
            missing: jv_checker::pattern::MissingBooleanCase::False,
            ..
        }
    )));
}

#[test]
fn missing_case_carries_quick_fix_metadata() {
    let facts = analyze("val label = when (flag) { true -> \"yes\" }\n");
    let first_case = facts
        .missing_cases()
        .first()
        .expect("missing case should be recorded");
    match first_case {
        jv_checker::pattern::MissingCase::Boolean {
            missing: jv_checker::pattern::MissingBooleanCase::False,
            suggestion,
        } => {
            assert_eq!(suggestion.quick_fix_id, "when.add.branch");
            assert!(suggestion.label_en.contains("false"));
            assert!(suggestion.label_ja.contains("false"));
        }
        other => panic!("expected boolean missing case, got {other:?}"),
    }
}

#[test]
fn else_branch_suppresses_missing_cases() {
    let facts = analyze("val label = when (flag) { true -> \"yes\" else -> \"no\" }\n");
    assert!(
        facts.is_exhaustive(),
        "else branch should cover remaining cases"
    );
}

#[test]
fn wildcard_pattern_covers_all_cases() {
    let facts = analyze("val label = when (flag) { _ -> \"any\" }\n");
    assert!(
        facts.is_exhaustive(),
        "wildcard should cover all boolean cases"
    );
}

#[test]
fn guard_pattern_delegates_to_inner_literal() {
    let facts = analyze("val label = when (flag) { true && shouldEmit() -> \"yes\" }\n");
    assert!(
        facts.missing_cases().iter().any(|case| matches!(
            case,
            jv_checker::pattern::MissingCase::Boolean {
                missing: jv_checker::pattern::MissingBooleanCase::False,
                ..
            }
        )),
        "guarded literal should behave like literal for coverage purposes",
    );
}

#[test]
fn sealed_variant_missing_case_is_reported() {
    let mut service = PatternMatchService::new();
    service.register_sealed_type("Result", ["Success", "Failure"]);
    let expr = when_with_constructors("result", &["Success"]);
    let facts = service.analyze(&expr, PatternTarget::Java25);
    assert!(facts.missing_cases().iter().any(|case| matches!(
        case,
        jv_checker::pattern::MissingCase::SealedVariant {
            type_name,
            variant,
            ..
        } if type_name == "Result" && variant == "Failure"
    )));
}

#[test]
fn enum_constant_missing_case_is_reported() {
    let mut service = PatternMatchService::new();
    service.register_enum("Color", ["Red", "Blue"]);
    let expr = when_with_constructors("color", &["Red"]);
    let facts = service.analyze(&expr, PatternTarget::Java25);
    assert!(facts.missing_cases().iter().any(|case| matches!(
        case,
        jv_checker::pattern::MissingCase::EnumConstant {
            enum_type,
            constant,
            ..
        } if enum_type == "Color" && constant == "Blue"
    )));
}

#[test]
fn cache_records_hits_after_repeated_analysis() {
    let expr = parse_when_expression("val label = when (flag) { true -> \"yes\" }\n");
    let mut service = PatternMatchService::new();
    let _ = service.analyze(&expr, PatternTarget::Java25);
    let _ = service.analyze(&expr, PatternTarget::Java25);
    let metrics = service.take_cache_metrics();
    assert!(
        metrics.hits >= 1,
        "expected cache hit after re-analyzing same expression"
    );
    assert!(metrics.misses >= 1, "first analysis should register a miss");
}

#[test]
fn null_arm_records_narrowing_for_else_branch() {
    let expr =
        parse_when_expression("val label = when (value) { null -> \"missing\" else -> value }\n");
    let mut service = PatternMatchService::new();
    let facts = service.analyze(&expr, PatternTarget::Java25);
    let fallback = facts
        .fallback_narrowing()
        .expect("fallback snapshot should exist when null arm present");
    assert!(fallback
        .on_match()
        .iter()
        .any(|binding| binding.variable == "value"
            && binding.nullability == NarrowedNullability::NonNull));
}

#[test]
fn guard_span_is_recorded_in_narrowing_snapshot() {
    let expr = parse_when_expression(
        "val label = when (value) { null && isSkippable() -> \"missing\" else -> value }\n",
    );
    let mut service = PatternMatchService::new();
    let facts = service.analyze(&expr, PatternTarget::Java25);
    let arm_snapshot = facts
        .arm_narrowing(0)
        .expect("first arm should have narrowing snapshot");
    assert!(
        arm_snapshot.guard_span().is_some(),
        "guard span should be captured"
    );
    assert!(
        arm_snapshot.guard_evaluated(),
        "guard evaluation flag should be set"
    );
}

fn when_with_constructors(subject: &str, variants: &[&str]) -> Expression {
    let span = Span::dummy();
    Expression::When {
        expr: Some(Box::new(Expression::Identifier(
            subject.into(),
            span.clone(),
        ))),
        arms: variants
            .iter()
            .map(|variant| WhenArm {
                pattern: Pattern::Constructor {
                    name: (*variant).to_string(),
                    patterns: Vec::new(),
                    span: span.clone(),
                },
                guard: None,
                body: Expression::Literal(Literal::Number("1".to_string()), span.clone()),
                span: span.clone(),
            })
            .collect(),
        else_arm: None,
        implicit_end: None,
        span,
    }
}

#[test]
fn analyze_records_facts_for_each_target() {
    let expr =
        parse_when_expression("val label = when (value) { null -> \"missing\" else -> value }\n");
    let mut service = PatternMatchService::new();
    service.analyze(&expr, PatternTarget::Java25);
    service.analyze(&expr, PatternTarget::Java21);

    let recorded = service.recorded_facts();
    assert_eq!(recorded.len(), 2, "facts should be stored per target");
    assert!(recorded
        .keys()
        .any(|(_, target)| *target == PatternTarget::Java25));
    assert!(recorded
        .keys()
        .any(|(_, target)| *target == PatternTarget::Java21));
}

#[test]
fn invalidate_target_drops_cached_entries() {
    let expr = parse_when_expression("val label = when (flag) { true -> \"yes\" }\n");
    let mut service = PatternMatchService::new();
    service.analyze(&expr, PatternTarget::Java25);
    service.analyze(&expr, PatternTarget::Java21);

    service.invalidate_target(PatternTarget::Java25);
    assert!(service
        .recorded_facts()
        .keys()
        .all(|(_, target)| *target != PatternTarget::Java25));
}

#[test]
fn invalidate_dirty_nodes_forces_cache_miss() {
    let expr = parse_when_expression("val label = when (flag) { true -> \"yes\" }\n");
    let mut service = PatternMatchService::new();
    service.take_cache_metrics();

    service.analyze(&expr, PatternTarget::Java25);
    let recorded_keys: Vec<_> = service.recorded_facts().keys().cloned().collect();
    let node_id = recorded_keys[0].0;

    service.take_cache_metrics();

    service.invalidate_dirty_nodes(&[node_id]);
    assert!(
        service.recorded_facts().is_empty(),
        "facts should be cleared"
    );

    service.analyze(&expr, PatternTarget::Java25);
    let metrics_after = service.take_cache_metrics();
    assert_eq!(metrics_after.hits, 0);
    assert_eq!(
        metrics_after.misses, 1,
        "analysis should recompute after invalidation"
    );
}

#[test]
fn recorded_facts_can_be_drained() {
    let expr =
        parse_when_expression("val label = when (flag) { true -> \"yes\" else -> \"no\" }\n");
    let mut service = PatternMatchService::new();
    service.analyze(&expr, PatternTarget::Java25);
    assert!(!service.recorded_facts().is_empty());

    let drained = service.take_recorded_facts();
    assert_eq!(drained.len(), 1);
    assert!(service.recorded_facts().is_empty());
}

#[test]
fn cache_capacity_matches_default_configuration() {
    let service = PatternMatchService::new();
    assert_eq!(service.cache_capacity(), 256);
}

#[test]
fn fallback_narrowing_absent_without_null_arm() {
    let expr =
        parse_when_expression("val label = when (flag) { true -> \"yes\" else -> \"no\" }\n");
    let mut service = PatternMatchService::new();
    let facts = service.analyze(&expr, PatternTarget::Java25);
    assert!(facts.fallback_narrowing().is_none());
}

#[test]
fn literal_arm_does_not_introduce_narrowing_snapshot() {
    let expr =
        parse_when_expression("val label = when (flag) { true -> \"yes\" else -> \"no\" }\n");
    let mut service = PatternMatchService::new();
    let facts = service.analyze(&expr, PatternTarget::Java25);
    assert!(facts.arm_narrowing(0).is_none());
}
