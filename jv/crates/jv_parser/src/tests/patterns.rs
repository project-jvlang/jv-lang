use super::support::{first_statement, parse_program, parse_program_result};
use jv_ast::{Expression, Literal, Pattern, Statement};

fn extract_when_expression(source: &str) -> Expression {
    let program = parse_program(source);
    match first_statement(&program) {
        Statement::ValDeclaration { initializer, .. }
        | Statement::VarDeclaration {
            initializer: Some(initializer),
            ..
        } => initializer.clone(),
        Statement::Expression { expr, .. } => expr.clone(),
        other => panic!("expected expression-bearing statement, got {other:?}"),
    }
}

fn first_arm_pattern(source: &str) -> Pattern {
    let Expression::When { arms, .. } = extract_when_expression(source) else {
        panic!("expected when expression");
    };
    arms[0].pattern.clone()
}

#[test]
fn parses_exclusive_range_pattern_as_non_inclusive() {
    let expr = extract_when_expression(
        "val bucket = when (score) { in 0..10 -> \"low\" else -> \"high\" }\n",
    );

    let Expression::When { arms, .. } = expr else {
        panic!("expected when expression");
    };
    let Pattern::Range {
        start,
        end,
        inclusive_end,
        ..
    } = &arms[0].pattern
    else {
        panic!("expected range pattern in first arm: {:?}", arms[0].pattern);
    };

    assert!(matches!(**start, Expression::Literal(Literal::Number(ref value), _) if value == "0"));
    assert!(matches!(**end, Expression::Literal(Literal::Number(ref value), _) if value == "10"));
    assert!(
        !inclusive_end,
        "exclusive range should have inclusive_end = false"
    );
}

#[test]
fn parses_inclusive_range_pattern_with_flag() {
    let expr = extract_when_expression(
        "val bucket = when (score) { in 0..=10 -> \"mid\" else -> \"high\" }\n",
    );
    let Expression::When { arms, .. } = expr else {
        panic!("expected when expression");
    };
    let Pattern::Range { inclusive_end, .. } = &arms[0].pattern else {
        panic!("expected range pattern");
    };
    assert!(
        *inclusive_end,
        "inclusive range should have inclusive_end = true"
    );
}

#[test]
fn parses_string_literal_pattern() {
    let pattern = first_arm_pattern("val result = when (value) { \"ok\" -> 1 else -> 0 }\n");
    match pattern {
        Pattern::Literal(Literal::String(value), _) => assert_eq!(value, "ok"),
        other => panic!("expected string literal pattern, got {other:?}"),
    }
}

#[test]
fn parses_boolean_literal_pattern() {
    let pattern = first_arm_pattern("val result = when (value) { true -> 1 else -> 0 }\n");
    match pattern {
        Pattern::Literal(Literal::Boolean(value), _) => assert!(value),
        other => panic!("expected boolean literal pattern, got {other:?}"),
    }
}

#[test]
fn parses_null_literal_pattern() {
    let pattern = first_arm_pattern("val result = when (value) { null -> 1 else -> 0 }\n");
    matches!(pattern, Pattern::Literal(Literal::Null, _))
        .then_some(())
        .expect("expected null literal pattern");
}

#[test]
fn identifier_pattern_captures_binding_name() {
    let pattern = first_arm_pattern("val result = when (value) { answer -> answer }\n");
    match pattern {
        Pattern::Identifier(name, _) => assert_eq!(name, "answer"),
        other => panic!("expected identifier pattern, got {other:?}"),
    }
}

#[test]
fn is_pattern_produces_constructor_without_arguments() {
    let pattern = first_arm_pattern("val result = when (value) { is String -> 1 else -> 0 }\n");
    match pattern {
        Pattern::Constructor { name, patterns, .. } => {
            assert_eq!(name, "String");
            assert!(
                patterns.is_empty(),
                "is-pattern should not record nested arguments"
            );
        }
        other => panic!("expected constructor pattern, got {other:?}"),
    }
}

#[test]
fn parses_constructor_pattern_with_nested_arguments() {
    let expr = extract_when_expression(
        "val result = when (point) { Point(origin(), radius) -> 1 else -> 0 }\n",
    );
    let Expression::When { arms, .. } = expr else {
        panic!("expected when expression");
    };
    let Pattern::Constructor { name, patterns, .. } = &arms[0].pattern else {
        panic!("expected constructor pattern");
    };
    assert_eq!(name, "Point");
    assert_eq!(patterns.len(), 2);

    match &patterns[0] {
        Pattern::Constructor {
            name: inner_name,
            patterns: inner_patterns,
            ..
        } => {
            assert_eq!(inner_name, "origin");
            assert!(inner_patterns.is_empty());
        }
        other => panic!("expected nested constructor pattern, got {other:?}"),
    }

    match &patterns[1] {
        Pattern::Identifier(identifier, _) => assert_eq!(identifier, "radius"),
        other => panic!("expected identifier binding, got {other:?}"),
    }
}

#[test]
fn constructor_pattern_allows_trailing_comma() {
    let pattern = first_arm_pattern(
        "val result = when (tuple) { Pair(first, second,) -> first else -> 0 }\n",
    );
    match pattern {
        Pattern::Constructor { name, patterns, .. } => {
            assert_eq!(name, "Pair");
            assert_eq!(patterns.len(), 2, "trailing comma should be ignored");
        }
        other => panic!("expected constructor pattern, got {other:?}"),
    }
}

#[test]
fn constructor_pattern_span_covers_entire_pattern() {
    let pattern = first_arm_pattern("val result = when (point) { Point(x, y) -> 1 else -> 0 }\n");
    let Pattern::Constructor { span, .. } = pattern else {
        panic!("expected constructor pattern");
    };
    assert_eq!(span.start_line, span.end_line);
    assert!(
        span.end_column.saturating_sub(span.start_column) >= "Point(x, y)".len(),
        "constructor span should cover identifier and arguments",
    );
}

#[test]
fn guard_pattern_span_covers_condition() {
    let expr = extract_when_expression(
        "val label = when (value) { is String && value.length > 0 -> value else -> \"empty\" }\n",
    );
    let Expression::When { arms, .. } = expr else {
        panic!("expected when expression");
    };
    let Pattern::Constructor {
        span: base_span, ..
    } = &arms[0].pattern
    else {
        panic!("expected constructor pattern for is String guard");
    };
    let guard = arms[0]
        .guard
        .as_ref()
        .expect("guard expression should be present");
    let Expression::Binary {
        span: condition_span,
        ..
    } = guard
    else {
        panic!("expected guard condition to be a binary expression");
    };

    assert!(
        condition_span.start_line >= base_span.start_line
            && condition_span.start_column > base_span.start_column,
        "guard condition span should follow base pattern span",
    );
}

#[test]
fn subjectless_when_expression_preserves_none_subject() {
    let expr = extract_when_expression("val flag = when { condition() -> true else -> false }\n");
    let Expression::When {
        expr: subject,
        arms,
        else_arm,
        ..
    } = expr
    else {
        panic!("expected when expression");
    };
    assert!(
        subject.is_none(),
        "subjectless when should have no subject expression"
    );
    assert_eq!(
        arms.len(),
        1,
        "subjectless when should record single conditional arm"
    );
    assert!(
        else_arm.is_some(),
        "subjectless when should preserve else branch separately"
    );
}

#[test]
fn wildcard_pattern_is_recognised() {
    let expr = extract_when_expression("val any = when (input) { _ -> 0 }\n");
    let Expression::When { arms, .. } = expr else {
        panic!("expected when expression");
    };
    assert!(
        matches!(arms[0].pattern, Pattern::Wildcard(_)),
        "underscore should parse as wildcard pattern",
    );
}

#[test]
fn range_pattern_accepts_identifier_bounds() {
    let expr = extract_when_expression(
        "val bucket = when (value) { in start..=end -> \"hit\" else -> \"miss\" }\n",
    );
    let Expression::When { arms, .. } = expr else {
        panic!("expected when expression");
    };
    let Pattern::Range { start, end, .. } = &arms[0].pattern else {
        panic!("expected range pattern");
    };

    assert!(
        matches!(**start, Expression::Identifier(ref name, _) if name == "start"),
        "start bound should be parsed as identifier",
    );
    assert!(
        matches!(**end, Expression::Identifier(ref name, _) if name == "end"),
        "end bound should be parsed as identifier",
    );
}

#[test]
fn guard_pattern_wraps_constructor_pattern() {
    let expr = extract_when_expression(
        "val result = when (value) { Point(x, y) && x > 0 -> 1 else -> 0 }\n",
    );
    let Expression::When { arms, .. } = expr else {
        panic!("expected when expression");
    };
    let arm = &arms[0];
    assert!(
        matches!(&arm.pattern, Pattern::Constructor { name, .. } if name == "Point"),
        "guard should wrap constructor pattern",
    );
    let guard = arm.guard.as_ref().expect("guard should be present");
    assert!(
        matches!(guard, Expression::Binary { .. }),
        "guard condition should remain an expression",
    );
}

#[test]
fn invalid_in_pattern_reports_jv3104() {
    let result = parse_program_result(
        "val bucket = when (score) { in is Positive -> \"bad\" else -> \"ok\" }\n",
    );
    assert!(result.is_err(), "malformed in pattern should fail");
    let message = format!("{:#?}", result.err().unwrap());
    assert!(
        message.contains("JV3104"),
        "diagnostic should mention JV3104, got: {message}"
    );
}

#[test]
fn nested_constructor_with_guard_retains_inner_pattern() {
    let pattern = first_arm_pattern(
        "val result = when (value) { Container(Point(x, y)) && x > 0 -> x else -> 0 }\n",
    );
    match pattern {
        Pattern::Constructor { name, patterns, .. } => {
            assert_eq!(name, "Container");
            assert_eq!(patterns.len(), 1);
            matches!(&patterns[0], Pattern::Constructor { name, .. } if name == "Point")
                .then_some(())
                .expect("nested constructor should be preserved");
        }
        other => panic!("expected constructor pattern, got {other:?}"),
    }
}

#[test]
fn multiple_when_arms_preserve_order() {
    let Expression::When { arms, else_arm, .. } =
        extract_when_expression("val value = when (x) { 0 -> 0 1 -> 1 else -> -1 }\n")
    else {
        panic!("expected when expression");
    };
    let labels: Vec<_> = arms
        .iter()
        .map(|arm| match &arm.pattern {
            Pattern::Literal(Literal::Number(value), _) => value.clone(),
            Pattern::Wildcard(_) => "_".to_string(),
            other => format!("{other:?}"),
        })
        .collect();
    assert_eq!(labels, vec!["0".to_string(), "1".to_string()]);
    assert!(
        else_arm.is_some(),
        "else branch should be tracked separately"
    );
}

#[test]
fn multiple_is_type_when_arms_parse() {
    let result = parse_program_result(
        r#"val value = when (x) {
    is String -> 1
    is Int -> 2
    else -> 3
}"#,
    );
    assert!(
        result.is_ok(),
        "when expression with consecutive is-pattern arms should parse"
    );
}

#[test]
fn when_arm_accepts_is_pattern_after_newline() {
    let Expression::When { arms, else_arm, .. } = extract_when_expression(
        "val routed = when (subject) {\n    is Type -> subject.accept()\n    else -> subject.reject()\n}\n",
    ) else {
        panic!("expected when expression");
    };

    assert_eq!(
        arms.len(),
        1,
        "expected a single pattern arm before the else branch"
    );
    match &arms[0].pattern {
        Pattern::Constructor { name, patterns, .. } => {
            assert_eq!(name, "Type");
            assert!(
                patterns.is_empty(),
                "is-pattern should not produce nested args"
            );
        }
        other => panic!("expected constructor pattern from is arm, got {other:?}"),
    }

    assert!(
        arms[0].guard.is_none(),
        "is-pattern arm should not include an implicit guard"
    );

    assert!(else_arm.is_some(), "else arm should be captured separately");
}

#[test]
fn binary_is_requires_same_line_expression() {
    // Steering 2025-10: 改行を挟んだ `is` はパターンでも二項演算子でもなく、独立した式として扱う。
    let program = parse_program("val outcome = subject\nis Type\n");

    assert_eq!(
        program.statements.len(),
        3,
        "expected val + two expression statements"
    );

    matches!(&program.statements[0], Statement::ValDeclaration { .. })
        .then_some(())
        .expect("first statement should remain the original val declaration");

    match &program.statements[1] {
        Statement::Expression { expr, .. } => {
            assert!(
                matches!(expr, Expression::Identifier(name, _) if name == "is"),
                "newline should keep `is` as a standalone identifier expression",
            );
        }
        other => {
            panic!("expected second statement to be bare identifier expression, got {other:?}")
        }
    }

    match &program.statements[2] {
        Statement::Expression { expr, .. } => {
            assert!(
                matches!(expr, Expression::Identifier(name, _) if name == "Type"),
                "third statement should be the trailing identifier expression",
            );
        }
        other => panic!("expected trailing identifier expression, got {other:?}"),
    }
}

#[test]
fn forbidden_if_expression_reports_jv3103() {
    let result = parse_program_result("val value = if (true) 1 else 0\n");
    assert!(result.is_err(), "if expression should be rejected");
    let error = result.err().unwrap();
    let message = format!("{error:?}");
    assert!(
        message.contains("JV3103"),
        "diagnostic should mention JV3103, got: {message}"
    );
}

#[test]
fn range_pattern_retains_source_span() {
    let expr = extract_when_expression(
        "val bucket = when (value) { in start..=end -> \"hit\" else -> \"miss\" }\n",
    );
    let Expression::When { arms, .. } = expr else {
        panic!("expected when expression");
    };
    let Pattern::Range { span, .. } = &arms[0].pattern else {
        panic!("expected range pattern");
    };
    assert!(
        span.end_column > span.start_column,
        "span should cover the entire pattern"
    );
}
