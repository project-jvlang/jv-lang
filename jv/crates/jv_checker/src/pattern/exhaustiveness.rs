use super::catalog::PatternTypeCatalog;
use super::facts::{MissingBooleanCase, MissingCase, MissingCaseSuggestion, PatternMatchFacts};
use jv_ast::{Expression, Literal, Pattern, WhenArm};
use std::collections::{BTreeMap, BTreeSet};

/// Analyzes a `when` expression and produces pattern matching facts focused on
/// exhaustiveness. The catalog provides contextual knowledge about sealed
/// hierarchies and enum constants so the analyzer can emit precise
/// suggestions.
pub fn analyze(expression: &Expression, catalog: &PatternTypeCatalog) -> PatternMatchFacts {
    match expression {
        Expression::When {
            expr: _,
            arms,
            else_arm,
            implicit_end,
            ..
        } => analyze_when_expression(arms, else_arm.is_some() || implicit_end.is_some(), catalog),
        _ => PatternMatchFacts::empty(),
    }
}

fn analyze_when_expression(
    arms: &[WhenArm],
    has_fallback_branch: bool,
    catalog: &PatternTypeCatalog,
) -> PatternMatchFacts {
    if has_fallback_branch {
        return PatternMatchFacts::empty();
    }

    let mut coverage = BooleanCoverage::default();
    let mut sealed_seen: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut enum_seen: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let mut has_catch_all_pattern = false;

    for arm in arms {
        coverage.accumulate(&arm.pattern);
        accumulate_pattern(
            &arm.pattern,
            catalog,
            &mut sealed_seen,
            &mut enum_seen,
            &mut has_catch_all_pattern,
        );
    }

    let mut missing_cases = Vec::new();
    if !coverage.covers_true {
        missing_cases.push(missing_boolean_case(MissingBooleanCase::True));
    }
    if !coverage.covers_false {
        missing_cases.push(missing_boolean_case(MissingBooleanCase::False));
    }

    if !has_catch_all_pattern {
        missing_cases.extend(missing_sealed_cases(&sealed_seen, catalog));
        missing_cases.extend(missing_enum_cases(&enum_seen, catalog));
    }

    if missing_cases.is_empty() {
        PatternMatchFacts::empty()
    } else {
        PatternMatchFacts::with_missing_cases(missing_cases)
    }
}

#[derive(Default)]
struct BooleanCoverage {
    covers_true: bool,
    covers_false: bool,
}

impl BooleanCoverage {
    fn accumulate(&mut self, pattern: &Pattern) {
        let (covers_true, covers_false) = boolean_coverage(pattern);
        self.covers_true |= covers_true;
        self.covers_false |= covers_false;
    }
}

fn boolean_coverage(pattern: &Pattern) -> (bool, bool) {
    match pattern {
        Pattern::Literal(Literal::Boolean(true), _) => (true, false),
        Pattern::Literal(Literal::Boolean(false), _) => (false, true),
        Pattern::Wildcard(_) => (true, true),
        Pattern::Guard { pattern, .. } => boolean_coverage(pattern),
        _ => (false, false),
    }
}

fn accumulate_pattern(
    pattern: &Pattern,
    catalog: &PatternTypeCatalog,
    sealed_seen: &mut BTreeMap<String, BTreeSet<String>>,
    enum_seen: &mut BTreeMap<String, BTreeSet<String>>,
    has_catch_all: &mut bool,
) {
    match pattern {
        Pattern::Wildcard(_) | Pattern::Identifier(_, _) => {
            *has_catch_all = true;
        }
        Pattern::Constructor { name, .. } => {
            if let Some(base) = catalog.sealed_base_for_variant(name) {
                sealed_seen
                    .entry(base.to_string())
                    .or_default()
                    .insert(name.clone());
            }
            if let Some(enum_name) = catalog.enum_for_constant(name) {
                enum_seen
                    .entry(enum_name.to_string())
                    .or_default()
                    .insert(name.clone());
            }
        }
        Pattern::Guard { pattern, .. } => {
            accumulate_pattern(pattern, catalog, sealed_seen, enum_seen, has_catch_all);
        }
        _ => {}
    }
}

fn missing_boolean_case(missing: MissingBooleanCase) -> MissingCase {
    let (label_en, label_ja) = boolean_labels(missing);
    let suggestion = MissingCaseSuggestion::new(
        "when.add.branch",
        format!("insert branch template for `{label_en}`"),
        format!("`{label_ja}` ケースの分岐テンプレートを挿入"),
    );
    MissingCase::Boolean {
        missing,
        suggestion,
    }
}

fn missing_sealed_cases(
    sealed_seen: &BTreeMap<String, BTreeSet<String>>,
    catalog: &PatternTypeCatalog,
) -> Vec<MissingCase> {
    let mut missing = Vec::new();
    for (type_name, seen_variants) in sealed_seen {
        if let Some(variants) = catalog.sealed_variants(type_name) {
            for variant in variants.iter() {
                if !seen_variants.contains(variant) {
                    missing.push(sealed_missing_case(type_name, variant));
                }
            }
        }
    }
    missing
}

fn missing_enum_cases(
    enum_seen: &BTreeMap<String, BTreeSet<String>>,
    catalog: &PatternTypeCatalog,
) -> Vec<MissingCase> {
    let mut missing = Vec::new();
    for (enum_name, seen_constants) in enum_seen {
        if let Some(constants) = catalog.enum_constants(enum_name) {
            for constant in constants.iter() {
                if !seen_constants.contains(constant) {
                    missing.push(enum_missing_case(enum_name, constant));
                }
            }
        }
    }
    missing
}

fn sealed_missing_case(type_name: &str, variant: &str) -> MissingCase {
    let suggestion = MissingCaseSuggestion::new(
        "when.add.branch",
        format!("add `is {variant}` branch"),
        format!("`is {variant}` 分岐を追加"),
    );
    MissingCase::SealedVariant {
        type_name: type_name.to_string(),
        variant: variant.to_string(),
        suggestion,
    }
}

fn enum_missing_case(enum_name: &str, constant: &str) -> MissingCase {
    let suggestion = MissingCaseSuggestion::new(
        "when.add.branch",
        format!("add `{constant}` branch"),
        format!("`{constant}` ケースの分岐を追加"),
    );
    MissingCase::EnumConstant {
        enum_type: enum_name.to_string(),
        constant: constant.to_string(),
        suggestion,
    }
}

fn boolean_labels(case: MissingBooleanCase) -> (&'static str, &'static str) {
    match case {
        MissingBooleanCase::True => ("true", "true"),
        MissingBooleanCase::False => ("false", "false"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::{Expression, Literal, Pattern, Span, WhenArm};

    fn dummy_span() -> Span {
        Span::dummy()
    }

    fn literal_bool(value: bool, span: &Span) -> Pattern {
        Pattern::Literal(Literal::Boolean(value), span.clone())
    }

    fn when_expression(arms: Vec<Pattern>, has_else: bool) -> Expression {
        let span = dummy_span();
        Expression::When {
            expr: Some(Box::new(Expression::Identifier(
                "flag".into(),
                span.clone(),
            ))),
            arms: arms
                .into_iter()
                .map(|pattern| WhenArm {
                    pattern,
                    guard: None,
                    body: Expression::Literal(Literal::Number("0".into()), span.clone()),
                    span: span.clone(),
                })
                .collect(),
            else_arm: if has_else {
                Some(Box::new(Expression::Literal(
                    Literal::Number("1".into()),
                    span.clone(),
                )))
            } else {
                None
            },
            implicit_end: None,
            span,
        }
    }

    #[test]
    fn missing_false_branch_is_detected() {
        let span = dummy_span();
        let when_expr = when_expression(vec![literal_bool(true, &span)], false);
        let catalog = PatternTypeCatalog::default();
        let facts = analyze(&when_expr, &catalog);
        let cases = facts.missing_cases();
        assert_eq!(cases.len(), 1);
        match &cases[0] {
            MissingCase::Boolean {
                missing: MissingBooleanCase::False,
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
    fn wildcard_covers_all_boolean_cases() {
        let span = dummy_span();
        let when_expr = when_expression(vec![Pattern::Wildcard(span.clone())], false);
        let catalog = PatternTypeCatalog::default();
        let facts = analyze(&when_expr, &catalog);
        assert!(facts.missing_cases().is_empty());
    }

    #[test]
    fn else_branch_suppresses_missing_case_diagnostics() {
        let span = dummy_span();
        let when_expr = when_expression(vec![literal_bool(true, &span)], true);
        let catalog = PatternTypeCatalog::default();
        let facts = analyze(&when_expr, &catalog);
        assert!(facts.missing_cases().is_empty());
    }

    #[test]
    fn missing_sealed_variant_is_detected() {
        let span = dummy_span();
        let when_expr = when_expression(
            vec![Pattern::Constructor {
                name: "Success".into(),
                patterns: Vec::new(),
                span: span.clone(),
            }],
            false,
        );
        let mut catalog = PatternTypeCatalog::default();
        catalog.register_sealed_type(
            "Result".to_string(),
            vec!["Success".to_string(), "Failure".to_string()].into_iter(),
        );
        let facts = analyze(&when_expr, &catalog);
        assert!(facts.missing_cases().iter().any(|case| matches!(
            case,
            MissingCase::SealedVariant {
                type_name,
                variant,
                ..
            } if type_name == "Result" && variant == "Failure"
        )));
    }

    #[test]
    fn missing_enum_constant_is_detected() {
        let span = dummy_span();
        let when_expr = when_expression(
            vec![Pattern::Constructor {
                name: "Red".into(),
                patterns: Vec::new(),
                span: span.clone(),
            }],
            false,
        );
        let mut catalog = PatternTypeCatalog::default();
        catalog.register_enum(
            "Color".to_string(),
            vec!["Red".to_string(), "Blue".to_string()].into_iter(),
        );
        let facts = analyze(&when_expr, &catalog);
        assert!(facts.missing_cases().iter().any(|case| matches!(
            case,
            MissingCase::EnumConstant {
                enum_type,
                constant,
                ..
            } if enum_type == "Color" && constant == "Blue"
        )));
    }
}
