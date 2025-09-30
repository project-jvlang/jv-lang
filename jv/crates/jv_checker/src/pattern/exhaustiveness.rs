use super::facts::{MissingBooleanCase, MissingCase, PatternMatchFacts};
use jv_ast::{Expression, Literal, Pattern, WhenArm};

/// Analyzes a `when` expression and produces pattern matching facts focused on
/// exhaustiveness. The current implementation targets boolean subjects and will
/// expand in subsequent tasks to cover enums, sealed classes, and ranges.
pub fn analyze(expression: &Expression) -> PatternMatchFacts {
    match expression {
        Expression::When {
            expr: _,
            arms,
            else_arm,
            implicit_end,
            ..
        } => analyze_when_expression(arms, else_arm.is_some() || implicit_end.is_some()),
        _ => PatternMatchFacts::empty(),
    }
}

fn analyze_when_expression(arms: &[WhenArm], has_fallback_branch: bool) -> PatternMatchFacts {
    if has_fallback_branch {
        return PatternMatchFacts::empty();
    }

    let mut coverage = BooleanCoverage::default();
    for arm in arms {
        coverage.accumulate(&arm.pattern);
        if coverage.is_fully_covered() {
            break;
        }
    }

    if coverage.is_fully_covered() {
        return PatternMatchFacts::empty();
    }

    let mut missing_cases = Vec::new();
    if !coverage.covers_true {
        missing_cases.push(MissingCase::Boolean {
            missing: MissingBooleanCase::True,
        });
    }
    if !coverage.covers_false {
        missing_cases.push(MissingCase::Boolean {
            missing: MissingBooleanCase::False,
        });
    }

    PatternMatchFacts::with_missing_cases(missing_cases)
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

    fn is_fully_covered(&self) -> bool {
        self.covers_true && self.covers_false
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
        let facts = analyze(&when_expr);
        let cases = facts.missing_cases();
        assert_eq!(cases.len(), 1);
        assert_eq!(
            cases[0],
            MissingCase::Boolean {
                missing: MissingBooleanCase::False
            }
        );
    }

    #[test]
    fn wildcard_covers_all_boolean_cases() {
        let span = dummy_span();
        let when_expr = when_expression(vec![Pattern::Wildcard(span.clone())], false);
        let facts = analyze(&when_expr);
        assert!(facts.missing_cases().is_empty());
    }

    #[test]
    fn else_branch_suppresses_missing_case_diagnostics() {
        let span = dummy_span();
        let when_expr = when_expression(vec![literal_bool(true, &span)], true);
        let facts = analyze(&when_expr);
        assert!(facts.missing_cases().is_empty());
    }
}
