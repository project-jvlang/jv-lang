use jv_ast::types::Literal;
use jv_ast::Expression;
use jv_ast::Pattern;

use super::{graph::BranchAssumption, NullabilityKind};

/// Captures the nullability assumptions derived from matching a pattern.
#[derive(Debug, Clone, Default)]
pub struct PatternMatchAssumptions {
    pub on_match: Option<BranchAssumption>,
    pub on_mismatch: Option<BranchAssumption>,
}

impl PatternMatchAssumptions {
    pub fn none() -> Self {
        Self::default()
    }
}

/// Computes the branch assumptions that apply when a pattern matches or fails to match.
pub fn analyze_pattern(subject: Option<&Expression>, pattern: &Pattern) -> PatternMatchAssumptions {
    match pattern {
        Pattern::Literal(Literal::Null, _) => subject
            .and_then(extract_identifier)
            .map(|identifier| PatternMatchAssumptions {
                on_match: Some(BranchAssumption::Equals {
                    variable: identifier.to_string(),
                    state: NullabilityKind::Nullable,
                }),
                on_mismatch: Some(BranchAssumption::NotEquals {
                    variable: identifier.to_string(),
                    state: NullabilityKind::NonNull,
                }),
            })
            .unwrap_or_default(),
        Pattern::Constructor { .. } => subject
            .and_then(extract_identifier)
            .map(|identifier| PatternMatchAssumptions {
                on_match: Some(BranchAssumption::Equals {
                    variable: identifier.to_string(),
                    state: NullabilityKind::NonNull,
                }),
                on_mismatch: None,
            })
            .unwrap_or_default(),
        Pattern::Guard { pattern, .. } => analyze_pattern(subject, pattern),
        _ => PatternMatchAssumptions::none(),
    }
}

fn extract_identifier(expr: &Expression) -> Option<&str> {
    match expr {
        Expression::Identifier(name, _) => Some(name.as_str()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::types::Span;

    #[test]
    fn literal_null_pattern_emits_branch_assumptions() {
        let subject = Expression::Identifier("value".into(), Span::dummy());
        let pattern = Pattern::Literal(Literal::Null, Span::dummy());

        let assumptions = analyze_pattern(Some(&subject), &pattern);

        match assumptions.on_match {
            Some(BranchAssumption::Equals {
                ref variable,
                state,
            }) => {
                assert_eq!(variable, "value");
                assert_eq!(state, NullabilityKind::Nullable);
            }
            other => panic!("unexpected on_match assumption: {:?}", other),
        }

        match assumptions.on_mismatch {
            Some(BranchAssumption::NotEquals {
                ref variable,
                state,
            }) => {
                assert_eq!(variable, "value");
                assert_eq!(state, NullabilityKind::NonNull);
            }
            other => panic!("unexpected on_mismatch assumption: {:?}", other),
        }
    }

    #[test]
    fn non_identifier_subject_yields_no_assumptions() {
        let subject = Expression::Literal(Literal::Null, Span::dummy());
        let pattern = Pattern::Literal(Literal::Null, Span::dummy());

        let assumptions = analyze_pattern(Some(&subject), &pattern);
        assert!(assumptions.on_match.is_none());
        assert!(assumptions.on_mismatch.is_none());
    }
}
