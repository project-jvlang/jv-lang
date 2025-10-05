use super::expression_span;
use super::facts::{
    ArmId, NarrowedBinding, NarrowedNullability, NarrowingFacts, NarrowingSnapshot,
};
use jv_ast::expression::ImplicitWhenEnd;
use jv_ast::Span;
use jv_ast::{Expression, Literal, Pattern};

/// Produces branch-level narrowing snapshots for a `when` expression.
pub fn analyze(expression: &Expression) -> NarrowingFacts {
    let mut facts = NarrowingFacts::default();

    let Expression::When {
        expr: subject,
        arms,
        else_arm,
        implicit_end,
        span,
        ..
    } = expression
    else {
        return facts;
    };

    let subject_name = subject
        .as_deref()
        .and_then(extract_identifier)
        .map(|name| name.to_string());

    let mut fallback_snapshot =
        NarrowingSnapshot::new(fallback_span(else_arm.as_deref(), implicit_end));
    let mut has_null_arm = false;

    for (index, arm) in arms.iter().enumerate() {
        let mut snapshot = NarrowingSnapshot::new(Some(arm.span.clone()));

        if let Some(name) = subject_name.as_ref() {
            if matches_null_pattern(&arm.pattern) {
                has_null_arm = true;
                snapshot.push_on_match(NarrowedBinding::new(name, NarrowedNullability::Nullable));
                snapshot.push_on_mismatch(NarrowedBinding::new(name, NarrowedNullability::NonNull));
                fallback_snapshot
                    .push_on_match(NarrowedBinding::new(name, NarrowedNullability::NonNull));
            } else if pattern_implies_non_null(&arm.pattern) {
                snapshot.push_on_match(NarrowedBinding::new(name, NarrowedNullability::NonNull));
            }
        }

        if let Some(guard) = &arm.guard {
            snapshot.set_guard_span(expression_span(guard).cloned());
            snapshot.mark_guard_evaluated();
        }

        facts.insert_arm(index as ArmId, snapshot);
    }

    if has_null_arm && subject_name.is_some() {
        // For else/no-match branch we already accumulated non-null bindings above.
        if fallback_snapshot.on_match().is_empty() {
            // Ensure fallback snapshot carries basic metadata even when no bindings were added.
            fallback_snapshot = NarrowingSnapshot::new(Some(span.clone()));
        }
        facts.set_fallback(fallback_snapshot);
    }

    facts
}

fn matches_null_pattern(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Literal(Literal::Null, _) => true,
        Pattern::Guard { pattern, .. } => matches_null_pattern(pattern),
        _ => false,
    }
}

fn pattern_implies_non_null(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Constructor { .. } => true,
        Pattern::Guard { pattern, .. } => pattern_implies_non_null(pattern),
        _ => false,
    }
}

fn extract_identifier(expr: &Expression) -> Option<&str> {
    match expr {
        Expression::Identifier(name, _) => Some(name.as_str()),
        _ => None,
    }
}

fn fallback_span(
    else_expr: Option<&Expression>,
    implicit_end: &Option<ImplicitWhenEnd>,
) -> Option<Span> {
    else_expr
        .and_then(expression_span)
        .cloned()
        .or_else(|| match implicit_end {
            Some(ImplicitWhenEnd::Unit { span }) => Some(span.clone()),
            None => None,
        })
}
