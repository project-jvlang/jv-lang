use jv_ast::{Expression, ForInStatement, LoopStrategy, NumericRangeLoop};

/// Classification of loop iterables after parsing.
#[derive(Debug, Clone, Copy)]
pub enum LoopClassification<'a> {
    NumericRange { range: &'a NumericRangeLoop },
    Iterable,
    LazySequence { needs_cleanup: bool },
}

/// Determine how a `for-in` loop should be treated downstream.
pub fn classify_loop<'a>(for_in: &'a ForInStatement) -> LoopClassification<'a> {
    match &for_in.strategy {
        LoopStrategy::NumericRange(range) => LoopClassification::NumericRange { range },
        LoopStrategy::Iterable => LoopClassification::Iterable,
        LoopStrategy::LazySequence { needs_cleanup } => LoopClassification::LazySequence {
            needs_cleanup: *needs_cleanup,
        },
        LoopStrategy::Unknown => LoopClassification::Iterable,
    }
}

/// Rough heuristic to determine whether an expression can plausibly yield an iterable value.
pub fn expression_can_yield_iterable(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Identifier(_, _)
            | Expression::Call { .. }
            | Expression::MemberAccess { .. }
            | Expression::NullSafeMemberAccess { .. }
            | Expression::IndexAccess { .. }
            | Expression::NullSafeIndexAccess { .. }
            | Expression::Array { .. }
            | Expression::Block { .. }
    )
}
