//! Type-level evaluation engine used to resolve const expressions during inference.
//!
//! The Phase 2 implementation focuses on finite arithmetic, comparison, and boolean
//! expressions. Results are cached using structural fingerprints and wired into the
//! incremental [`DependencyTracker`] so that downstream invalidations can reuse the
//! existing infrastructure. Termination guarantees are enforced through depth limits
//! and simple cycle detection to avoid non-terminating evaluations.

mod evaluator;
mod termination;

#[cfg(test)]
mod tests;

pub use evaluator::{
    EvaluationContext, EvaluationDependencies, TypeLevelBinding, TypeLevelDiagnostic,
    TypeLevelEvaluationOutcome, TypeLevelEvaluator,
};
pub use termination::{TerminationConfig, TerminationError};
