//! Kind inference environment and solver for higher-kinded types.
//!
//! Phase 2 introduces a dedicated module that tracks kind annotations on
//! generic parameters and validates type constructor applications such as
//! `Functor<F<_>>`.  The implementation focuses on single-level HKTs and
//! produces `JV2008` diagnostics when mismatches are detected.

pub mod solver;

#[cfg(test)]
mod tests;

pub use solver::{
    KindArgument, KindConstraint, KindEnvironment, KindSolution, KindSolver, KindSolverDiagnostic,
    KindSolverTelemetry,
};
