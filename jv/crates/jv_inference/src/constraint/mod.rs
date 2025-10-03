//! Constraint graph, builders, and related utilities for the type inference pipeline.
//!
//! The detailed implementation will be introduced in subsequent tasks. For now, this
//! module exists as a placeholder that documents the intended separation of concerns.

pub mod calls;
pub mod compact;
pub mod generic;
pub mod graph;
pub mod incremental;

pub use graph::{
    ConstraintGraph, ConstraintKind, ConstraintNode, ConstraintNodeId, Edge, EdgeKind, NodeId,
    SourceSpanTable, TypeNode, TypeNodeId,
};

pub use generic::{GenericConstraint, GenericConstraintKind};

pub use calls::{
    CallArgument, CallConstraintBuilder, CallConstraintInput, CallConstraintResult,
    CallResultBinding,
};

pub use compact::CompactConstraintGraph;

pub use incremental::{AstId, ConstraintDiff, IncrementalConstraintBuilder, IncrementalTelemetry};
