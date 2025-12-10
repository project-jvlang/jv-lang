//! 型制約システムのエントリポイント。
//!
//! ここでは制約の型定義、HIR からの収集、グラフ構築をまとめて公開する。

pub mod collect;
pub mod graph;
#[cfg(test)]
mod tests;
pub mod types;

pub use collect::collect_constraints;
pub use graph::{ConstraintGraph, ConstraintGraphEdge, ConstraintGraphNode};
pub use types::{Constraint, TypeRef, TypeVarId};
