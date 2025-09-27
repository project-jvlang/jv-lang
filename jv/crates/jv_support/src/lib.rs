//! Shared runtime utilities for the jv compiler workspace.
//!
//! The `jv_support` crate hosts infrastructure that is reused across
//! compilation stages. The initial module exposes arena-backed memory pools
//! that enable high reuse between AST lowering runs.

pub mod arena;

pub use arena::{
    ArenaAccessor, PoolMetrics, PoolSessionMetrics, TransformPools, TransformPoolsGuard,
};
