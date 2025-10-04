//! Shared runtime utilities for the jv compiler workspace.
//!
//! The `jv_support` crate hosts infrastructure that is reused across
//! compilation stages. The initial module exposes arena-backed memory pools
//! that enable high reuse between AST lowering runs.

pub mod arena;
pub mod i18n;
pub mod perf;

pub use arena::{
    ArenaAccessor, PoolMetrics, PoolSessionMetrics, TransformPools, TransformPoolsGuard,
};
pub use i18n::{catalog as load_catalog, LocaleCode, TemplateCatalog};
pub use perf::report::{
    BudgetChecks as PerfBudgetChecks, PerfBudget, PerfReport, RunSample as PerfRunSample,
    Summary as PerfSummary,
};
