pub mod context;
pub mod error;
pub mod profiling;
pub mod sequence_pipeline;
pub mod transform;
pub mod types;

#[cfg(feature = "debug-ir")]
pub mod debug;

pub use jv_support::arena::{
    ArenaAccessor as TransformArenaAccessor, PoolMetrics as TransformPoolMetrics,
    PoolSessionMetrics as TransformPoolSessionMetrics, TransformPools, TransformPoolsGuard,
};

pub use profiling::{PerfMetrics, StageTiming, TransformProfiler};

/// Convenience re-exports for common IR types and transformation helpers.
pub mod prelude {
    pub use crate::context::TransformContext;
    pub use crate::error::TransformError;
    pub use crate::sequence_pipeline::*;
    pub use crate::transform::*;
    pub use crate::types::*;
    pub use crate::{
        PerfMetrics, StageTiming, TransformArenaAccessor, TransformPoolMetrics,
        TransformPoolSessionMetrics, TransformPools, TransformPoolsGuard, TransformProfiler,
    };
}

pub use prelude::*;

#[cfg(test)]
mod tests;
