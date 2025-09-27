pub mod context;
pub mod error;
pub mod transform;
pub mod types;

#[cfg(feature = "debug-ir")]
pub mod debug;

/// Convenience re-exports for common IR types and transformation helpers.
pub mod prelude {
    pub use crate::context::TransformContext;
    pub use crate::error::TransformError;
    pub use crate::transform::*;
    pub use crate::types::*;
}

pub use prelude::*;

#[cfg(test)]
mod tests;
