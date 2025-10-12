//! Binding analysis and normalization for implicit declarations.

mod resolver;

pub use resolver::{
    resolve_bindings, BindingResolution, BindingUsageSummary, LateInitManifest, LateInitSeed,
};
