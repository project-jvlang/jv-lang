//! Binding analysis and normalization for implicit declarations.

mod resolver;

pub use resolver::{
    BindingResolution, BindingUsageSummary, LateInitManifest, LateInitSeed,
    normalize_import_aliases, resolve_bindings,
};
