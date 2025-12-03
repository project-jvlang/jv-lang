//! DSLプラグイン周辺の管理機構。

pub mod registry;

pub use registry::{DslKeywordCache, PluginEntry, PluginRegistry};
