//! Import resolution services and supporting data structures.
pub mod resolution;

pub use resolution::{ImportError, ImportResolutionService, ResolvedImport, ResolvedImportKind};
