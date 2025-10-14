//! Import resolution services and supporting data structures.
pub mod diagnostics;
pub mod resolution;

pub use diagnostics::{ambiguous_import, from_error, missing_module, unknown_import};
pub use resolution::{ImportError, ImportResolutionService, ResolvedImport, ResolvedImportKind};
