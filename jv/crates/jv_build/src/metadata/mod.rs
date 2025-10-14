//! Symbol metadata indexing for smart import resolution.
//!
//! This module is responsible for scanning JDK modules and project classpath
//! entries to build a lightweight symbol index that the checker can consult
//! when resolving imports. The implementation emphasises streaming IO so that
//! large archives do not have to be fully loaded in memory.

mod builder;
mod classfile;
mod index;

pub use builder::{BuildContext, IndexError, SymbolIndexBuilder};
pub use classfile::{JavaMethodSignature, ModuleInfo, StaticMember, StaticMemberKind};
pub use index::{ModuleEntry, PackageEntry, SymbolIndex, TypeEntry};
