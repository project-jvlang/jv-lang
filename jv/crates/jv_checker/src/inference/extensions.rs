//! Registry for built-in extension functions used during type inference.
//!
//! The registry maps a receiver type (e.g., `SequenceCore`) and method name (e.g., `map`)
//! to a `TypeScheme` describing the callable signature. Constraint generation consults the
//! registry when it encounters member access expressions so that extension functions defined
//! in the standard library can participate in inference even if their source code is not part
//! of the current compilation unit.

use crate::inference::environment::TypeScheme;
use std::collections::HashMap;

/// Describes an extension function that can be applied to a receiver type.
#[derive(Debug, Clone)]
pub struct ExtensionEntry {
    /// Method name as it appears in source (e.g., `map`).
    pub method: &'static str,
    /// Type scheme describing the callable signature of the extension.
    pub scheme: TypeScheme,
}

/// Lookup table for extension functions keyed by receiver type name.
#[derive(Debug, Default, Clone)]
pub struct ExtensionRegistry {
    entries: HashMap<&'static str, Vec<ExtensionEntry>>,
}

impl ExtensionRegistry {
    /// Creates an empty registry.
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    /// Registers an extension function for the specified receiver type.
    pub fn register(&mut self, receiver: &'static str, method: &'static str, scheme: TypeScheme) {
        self.entries
            .entry(receiver)
            .or_insert_with(Vec::new)
            .push(ExtensionEntry { method, scheme });
    }

    /// Looks up the type scheme for a receiver/method pair.
    pub fn lookup(&self, receiver: &str, method: &str) -> Option<&TypeScheme> {
        self.entries.get(receiver).and_then(|candidates| {
            candidates
                .iter()
                .find(|entry| entry.method == method)
                .map(|entry| &entry.scheme)
        })
    }
}
