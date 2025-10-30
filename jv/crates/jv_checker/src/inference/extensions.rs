//! Registry for built-in extension functions used during type inference.
//!
//! The registry maps a receiver type (e.g., `java.util.stream.Stream`) and method name (e.g., `map`)
//! to a `TypeScheme` describing the callable signature. Constraint generation consults the
//! registry when it encounters member access expressions so that extension functions defined
//! in the standard library can participate in inference even if their source code is not part
//! of the current compilation unit.

use crate::inference::environment::TypeScheme;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct MethodCandidate {
    receiver: &'static str,
    entry_index: usize,
}

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
    method_index: HashMap<&'static str, Vec<MethodCandidate>>,
}

impl ExtensionRegistry {
    /// Creates an empty registry.
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            method_index: HashMap::new(),
        }
    }

    /// Registers an extension function for the specified receiver type.
    pub fn register(&mut self, receiver: &'static str, method: &'static str, scheme: TypeScheme) {
        let entry_vec = self.entries.entry(receiver).or_insert_with(Vec::new);
        entry_vec.push(ExtensionEntry { method, scheme });
        let entry_index = entry_vec.len().saturating_sub(1);

        self.method_index
            .entry(method)
            .or_insert_with(Vec::new)
            .push(MethodCandidate {
                receiver,
                entry_index,
            });
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

    /// Returns all receivers that declare the given method.
    pub fn candidates_for_method(&self, method: &str) -> Vec<(&'static str, &TypeScheme)> {
        self.method_index
            .get(method)
            .into_iter()
            .flat_map(|candidates| {
                candidates.iter().filter_map(|candidate| {
                    self.entries
                        .get(candidate.receiver)
                        .and_then(|entries| entries.get(candidate.entry_index))
                        .map(|entry| (candidate.receiver, &entry.scheme))
                })
            })
            .collect()
    }
}
