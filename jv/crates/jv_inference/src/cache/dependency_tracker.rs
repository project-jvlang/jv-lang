//! Dependency tracking utilities supporting incremental inference.

use super::FingerprintHash;
use crate::constraint::AstId;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;

/// Maintains dependency edges between AST nodes and tracks change fingerprints.
#[derive(Debug, Default)]
pub struct DependencyTracker {
    /// Mapping from a dependency node to the nodes that rely on it.
    type_dependencies: FxHashMap<AstId, Vec<AstId>>,
    /// Reverse mapping from a dependent node to the dependencies it references.
    reverse_dependencies: FxHashMap<AstId, Vec<AstId>>,
    /// Cached fingerprints associated with each node.
    inference_fingerprints: FxHashMap<AstId, FingerprintHash>,
    /// Most recent invalidation cascade produced by [`Self::invalidate_from`].
    invalidation_cascade: Vec<AstId>,
}

impl DependencyTracker {
    /// Creates an empty dependency tracker.
    pub fn new() -> Self {
        Self::default()
    }

    /// Replaces the dependency set for a particular node.
    pub fn set_dependencies<I>(&mut self, dependent: AstId, dependencies: I)
    where
        I: IntoIterator<Item = AstId>,
    {
        let mut unique_deps = FxHashSet::default();
        let mut ordered = Vec::new();
        for dep in dependencies {
            if unique_deps.insert(dep) {
                ordered.push(dep);
            }
        }

        if let Some(previous) = self.reverse_dependencies.remove(&dependent) {
            for dep in previous {
                if let Some(dependents) = self.type_dependencies.get_mut(&dep) {
                    dependents.retain(|&node| node != dependent);
                    if dependents.is_empty() {
                        self.type_dependencies.remove(&dep);
                    }
                }
            }
        }

        if ordered.is_empty() {
            return;
        }

        for dep in &ordered {
            let entry = self.type_dependencies.entry(*dep).or_default();
            if !entry.contains(&dependent) {
                entry.push(dependent);
            }
        }
        self.reverse_dependencies.insert(dependent, ordered);
    }

    /// Returns the dependents that reference the provided node.
    pub fn dependents_of(&self, dependency: AstId) -> &[AstId] {
        self.type_dependencies
            .get(&dependency)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    /// Updates the fingerprint for the specified node and returns `true` when it
    /// differs from the previously recorded value.
    pub fn fingerprint_changed(&mut self, node: AstId, fingerprint: FingerprintHash) -> bool {
        match self.inference_fingerprints.get(&node) {
            Some(existing) if *existing == fingerprint => false,
            _ => {
                self.inference_fingerprints.insert(node, fingerprint);
                true
            }
        }
    }

    /// Retrieves the fingerprint for the specified node when available.
    pub fn fingerprint(&self, node: AstId) -> Option<FingerprintHash> {
        self.inference_fingerprints.get(&node).copied()
    }

    /// Removes fingerprint and dependency information for the provided node.
    pub fn evict(&mut self, node: AstId) {
        self.inference_fingerprints.remove(&node);

        if let Some(dependencies) = self.reverse_dependencies.remove(&node) {
            for dep in dependencies {
                if let Some(dependents) = self.type_dependencies.get_mut(&dep) {
                    dependents.retain(|&candidate| candidate != node);
                    if dependents.is_empty() {
                        self.type_dependencies.remove(&dep);
                    }
                }
            }
        }

        if let Some(mut dependents) = self.type_dependencies.remove(&node) {
            for dependent in dependents.drain(..) {
                if let Some(deps) = self.reverse_dependencies.get_mut(&dependent) {
                    deps.retain(|&candidate| candidate != node);
                    if deps.is_empty() {
                        self.reverse_dependencies.remove(&dependent);
                    }
                }
            }
        }
    }

    /// Computes the cascade of nodes affected by the supplied dirty roots.
    pub fn invalidate_from<I>(&mut self, roots: I) -> Vec<AstId>
    where
        I: IntoIterator<Item = AstId>,
    {
        let mut visited = FxHashSet::default();
        let mut queue = VecDeque::new();
        self.invalidation_cascade.clear();

        for root in roots {
            if visited.insert(root) {
                queue.push_back(root);
            }
        }

        while let Some(node) = queue.pop_front() {
            self.invalidation_cascade.push(node);
            if let Some(dependents) = self.type_dependencies.get(&node) {
                for dependent in dependents {
                    if visited.insert(*dependent) {
                        queue.push_back(*dependent);
                    }
                }
            }
        }

        self.invalidation_cascade.clone()
    }

    /// Returns the last computed invalidation cascade.
    pub fn last_cascade(&self) -> &[AstId] {
        &self.invalidation_cascade
    }

    /// Depth (in nodes) of the last invalidation cascade.
    pub fn cascade_depth(&self) -> usize {
        self.invalidation_cascade.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn set_dependencies_updates_bidirectional_maps() {
        let mut tracker = DependencyTracker::default();
        tracker.set_dependencies(2, [1, 3]);
        tracker.set_dependencies(4, [1]);

        assert_eq!(tracker.dependents_of(1), [2, 4]);
        assert_eq!(tracker.dependents_of(3), [2]);

        tracker.set_dependencies(2, [3]);
        assert_eq!(tracker.dependents_of(1), [4]);
        assert_eq!(tracker.dependents_of(3), [2]);
    }

    #[test]
    fn fingerprint_change_detection() {
        let mut tracker = DependencyTracker::default();
        let fp = FingerprintHash::of(&123u32);
        assert!(tracker.fingerprint_changed(1, fp));
        assert!(!tracker.fingerprint_changed(1, fp));
    }

    #[test]
    fn invalidate_from_walks_dependents() {
        let mut tracker = DependencyTracker::default();
        tracker.set_dependencies(2, [1]);
        tracker.set_dependencies(3, [2]);
        tracker.set_dependencies(4, [2]);

        let cascade = tracker.invalidate_from([1]);
        assert_eq!(cascade, vec![1, 2, 3, 4]);
        assert_eq!(tracker.cascade_depth(), 4);
    }

    #[test]
    fn evict_removes_reverse_links() {
        let mut tracker = DependencyTracker::default();
        tracker.set_dependencies(2, [1, 3]);
        tracker.set_dependencies(4, [2]);

        tracker.evict(2);
        assert!(tracker.dependents_of(1).is_empty());
        assert!(tracker.dependents_of(3).is_empty());

        let cascade = tracker.invalidate_from([1]);
        assert_eq!(cascade, vec![1]);
    }
}
