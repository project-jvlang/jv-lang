//! Incremental cache structures for memoizing inference results in the jv compiler.
//!
//! The cache stores solved type signatures together with the substitutions and
//! diagnostics required to rebuild downstream artefacts. Entries are indexed by
//! stable AST identifiers (`AstId`) and guarded by a `FingerprintHash` derived
//! from the source node. When the fingerprint remains unchanged we can re-use
//! the cached signature and skip constraint regeneration.

mod constraint_cache;
mod dependency_tracker;

use crate::constraint::AstId;
use crate::service::{FactSpan, TypeScheme};
use crate::types::{TypeId, TypeKind};
use rustc_hash::FxHasher;
use serde::Serialize;
use smallvec::SmallVec;
use std::cell::Cell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::time::Instant;

pub use constraint_cache::ConstraintCache;
pub use dependency_tracker::DependencyTracker;

/// Tuple describing a single type substitution `(type_variable, resolved_type)`.
pub type Substitution = (TypeId, TypeKind);

/// Small buffer used to keep substitutions without immediate heap allocation.
pub type SubstitutionList = SmallVec<[Substitution; 4]>;

/// Hash used to detect structural changes in AST nodes between inference runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct FingerprintHash(u64);

impl FingerprintHash {
    /// Sentinel value representing an unknown fingerprint.
    pub const ZERO: Self = Self(0);

    /// Creates a new hash from the provided raw value.
    pub fn new(value: u64) -> Self {
        Self(value)
    }

    /// Convenience helper that hashes an arbitrary value using the `FxHasher`.
    pub fn of<T: Hash>(value: &T) -> Self {
        let mut hasher = FxHasher::default();
        value.hash(&mut hasher);
        Self(hasher.finish())
    }

    /// Returns the raw 64-bit representation of the fingerprint.
    pub fn value(self) -> u64 {
        self.0
    }
}

impl Default for FingerprintHash {
    fn default() -> Self {
        FingerprintHash::ZERO
    }
}

/// Aggregated metrics describing cache effectiveness.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize)]
pub struct CacheMetrics {
    pub lookups: u64,
    pub hits: u64,
    pub misses: u64,
    pub invalidations: u64,
    pub preserved_constraints: u64,
}

impl CacheMetrics {
    pub fn record_hit(&mut self) {
        self.lookups = self.lookups.saturating_add(1);
        self.hits = self.hits.saturating_add(1);
    }

    pub fn record_miss(&mut self) {
        self.lookups = self.lookups.saturating_add(1);
        self.misses = self.misses.saturating_add(1);
    }

    pub fn record_invalidation(&mut self, count: u64) {
        self.invalidations = self.invalidations.saturating_add(count);
    }

    pub fn record_preserved(&mut self, count: u64) {
        self.preserved_constraints = self.preserved_constraints.saturating_add(count);
    }

    /// Returns the cache hit rate in the range `[0.0, 1.0]` when at least one
    /// lookup has been performed.
    pub fn hit_rate(self) -> Option<f64> {
        if self.lookups == 0 {
            None
        } else {
            Some(self.hits as f64 / self.lookups as f64)
        }
    }
}

/// Lightweight diagnostic payload retained with cache entries so we can replay
/// user-facing errors without re-running the full pipeline.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CachedDiagnostic {
    pub code: String,
    pub message: String,
    pub span: Option<FactSpan>,
    pub notes: Vec<String>,
}

impl CachedDiagnostic {
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
            span: None,
            notes: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: FactSpan) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_notes<I, S>(mut self, notes: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        self.notes = notes.into_iter().map(Into::into).collect();
        self
    }
}

/// Immutable view returned by [`InferenceCache::lookup`].
#[derive(Debug, Clone)]
pub struct CachedSignature {
    pub fingerprint: FingerprintHash,
    pub signature: TypeScheme,
    pub substitutions: SubstitutionList,
    pub diagnostics: Vec<CachedDiagnostic>,
    pub last_used: Instant,
}

impl CachedSignature {
    fn from_entry(entry: &InferenceCacheEntry) -> Self {
        Self {
            fingerprint: entry.fingerprint,
            signature: entry.signature.clone(),
            substitutions: entry.substitutions.clone(),
            diagnostics: entry.diagnostics.clone(),
            last_used: entry.last_used,
        }
    }
}

/// Result emitted by incremental inference passes for inserting or refreshing
/// cache entries.
#[derive(Debug, Clone)]
pub struct SignatureUpdate {
    pub node: AstId,
    pub fingerprint: FingerprintHash,
    pub signature: TypeScheme,
    pub substitutions: SubstitutionList,
    pub diagnostics: Vec<CachedDiagnostic>,
    pub preserved_constraints: usize,
}

impl SignatureUpdate {
    pub fn new(node: AstId, fingerprint: FingerprintHash, signature: TypeScheme) -> Self {
        Self {
            node,
            fingerprint,
            signature,
            substitutions: SubstitutionList::new(),
            diagnostics: Vec::new(),
            preserved_constraints: 0,
        }
    }

    pub fn with_substitutions<I>(mut self, substitutions: I) -> Self
    where
        I: IntoIterator<Item = Substitution>,
    {
        self.substitutions.extend(substitutions);
        self
    }

    pub fn with_diagnostics(mut self, diagnostics: Vec<CachedDiagnostic>) -> Self {
        self.diagnostics = diagnostics;
        self
    }

    pub fn with_preserved_constraints(mut self, count: usize) -> Self {
        self.preserved_constraints = count;
        self
    }
}

#[derive(Debug, Clone)]
struct InferenceCacheEntry {
    fingerprint: FingerprintHash,
    signature: TypeScheme,
    substitutions: SubstitutionList,
    diagnostics: Vec<CachedDiagnostic>,
    last_used: Instant,
}

impl InferenceCacheEntry {
    fn from_update(update: &SignatureUpdate) -> Self {
        Self {
            fingerprint: update.fingerprint,
            signature: update.signature.clone(),
            substitutions: update.substitutions.clone(),
            diagnostics: update.diagnostics.clone(),
            last_used: Instant::now(),
        }
    }

    fn refresh(&mut self, update: &SignatureUpdate) {
        self.fingerprint = update.fingerprint;
        self.signature = update.signature.clone();
        self.substitutions = update.substitutions.clone();
        self.diagnostics = update.diagnostics.clone();
        self.last_used = Instant::now();
    }
}

/// Primary cache responsible for memoising inference outcomes across edits.
#[derive(Debug, Default)]
pub struct InferenceCache {
    entries: HashMap<AstId, InferenceCacheEntry>,
    metrics: Cell<CacheMetrics>,
}

impl InferenceCache {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            metrics: Cell::new(CacheMetrics::default()),
        }
    }

    /// Returns `Some(CachedSignature)` when an entry exists for the given node.
    /// The returned value owns cloned data so the caller can safely cache it
    /// without holding a reference into the cache itself.
    pub fn lookup(&self, node: AstId) -> Option<CachedSignature> {
        let mut metrics = self.metrics.get();
        let result = self.entries.get(&node).map(CachedSignature::from_entry);
        if result.is_some() {
            metrics.record_hit();
        } else {
            metrics.record_miss();
        }
        self.metrics.set(metrics);
        result
    }

    /// Inserts or refreshes cache entries according to the provided updates.
    pub fn update(&mut self, updates: &[SignatureUpdate]) {
        if updates.is_empty() {
            return;
        }

        let mut metrics = self.metrics.get();
        for update in updates {
            metrics.record_preserved(update.preserved_constraints as u64);
            self.entries
                .entry(update.node)
                .and_modify(|entry| entry.refresh(update))
                .or_insert_with(|| InferenceCacheEntry::from_update(update));
        }
        self.metrics.set(metrics);
    }

    /// Removes entries associated with the provided AST identifiers and records
    /// the number of invalidations.
    pub fn invalidate(&mut self, dirty: &[AstId]) {
        if dirty.is_empty() {
            return;
        }

        let mut removed = 0u64;
        for node in dirty {
            if self.entries.remove(node).is_some() {
                removed = removed.saturating_add(1);
            }
        }

        if removed > 0 {
            let mut metrics = self.metrics.get();
            metrics.record_invalidation(removed);
            self.metrics.set(metrics);
        }
    }

    /// Uses a [`DependencyTracker`] to expand the dirty set before performing an
    /// invalidation. Returns the cascade of nodes that were considered dirty.
    pub fn invalidate_with_tracker<I>(
        &mut self,
        tracker: &mut DependencyTracker,
        dirty: I,
    ) -> Vec<AstId>
    where
        I: IntoIterator<Item = AstId>,
    {
        let cascade = tracker.invalidate_from(dirty);
        self.invalidate(cascade.as_slice());
        cascade
    }

    /// Returns the current number of cached entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Returns true when the cache has no entries.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Provides a snapshot of the cache metrics.
    pub fn metrics(&self) -> CacheMetrics {
        self.metrics.get()
    }

    /// Clears all entries and resets metrics.
    pub fn clear(&mut self) {
        self.entries.clear();
        self.metrics.set(CacheMetrics::default());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{TypeKind, TypeVariant};

    fn sample_scheme() -> TypeScheme {
        TypeScheme::new(Vec::new(), sample_type("Int"))
    }

    fn sample_type(name: &'static str) -> TypeKind {
        TypeKind::new(TypeVariant::Primitive(name))
    }

    fn substitution_list() -> SubstitutionList {
        let mut list = SubstitutionList::new();
        list.push((TypeId::new(0), sample_type("Int")));
        list
    }

    #[test]
    fn fingerprint_hash_is_deterministic() {
        let f1 = FingerprintHash::of(&"let x = 1");
        let f2 = FingerprintHash::of(&"let x = 1");
        assert_eq!(f1, f2);
    }

    #[test]
    fn lookup_records_hits_and_misses() {
        let mut cache = InferenceCache::new();
        let fingerprint = FingerprintHash::of(&42u32);
        let update = SignatureUpdate::new(1, fingerprint, sample_scheme())
            .with_substitutions(substitution_list());
        cache.update(&[update]);

        assert!(cache.lookup(1).is_some());
        assert!(cache.lookup(2).is_none());

        let metrics = cache.metrics();
        assert_eq!(metrics.lookups, 2);
        assert_eq!(metrics.hits, 1);
        assert_eq!(metrics.misses, 1);
        assert!(metrics.hit_rate().is_some());
    }

    #[test]
    fn invalidate_removes_entries_and_updates_metrics() {
        let mut cache = InferenceCache::new();
        let fingerprint = FingerprintHash::of(&7u32);
        let update = SignatureUpdate::new(1, fingerprint, sample_scheme());
        cache.update(&[update]);
        assert_eq!(cache.len(), 1);

        cache.invalidate(&[1]);
        assert!(cache.is_empty());
        assert_eq!(cache.metrics().invalidations, 1);
    }

    #[test]
    fn metrics_reset_with_clear() {
        let mut cache = InferenceCache::new();
        cache.lookup(1);
        cache.clear();
        assert_eq!(cache.len(), 0);
        let metrics = cache.metrics();
        assert_eq!(metrics.lookups, 0);
        assert_eq!(metrics.hits, 0);
        assert_eq!(metrics.misses, 0);
    }

    #[test]
    fn signature_update_tracks_preserved_constraints() {
        let mut cache = InferenceCache::new();
        let update = SignatureUpdate::new(1, FingerprintHash::new(5), sample_scheme())
            .with_preserved_constraints(3);
        cache.update(&[update]);
        assert_eq!(cache.metrics().preserved_constraints, 3);
    }

    #[test]
    fn cached_signature_contains_cloned_data() {
        let mut cache = InferenceCache::new();
        let update = SignatureUpdate::new(1, FingerprintHash::new(9), sample_scheme())
            .with_substitutions(substitution_list());
        cache.update(&[update]);
        let mut result = cache.lookup(1).expect("hit");
        result
            .substitutions
            .push((TypeId::new(10), sample_type("String")));
        assert_eq!(cache.lookup(1).unwrap().substitutions.len(), 1);
    }

    #[test]
    fn invalidate_with_tracker_cascades_dependencies() {
        let mut cache = InferenceCache::new();
        let mut tracker = DependencyTracker::new();
        tracker.set_dependencies(2, [1]);
        tracker.set_dependencies(3, [2]);

        let scheme = sample_scheme();
        cache.update(&[
            SignatureUpdate::new(1, FingerprintHash::new(1), scheme.clone()),
            SignatureUpdate::new(2, FingerprintHash::new(2), scheme.clone()),
            SignatureUpdate::new(3, FingerprintHash::new(3), scheme),
        ]);

        let cascade = cache.invalidate_with_tracker(&mut tracker, [1]);
        assert_eq!(cascade, vec![1, 2, 3]);
        assert!(cache.lookup(1).is_none());
        assert!(cache.lookup(2).is_none());
        assert!(cache.lookup(3).is_none());
        assert_eq!(cache.metrics().invalidations, 3);
    }
}
