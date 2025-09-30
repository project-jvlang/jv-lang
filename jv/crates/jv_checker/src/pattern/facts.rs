//! Data structures shared across pattern matching analyses.

/// Analysis facts produced by the pattern matcher.
///
/// The structure intentionally starts empty in Task 2 so that later phases can
/// extend it with exhaustiveness, narrowing, and telemetry payloads without
/// breaking the API surface. Keeping the type clonable allows callers to cache
/// and re-use results across compiler stages.
#[derive(Debug, Clone, Default)]
pub struct PatternMatchFacts {
    // Placeholder field to keep the struct non-empty and future-proof.
    _reserved: (),
}

impl PatternMatchFacts {
    /// Returns an empty fact set. Provided for readability at call sites.
    pub fn empty() -> Self {
        Self::default()
    }
}

/// Aggregated metrics describing cache behaviour for pattern analysis.
#[derive(Debug, Clone, Copy, Default)]
pub struct PatternCacheMetrics {
    pub hits: u64,
    pub misses: u64,
}

impl PatternCacheMetrics {
    pub fn record_hit(&mut self) {
        self.hits = self.hits.saturating_add(1);
    }

    pub fn record_miss(&mut self) {
        self.misses = self.misses.saturating_add(1);
    }
}
