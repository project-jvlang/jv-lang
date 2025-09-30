//! Data structures shared across pattern matching analyses.

/// Analysis facts produced by the pattern matcher.
///
/// The structure now tracks missing cases discovered during exhaustiveness
/// analysis. Subsequent tasks will extend it with narrowing facts and telemetry
/// payloads that other compiler stages can consume.
#[derive(Debug, Clone, Default)]
pub struct PatternMatchFacts {
    missing_cases: Vec<MissingCase>,
}

impl PatternMatchFacts {
    /// Returns an empty fact set. Provided for readability at call sites.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Constructs a fact set with the provided missing-case information.
    pub fn with_missing_cases(missing_cases: Vec<MissingCase>) -> Self {
        Self { missing_cases }
    }

    pub fn missing_cases(&self) -> &[MissingCase] {
        &self.missing_cases
    }

    pub fn into_missing_cases(self) -> Vec<MissingCase> {
        self.missing_cases
    }

    pub fn is_exhaustive(&self) -> bool {
        self.missing_cases.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MissingCase {
    Boolean { missing: MissingBooleanCase },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MissingBooleanCase {
    True,
    False,
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
