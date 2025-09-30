//! Pattern analysis service scaffold.

mod exhaustiveness;
mod facts;
mod narrowing;
mod normalizer;
mod validator;

use crate::CheckError;
use jv_ast::{Expression, Program, Span};
use std::collections::{hash_map::DefaultHasher, HashMap, HashSet};
use std::hash::{Hash, Hasher};

pub use facts::{
    ArmId, MissingBooleanCase, MissingCase, NarrowedBinding, NarrowedNullability, NarrowingFacts,
    NarrowingSnapshot, PatternCacheMetrics, PatternMatchFacts,
};
pub use normalizer::PatternNormalizer;

const DEFAULT_CACHE_CAPACITY: usize = 256;

/// Target Java version for lowering decisions. Included in cache keys so that
/// per-target analyses can coexist without invalidating one another.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum PatternTarget {
    #[default]
    Java25,
    Java21,
}

/// Facade coordinating pattern analysis. In Task 2 it handles validation and
/// cache plumbing so later phases can focus on analysis algorithms.
#[derive(Debug)]
pub struct PatternMatchService {
    cache: PatternFactsCache,
    metrics: PatternCacheMetrics,
    normalizer: PatternNormalizer,
    recorded_facts: HashMap<(u64, PatternTarget), PatternMatchFacts>,
}

impl PatternMatchService {
    pub fn new() -> Self {
        Self {
            cache: PatternFactsCache::with_capacity(DEFAULT_CACHE_CAPACITY),
            metrics: PatternCacheMetrics::default(),
            normalizer: PatternNormalizer::new(),
            recorded_facts: HashMap::new(),
        }
    }

    /// Delegates structural validation (legacy WhenUsageValidator) to the new
    /// module, keeping existing diagnostics intact.
    pub fn validate_program(&mut self, program: &Program) -> Vec<CheckError> {
        validator::validate_program(self, program)
    }

    /// Analyzes the provided `when` expression. The current task returns stub
    /// facts while wiring cache bookkeeping so future stages can swap in a real
    /// analyzer without touching the call sites.
    pub fn analyze(&mut self, expression: &Expression, target: PatternTarget) -> PatternMatchFacts {
        self.normalizer.normalize(expression);
        let key = PatternCacheKey::from_expression(expression, target);
        if let Some(facts) = self.cache.get(&key).cloned() {
            self.metrics.record_hit();
            self.record_facts(&key, &facts);
            return facts;
        }

        self.metrics.record_miss();
        let mut facts = exhaustiveness::analyze(expression);
        let narrowing_facts = narrowing::analyze(expression);
        facts.set_narrowing(narrowing_facts);
        self.record_facts(&key, &facts);
        self.cache.insert(key, facts.clone());
        facts
    }

    /// Removes cached entries that belong to invalidated AST nodes.
    pub fn invalidate_dirty_nodes(&mut self, dirty_nodes: &[u64]) {
        self.cache.invalidate_nodes(dirty_nodes);
        if dirty_nodes.is_empty() {
            return;
        }
        let dirty: HashSet<u64> = dirty_nodes.iter().copied().collect();
        self.recorded_facts
            .retain(|(node_id, _), _| !dirty.contains(node_id));
    }

    /// Clears cached analyses for a specific lowering target.
    pub fn invalidate_target(&mut self, target: PatternTarget) {
        self.cache.invalidate_target(target);
        self.recorded_facts
            .retain(|(_, recorded_target), _| *recorded_target != target);
    }

    /// Extracts cache metrics, resetting the internal counters.
    pub fn take_cache_metrics(&mut self) -> PatternCacheMetrics {
        let metrics = self.metrics;
        self.metrics = PatternCacheMetrics::default();
        metrics
    }

    pub fn cache_capacity(&self) -> usize {
        self.cache.capacity
    }

    pub fn take_recorded_facts(&mut self) -> HashMap<(u64, PatternTarget), PatternMatchFacts> {
        std::mem::take(&mut self.recorded_facts)
    }

    pub fn recorded_facts(&self) -> &HashMap<(u64, PatternTarget), PatternMatchFacts> {
        &self.recorded_facts
    }

    fn record_facts(&mut self, key: &PatternCacheKey, facts: &PatternMatchFacts) {
        if key.node_id == 0 {
            return;
        }
        self.recorded_facts
            .insert((key.node_id, key.target), facts.clone());
    }
}

#[derive(Debug)]
struct PatternFactsCache {
    entries: HashMap<PatternCacheKey, PatternMatchFacts>,
    capacity: usize,
}

impl PatternFactsCache {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            entries: HashMap::new(),
            capacity,
        }
    }

    fn get(&mut self, key: &PatternCacheKey) -> Option<&PatternMatchFacts> {
        self.entries.get(key)
    }

    fn insert(&mut self, key: PatternCacheKey, facts: PatternMatchFacts) {
        if self.entries.len() >= self.capacity {
            if let Some(first_key) = self.entries.keys().next().cloned() {
                self.entries.remove(&first_key);
            }
        }
        self.entries.insert(key, facts);
    }

    fn invalidate_nodes(&mut self, dirty_nodes: &[u64]) {
        if dirty_nodes.is_empty() {
            return;
        }
        let dirty: HashSet<u64> = dirty_nodes.iter().copied().collect();
        self.entries.retain(|key, _| !dirty.contains(&key.node_id));
    }

    fn invalidate_target(&mut self, target: PatternTarget) {
        self.entries.retain(|key, _| key.target != target);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PatternCacheKey {
    node_id: u64,
    fingerprint: u64,
    target: PatternTarget,
}

impl PatternCacheKey {
    fn from_expression(expression: &Expression, target: PatternTarget) -> Self {
        let span = expression_span(expression);
        let (node_id, fingerprint) = span
            .map(|span| (node_identifier(span), hash_span(span)))
            .unwrap_or_default();
        Self {
            node_id,
            fingerprint,
            target,
        }
    }
}

pub(crate) fn expression_span(expression: &Expression) -> Option<&Span> {
    match expression {
        Expression::Literal(_, span)
        | Expression::Identifier(_, span)
        | Expression::Binary { span, .. }
        | Expression::Unary { span, .. }
        | Expression::Call { span, .. }
        | Expression::MemberAccess { span, .. }
        | Expression::NullSafeMemberAccess { span, .. }
        | Expression::IndexAccess { span, .. }
        | Expression::NullSafeIndexAccess { span, .. }
        | Expression::StringInterpolation { span, .. }
        | Expression::When { span, .. }
        | Expression::If { span, .. }
        | Expression::Block { span, .. }
        | Expression::Array { span, .. }
        | Expression::Lambda { span, .. }
        | Expression::Try { span, .. }
        | Expression::This(span)
        | Expression::Super(span) => Some(span),
    }
}

pub(crate) fn node_identifier(span: &Span) -> u64 {
    ((span.start_line as u64) << 32) | span.start_column as u64
}

fn hash_span(span: &Span) -> u64 {
    let mut hasher = DefaultHasher::new();
    span.start_line.hash(&mut hasher);
    span.start_column.hash(&mut hasher);
    span.end_line.hash(&mut hasher);
    span.end_column.hash(&mut hasher);
    hasher.finish()
}
