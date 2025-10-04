use super::{CacheMetrics, FingerprintHash};
use crate::constraint::{ConstraintSolution, GenericConstraint, GenericConstraintKind};
use crate::types::{SymbolId, TypeId};
use rustc_hash::FxHashMap;
use std::cell::Cell;
use std::cmp::Ordering;
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ConstraintCacheKey {
    symbol: SymbolId,
    fingerprint: FingerprintHash,
}

impl ConstraintCacheKey {
    fn new(symbol: SymbolId, fingerprint: FingerprintHash) -> Self {
        Self {
            symbol,
            fingerprint,
        }
    }
}

#[derive(Debug, Default)]
pub struct ConstraintCache {
    entries: FxHashMap<ConstraintCacheKey, ConstraintSolution>,
    metrics: Cell<CacheMetrics>,
}

impl ConstraintCache {
    pub fn new() -> Self {
        Self {
            entries: FxHashMap::default(),
            metrics: Cell::new(CacheMetrics::default()),
        }
    }

    pub fn lookup(
        &self,
        symbol: &SymbolId,
        fingerprint: FingerprintHash,
    ) -> Option<ConstraintSolution> {
        let mut metrics = self.metrics.get();
        let key = ConstraintCacheKey::new(symbol.clone(), fingerprint);
        let result = self.entries.get(&key).cloned();
        if result.is_some() {
            metrics.record_hit();
        } else {
            metrics.record_miss();
        }
        self.metrics.set(metrics);
        result
    }

    pub fn store(
        &mut self,
        symbol: SymbolId,
        fingerprint: FingerprintHash,
        solution: ConstraintSolution,
    ) {
        let key = ConstraintCacheKey::new(symbol, fingerprint);
        self.entries.insert(key, solution);
    }

    pub fn metrics(&self) -> CacheMetrics {
        self.metrics.get()
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.metrics.set(CacheMetrics::default());
    }

    pub fn fingerprint_for(constraints: &[GenericConstraint]) -> FingerprintHash {
        let mut components: Vec<FingerprintComponent> = constraints
            .iter()
            .filter_map(|constraint| {
                if let GenericConstraintKind::BoundRequirement {
                    owner,
                    parameter,
                    predicate,
                } = &constraint.kind
                {
                    Some(FingerprintComponent::new(
                        owner.clone(),
                        *parameter,
                        predicate.key(),
                    ))
                } else {
                    None
                }
            })
            .collect();

        if components.is_empty() {
            return FingerprintHash::ZERO;
        }

        components.sort();
        FingerprintHash::of(&components)
    }
}

#[derive(Debug, Clone, Eq)]
struct FingerprintComponent {
    symbol: SymbolId,
    parameter: u32,
    predicate: String,
}

impl FingerprintComponent {
    fn new(symbol: SymbolId, parameter: TypeId, predicate: String) -> Self {
        Self {
            symbol,
            parameter: parameter.to_raw(),
            predicate,
        }
    }
}

impl Ord for FingerprintComponent {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.symbol.as_str(), self.parameter, &self.predicate).cmp(&(
            other.symbol.as_str(),
            other.parameter,
            &other.predicate,
        ))
    }
}

impl PartialOrd for FingerprintComponent {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for FingerprintComponent {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
            && self.parameter == other.parameter
            && self.predicate == other.predicate
    }
}

impl Hash for FingerprintComponent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
        self.parameter.hash(state);
        self.predicate.hash(state);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BoundPredicate, TraitBound};
    use jv_ast::Span;

    fn constraint(owner: &str, parameter: u32, predicate: &str) -> GenericConstraint {
        GenericConstraint::new(
            GenericConstraintKind::BoundRequirement {
                owner: SymbolId::from(owner),
                parameter: TypeId::new(parameter),
                predicate: BoundPredicate::Trait(TraitBound::simple(predicate)),
            },
            Span::dummy(),
        )
    }

    #[test]
    fn fingerprint_is_order_independent() {
        let a = constraint("demo.Foo", 0, "Comparable");
        let b = constraint("demo.Foo", 1, "Numeric");
        let first = vec![a.clone(), b.clone()];
        let second = vec![b, a];
        assert_eq!(
            ConstraintCache::fingerprint_for(&first),
            ConstraintCache::fingerprint_for(&second)
        );
    }

    #[test]
    fn cache_records_hits_and_misses() {
        let mut cache = ConstraintCache::new();
        let symbol = SymbolId::from("demo.Foo");
        let constraints = vec![constraint(symbol.as_str(), 0, "Comparable")];
        let fingerprint = ConstraintCache::fingerprint_for(&constraints);
        assert!(cache.lookup(&symbol, fingerprint).is_none());
        let solution = ConstraintSolution::new(symbol.clone());
        cache.store(symbol.clone(), fingerprint, solution.clone());
        assert!(cache.lookup(&symbol, fingerprint).is_some());
        let metrics = cache.metrics();
        assert_eq!(metrics.lookups, 2);
        assert_eq!(metrics.hits, 1);
        assert_eq!(metrics.misses, 1);
    }
}
