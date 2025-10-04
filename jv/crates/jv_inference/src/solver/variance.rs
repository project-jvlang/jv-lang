use crate::constraint::{GenericConstraint, GenericConstraintKind};
use crate::types::TypeId;
use std::collections::HashMap;

/// Variance annotation associated with a type parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variance {
    Covariant,
    Contravariant,
    Invariant,
    Bivariant,
}

impl Default for Variance {
    fn default() -> Self {
        Variance::Bivariant
    }
}

/// Individual usage position encountered during analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariancePosition {
    Covariant,
    Contravariant,
    Invariant,
}

#[derive(Debug, Default, Clone, Copy)]
struct VarianceStats {
    covariant: bool,
    contravariant: bool,
    invariant: bool,
}

impl VarianceStats {
    fn record(&mut self, position: VariancePosition) {
        match position {
            VariancePosition::Covariant => self.covariant = true,
            VariancePosition::Contravariant => self.contravariant = true,
            VariancePosition::Invariant => self.invariant = true,
        }
    }

    fn into_variance(self) -> Variance {
        if self.invariant || (self.covariant && self.contravariant) {
            Variance::Invariant
        } else if self.covariant {
            Variance::Covariant
        } else if self.contravariant {
            Variance::Contravariant
        } else {
            Variance::Bivariant
        }
    }
}

/// Table storing variance information for quantified type parameters.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct VarianceTable {
    entries: HashMap<TypeId, Variance>,
}

impl VarianceTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_entries(entries: HashMap<TypeId, Variance>) -> Self {
        Self { entries }
    }

    pub fn variance_for(&self, id: TypeId) -> Option<Variance> {
        self.entries.get(&id).copied()
    }

    pub fn insert(&mut self, id: TypeId, variance: Variance) {
        self.entries.insert(id, variance);
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&TypeId, &Variance)> {
        self.entries.iter()
    }
}

/// Computes variance assignments from recorded usage information.
#[derive(Debug, Default)]
pub struct VarianceAnalyzer;

impl VarianceAnalyzer {
    /// Consumes `VarianceUsage` constraints and produces a [`VarianceTable`].
    pub fn analyze(constraints: &[GenericConstraint]) -> VarianceTable {
        let mut stats: HashMap<TypeId, VarianceStats> = HashMap::new();
        for constraint in constraints {
            if let GenericConstraintKind::VarianceUsage {
                parameter,
                position,
            } = &constraint.kind
            {
                stats
                    .entry(*parameter)
                    .and_modify(|entry| entry.record(*position))
                    .or_insert_with(|| {
                        let mut initial = VarianceStats::default();
                        initial.record(*position);
                        initial
                    });
            }
        }
        Self::from_stats(stats)
    }

    /// Convenience helper used by tests to compute variance directly from usage tuples.
    pub fn analyze_usages<I>(usages: I) -> VarianceTable
    where
        I: IntoIterator<Item = (TypeId, VariancePosition)>,
    {
        let mut stats: HashMap<TypeId, VarianceStats> = HashMap::new();
        for (param, position) in usages {
            stats
                .entry(param)
                .and_modify(|entry| entry.record(position))
                .or_insert_with(|| {
                    let mut initial = VarianceStats::default();
                    initial.record(position);
                    initial
                });
        }
        Self::from_stats(stats)
    }

    fn from_stats(stats: HashMap<TypeId, VarianceStats>) -> VarianceTable {
        let entries = stats
            .into_iter()
            .map(|(param, stat)| (param, stat.into_variance()))
            .collect::<HashMap<_, _>>();
        VarianceTable::with_entries(entries)
    }
}
