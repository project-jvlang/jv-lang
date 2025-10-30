use super::{GenericConstraint, GenericConstraintKind};
use crate::ConstraintCache;
use crate::types::{
    BoundConstraint, BoundPredicate, CapabilitySolution, GenericBounds, SymbolId, TypeId,
};
use std::collections::{HashMap, HashSet};

/// Collection of resolved bounds for a specific type parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedBound {
    pub parameter: TypeId,
    pub bounds: GenericBounds,
}

impl ResolvedBound {
    pub fn new(parameter: TypeId, bounds: GenericBounds) -> Self {
        Self { parameter, bounds }
    }

    pub fn is_empty(&self) -> bool {
        self.bounds.is_empty()
    }
}

/// Summary describing which type parameters should be treated as nullable.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct NullabilitySummary {
    nullable: HashSet<TypeId>,
}

impl NullabilitySummary {
    pub fn mark_nullable(&mut self, parameter: TypeId) {
        self.nullable.insert(parameter);
    }

    pub fn merge<I>(&mut self, items: I)
    where
        I: IntoIterator<Item = TypeId>,
    {
        self.nullable.extend(items);
    }

    pub fn is_nullable(&self, parameter: &TypeId) -> bool {
        self.nullable.contains(parameter)
    }

    pub fn iter(&self) -> impl Iterator<Item = &TypeId> {
        self.nullable.iter()
    }
}

/// Final result of solving constraints emitted for a specific owner symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintSolution {
    pub symbol: SymbolId,
    pub resolved_bounds: Vec<ResolvedBound>,
    pub capability_bindings: Vec<CapabilitySolution>,
    pub nullability: NullabilitySummary,
}

impl ConstraintSolution {
    pub fn new(symbol: SymbolId) -> Self {
        Self {
            symbol,
            resolved_bounds: Vec::new(),
            capability_bindings: Vec::new(),
            nullability: NullabilitySummary::default(),
        }
    }

    /// Builds a constraint solution by grouping bound requirements per type parameter and
    /// constructing [`GenericBounds`] for each entry. Capability bindings are currently
    /// passed through without additional processing.
    pub fn from_generic_constraints(
        symbol: SymbolId,
        constraints: &[GenericConstraint],
        nullability: NullabilitySummary,
        capability_bindings: Vec<CapabilitySolution>,
    ) -> Self {
        Self::build_solution(symbol, constraints, nullability, capability_bindings)
    }

    pub fn from_generic_constraints_cached(
        symbol: SymbolId,
        constraints: &[GenericConstraint],
        nullability: NullabilitySummary,
        capability_bindings: Vec<CapabilitySolution>,
        mut cache: Option<&mut ConstraintCache>,
    ) -> (Self, bool) {
        if let Some(cache) = cache.as_mut() {
            let fingerprint = ConstraintCache::fingerprint_for(constraints);
            if let Some(existing) = cache.lookup(&symbol, fingerprint) {
                return (existing, true);
            }
            let solution = Self::build_solution(
                symbol.clone(),
                constraints,
                nullability,
                capability_bindings,
            );
            cache.store(symbol, fingerprint, solution.clone());
            (solution, false)
        } else {
            (
                Self::build_solution(symbol, constraints, nullability, capability_bindings),
                false,
            )
        }
    }

    fn build_solution(
        symbol: SymbolId,
        constraints: &[GenericConstraint],
        nullability: NullabilitySummary,
        capability_bindings: Vec<CapabilitySolution>,
    ) -> Self {
        let mut grouped: HashMap<TypeId, HashMap<String, BoundPredicate>> = HashMap::new();

        for constraint in constraints {
            if let GenericConstraintKind::BoundRequirement {
                parameter,
                predicate,
                ..
            } = &constraint.kind
            {
                grouped
                    .entry(*parameter)
                    .or_default()
                    .entry(predicate.key())
                    .or_insert_with(|| predicate.clone());
            }
        }

        let mut resolved_bounds: Vec<ResolvedBound> = grouped
            .into_iter()
            .map(|(parameter, predicates)| {
                let constraints = predicates
                    .into_values()
                    .map(|predicate| BoundConstraint::new(parameter, predicate))
                    .collect::<Vec<_>>();
                ResolvedBound::new(parameter, GenericBounds::new(constraints))
            })
            .filter(|resolved| !resolved.is_empty())
            .collect();

        resolved_bounds.sort_by_key(|entry| entry.parameter.to_raw());

        Self {
            symbol,
            resolved_bounds,
            capability_bindings,
            nullability,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ConstraintCache;
    use crate::types::{
        BoundTypeReference, CapabilityBound, CapabilityHints, DispatchKind, TraitBound, TypeKind,
        TypeVariant,
    };
    use jv_ast::Span;

    fn constraint(predicate: BoundPredicate, parameter: TypeId) -> GenericConstraint {
        GenericConstraint::new(
            GenericConstraintKind::BoundRequirement {
                owner: SymbolId::from("pkg::Owner"),
                parameter,
                predicate,
            },
            Span::dummy(),
        )
    }

    #[test]
    fn groups_constraints_per_parameter() {
        let param = TypeId::new(1);
        let mut nullability = NullabilitySummary::default();
        nullability.mark_nullable(param);
        let constraints = vec![
            constraint(
                BoundPredicate::Trait(TraitBound::simple("Comparable")),
                param,
            ),
            constraint(
                BoundPredicate::Trait(TraitBound::simple("Serializable")),
                param,
            ),
        ];

        let solution = ConstraintSolution::from_generic_constraints(
            SymbolId::from("pkg::Owner"),
            &constraints,
            nullability.clone(),
            Vec::new(),
        );

        assert_eq!(solution.resolved_bounds.len(), 1);
        assert_eq!(solution.resolved_bounds[0].parameter, param);
        assert_eq!(solution.resolved_bounds[0].bounds.constraints().len(), 2);
        assert!(solution.nullability.is_nullable(&param));
    }

    #[test]
    fn deduplicates_predicates_by_key() {
        let param = TypeId::new(2);
        let capability = BoundPredicate::Capability(CapabilityBound::new(
            "Numeric",
            BoundTypeReference::Named("Int".into()),
            CapabilityHints::default(),
        ));
        let constraints = vec![
            constraint(capability.clone(), param),
            constraint(capability, param),
        ];

        let solution = ConstraintSolution::from_generic_constraints(
            SymbolId::from("pkg::Owner"),
            &constraints,
            NullabilitySummary::default(),
            Vec::new(),
        );

        assert_eq!(solution.resolved_bounds.len(), 1);
        assert_eq!(solution.resolved_bounds[0].bounds.constraints().len(), 1);
    }

    #[test]
    fn cached_solution_hits_on_second_lookup() {
        let param = TypeId::new(7);
        let constraints = vec![constraint(
            BoundPredicate::Trait(TraitBound::simple("Display")),
            param,
        )];
        let mut cache = ConstraintCache::new();
        let (first, hit_first) = ConstraintSolution::from_generic_constraints_cached(
            SymbolId::from("pkg::Owner"),
            &constraints,
            NullabilitySummary::default(),
            Vec::new(),
            Some(&mut cache),
        );
        assert!(!hit_first);

        let (second, hit_second) = ConstraintSolution::from_generic_constraints_cached(
            SymbolId::from("pkg::Owner"),
            &constraints,
            NullabilitySummary::default(),
            Vec::new(),
            Some(&mut cache),
        );
        assert!(hit_second);
        assert_eq!(first, second);
    }

    #[test]
    fn preserves_capability_bindings() {
        let solution = ConstraintSolution::from_generic_constraints(
            SymbolId::from("pkg::Owner"),
            &[],
            NullabilitySummary::default(),
            vec![CapabilitySolution::new(
                SymbolId::from("impl.Numeric"),
                TypeKind::new(TypeVariant::Primitive("Int")),
                DispatchKind::Static,
            )],
        );
        assert_eq!(solution.capability_bindings.len(), 1);
    }
}
