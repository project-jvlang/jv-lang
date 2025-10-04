//! Generalization and instantiation logic for polymorphic bindings.
//!
//! This module provides utilities to generalize solved types into reusable
//! [`TypeScheme`] instances and to instantiate those schemes back into concrete
//! [`TypeKind`] values while preserving bound metadata.

use crate::service::TypeScheme;
use crate::solver::VarianceTable;
use crate::types::{BoundConstraint, FieldType, GenericBounds, TypeId, TypeKind, TypeVariant};
use std::collections::HashMap;

/// Generalises a solved type into a [`TypeScheme`] while retaining relevant bound
/// metadata for the quantified parameters.
pub fn generalize_with_bounds(kind: TypeKind, bounds: &GenericBounds) -> TypeScheme {
    let generics = kind.free_type_vars();
    if generics.is_empty() {
        return TypeScheme::monomorphic(kind);
    }

    let mut bound_map = HashMap::new();
    for param in &generics {
        let related = bounds
            .constraints()
            .iter()
            .filter(|constraint| constraint.type_param == *param)
            .cloned()
            .collect::<Vec<_>>();
        if !related.is_empty() {
            bound_map.insert(*param, GenericBounds::new(related));
        }
    }

    TypeScheme::with_bounds(generics, kind, bound_map)
}

/// Instantiates a [`TypeScheme`] by allocating fresh type variables for each
/// quantifier. Bound metadata is attached to the newly created variables so that
/// downstream consumers can enforce the recorded predicates.
pub fn instantiate_scheme(scheme: &TypeScheme, _variance: &VarianceTable) -> TypeKind {
    let mut substitutions = HashMap::new();
    for quantifier in scheme.generics() {
        let fresh_id = TypeId::fresh();
        let mut instantiated = TypeKind::variable(fresh_id);
        if let Some(bounds) = scheme.bounds_for(*quantifier) {
            let remapped = bounds
                .constraints()
                .iter()
                .map(|constraint| BoundConstraint::new(fresh_id, constraint.predicate.clone()))
                .collect::<Vec<_>>();
            if !remapped.is_empty() {
                instantiated = instantiated.with_bounds(GenericBounds::new(remapped));
            }
        }
        substitutions.insert(*quantifier, instantiated);
    }

    substitute_type(scheme.body(), &substitutions)
}

fn substitute_type(original: &TypeKind, subs: &HashMap<TypeId, TypeKind>) -> TypeKind {
    match original.variant() {
        TypeVariant::Variable(id) => subs.get(id).cloned().unwrap_or_else(|| {
            let mut variable = TypeKind::variable(*id).with_nullability(original.nullability());
            if let Some(bounds) = original.bounds() {
                variable = variable.with_bounds(bounds.clone());
            }
            variable
        }),
        TypeVariant::Primitive(name) => {
            let mut result = TypeKind::new(TypeVariant::Primitive(*name));
            result = result.with_nullability(original.nullability());
            if let Some(bounds) = original.bounds() {
                result = result.with_bounds(bounds.clone());
            }
            result
        }
        TypeVariant::Optional(inner) => {
            let substituted = substitute_type(inner, subs);
            let mut result = TypeKind::optional(substituted);
            result = result.with_nullability(original.nullability());
            if let Some(bounds) = original.bounds() {
                result = result.with_bounds(bounds.clone());
            }
            result
        }
        TypeVariant::Function(params, ret) => {
            let substituted_params = params
                .iter()
                .map(|param| substitute_type(param, subs))
                .collect();
            let substituted_ret = substitute_type(ret, subs);
            let mut result = TypeKind::function(substituted_params, substituted_ret);
            result = result.with_nullability(original.nullability());
            if let Some(bounds) = original.bounds() {
                result = result.with_bounds(bounds.clone());
            }
            result
        }
        TypeVariant::Record { fields } => {
            let substituted_fields = fields
                .iter()
                .map(|field| FieldType::new(field.name.clone(), substitute_type(&field.ty, subs)))
                .collect();
            let mut result = TypeKind::record(substituted_fields);
            result = result.with_nullability(original.nullability());
            if let Some(bounds) = original.bounds() {
                result = result.with_bounds(bounds.clone());
            }
            result
        }
        TypeVariant::Union { arms } => {
            let substituted_arms = arms.iter().map(|arm| substitute_type(arm, subs)).collect();
            let mut result = TypeKind::union(substituted_arms);
            result = result.with_nullability(original.nullability());
            if let Some(bounds) = original.bounds() {
                result = result.with_bounds(bounds.clone());
            }
            result
        }
        TypeVariant::Unknown => {
            let mut result = TypeKind::default();
            result = result.with_nullability(original.nullability());
            if let Some(bounds) = original.bounds() {
                result = result.with_bounds(bounds.clone());
            }
            result
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BoundConstraint, BoundPredicate, TraitBound};

    #[test]
    fn generalize_collects_bounds_for_quantified_variables() {
        let t = TypeId::new(10);
        let u = TypeId::new(11);
        let ty = TypeKind::function(vec![TypeKind::variable(t)], TypeKind::variable(u));
        let bounds = GenericBounds::new(vec![
            BoundConstraint::new(t, BoundPredicate::Trait(TraitBound::simple("Display"))),
            BoundConstraint::new(
                TypeId::new(99),
                BoundPredicate::Trait(TraitBound::simple("Clone")),
            ),
        ]);

        let scheme = generalize_with_bounds(ty.clone(), &bounds);

        assert_eq!(scheme.generics(), &[t, u]);
        assert!(scheme.bounds_for(t).is_some());
        // Constraints unrelated to quantified variables are ignored.
        assert!(scheme.bounds_for(u).is_none());
    }

    #[test]
    fn instantiate_mints_fresh_variables_and_preserves_bounds() {
        TypeId::reset_counter(1000);
        let t = TypeId::new(21);
        let ty = TypeKind::variable(t);
        let bounds = GenericBounds::new(vec![BoundConstraint::new(
            t,
            BoundPredicate::Trait(TraitBound::simple("Debug")),
        )]);
        let scheme = generalize_with_bounds(ty, &bounds);

        TypeId::reset_counter(2000);
        let instantiated = instantiate_scheme(&scheme, &VarianceTable::new());

        match instantiated.variant() {
            TypeVariant::Variable(new_id) => {
                assert_ne!(new_id, &t);
                let bounds = instantiated.bounds().expect("bounds");
                assert_eq!(bounds.constraints()[0].type_param, *new_id);
            }
            other => panic!("expected variable, got {other:?}"),
        }
    }
}
