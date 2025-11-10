use crate::constraint::GenericConstraintKind;
use crate::constraint::generic::GenericConstraint;
use crate::solver::variance::VariancePosition;
use crate::types::{
    BoundPredicate, BoundTypeReference, CapabilityBound, CapabilityHints, FunctionSignatureBound,
    PrimitiveBoundConstraint, PrimitiveBoundMetadata, PrimitiveTypeName, PrimitiveTypeNameExt,
    PrimitiveTypeReference, SymbolId, TraitBound, TypeId,
};
use jv_ast::Span;
use jv_ast::types::{
    CapabilityHints as AstCapabilityHints, FunctionConstraintSignature, TypeAnnotation,
    WhereClause, WherePredicate,
};
use std::cmp::Ordering;
use std::collections::HashMap;

/// Translates `where` clause predicates into generic constraints understood by the solver.
pub struct WhereConstraintResolver<'a> {
    owner: SymbolId,
    type_parameters: &'a HashMap<String, TypeId>,
}

impl<'a> WhereConstraintResolver<'a> {
    /// Creates a new resolver for the provided symbol and generic parameter mapping.
    pub fn new(owner: SymbolId, type_parameters: &'a HashMap<String, TypeId>) -> Self {
        Self {
            owner,
            type_parameters,
        }
    }

    /// Converts a parsed [`WhereClause`] into a flat list of generic constraints.
    pub fn from_clause(&self, clause: &WhereClause) -> Vec<GenericConstraint> {
        let mut constraints = Vec::new();
        for predicate in &clause.predicates {
            self.resolve_predicate(predicate, &mut constraints);
        }
        self.resolve_primitive_bounds(&clause.primitive_bounds, &mut constraints);
        constraints
    }

    fn resolve_predicate(
        &self,
        predicate: &WherePredicate,
        constraints: &mut Vec<GenericConstraint>,
    ) {
        let type_param_name = match predicate {
            WherePredicate::TraitBound { type_param, .. }
            | WherePredicate::Capability { type_param, .. }
            | WherePredicate::FunctionSignature { type_param, .. } => type_param,
        };

        let Some(&parameter) = self.type_parameters.get(type_param_name) else {
            return;
        };

        let span = predicate.span().clone();
        let mut variance_records = vec![(parameter, VariancePosition::Invariant)];

        if let Some(bound_predicate) = self.convert_predicate(predicate, &mut variance_records) {
            constraints.push(GenericConstraint::new(
                GenericConstraintKind::BoundRequirement {
                    owner: self.owner.clone(),
                    parameter,
                    predicate: bound_predicate,
                },
                span.clone(),
            ));
        }

        dedup_variance_records(&mut variance_records);
        for (id, position) in variance_records {
            constraints.push(GenericConstraint::new(
                GenericConstraintKind::VarianceUsage {
                    parameter: id,
                    position,
                },
                span.clone(),
            ));
        }
    }

    fn convert_predicate(
        &self,
        predicate: &WherePredicate,
        variance: &mut Vec<(TypeId, VariancePosition)>,
    ) -> Option<BoundPredicate> {
        match predicate {
            WherePredicate::TraitBound {
                trait_name,
                type_args,
                ..
            } => {
                let arguments = type_args
                    .iter()
                    .map(|arg| self.convert_annotation(arg, VariancePosition::Covariant, variance))
                    .collect();
                Some(BoundPredicate::Trait(TraitBound::new(
                    trait_name.qualified(),
                    arguments,
                )))
            }
            WherePredicate::Capability { capability, .. } => {
                Some(BoundPredicate::Capability(CapabilityBound::new(
                    capability.name.qualified(),
                    self.convert_annotation(
                        &capability.target,
                        VariancePosition::Covariant,
                        variance,
                    ),
                    self.convert_hints(&capability.hints),
                )))
            }
            WherePredicate::FunctionSignature { signature, .. } => Some(
                BoundPredicate::FunctionSignature(self.convert_signature(signature, variance)),
            ),
        }
    }

    fn convert_signature(
        &self,
        signature: &FunctionConstraintSignature,
        variance: &mut Vec<(TypeId, VariancePosition)>,
    ) -> FunctionSignatureBound {
        let parameters = signature
            .parameters
            .iter()
            .map(|param| self.convert_annotation(param, VariancePosition::Contravariant, variance))
            .collect();
        let return_type = signature
            .return_type
            .as_ref()
            .map(|ret| self.convert_annotation(ret, VariancePosition::Covariant, variance));
        FunctionSignatureBound::new(parameters, return_type)
    }

    fn convert_annotation(
        &self,
        annotation: &TypeAnnotation,
        position: VariancePosition,
        variance: &mut Vec<(TypeId, VariancePosition)>,
    ) -> BoundTypeReference {
        match annotation {
            TypeAnnotation::Simple(name) => {
                if let Some(&id) = self.type_parameters.get(name) {
                    variance.push((id, position));
                    BoundTypeReference::TypeParameter {
                        id,
                        name: name.clone(),
                    }
                } else {
                    BoundTypeReference::Named(name.clone())
                }
            }
            TypeAnnotation::Generic { name, type_args } => {
                let arguments = type_args
                    .iter()
                    .map(|arg| self.convert_annotation(arg, position, variance))
                    .collect();
                BoundTypeReference::Generic {
                    name: name.clone(),
                    arguments,
                }
            }
            TypeAnnotation::Nullable(inner) => BoundTypeReference::Optional(Box::new(
                self.convert_annotation(inner, position, variance),
            )),
            TypeAnnotation::Array(inner) => BoundTypeReference::Array(Box::new(
                self.convert_annotation(inner, position, variance),
            )),
            TypeAnnotation::Function {
                params,
                return_type,
            } => {
                let parameters = params
                    .iter()
                    .map(|param| {
                        self.convert_annotation(param, VariancePosition::Contravariant, variance)
                    })
                    .collect();
                let result =
                    self.convert_annotation(return_type, VariancePosition::Covariant, variance);
                BoundTypeReference::Function {
                    parameters,
                    return_type: Some(Box::new(result)),
                }
            }
            TypeAnnotation::Unit { base, .. } => {
                self.convert_annotation(base, position, variance)
            }
        }
    }

    fn convert_hints(&self, hints: &AstCapabilityHints) -> CapabilityHints {
        CapabilityHints {
            preferred_impl: hints.preferred_impl.clone(),
            inline_only: hints.inline_only,
        }
    }

    fn resolve_primitive_bounds(
        &self,
        bounds: &[PrimitiveBoundMetadata],
        constraints: &mut Vec<GenericConstraint>,
    ) {
        if bounds.is_empty() {
            return;
        }

        let mut grouped: HashMap<TypeId, Vec<PrimitiveAccumulator>> = HashMap::new();

        for bound in bounds {
            let Some(&parameter) = self.type_parameters.get(&bound.type_param) else {
                continue;
            };

            let family_list = grouped.entry(parameter).or_insert_with(Vec::new);

            let canonical = bound.reference.primitive.canonical_family();
            match family_list
                .iter_mut()
                .find(|accumulator| accumulator.canonical == canonical)
            {
                Some(accumulator) => accumulator.record_reference(&bound.reference),
                None => {
                    let mut accumulator = PrimitiveAccumulator::new(canonical, bound.span.clone());
                    accumulator.record_reference(&bound.reference);
                    family_list.push(accumulator);
                }
            }

            for alias in &bound.compatible_aliases {
                let alias_canonical = alias.primitive.canonical_family();
                match family_list
                    .iter_mut()
                    .find(|accumulator| accumulator.canonical == alias_canonical)
                {
                    Some(accumulator) => accumulator.record_reference(alias),
                    None => {
                        let mut accumulator =
                            PrimitiveAccumulator::new(alias_canonical, alias.span.clone());
                        accumulator.record_reference(alias);
                        family_list.push(accumulator);
                    }
                }
            }
        }

        for (parameter, mut families) in grouped {
            families.sort_by_key(|accumulator| accumulator.canonical as u8);
            for accumulator in families {
                let (predicate, span) = accumulator.into_parts();
                constraints.push(GenericConstraint::new(
                    GenericConstraintKind::BoundRequirement {
                        owner: self.owner.clone(),
                        parameter,
                        predicate: BoundPredicate::Primitive(predicate),
                    },
                    span,
                ));
            }
        }
    }
}

struct PrimitiveAccumulator {
    canonical: PrimitiveTypeName,
    aliases: Vec<PrimitiveTypeName>,
    span: Span,
}

impl PrimitiveAccumulator {
    fn new(canonical: PrimitiveTypeName, span: Span) -> Self {
        Self {
            canonical,
            aliases: Vec::new(),
            span,
        }
    }

    fn record_reference(&mut self, reference: &PrimitiveTypeReference) {
        self.add_alias(reference.primitive);
    }

    fn add_alias(&mut self, alias: PrimitiveTypeName) {
        if alias != self.canonical && !self.aliases.contains(&alias) {
            self.aliases.push(alias);
        }
    }

    fn into_parts(self) -> (PrimitiveBoundConstraint, Span) {
        let PrimitiveAccumulator {
            canonical,
            aliases,
            span,
        } = self;
        (PrimitiveBoundConstraint::new(canonical, aliases), span)
    }
}

fn dedup_variance_records(records: &mut Vec<(TypeId, VariancePosition)>) {
    records.sort_by(|(lhs_id, lhs_pos), (rhs_id, rhs_pos)| {
        match lhs_id.to_raw().cmp(&rhs_id.to_raw()) {
            Ordering::Equal => variance_rank(*lhs_pos).cmp(&variance_rank(*rhs_pos)),
            other => other,
        }
    });
    records.dedup();
}

fn variance_rank(position: VariancePosition) -> u8 {
    match position {
        VariancePosition::Covariant => 0,
        VariancePosition::Contravariant => 1,
        VariancePosition::Invariant => 2,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constraint::generic::GenericConstraintKind;
    use crate::types::BoundPredicate;
    use jv_ast::Span;
    use jv_ast::types::{
        CapabilityHints as AstCapabilityHints, CapabilityRequirement, PrimitiveBound,
        PrimitiveTypeName, PrimitiveTypeReference, PrimitiveTypeSource, QualifiedName,
    };
    use std::collections::HashMap;

    fn symbol() -> SymbolId {
        SymbolId::from("pkg::Example")
    }

    fn map_with(names: &[(&str, u32)]) -> HashMap<String, TypeId> {
        names
            .iter()
            .map(|(name, id)| ((*name).to_string(), TypeId::new(*id)))
            .collect()
    }

    fn primitive_reference(name: PrimitiveTypeName) -> PrimitiveTypeReference {
        PrimitiveTypeReference {
            primitive: name,
            source: PrimitiveTypeSource::PrimitiveKeyword,
            raw_path: Vec::new(),
            span: Span::dummy(),
        }
    }

    #[test]
    fn generates_constraints_for_trait_bound() {
        let clause = WhereClause {
            predicates: vec![WherePredicate::TraitBound {
                type_param: "T".into(),
                trait_name: QualifiedName::new(vec!["Comparable".into()], Span::dummy()),
                type_args: vec![TypeAnnotation::Simple("T".into())],
                span: Span::dummy(),
            }],
            primitive_bounds: Vec::new(),
            span: Span::dummy(),
        };
        let params = map_with(&[("T", 1)]);
        let resolver = WhereConstraintResolver::new(symbol(), &params);
        let constraints = resolver.from_clause(&clause);

        assert_eq!(constraints.len(), 3);
        assert!(matches!(
            constraints[0].kind,
            GenericConstraintKind::BoundRequirement { parameter, .. } if parameter == TypeId::new(1)
        ));
        match &constraints[0].kind {
            GenericConstraintKind::BoundRequirement { predicate, .. } => match predicate {
                BoundPredicate::Trait(bound) => {
                    assert_eq!(bound.name, "Comparable");
                    assert_eq!(bound.arguments.len(), 1);
                    assert!(matches!(
                        bound.arguments[0],
                        BoundTypeReference::TypeParameter { id, .. } if id == TypeId::new(1)
                    ));
                }
                other => panic!("unexpected predicate: {other:?}"),
            },
            _ => unreachable!(),
        }

        let variance_positions: Vec<_> = constraints
            .iter()
            .filter_map(|constraint| match &constraint.kind {
                GenericConstraintKind::VarianceUsage {
                    parameter,
                    position,
                } => Some((*parameter, *position)),
                _ => None,
            })
            .collect();
        assert!(variance_positions.contains(&(TypeId::new(1), VariancePosition::Invariant)));
        assert!(variance_positions.contains(&(TypeId::new(1), VariancePosition::Covariant)));
    }

    #[test]
    fn includes_capability_bounds_and_variance() {
        let clause = WhereClause {
            predicates: vec![WherePredicate::Capability {
                type_param: "T".into(),
                capability: CapabilityRequirement {
                    name: QualifiedName::new(vec!["Numeric".into()], Span::dummy()),
                    target: TypeAnnotation::Simple("T".into()),
                    hints: AstCapabilityHints {
                        preferred_impl: Some("NumericInt".into()),
                        inline_only: true,
                    },
                    span: Span::dummy(),
                },
                span: Span::dummy(),
            }],
            primitive_bounds: Vec::new(),
            span: Span::dummy(),
        };
        let params = map_with(&[("T", 7)]);
        let resolver = WhereConstraintResolver::new(symbol(), &params);
        let constraints = resolver.from_clause(&clause);

        assert_eq!(constraints.len(), 3);
        match &constraints[0].kind {
            GenericConstraintKind::BoundRequirement { predicate, .. } => match predicate {
                BoundPredicate::Capability(bound) => {
                    assert_eq!(bound.name, "Numeric");
                    assert_eq!(bound.hints.preferred_impl.as_deref(), Some("NumericInt"));
                    assert!(bound.hints.inline_only);
                }
                other => panic!("unexpected predicate: {other:?}"),
            },
            other => panic!("unexpected constraint: {other:?}"),
        }
        let variance: Vec<_> = constraints
            .iter()
            .filter_map(|constraint| match &constraint.kind {
                GenericConstraintKind::VarianceUsage {
                    parameter,
                    position,
                } => Some((*parameter, *position)),
                _ => None,
            })
            .collect();
        assert!(variance.contains(&(TypeId::new(7), VariancePosition::Invariant)));
        assert!(variance.contains(&(TypeId::new(7), VariancePosition::Covariant)));
    }

    #[test]
    fn function_signature_records_variance_positions() {
        let clause = WhereClause {
            predicates: vec![WherePredicate::FunctionSignature {
                type_param: "T".into(),
                signature: FunctionConstraintSignature {
                    parameters: vec![TypeAnnotation::Simple("T".into())],
                    return_type: Some(TypeAnnotation::Simple("T".into())),
                    span: Span::dummy(),
                },
                span: Span::dummy(),
            }],
            primitive_bounds: Vec::new(),
            span: Span::dummy(),
        };
        let params = map_with(&[("T", 11)]);
        let resolver = WhereConstraintResolver::new(symbol(), &params);
        let constraints = resolver.from_clause(&clause);

        assert_eq!(constraints.len(), 4);
        assert!(constraints.iter().any(|constraint| matches!(
            constraint.kind,
            GenericConstraintKind::BoundRequirement { .. }
        )));
        let mut recorded = constraints
            .iter()
            .filter_map(|constraint| match &constraint.kind {
                GenericConstraintKind::VarianceUsage {
                    parameter,
                    position,
                } => Some((*parameter, *position)),
                _ => None,
            })
            .collect::<Vec<_>>();
        recorded.sort_by_key(|(id, pos)| (id.to_raw(), variance_rank(*pos)));

        assert_eq!(recorded.len(), 3);
        assert_eq!(recorded[0], (TypeId::new(11), VariancePosition::Covariant));
        assert_eq!(
            recorded[1],
            (TypeId::new(11), VariancePosition::Contravariant)
        );
        assert_eq!(recorded[2], (TypeId::new(11), VariancePosition::Invariant));
    }

    #[test]
    fn skips_predicates_with_unknown_parameters() {
        let clause = WhereClause {
            predicates: vec![WherePredicate::TraitBound {
                type_param: "Unknown".into(),
                trait_name: QualifiedName::new(vec!["Debug".into()], Span::dummy()),
                type_args: Vec::new(),
                span: Span::dummy(),
            }],
            primitive_bounds: Vec::new(),
            span: Span::dummy(),
        };
        let params: HashMap<String, TypeId> = HashMap::new();
        let resolver = WhereConstraintResolver::new(symbol(), &params);
        assert!(resolver.from_clause(&clause).is_empty());
    }

    #[test]
    fn primitive_bounds_are_canonicalized_and_grouped() {
        let clause = WhereClause {
            predicates: Vec::new(),
            primitive_bounds: vec![
                PrimitiveBound {
                    type_param: "T".into(),
                    reference: primitive_reference(PrimitiveTypeName::Char),
                    compatible_aliases: Vec::new(),
                    span: Span::dummy(),
                },
                PrimitiveBound {
                    type_param: "T".into(),
                    reference: primitive_reference(PrimitiveTypeName::Short),
                    compatible_aliases: Vec::new(),
                    span: Span::dummy(),
                },
            ],
            span: Span::dummy(),
        };
        let params = map_with(&[("T", 42)]);
        let resolver = WhereConstraintResolver::new(symbol(), &params);
        let constraints = resolver.from_clause(&clause);

        assert_eq!(constraints.len(), 1);
        match &constraints[0].kind {
            GenericConstraintKind::BoundRequirement {
                parameter,
                predicate,
                ..
            } => {
                assert_eq!(*parameter, TypeId::new(42));
                let BoundPredicate::Primitive(bound) = predicate else {
                    panic!("expected primitive predicate: {predicate:?}");
                };
                assert_eq!(bound.canonical(), PrimitiveTypeName::Int);
                let aliases = bound.alias_families();
                assert_eq!(aliases.len(), 2);
                assert!(aliases.contains(&PrimitiveTypeName::Char));
                assert!(aliases.contains(&PrimitiveTypeName::Short));
            }
            other => panic!("unexpected constraint: {other:?}"),
        }
    }
}
