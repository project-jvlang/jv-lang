use crate::types::{BoundPredicate, BoundTypeReference, FunctionSignatureBound, TypeId};
use std::collections::HashSet;

/// Returns true when the provided predicate implies that the subject type
/// parameter may be nullable. The check looks for optional references wrapping
/// the same type parameter.
pub fn predicate_requires_nullable(predicate: &BoundPredicate, parameter: TypeId) -> bool {
    match predicate {
        BoundPredicate::Trait(bound) => bound
            .arguments
            .iter()
            .any(|reference| reference_introduces_nullability(reference, parameter)),
        BoundPredicate::Interface(_) => false,
        BoundPredicate::Capability(bound) => {
            reference_introduces_nullability(&bound.target, parameter)
        }
        BoundPredicate::FunctionSignature(signature) => {
            function_signature_introduces_nullable(signature, parameter)
        }
        BoundPredicate::WhereClause(predicates) => predicates
            .iter()
            .any(|inner| predicate_requires_nullable(inner, parameter)),
    }
}

fn function_signature_introduces_nullable(
    signature: &FunctionSignatureBound,
    parameter: TypeId,
) -> bool {
    signature
        .parameters
        .iter()
        .any(|reference| reference_introduces_nullability(reference, parameter))
        || signature
            .return_type
            .as_ref()
            .map(|reference| reference_introduces_nullability(reference, parameter))
            .unwrap_or(false)
}

fn reference_introduces_nullability(reference: &BoundTypeReference, parameter: TypeId) -> bool {
    match reference {
        BoundTypeReference::Optional(inner) => {
            contains_parameter(inner, parameter)
                || reference_introduces_nullability(inner, parameter)
        }
        BoundTypeReference::Array(inner) => reference_introduces_nullability(inner, parameter),
        BoundTypeReference::Generic { arguments, .. } => arguments
            .iter()
            .any(|argument| reference_introduces_nullability(argument, parameter)),
        BoundTypeReference::Function {
            parameters,
            return_type,
        } => {
            parameters
                .iter()
                .any(|argument| reference_introduces_nullability(argument, parameter))
                || return_type
                    .as_ref()
                    .map(|ret| reference_introduces_nullability(ret, parameter))
                    .unwrap_or(false)
        }
        BoundTypeReference::TypeParameter { .. } | BoundTypeReference::Named(_) => false,
    }
}

fn contains_parameter(reference: &BoundTypeReference, parameter: TypeId) -> bool {
    match reference {
        BoundTypeReference::TypeParameter { id, .. } => *id == parameter,
        BoundTypeReference::Optional(inner) | BoundTypeReference::Array(inner) => {
            contains_parameter(inner, parameter)
        }
        BoundTypeReference::Generic { arguments, .. } => arguments
            .iter()
            .any(|argument| contains_parameter(argument, parameter)),
        BoundTypeReference::Function {
            parameters,
            return_type,
        } => {
            parameters
                .iter()
                .any(|argument| contains_parameter(argument, parameter))
                || return_type
                    .as_ref()
                    .map(|ret| contains_parameter(ret, parameter))
                    .unwrap_or(false)
        }
        BoundTypeReference::Named(_) => false,
    }
}

/// Collects all type parameters referenced by the predicate into the provided
/// buffer. Duplicates are removed before insertion.
pub fn referenced_type_parameters(predicate: &BoundPredicate, buffer: &mut Vec<TypeId>) {
    let mut set = HashSet::new();
    collect(predicate, &mut set);
    buffer.extend(set.into_iter());
}

fn collect(predicate: &BoundPredicate, set: &mut HashSet<TypeId>) {
    match predicate {
        BoundPredicate::Trait(bound) => {
            for reference in &bound.arguments {
                collect_from_reference(reference, set);
            }
        }
        BoundPredicate::Interface(_) => {}
        BoundPredicate::Capability(bound) => collect_from_reference(&bound.target, set),
        BoundPredicate::FunctionSignature(signature) => {
            for reference in &signature.parameters {
                collect_from_reference(reference, set);
            }
            if let Some(reference) = &signature.return_type {
                collect_from_reference(reference, set);
            }
        }
        BoundPredicate::WhereClause(predicates) => {
            for predicate in predicates {
                collect(predicate, set);
            }
        }
    }
}

fn collect_from_reference(reference: &BoundTypeReference, set: &mut HashSet<TypeId>) {
    match reference {
        BoundTypeReference::TypeParameter { id, .. } => {
            set.insert(*id);
        }
        BoundTypeReference::Optional(inner) | BoundTypeReference::Array(inner) => {
            collect_from_reference(inner, set)
        }
        BoundTypeReference::Generic { arguments, .. } => {
            for argument in arguments {
                collect_from_reference(argument, set);
            }
        }
        BoundTypeReference::Function {
            parameters,
            return_type,
        } => {
            for parameter in parameters {
                collect_from_reference(parameter, set);
            }
            if let Some(return_type) = return_type {
                collect_from_reference(return_type, set);
            }
        }
        BoundTypeReference::Named(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{BoundPredicate, BoundTypeReference, FunctionSignatureBound, TraitBound};

    fn param(id: u32, name: &str) -> BoundTypeReference {
        BoundTypeReference::TypeParameter {
            id: TypeId::new(id),
            name: name.to_string(),
        }
    }

    #[test]
    fn detects_nullable_when_optional_wraps_subject() {
        let predicate = BoundPredicate::Trait(TraitBound::new(
            "Comparable",
            vec![BoundTypeReference::Optional(Box::new(param(1, "T")))],
        ));
        assert!(predicate_requires_nullable(&predicate, TypeId::new(1)));
        assert!(!predicate_requires_nullable(&predicate, TypeId::new(2)));
    }

    #[test]
    fn nullable_propagates_through_function_signature() {
        let signature = FunctionSignatureBound::new(
            vec![BoundTypeReference::Optional(Box::new(param(3, "T")))],
            None,
        );
        let predicate = BoundPredicate::FunctionSignature(signature);
        assert!(predicate_requires_nullable(&predicate, TypeId::new(3)));
    }

    #[test]
    fn collects_type_parameters_from_nested_predicate() {
        let predicate = BoundPredicate::Trait(TraitBound::new(
            "Pair",
            vec![
                BoundTypeReference::Generic {
                    name: "Box".into(),
                    arguments: vec![param(7, "T")],
                },
                BoundTypeReference::Optional(Box::new(param(8, "U"))),
            ],
        ));

        let mut buffer = Vec::new();
        referenced_type_parameters(&predicate, &mut buffer);
        buffer.sort_by_key(|id| id.to_raw());
        assert_eq!(buffer, vec![TypeId::new(7), TypeId::new(8)]);
    }
}
