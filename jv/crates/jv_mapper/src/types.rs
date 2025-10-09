use jv_ast::types::{
    CapabilityHints as AstCapabilityHints, CapabilityRequirement, FunctionConstraintSignature,
    GenericParameter as AstGenericParameter, GenericSignature as AstGenericSignature,
    TypeAnnotation, VarianceMarker, WhereClause as AstWhereClause,
    WherePredicate as AstWherePredicate,
};
use jv_inference::solver::Variance;
use jv_inference::types::{
    BoundPredicate, BoundTypeReference, CapabilityBound, CapabilityHints, FunctionSignatureBound,
    GenericParameterInfo, GenericSignature, GenericWhereClause, GenericWherePredicate, TraitBound,
};

/// Lowers an AST-level [`GenericSignature`](AstGenericSignature) into the metadata structure
/// consumed by the inference pipeline.
pub fn lower_generic_signature(signature: &AstGenericSignature) -> GenericSignature {
    let parameters = signature
        .parameters
        .iter()
        .map(lower_generic_parameter)
        .collect::<Vec<_>>();

    let where_clause = signature.where_clause.as_ref().map(lower_where_clause);

    GenericSignature::new(
        parameters,
        where_clause,
        signature.raw_directives.clone(),
        signature.span.clone(),
    )
}

fn lower_generic_parameter(parameter: &AstGenericParameter) -> GenericParameterInfo {
    let bounds = parameter
        .bounds
        .iter()
        .map(lower_type_annotation)
        .collect::<Vec<_>>();
    let variance = parameter.variance.as_ref().map(lower_variance_marker);
    let default = parameter.default.as_ref().map(lower_type_annotation);

    GenericParameterInfo::new(
        parameter.name.clone(),
        bounds,
        variance,
        default,
        parameter.span.clone(),
    )
}

fn lower_variance_marker(marker: &VarianceMarker) -> Variance {
    match marker {
        VarianceMarker::Covariant => Variance::Covariant,
        VarianceMarker::Contravariant => Variance::Contravariant,
    }
}

fn lower_where_clause(clause: &AstWhereClause) -> GenericWhereClause {
    let predicates = clause
        .predicates
        .iter()
        .map(lower_where_predicate)
        .collect::<Vec<_>>();
    GenericWhereClause::new(predicates, clause.span.clone())
}

fn lower_where_predicate(predicate: &AstWherePredicate) -> GenericWherePredicate {
    match predicate {
        AstWherePredicate::TraitBound {
            type_param,
            trait_name,
            type_args,
            span,
        } => {
            let arguments = type_args
                .iter()
                .map(lower_type_annotation)
                .collect::<Vec<_>>();
            let bound = BoundPredicate::Trait(TraitBound::new(trait_name.qualified(), arguments));
            GenericWherePredicate::new(type_param.clone(), bound, span.clone())
        }
        AstWherePredicate::Capability {
            type_param,
            capability,
            span,
        } => {
            let bound = BoundPredicate::Capability(lower_capability_requirement(capability));
            GenericWherePredicate::new(type_param.clone(), bound, span.clone())
        }
        AstWherePredicate::FunctionSignature {
            type_param,
            signature,
            span,
        } => {
            let bound = BoundPredicate::FunctionSignature(lower_function_signature(signature));
            GenericWherePredicate::new(type_param.clone(), bound, span.clone())
        }
    }
}

fn lower_capability_requirement(requirement: &CapabilityRequirement) -> CapabilityBound {
    CapabilityBound::new(
        requirement.name.qualified(),
        lower_type_annotation(&requirement.target),
        lower_capability_hints(&requirement.hints),
    )
}

fn lower_capability_hints(hints: &AstCapabilityHints) -> CapabilityHints {
    CapabilityHints {
        preferred_impl: hints.preferred_impl.clone(),
        inline_only: hints.inline_only,
    }
}

fn lower_function_signature(signature: &FunctionConstraintSignature) -> FunctionSignatureBound {
    let parameters = signature
        .parameters
        .iter()
        .map(lower_type_annotation)
        .collect::<Vec<_>>();
    let return_type = signature.return_type.as_ref().map(lower_type_annotation);
    FunctionSignatureBound::new(parameters, return_type)
}

fn lower_type_annotation(annotation: &TypeAnnotation) -> BoundTypeReference {
    match annotation {
        TypeAnnotation::Simple(name) => BoundTypeReference::Named(name.clone()),
        TypeAnnotation::Nullable(inner) => {
            BoundTypeReference::Optional(Box::new(lower_type_annotation(inner)))
        }
        TypeAnnotation::Generic { name, type_args } => BoundTypeReference::Generic {
            name: name.clone(),
            arguments: type_args
                .iter()
                .map(lower_type_annotation)
                .collect::<Vec<_>>(),
        },
        TypeAnnotation::Function {
            params,
            return_type,
        } => BoundTypeReference::Function {
            parameters: params.iter().map(lower_type_annotation).collect::<Vec<_>>(),
            return_type: Some(Box::new(lower_type_annotation(return_type))),
        },
        TypeAnnotation::Array(element_type) => {
            BoundTypeReference::Array(Box::new(lower_type_annotation(element_type)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::Span;
    use jv_inference::types::RawTypeDirective;

    fn span() -> Span {
        Span::new(1, 0, 1, 0)
    }

    #[test]
    fn lowers_simple_generic_signature() {
        let ast_signature = AstGenericSignature {
            parameters: vec![AstGenericParameter {
                name: "T".to_string(),
                bounds: vec![TypeAnnotation::Simple("Number".into())],
                variance: Some(VarianceMarker::Covariant),
                default: Some(TypeAnnotation::Simple("Object".into())),
                span: span(),
            }],
            where_clause: None,
            raw_directives: vec![RawTypeDirective {
                owner: jv_ast::types::QualifiedName::new(vec!["raw".into()], span()),
                span: span(),
                mode: jv_ast::types::RawTypeContinuation::DefaultPolicy,
            }],
            span: span(),
        };

        let lowered = lower_generic_signature(&ast_signature);

        assert_eq!(lowered.parameters.len(), 1);
        let param = &lowered.parameters[0];
        assert_eq!(param.name, "T");
        assert_eq!(param.variance(), Some(Variance::Covariant));
        assert_eq!(
            param.bounds,
            vec![BoundTypeReference::Named("Number".into())]
        );
        assert_eq!(
            param.default(),
            Some(&BoundTypeReference::Named("Object".into()))
        );

        assert!(lowered.where_clause().is_none());
        assert!(lowered.has_raw_directives());
        assert!(!lowered.is_empty());
    }

    #[test]
    fn lowers_where_clause_and_capabilities() {
        let predicate_span = Span::new(2, 0, 2, 10);
        let where_clause = AstWhereClause {
            predicates: vec![
                AstWherePredicate::TraitBound {
                    type_param: "T".into(),
                    trait_name: jv_ast::types::QualifiedName::new(
                        vec!["com".into(), "example".into(), "Comparable".into()],
                        predicate_span.clone(),
                    ),
                    type_args: vec![TypeAnnotation::Simple("String".into())],
                    span: predicate_span.clone(),
                },
                AstWherePredicate::Capability {
                    type_param: "T".into(),
                    capability: CapabilityRequirement {
                        name: jv_ast::types::QualifiedName::new(
                            vec!["com".into(), "example".into(), "Hashable".into()],
                            predicate_span.clone(),
                        ),
                        target: TypeAnnotation::Simple("T".into()),
                        hints: AstCapabilityHints {
                            preferred_impl: Some("DefaultHasher".into()),
                            inline_only: true,
                        },
                        span: predicate_span.clone(),
                    },
                    span: predicate_span.clone(),
                },
                AstWherePredicate::FunctionSignature {
                    type_param: "R".into(),
                    signature: FunctionConstraintSignature {
                        parameters: vec![TypeAnnotation::Simple("T".into())],
                        return_type: Some(TypeAnnotation::Nullable(Box::new(
                            TypeAnnotation::Simple("R".into()),
                        ))),
                        span: predicate_span.clone(),
                    },
                    span: predicate_span.clone(),
                },
            ],
            span: predicate_span.clone(),
        };

        let ast_signature = AstGenericSignature {
            parameters: vec![
                AstGenericParameter {
                    name: "T".into(),
                    bounds: Vec::new(),
                    variance: None,
                    default: None,
                    span: span(),
                },
                AstGenericParameter {
                    name: "R".into(),
                    bounds: Vec::new(),
                    variance: Some(VarianceMarker::Contravariant),
                    default: None,
                    span: span(),
                },
            ],
            where_clause: Some(where_clause),
            raw_directives: Vec::new(),
            span: span(),
        };

        let lowered = lower_generic_signature(&ast_signature);
        let clause = lowered.where_clause().expect("where clause");
        assert_eq!(clause.predicates.len(), 3);

        match &clause.predicates[0].predicate {
            BoundPredicate::Trait(bound) => {
                assert_eq!(bound.name, "com.example.Comparable");
                assert_eq!(
                    bound.arguments,
                    vec![BoundTypeReference::Named("String".into())]
                );
            }
            other => panic!("expected trait bound, got {other:?}"),
        }

        match &clause.predicates[1].predicate {
            BoundPredicate::Capability(bound) => {
                assert_eq!(bound.name, "com.example.Hashable");
                assert!(bound.hints.inline_only);
            }
            other => panic!("expected capability bound, got {other:?}"),
        }

        match &clause.predicates[2].predicate {
            BoundPredicate::FunctionSignature(signature) => {
                assert_eq!(signature.parameters.len(), 1);
                match signature.return_type.as_ref().expect("return type") {
                    BoundTypeReference::Optional(inner) => match inner.as_ref() {
                        BoundTypeReference::Named(name) => assert_eq!(name, "R"),
                        unexpected => panic!("unexpected inner type {unexpected:?}"),
                    },
                    unexpected => panic!("unexpected return type {unexpected:?}"),
                }
            }
            other => panic!("expected function signature, got {other:?}"),
        }

        let second_param = &lowered.parameters[1];
        assert_eq!(second_param.variance(), Some(Variance::Contravariant));
    }
}
