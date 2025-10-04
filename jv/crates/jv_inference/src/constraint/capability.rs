use crate::environment::CapabilityEnvironment;
use crate::types::{CapabilityBound, CapabilitySolution, SymbolId, TypeKind};
use std::fmt;

/// Errors produced while resolving a capability requirement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CapabilityResolutionError {
    NotFound {
        capability: String,
        target: String,
        preferred_impl: Option<String>,
        inline_only: bool,
    },
    Ambiguous {
        capability: String,
        target: String,
        candidates: Vec<SymbolId>,
        preferred_impl: Option<String>,
    },
}

impl fmt::Display for CapabilityResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CapabilityResolutionError::NotFound {
                capability,
                target,
                preferred_impl,
                inline_only,
            } => {
                if let Some(preferred) = preferred_impl {
                    write!(
                        f,
                        "no implementation found for {capability}<{target}> (preferred impl: {preferred}, inline-only: {inline_only})"
                    )
                } else {
                    write!(
                        f,
                        "no implementation found for {capability}<{target}> (inline-only: {inline_only})"
                    )
                }
            }
            CapabilityResolutionError::Ambiguous {
                capability,
                target,
                candidates,
                preferred_impl,
            } => {
                let names = candidates
                    .iter()
                    .map(SymbolId::as_str)
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(preferred) = preferred_impl {
                    write!(
                        f,
                        "ambiguous implementations for {capability}<{target}> (preferred impl: {preferred}): {names}"
                    )
                } else {
                    write!(
                        f,
                        "ambiguous implementations for {capability}<{target}>: {names}"
                    )
                }
            }
        }
    }
}

impl std::error::Error for CapabilityResolutionError {}

/// Resolves capability requirements using the environment registry.
pub struct CapabilityDictionaryResolver<'env> {
    environment: &'env CapabilityEnvironment,
}

impl<'env> CapabilityDictionaryResolver<'env> {
    pub fn new(environment: &'env CapabilityEnvironment) -> Self {
        Self { environment }
    }

    pub fn resolve(
        &self,
        bound: &CapabilityBound,
        candidate: &TypeKind,
    ) -> Result<CapabilitySolution, CapabilityResolutionError> {
        let candidate_key = CapabilityEnvironment::canonical_type_key(candidate);
        let mut candidates = self
            .environment
            .find_candidates(&bound.name, &candidate_key);

        if bound.hints.inline_only {
            candidates.retain(|implementation| implementation.dispatch.supports_inline());
        }

        if let Some(preferred) = &bound.hints.preferred_impl {
            if let Some(implementation) = candidates
                .iter()
                .find(|entry| entry.implementor.as_str() == preferred)
            {
                return Ok(CapabilitySolution::new(
                    implementation.implementor.clone(),
                    implementation.binding_type.clone(),
                    implementation.dispatch,
                ));
            }
        }

        if candidates.is_empty() {
            return Err(CapabilityResolutionError::NotFound {
                capability: bound.name.clone(),
                target: candidate_key,
                preferred_impl: bound.hints.preferred_impl.clone(),
                inline_only: bound.hints.inline_only,
            });
        }

        candidates.sort_by_key(|entry| entry.priority.rank());
        let best_rank = candidates
            .first()
            .map(|entry| entry.priority.rank())
            .unwrap_or(0);
        let finalists: Vec<_> = candidates
            .into_iter()
            .filter(|entry| entry.priority.rank() == best_rank)
            .collect();

        if finalists.len() == 1 {
            let implementation = finalists[0];
            Ok(CapabilitySolution::new(
                implementation.implementor.clone(),
                implementation.binding_type.clone(),
                implementation.dispatch,
            ))
        } else {
            Err(CapabilityResolutionError::Ambiguous {
                capability: bound.name.clone(),
                target: candidate_key,
                candidates: finalists
                    .into_iter()
                    .map(|entry| entry.implementor.clone())
                    .collect(),
                preferred_impl: bound.hints.preferred_impl.clone(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::environment::{CapabilityEnvironment, CapabilityImplementation, CapabilityPriority};
    use crate::types::{
        BoundTypeReference, CapabilityBound, CapabilityHints, DispatchKind, SymbolId, TypeKind,
        TypeVariant,
    };

    fn capability(capability: &str, target: &str) -> CapabilityBound {
        CapabilityBound::new(
            capability,
            BoundTypeReference::Named(target.into()),
            CapabilityHints::default(),
        )
    }

    #[test]
    fn picks_preferred_implementation_when_present() {
        let mut environment = CapabilityEnvironment::new();
        environment.register(CapabilityImplementation::new(
            "Numeric",
            TypeKind::new(TypeVariant::Primitive("Int")),
            SymbolId::from("impl.NumericInt"),
            DispatchKind::Static,
            CapabilityPriority::Local,
            None,
        ));
        environment.register(CapabilityImplementation::new(
            "Numeric",
            TypeKind::new(TypeVariant::Primitive("Int")),
            SymbolId::from("impl.NumericInline"),
            DispatchKind::Inline,
            CapabilityPriority::Explicit,
            None,
        ));
        let mut bound = capability("Numeric", "Int");
        bound.hints.preferred_impl = Some("impl.NumericInline".into());
        let resolver = CapabilityDictionaryResolver::new(&environment);
        let result = resolver
            .resolve(&bound, &TypeKind::new(TypeVariant::Primitive("Int")))
            .unwrap();
        assert_eq!(result.implementor.as_str(), "impl.NumericInline");
        assert_eq!(result.dispatch_kind, DispatchKind::Inline);
    }

    #[test]
    fn errors_when_no_candidates() {
        let environment = CapabilityEnvironment::new();
        let resolver = CapabilityDictionaryResolver::new(&environment);
        let bound = capability("Serializer", "String");
        let err = resolver
            .resolve(&bound, &TypeKind::new(TypeVariant::Primitive("String")))
            .unwrap_err();
        assert!(matches!(err, CapabilityResolutionError::NotFound { .. }));
    }
}
