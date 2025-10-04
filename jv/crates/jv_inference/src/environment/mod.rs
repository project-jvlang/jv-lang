//! Capability-aware environment tables used by the inference engine.
//!
//! The environment records capability implementations discovered during symbol
//! collection.  Resolver stages consult this registry to map logical capability
//! requirements (e.g. `Numeric<Int>`) to concrete implementation symbols while
//! maintaining deterministic priority rules.

use crate::types::{DispatchKind, SymbolId, TypeKind, TypeVariant};
use jv_ast::Span;
use std::collections::HashMap;

/// Priority tiers used to disambiguate competing capability implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapabilityPriority {
    /// Direct user-provided override (e.g. explicit `impl` in the same module).
    Explicit,
    /// Implementation discovered within the current package scope.
    Local,
    /// Implementation imported from external packages or stdlib fallbacks.
    Imported,
}

impl CapabilityPriority {
    pub(crate) fn rank(self) -> u8 {
        match self {
            CapabilityPriority::Explicit => 0,
            CapabilityPriority::Local => 1,
            CapabilityPriority::Imported => 2,
        }
    }
}

/// Capability implementation entry stored in the environment registry.
#[derive(Debug, Clone)]
pub struct CapabilityImplementation {
    capability: String,
    target_key: String,
    pub binding_type: TypeKind,
    pub implementor: SymbolId,
    pub dispatch: DispatchKind,
    pub priority: CapabilityPriority,
    pub span: Option<Span>,
}

impl CapabilityImplementation {
    /// Creates a new implementation record with a canonical target key.
    pub fn new(
        capability: impl Into<String>,
        binding_type: TypeKind,
        implementor: SymbolId,
        dispatch: DispatchKind,
        priority: CapabilityPriority,
        span: Option<Span>,
    ) -> Self {
        let capability = capability.into();
        let target_key = canonical_type_key(&binding_type);
        Self {
            capability,
            target_key,
            binding_type,
            implementor,
            dispatch,
            priority,
            span,
        }
    }

    pub fn capability(&self) -> &str {
        &self.capability
    }

    pub fn target_key(&self) -> &str {
        &self.target_key
    }
}

/// Registry mapping capability names to available implementations.
#[derive(Debug, Default, Clone)]
pub struct CapabilityEnvironment {
    registry: HashMap<String, Vec<CapabilityImplementation>>,
}

impl CapabilityEnvironment {
    pub fn new() -> Self {
        Self::default()
    }

    /// Registers an implementation within the environment.
    pub fn register(&mut self, implementation: CapabilityImplementation) {
        self.registry
            .entry(implementation.capability().to_string())
            .or_default()
            .push(implementation);
    }

    /// Returns implementations matching a capability and canonical target key.
    pub fn find_candidates(
        &self,
        capability: &str,
        target_key: &str,
    ) -> Vec<&CapabilityImplementation> {
        self.registry
            .get(capability)
            .map(|entries| {
                entries
                    .iter()
                    .filter(|implementation| implementation.target_key() == target_key)
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Produces a canonical comparison key for a [`TypeKind`].
    pub fn canonical_type_key(kind: &TypeKind) -> String {
        canonical_type_key(kind)
    }
}

fn canonical_type_key(kind: &TypeKind) -> String {
    match kind.variant() {
        TypeVariant::Primitive(name) => (*name).to_string(),
        TypeVariant::Optional(inner) => format!("{}?", canonical_type_key(inner)),
        TypeVariant::Function(params, ret) => {
            let params = params
                .iter()
                .map(canonical_type_key)
                .collect::<Vec<_>>()
                .join(",");
            format!("fn({params})->{}", canonical_type_key(ret))
        }
        TypeVariant::Record { fields } => {
            let parts = fields
                .iter()
                .map(|field| format!("{}:{}", field.name, canonical_type_key(&field.ty)))
                .collect::<Vec<_>>()
                .join(",");
            format!("{{{parts}}}")
        }
        TypeVariant::Union { arms } => {
            let parts = arms
                .iter()
                .map(canonical_type_key)
                .collect::<Vec<_>>()
                .join("|");
            format!("union({parts})")
        }
        TypeVariant::Variable(id) => format!("var#{}", id.to_raw()),
        TypeVariant::Unknown => "_".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonical_key_handles_nested_types() {
        let fn_type = TypeKind::function(
            vec![TypeKind::new(TypeVariant::Primitive("Int"))],
            TypeKind::optional(TypeKind::new(TypeVariant::Primitive("String"))),
        );
        assert_eq!(canonical_type_key(&fn_type), "fn(Int)->String?");
    }
}
