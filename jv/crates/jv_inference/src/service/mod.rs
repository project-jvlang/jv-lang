//! Service-layer abstractions used to expose inference results to the rest of the compiler.
//!
//! `TypeFacts` acts as the shared contract between the inference engine and downstream
//! consumers such as the checker, code generator, and LSP server. Concrete implementations
//! live outside this crate so that each consumer can decide how to persist and surface data.

use crate::solver::TypeBinding;
use crate::types::{TypeId, TypeKind};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Arc;

/// Alias for spans reported by the inference pipeline.
pub type FactSpan = jv_ast::Span;

/// Identifier used when registering node-level type information. Downstream
/// crates can map this identifier to their concrete AST representations (e.g.
/// `AstId`).
pub type TypeFactsNodeId = u32;

/// Describes the read-only facts that the inference engine exposes once analysis completes.
///
/// The trait is intentionally generic so that downstream crates can project their own
/// representations of environments, schemes, and diagnostic payloads while still sharing
/// a common vocabulary.
pub trait TypeFacts {
    /// Type of node identifiers used to query facts.
    type NodeId: Copy + Eq + Hash;
    /// Concrete type environment snapshot.
    type Environment;
    /// Binding information for type variables.
    type Binding;
    /// Function or let-binding schemes that callers can instantiate.
    type Scheme;
    /// Fully resolved type expressions.
    type Type;

    /// Returns the global type environment recorded by the inference engine.
    fn environment(&self) -> &Self::Environment;

    /// Returns all type variable bindings captured during solving.
    fn bindings(&self) -> &[Self::Binding];

    /// Resolves the type scheme associated with a named binding, if available.
    fn scheme_for(&self, name: &str) -> Option<&Self::Scheme>;

    /// Enumerates all known schemes as `(name, scheme)` pairs for downstream iteration.
    fn all_schemes(&self) -> Vec<(&str, &Self::Scheme)>;

    /// Queries the resolved type for the specified AST node identifier.
    fn type_for_node(&self, node: Self::NodeId) -> Option<&Self::Type>;

    /// Retrieves the inferred type for the outermost expression or declaration, if present.
    fn root_type(&self) -> Option<&Self::Type>;
}

/// Snapshot of the type environment captured after solving.
#[derive(Debug, Clone, Default)]
pub struct TypeEnvironmentSnapshot {
    values: HashMap<String, TypeKind>,
}

impl TypeEnvironmentSnapshot {
    pub fn new(values: HashMap<String, TypeKind>) -> Self {
        Self { values }
    }

    pub fn values(&self) -> &HashMap<String, TypeKind> {
        &self.values
    }
}

/// Describes a type scheme that can be instantiated by downstream consumers.
#[derive(Debug, Clone)]
pub struct TypeScheme {
    pub generics: Vec<TypeId>,
    pub body: TypeKind,
}

impl TypeScheme {
    pub fn new(generics: Vec<TypeId>, body: TypeKind) -> Self {
        Self { generics, body }
    }
}

/// Concrete implementation of [`TypeFacts`] that stores all inference outputs in
/// owned collections so the results can be shared across threads.
#[derive(Debug, Clone)]
pub struct TypeFactsSnapshot {
    environment: Arc<TypeEnvironmentSnapshot>,
    bindings: Arc<Vec<TypeBinding>>,
    schemes: Arc<HashMap<String, TypeScheme>>,
    node_types: Arc<HashMap<TypeFactsNodeId, TypeKind>>,
    root_type: Option<TypeKind>,
}

impl TypeFactsSnapshot {
    fn new(
        environment: Arc<TypeEnvironmentSnapshot>,
        bindings: Arc<Vec<TypeBinding>>,
        schemes: Arc<HashMap<String, TypeScheme>>,
        node_types: Arc<HashMap<TypeFactsNodeId, TypeKind>>,
        root_type: Option<TypeKind>,
    ) -> Self {
        Self {
            environment,
            bindings,
            schemes,
            node_types,
            root_type,
        }
    }

    /// Produces a debug JSON representation of the snapshot. Complex values are
    /// stringified via their `Debug` implementation so we do not require serde
    /// integration for every type structure.
    pub fn to_json(&self) -> Value {
        let environment = self
            .environment
            .as_ref()
            .values()
            .iter()
            .map(|(name, ty)| (name.clone(), format_type(ty)))
            .collect::<HashMap<_, _>>();

        let bindings = self
            .bindings
            .iter()
            .map(|binding| format!("{:?}", binding))
            .collect::<Vec<_>>();

        let schemes = self
            .schemes
            .iter()
            .map(|(name, scheme)| {
                (
                    name.clone(),
                    json!({
                        "generics": scheme
                            .generics
                            .iter()
                            .map(|id| id.to_raw())
                            .collect::<Vec<_>>(),
                        "body": format_type(&scheme.body),
                    }),
                )
            })
            .collect::<HashMap<_, _>>();

        let node_types = self
            .node_types
            .iter()
            .map(|(id, ty)| (id.to_string(), format_type(ty)))
            .collect::<HashMap<_, _>>();

        json!({
            "environment": environment,
            "bindings": bindings,
            "schemes": schemes,
            "node_types": node_types,
            "root_type": self.root_type.as_ref().map(format_type),
        })
    }

    /// Serialises the facts as a pretty JSON string for CLI debug output.
    pub fn to_pretty_json(&self) -> serde_json::Result<String> {
        serde_json::to_string_pretty(&self.to_json())
    }
}

impl TypeFacts for TypeFactsSnapshot {
    type NodeId = TypeFactsNodeId;
    type Environment = TypeEnvironmentSnapshot;
    type Binding = TypeBinding;
    type Scheme = TypeScheme;
    type Type = TypeKind;

    fn environment(&self) -> &Self::Environment {
        self.environment.as_ref()
    }

    fn bindings(&self) -> &[Self::Binding] {
        self.bindings.as_slice()
    }

    fn scheme_for(&self, name: &str) -> Option<&Self::Scheme> {
        self.schemes.get(name)
    }

    fn all_schemes(&self) -> Vec<(&str, &Self::Scheme)> {
        self.schemes
            .iter()
            .map(|(name, scheme)| (name.as_str(), scheme))
            .collect()
    }

    fn type_for_node(&self, node: Self::NodeId) -> Option<&Self::Type> {
        self.node_types.get(&node)
    }

    fn root_type(&self) -> Option<&Self::Type> {
        self.root_type.as_ref()
    }
}

/// Builder used to assemble [`TypeFactsSnapshot`] instances in a structured way.
#[derive(Debug, Default)]
pub struct TypeFactsBuilder {
    environment: HashMap<String, TypeKind>,
    bindings: Vec<TypeBinding>,
    schemes: HashMap<String, TypeScheme>,
    node_types: HashMap<TypeFactsNodeId, TypeKind>,
    root_type: Option<TypeKind>,
}

impl TypeFactsBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn environment_entry(&mut self, name: impl Into<String>, ty: TypeKind) -> &mut Self {
        self.environment.insert(name.into(), ty);
        self
    }

    pub fn set_environment(&mut self, values: HashMap<String, TypeKind>) -> &mut Self {
        self.environment = values;
        self
    }

    pub fn add_binding(&mut self, binding: TypeBinding) -> &mut Self {
        self.bindings.push(binding);
        self
    }

    pub fn add_scheme(&mut self, name: impl Into<String>, scheme: TypeScheme) -> &mut Self {
        self.schemes.insert(name.into(), scheme);
        self
    }

    pub fn record_node_type(&mut self, node: TypeFactsNodeId, ty: TypeKind) -> &mut Self {
        self.node_types.insert(node, ty);
        self
    }

    pub fn set_root_type(&mut self, ty: TypeKind) -> &mut Self {
        self.root_type = Some(ty);
        self
    }

    pub fn build(self) -> TypeFactsSnapshot {
        TypeFactsSnapshot::new(
            Arc::new(TypeEnvironmentSnapshot::new(self.environment)),
            Arc::new(self.bindings),
            Arc::new(self.schemes),
            Arc::new(self.node_types),
            self.root_type,
        )
    }
}

/// Cache storing the most recent type facts per logical unit (e.g. module).
#[derive(Debug, Default)]
pub struct TypeFactsCache {
    entries: HashMap<String, TypeFactsSnapshot>,
}

impl TypeFactsCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, key: impl Into<String>, snapshot: TypeFactsSnapshot) {
        self.entries.insert(key.into(), snapshot);
    }

    pub fn get(&self, key: &str) -> Option<&TypeFactsSnapshot> {
        self.entries.get(key)
    }

    pub fn remove(&mut self, key: &str) -> Option<TypeFactsSnapshot> {
        self.entries.remove(key)
    }

    pub fn clear(&mut self) {
        self.entries.clear();
    }
}

fn format_type(ty: &TypeKind) -> String {
    format!("{:?}", ty)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::TypeVariant;

    fn sample_type(name: &'static str) -> TypeKind {
        TypeKind::new(TypeVariant::Primitive(name))
    }

    #[test]
    fn builder_constructs_snapshot() {
        let mut builder = TypeFactsBuilder::new();
        builder.record_node_type(1, sample_type("Int"));
        builder.add_binding(TypeBinding {
            id: TypeId::new(0),
            ty: sample_type("Int"),
        });
        builder.add_scheme(
            "id",
            TypeScheme::new(vec![TypeId::new(1)], sample_type("Int")),
        );
        builder.environment_entry("x", sample_type("Int"));
        builder.set_root_type(sample_type("Int"));

        let snapshot = builder.build();

        assert_eq!(snapshot.bindings().len(), 1);
        assert!(snapshot.scheme_for("id").is_some());
        assert!(snapshot.type_for_node(1).is_some());
        assert!(snapshot.environment().values().contains_key("x"));
        assert!(snapshot.root_type().is_some());
    }

    #[test]
    fn snapshot_produces_json() {
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry("x", sample_type("Int"));
        let snapshot = builder.build();

        let json = snapshot.to_pretty_json().expect("json");
        assert!(json.contains("\"environment\""));
    }

    #[test]
    fn cache_stores_snapshots() {
        let snapshot = TypeFactsBuilder::new().build();
        let mut cache = TypeFactsCache::new();
        cache.insert("module", snapshot.clone());
        assert!(cache.get("module").is_some());
        assert!(cache.remove("module").is_some());
    }

    #[test]
    fn snapshot_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<TypeFactsSnapshot>();
    }
}
