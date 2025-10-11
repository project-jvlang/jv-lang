//! Service-layer abstractions used to expose inference results to the rest of the compiler.
//!
//! `TypeFacts` acts as the shared contract between the inference engine and downstream
//! consumers such as the checker, code generator, and LSP server. Concrete implementations
//! live outside this crate so that each consumer can decide how to persist and surface data.

use crate::cache::CacheMetrics;
use crate::constraint::ConstraintSolution;
use crate::solver::{TypeBinding, Variance};
use crate::types::{GenericBounds, NullabilityFlag, SymbolId, TypeId, TypeKind};
use hex::encode as hex_encode;
use jv_ast::json::{JsonLiteral, JsonValue};
use jv_ast::types::Kind;
use jv_ir::transform::infer_json_value_schema;
use jv_ir::types::Schema;
use serde_json::{json, Value};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::sync::Arc;

/// Alias for spans reported by the inference pipeline.
pub type FactSpan = jv_ast::Span;

/// Identifier used when registering node-level type information. Downstream
/// crates can map this identifier to their concrete AST representations (e.g.
/// `AstId`).
pub type TypeFactsNodeId = u32;

/// Evaluated value produced by type-level computation or const bindings.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeLevelValue {
    Int(i64),
    Bool(bool),
    String(String),
}

impl TypeLevelValue {
    /// Returns the inner integer when the value represents an `Int`.
    pub fn as_int(&self) -> Option<i64> {
        if let Self::Int(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the inner boolean when the value represents a `Bool`.
    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the inner string when the value represents a `String`.
    pub fn as_str(&self) -> Option<&str> {
        if let Self::String(value) = self {
            Some(value.as_str())
        } else {
            None
        }
    }

    fn to_json_value(&self) -> Value {
        match self {
            Self::Int(value) => json!(value),
            Self::Bool(value) => json!(value),
            Self::String(value) => json!(value),
        }
    }
}

impl fmt::Display for TypeLevelValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "\"{value}\""),
        }
    }
}

/// Telemetry counters emitted by the TypeFacts service for `--telemetry`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeFactsTelemetry {
    /// Number of kind checks performed during the inference pass.
    pub kind_checks_count: u64,
    /// Hit rate for the kind assignment cache when reusing results.
    pub kind_cache_hit_rate: Option<f64>,
    /// Number of const evaluations executed for dependent generics.
    pub const_evaluations: u64,
    /// Size of the type-level evaluation cache after inference completes.
    pub type_level_cache_size: usize,
}

impl TypeFactsTelemetry {
    /// Creates a telemetry snapshot with the provided counters.
    pub fn new(
        kind_checks_count: u64,
        kind_cache_hit_rate: Option<f64>,
        const_evaluations: u64,
        type_level_cache_size: usize,
    ) -> Self {
        Self {
            kind_checks_count,
            kind_cache_hit_rate,
            const_evaluations,
            type_level_cache_size,
        }
    }
}

impl Default for TypeFactsTelemetry {
    fn default() -> Self {
        Self {
            kind_checks_count: 0,
            kind_cache_hit_rate: None,
            const_evaluations: 0,
            type_level_cache_size: 0,
        }
    }
}

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
    generics: Vec<TypeId>,
    body: TypeKind,
    bounds: HashMap<TypeId, GenericBounds>,
    variance: HashMap<TypeId, Variance>,
}

impl TypeScheme {
    pub fn new(generics: Vec<TypeId>, body: TypeKind) -> Self {
        Self::with_bounds(generics, body, HashMap::new())
    }

    pub fn monomorphic(body: TypeKind) -> Self {
        Self::new(Vec::new(), body)
    }

    pub fn with_bounds(
        generics: Vec<TypeId>,
        body: TypeKind,
        bounds: HashMap<TypeId, GenericBounds>,
    ) -> Self {
        let mut sorted = generics;
        sorted.sort_by_key(|id| id.to_raw());
        sorted.dedup_by_key(|id| id.to_raw());
        Self {
            generics: sorted,
            body,
            bounds,
            variance: HashMap::new(),
        }
    }

    pub fn generics(&self) -> &[TypeId] {
        &self.generics
    }

    pub fn body(&self) -> &TypeKind {
        &self.body
    }

    pub fn bounds(&self) -> &HashMap<TypeId, GenericBounds> {
        &self.bounds
    }

    pub fn bounds_for(&self, id: TypeId) -> Option<&GenericBounds> {
        self.bounds.get(&id)
    }

    pub fn variance(&self) -> &HashMap<TypeId, Variance> {
        &self.variance
    }

    pub fn variance_for(&self, id: TypeId) -> Option<&Variance> {
        self.variance.get(&id)
    }

    pub fn with_variance(mut self, variance: HashMap<TypeId, Variance>) -> Self {
        self.variance = variance;
        self
    }

    pub fn set_variance(&mut self, id: TypeId, variance: Variance) {
        self.variance.insert(id, variance);
    }

    pub fn is_polymorphic(&self) -> bool {
        !self.generics.is_empty()
    }
}

#[derive(Debug, Clone, Default)]
struct GenericFacts {
    arguments: HashMap<SymbolId, Vec<TypeKind>>,
    bounds: HashMap<TypeId, GenericBounds>,
    variance: HashMap<TypeId, Variance>,
    sealed_permits: HashMap<TypeId, Vec<TypeKind>>,
    kinds: HashMap<TypeId, Kind>,
    const_bindings: HashMap<SymbolId, HashMap<String, TypeLevelValue>>,
    type_level_results: HashMap<SymbolId, HashMap<String, TypeLevelValue>>,
}

impl GenericFacts {
    fn record_arguments(&mut self, symbol: SymbolId, args: Vec<TypeKind>) {
        self.arguments.insert(symbol, args);
    }

    fn record_bounds(&mut self, type_param: TypeId, bounds: GenericBounds) {
        self.bounds.insert(type_param, bounds);
    }

    fn record_variance(&mut self, type_param: TypeId, variance: Variance) {
        self.variance.insert(type_param, variance);
    }

    fn record_sealed_permits(&mut self, type_param: TypeId, permits: Vec<TypeKind>) {
        self.sealed_permits.insert(type_param, permits);
    }

    fn record_kind(&mut self, type_param: TypeId, kind: Kind) {
        self.kinds.insert(type_param, kind);
    }

    fn kind_for(&self, type_param: &TypeId) -> Option<&Kind> {
        self.kinds.get(type_param)
    }

    fn kinds(&self) -> &HashMap<TypeId, Kind> {
        &self.kinds
    }

    fn record_const_binding(&mut self, owner: SymbolId, parameter: String, value: TypeLevelValue) {
        self.const_bindings
            .entry(owner)
            .or_default()
            .insert(parameter, value);
    }

    fn const_bindings_for(&self, owner: &SymbolId) -> Option<&HashMap<String, TypeLevelValue>> {
        self.const_bindings.get(owner)
    }

    fn const_binding(&self, owner: &SymbolId, parameter: &str) -> Option<&TypeLevelValue> {
        self.const_bindings_for(owner)
            .and_then(|bindings| bindings.get(parameter))
    }

    fn const_bindings(&self) -> &HashMap<SymbolId, HashMap<String, TypeLevelValue>> {
        &self.const_bindings
    }

    fn record_type_level_result(&mut self, owner: SymbolId, slot: String, value: TypeLevelValue) {
        self.type_level_results
            .entry(owner)
            .or_default()
            .insert(slot, value);
    }

    fn type_level_results_for(&self, owner: &SymbolId) -> Option<&HashMap<String, TypeLevelValue>> {
        self.type_level_results.get(owner)
    }

    fn type_level_result(&self, owner: &SymbolId, slot: &str) -> Option<&TypeLevelValue> {
        self.type_level_results_for(owner)
            .and_then(|results| results.get(slot))
    }

    fn type_level_results(&self) -> &HashMap<SymbolId, HashMap<String, TypeLevelValue>> {
        &self.type_level_results
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
    cache_metrics: Option<CacheMetrics>,
    java_annotations: Arc<HashMap<String, Vec<String>>>,
    generic_facts: Arc<GenericFacts>,
    nullability_overrides: Arc<HashMap<TypeFactsNodeId, NullabilityFlag>>,
    telemetry: TypeFactsTelemetry,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub name: String,
    pub generics: Vec<TypeId>,
    pub arguments: Vec<TypeKind>,
    pub variance: HashMap<TypeId, Variance>,
    pub body: TypeKind,
}

impl FunctionSignature {
    fn new(
        name: impl Into<String>,
        scheme: &TypeScheme,
        arguments: Vec<TypeKind>,
        variance: HashMap<TypeId, Variance>,
    ) -> Self {
        Self {
            name: name.into(),
            generics: scheme.generics().to_vec(),
            arguments,
            variance,
            body: scheme.body().clone(),
        }
    }
}

impl TypeFactsSnapshot {
    fn new(
        environment: Arc<TypeEnvironmentSnapshot>,
        bindings: Arc<Vec<TypeBinding>>,
        schemes: Arc<HashMap<String, TypeScheme>>,
        node_types: Arc<HashMap<TypeFactsNodeId, TypeKind>>,
        root_type: Option<TypeKind>,
        cache_metrics: Option<CacheMetrics>,
        java_annotations: Arc<HashMap<String, Vec<String>>>,
        generic_facts: Arc<GenericFacts>,
        nullability_overrides: Arc<HashMap<TypeFactsNodeId, NullabilityFlag>>,
        telemetry: TypeFactsTelemetry,
    ) -> Self {
        Self {
            environment,
            bindings,
            schemes,
            node_types,
            root_type,
            cache_metrics,
            java_annotations,
            generic_facts,
            nullability_overrides,
            telemetry,
        }
    }

    pub fn generic_arguments_for(&self, symbol: &SymbolId) -> Option<&[TypeKind]> {
        self.generic_facts
            .arguments
            .get(symbol)
            .map(|args| args.as_slice())
    }

    pub fn recorded_bounds(&self, type_param: TypeId) -> Option<&GenericBounds> {
        self.generic_facts.bounds.get(&type_param)
    }

    pub fn recorded_variance(&self, type_param: TypeId) -> Option<Variance> {
        self.generic_facts.variance.get(&type_param).copied()
    }

    pub fn sealed_permits_for(&self, type_param: TypeId) -> Option<&[TypeKind]> {
        self.generic_facts
            .sealed_permits
            .get(&type_param)
            .map(|permits| permits.as_slice())
    }

    pub fn kind_for(&self, type_param: TypeId) -> Option<&Kind> {
        self.generic_facts.kind_for(&type_param)
    }

    pub fn kind_assignments(&self) -> &HashMap<TypeId, Kind> {
        self.generic_facts.kinds()
    }

    pub fn const_bindings_for(&self, owner: &SymbolId) -> Option<&HashMap<String, TypeLevelValue>> {
        self.generic_facts.const_bindings_for(owner)
    }

    pub fn const_binding(&self, owner: &SymbolId, parameter: &str) -> Option<&TypeLevelValue> {
        self.generic_facts.const_binding(owner, parameter)
    }

    pub fn const_bindings(&self) -> &HashMap<SymbolId, HashMap<String, TypeLevelValue>> {
        self.generic_facts.const_bindings()
    }

    pub fn type_level_results_for(
        &self,
        owner: &SymbolId,
    ) -> Option<&HashMap<String, TypeLevelValue>> {
        self.generic_facts.type_level_results_for(owner)
    }

    pub fn type_level_result(&self, owner: &SymbolId, slot: &str) -> Option<&TypeLevelValue> {
        self.generic_facts.type_level_result(owner, slot)
    }

    pub fn type_level_results(&self) -> &HashMap<SymbolId, HashMap<String, TypeLevelValue>> {
        self.generic_facts.type_level_results()
    }

    pub fn nullability_override_for(&self, node: TypeFactsNodeId) -> Option<NullabilityFlag> {
        self.nullability_overrides.get(&node).copied()
    }

    pub fn function_signature(&self, name: &str) -> Option<FunctionSignature> {
        let scheme = self.scheme_for(name)?;
        let symbol = SymbolId::from(name);
        let arguments = self
            .generic_facts
            .arguments
            .get(&symbol)
            .cloned()
            .unwrap_or_default();

        let mut variance = scheme
            .variance()
            .iter()
            .map(|(id, value)| (*id, *value))
            .collect::<HashMap<_, _>>();
        for (id, recorded) in &self.generic_facts.variance {
            variance.insert(*id, *recorded);
        }

        Some(FunctionSignature::new(
            name.to_string(),
            scheme,
            arguments,
            variance,
        ))
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
                let bounds = scheme
                    .bounds()
                    .iter()
                    .map(|(id, bound_set)| {
                        let predicates = bound_set
                            .constraints()
                            .iter()
                            .map(|constraint| format!("{:?}", constraint.predicate))
                            .collect::<Vec<_>>();
                        (id.to_raw().to_string(), predicates)
                    })
                    .collect::<HashMap<_, _>>();

                (
                    name.clone(),
                    json!({
                        "generics": scheme
                            .generics()
                            .iter()
                            .map(|id| id.to_raw())
                            .collect::<Vec<_>>(),
                        "body": format_type(scheme.body()),
                        "bounds": bounds,
                    }),
                )
            })
            .collect::<HashMap<_, _>>();

        let node_types = self
            .node_types
            .iter()
            .map(|(id, ty)| (id.to_string(), format_type(ty)))
            .collect::<HashMap<_, _>>();

        let cache_metrics = self.cache_metrics.map(|metrics| {
            json!({
                "lookups": metrics.lookups,
                "hits": metrics.hits,
                "misses": metrics.misses,
                "invalidations": metrics.invalidations,
                "preserved_constraints": metrics.preserved_constraints,
                "hit_rate": metrics.hit_rate(),
            })
        });

        let java_annotations = self
            .java_annotations
            .iter()
            .map(|(symbol, annotations)| (symbol.clone(), annotations.clone()))
            .collect::<HashMap<_, _>>();

        let generic_arguments = self
            .generic_facts
            .arguments
            .iter()
            .map(|(symbol, args)| {
                (
                    symbol.as_str().to_string(),
                    args.iter().map(format_type).collect::<Vec<_>>(),
                )
            })
            .collect::<HashMap<_, _>>();

        let generic_bounds = self
            .generic_facts
            .bounds
            .iter()
            .map(|(id, bounds)| {
                let predicates = bounds
                    .constraints()
                    .iter()
                    .map(|constraint| format!("{:?}", constraint.predicate))
                    .collect::<Vec<_>>();
                (id.to_raw().to_string(), predicates)
            })
            .collect::<HashMap<_, _>>();

        let variance_map = self
            .generic_facts
            .variance
            .iter()
            .map(|(id, variance)| (id.to_raw().to_string(), format!("{:?}", variance)))
            .collect::<HashMap<_, _>>();

        let sealed_permits = self
            .generic_facts
            .sealed_permits
            .iter()
            .map(|(id, permits)| {
                (
                    id.to_raw().to_string(),
                    permits.iter().map(format_type).collect::<Vec<_>>(),
                )
            })
            .collect::<HashMap<_, _>>();

        let kind_assignments = self
            .generic_facts
            .kinds()
            .iter()
            .map(|(id, kind)| (id.to_raw().to_string(), format!("{kind:?}")))
            .collect::<HashMap<_, _>>();

        let const_bindings = self
            .generic_facts
            .const_bindings()
            .iter()
            .map(|(owner, bindings)| {
                let entries = bindings
                    .iter()
                    .map(|(name, value)| (name.clone(), value.to_json_value()))
                    .collect::<HashMap<_, _>>();
                (owner.as_str().to_string(), json!(entries))
            })
            .collect::<HashMap<_, _>>();

        let type_level_results = self
            .generic_facts
            .type_level_results()
            .iter()
            .map(|(owner, results)| {
                let entries = results
                    .iter()
                    .map(|(slot, value)| (slot.clone(), value.to_json_value()))
                    .collect::<HashMap<_, _>>();
                (owner.as_str().to_string(), json!(entries))
            })
            .collect::<HashMap<_, _>>();

        let nullability_overrides = self
            .nullability_overrides
            .iter()
            .map(|(node, flag)| (node.to_string(), format!("{:?}", flag)))
            .collect::<HashMap<_, _>>();

        json!({
            "environment": environment,
            "bindings": bindings,
            "schemes": schemes,
            "node_types": node_types,
            "root_type": self.root_type.as_ref().map(format_type),
            "cache_metrics": cache_metrics,
            "java_annotations": java_annotations,
            "generic_facts": {
                "arguments": generic_arguments,
                "bounds": generic_bounds,
                "variance": variance_map,
                "sealed_permits": sealed_permits,
                "kinds": kind_assignments,
                "const_bindings": const_bindings,
                "type_level_results": type_level_results,
            },
            "nullability_overrides": nullability_overrides,
            "telemetry": {
                "kind_checks_count": self.telemetry.kind_checks_count,
                "kind_cache_hit_rate": self.telemetry.kind_cache_hit_rate,
                "const_evaluations": self.telemetry.const_evaluations,
                "type_level_cache_size": self.telemetry.type_level_cache_size,
            },
        })
    }

    /// Serialises the facts as a pretty JSON string for CLI debug output.
    pub fn to_pretty_json(&self) -> serde_json::Result<String> {
        serde_json::to_string_pretty(&self.to_json())
    }

    /// Returns cache telemetry captured during the inference run if available.
    pub fn cache_metrics(&self) -> Option<CacheMetrics> {
        self.cache_metrics
    }

    /// Returns raw Java annotation metadata captured for external symbols.
    pub fn java_annotations(&self) -> &HashMap<String, Vec<String>> {
        self.java_annotations.as_ref()
    }

    /// Returns telemetry counters captured during the inference pass.
    pub fn telemetry(&self) -> &TypeFactsTelemetry {
        &self.telemetry
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
    cache_metrics: Option<CacheMetrics>,
    java_annotations: HashMap<String, Vec<String>>,
    generic_facts: GenericFacts,
    nullability_overrides: HashMap<TypeFactsNodeId, NullabilityFlag>,
    telemetry: TypeFactsTelemetry,
}

impl TypeFactsBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a builder pre-populated with the contents of an existing snapshot so callers
    /// can apply incremental overrides (e.g. nullability refinements) before rebuilding.
    pub fn from_snapshot(snapshot: &TypeFactsSnapshot) -> Self {
        Self {
            environment: snapshot.environment.as_ref().values().clone(),
            bindings: snapshot.bindings.as_ref().clone(),
            schemes: snapshot.schemes.as_ref().clone(),
            node_types: snapshot.node_types.as_ref().clone(),
            root_type: snapshot.root_type.clone(),
            cache_metrics: snapshot.cache_metrics,
            java_annotations: snapshot.java_annotations.as_ref().clone(),
            generic_facts: snapshot.generic_facts.as_ref().clone(),
            nullability_overrides: snapshot.nullability_overrides.as_ref().clone(),
            telemetry: *snapshot.telemetry(),
        }
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

    pub fn set_cache_metrics(&mut self, metrics: CacheMetrics) -> &mut Self {
        self.cache_metrics = Some(metrics);
        self
    }

    /// Registers Java annotation metadata associated with the provided symbol.
    pub fn add_java_annotation(
        &mut self,
        symbol: impl Into<String>,
        annotation: impl Into<String>,
    ) -> &mut Self {
        let entry = self
            .java_annotations
            .entry(symbol.into())
            .or_insert_with(Vec::new);
        entry.push(annotation.into());
        self
    }

    /// Replaces the entire Java annotation mapping.
    pub fn set_java_annotations(&mut self, annotations: HashMap<String, Vec<String>>) -> &mut Self {
        self.java_annotations = annotations;
        self
    }

    pub fn record_generic_arguments(
        &mut self,
        symbol: impl Into<SymbolId>,
        args: Vec<TypeKind>,
    ) -> &mut Self {
        self.generic_facts.record_arguments(symbol.into(), args);
        self
    }

    pub fn record_bounds(&mut self, type_param: TypeId, bounds: GenericBounds) -> &mut Self {
        self.generic_facts.record_bounds(type_param, bounds);
        self
    }

    pub fn apply_constraint_solution(&mut self, solution: &ConstraintSolution) -> &mut Self {
        for resolved in &solution.resolved_bounds {
            if !resolved.is_empty() {
                self.record_bounds(resolved.parameter, resolved.bounds.clone());
            }
        }
        for parameter in solution.nullability.iter() {
            self.record_nullability_override(parameter.to_raw(), NullabilityFlag::Nullable);
        }
        // Capability bindings will be integrated once downstream consumers are ready.
        self
    }

    pub fn record_variance(&mut self, type_param: TypeId, variance: Variance) -> &mut Self {
        self.generic_facts.record_variance(type_param, variance);
        self
    }

    pub fn record_sealed_permits(
        &mut self,
        type_param: TypeId,
        permits: Vec<TypeKind>,
    ) -> &mut Self {
        self.generic_facts
            .record_sealed_permits(type_param, permits);
        self
    }

    pub fn record_nullability_override(
        &mut self,
        node: TypeFactsNodeId,
        nullability: NullabilityFlag,
    ) -> &mut Self {
        self.nullability_overrides.insert(node, nullability);
        self
    }

    pub fn record_kind_assignment(&mut self, type_param: TypeId, kind: Kind) -> &mut Self {
        self.generic_facts.record_kind(type_param, kind);
        self
    }

    pub fn record_const_binding(
        &mut self,
        owner: impl Into<SymbolId>,
        parameter: impl Into<String>,
        value: TypeLevelValue,
    ) -> &mut Self {
        self.generic_facts
            .record_const_binding(owner.into(), parameter.into(), value);
        self
    }

    pub fn record_type_level_evaluation(
        &mut self,
        owner: impl Into<SymbolId>,
        slot: impl Into<String>,
        value: TypeLevelValue,
    ) -> &mut Self {
        self.generic_facts
            .record_type_level_result(owner.into(), slot.into(), value);
        self
    }

    pub fn set_telemetry(&mut self, telemetry: TypeFactsTelemetry) -> &mut Self {
        self.telemetry = telemetry;
        self
    }

    pub fn telemetry(&self) -> &TypeFactsTelemetry {
        &self.telemetry
    }

    pub fn telemetry_mut(&mut self) -> &mut TypeFactsTelemetry {
        &mut self.telemetry
    }

    pub fn build(self) -> TypeFactsSnapshot {
        TypeFactsSnapshot::new(
            Arc::new(TypeEnvironmentSnapshot::new(self.environment)),
            Arc::new(self.bindings),
            Arc::new(self.schemes),
            Arc::new(self.node_types),
            self.root_type,
            self.cache_metrics,
            Arc::new(self.java_annotations),
            Arc::new(self.generic_facts),
            Arc::new(self.nullability_overrides),
            self.telemetry,
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

/// In-memory cache mapping canonical JSON literals to inferred schemas.
#[derive(Debug, Default, Clone)]
pub struct SchemaCache {
    entries: HashMap<String, Schema>,
}

impl SchemaCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn clear(&mut self) {
        self.entries.clear();
    }

    pub fn infer_schema(&mut self, literal: &JsonLiteral) -> Schema {
        let canonical_value = literal_to_value(literal);
        let key = compute_schema_key(&canonical_value);

        if let Some(schema) = self.entries.get(&key) {
            return schema.clone();
        }

        let schema = infer_json_value_schema(&canonical_value);
        self.entries.insert(key, schema.clone());
        schema
    }
}

/// Convenience wrapper for inferring schemas from `JsonLiteral` nodes while
/// leveraging the provided cache.
pub fn json_literal_to_schema(cache: &mut SchemaCache, literal: &JsonLiteral) -> Schema {
    cache.infer_schema(literal)
}

/// Converts a `JsonLiteral` into a canonical JSON value suitable for embedding or hashing.
pub fn json_literal_to_value(literal: &JsonLiteral) -> Value {
    literal_to_value(literal)
}

/// Converts snake_case or kebab-case identifiers into camelCase.
pub fn snake_to_camel(input: &str) -> String {
    if !input
        .chars()
        .any(|c| c == '_' || c == '-' || c.is_whitespace())
    {
        let mut chars = input.chars();
        if let Some(first) = chars.next() {
            let mut result = String::new();
            result.push(first.to_ascii_lowercase());
            result.extend(chars);
            return result;
        } else {
            return String::new();
        }
    }

    let mut parts = input
        .split(|c: char| c == '_' || c == '-' || c.is_whitespace())
        .filter(|segment| !segment.is_empty());

    let first = match parts.next() {
        Some(segment) => segment.to_ascii_lowercase(),
        None => String::new(),
    };

    let mut result = first;
    for segment in parts {
        let mut chars = segment.chars();
        if let Some(first_char) = chars.next() {
            result.push(first_char.to_ascii_uppercase());
            for ch in chars {
                result.push(ch.to_ascii_lowercase());
            }
        }
    }

    result
}

/// Ensures the provided identifier is a valid, non-reserved Java identifier.
pub fn sanitize_java_identifier(name: &str) -> String {
    let raw = if name.is_empty() {
        "value".to_string()
    } else {
        name.to_string()
    };

    let mut chars = raw.chars();
    let mut normalized = String::new();

    if let Some(first) = chars.next() {
        if first.is_ascii_alphabetic() || first == '_' {
            normalized.push(first);
        } else if first.is_ascii_digit() {
            normalized.push('_');
            normalized.push(first);
        } else {
            normalized.push('_');
        }
    } else {
        normalized.push('_');
    }

    for ch in chars {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            normalized.push(ch);
        } else {
            normalized.push('_');
        }
    }

    let lowered = normalized.to_ascii_lowercase();
    if JAVA_RESERVED_KEYWORDS.contains(&lowered.as_str()) {
        normalized.push('_');
    }

    normalized
}

fn literal_to_value(literal: &JsonLiteral) -> Value {
    json_value_to_value(&literal.value)
}

fn json_value_to_value(value: &JsonValue) -> Value {
    match value {
        JsonValue::Object { entries, .. } => {
            let mut used = HashSet::new();
            let mut map = serde_json::Map::new();
            for entry in entries {
                let key = canonical_field_name(&entry.key, &mut used);
                map.insert(key, json_value_to_value(&entry.value));
            }
            Value::Object(map)
        }
        JsonValue::Array { elements, .. } => {
            Value::Array(elements.iter().map(json_value_to_value).collect::<Vec<_>>())
        }
        JsonValue::String { value, .. } => Value::String(value.clone()),
        JsonValue::Number { literal, .. } => parse_json_number(literal),
        JsonValue::Boolean { value, .. } => Value::Bool(*value),
        JsonValue::Null { .. } => Value::Null,
    }
}

fn canonical_field_name(raw: &str, used: &mut HashSet<String>) -> String {
    let mut candidate = sanitize_java_identifier(&snake_to_camel(raw));
    if candidate.is_empty() {
        candidate = "value".to_string();
    }

    while used.contains(&candidate) {
        candidate.push('_');
    }

    used.insert(candidate.clone());
    candidate
}

fn parse_json_number(literal: &str) -> Value {
    match serde_json::from_str::<Value>(literal) {
        Ok(Value::Number(number)) => Value::Number(number),
        _ => Value::String(literal.to_string()),
    }
}

fn compute_schema_key(value: &Value) -> String {
    let bytes = serde_json::to_vec(value).expect("serialize canonical json literal");
    let mut hasher = Sha256::new();
    hasher.update(&bytes);
    hex_encode(hasher.finalize())
}

const JAVA_RESERVED_KEYWORDS: &[&str] = &[
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extends",
    "final",
    "finally",
    "float",
    "for",
    "goto",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "native",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "strictfp",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "try",
    "void",
    "volatile",
    "while",
    "true",
    "false",
    "null",
];

fn format_type(ty: &TypeKind) -> String {
    format!("{:?}", ty)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cache::CacheMetrics;
    use crate::constraint::{ConstraintSolution, ResolvedBound};
    use crate::solver::Variance;
    use crate::types::{BoundConstraint, BoundPredicate, NullabilityFlag, TraitBound, TypeVariant};
    use jv_ast::json::{JsonEntry, JsonLiteral, JsonValue, NumberGrouping};
    use jv_ast::types::Kind;
    use jv_ast::types::Span;
    use std::collections::HashSet;

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
        assert!(snapshot.cache_metrics().is_none());
    }

    #[test]
    fn snapshot_produces_json() {
        let mut builder = TypeFactsBuilder::new();
        builder.environment_entry("x", sample_type("Int"));
        builder.set_cache_metrics(CacheMetrics::default());
        builder.telemetry_mut().kind_checks_count = 1;
        let snapshot = builder.build();

        let json = snapshot.to_pretty_json().expect("json");
        assert!(json.contains("\"environment\""));
        assert!(json.contains("\"cache_metrics\""));
        assert!(json.contains("\"kind_checks_count\""));
    }

    #[test]
    fn builder_from_snapshot_preserves_entries() {
        let mut original = TypeFactsBuilder::new();
        original.environment_entry("user", sample_type("String"));
        original.add_binding(TypeBinding {
            id: TypeId::new(4),
            ty: sample_type("String"),
        });
        original.record_node_type(9, sample_type("Bool"));
        let snapshot = original.build();

        let cloned = TypeFactsBuilder::from_snapshot(&snapshot).build();

        assert_eq!(
            cloned.environment().values(),
            snapshot.environment().values()
        );
        assert_eq!(cloned.bindings(), snapshot.bindings());
        assert_eq!(
            cloned.type_for_node(9).map(format_type),
            snapshot.type_for_node(9).map(format_type)
        );
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

    #[test]
    fn generic_facts_roundtrip() {
        let mut builder = TypeFactsBuilder::new();
        let symbol = SymbolId::from("pkg::Foo");
        builder.record_generic_arguments(symbol.clone(), vec![sample_type("String")]);

        let bounds = GenericBounds::new(vec![BoundConstraint::new(
            TypeId::new(7),
            BoundPredicate::Trait(TraitBound::simple("Debug")),
        )]);
        builder.record_bounds(TypeId::new(7), bounds.clone());
        builder.record_variance(TypeId::new(7), Variance::Covariant);
        builder.record_sealed_permits(TypeId::new(7), vec![sample_type("Bool")]);

        let snapshot = builder.build();

        let recorded_args = snapshot.generic_arguments_for(&symbol).unwrap();
        assert_eq!(recorded_args.len(), 1);
        assert_eq!(snapshot.recorded_bounds(TypeId::new(7)), Some(&bounds));
        assert_eq!(
            snapshot.recorded_variance(TypeId::new(7)),
            Some(Variance::Covariant)
        );
        assert!(snapshot.sealed_permits_for(TypeId::new(7)).is_some());
    }

    #[test]
    fn records_kind_const_and_type_level_results() {
        let mut builder = TypeFactsBuilder::new();
        let type_param = TypeId::new(21);
        builder.record_kind_assignment(type_param, Kind::Star);

        let owner = SymbolId::from("pkg::Vector");
        builder.record_const_binding(owner.clone(), "N", TypeLevelValue::Int(32));
        builder.record_type_level_evaluation(
            owner.clone(),
            "dimension",
            TypeLevelValue::String("3D".to_string()),
        );

        let snapshot = builder.build();
        assert_eq!(snapshot.kind_for(type_param), Some(&Kind::Star));
        let bindings = snapshot
            .const_bindings_for(&owner)
            .expect("const bindings should be present");
        assert_eq!(bindings.get("N"), Some(&TypeLevelValue::Int(32)));
        assert_eq!(
            snapshot.type_level_result(&owner, "dimension"),
            Some(&TypeLevelValue::String("3D".to_string()))
        );
    }

    #[test]
    fn telemetry_defaults_and_survives_roundtrip() {
        let mut builder = TypeFactsBuilder::new();
        builder.telemetry_mut().kind_checks_count = 5;
        builder.telemetry_mut().kind_cache_hit_rate = Some(0.75);
        builder.telemetry_mut().const_evaluations = 2;
        builder.telemetry_mut().type_level_cache_size = 4;

        let snapshot = builder.build();
        let telemetry = snapshot.telemetry();
        assert_eq!(telemetry.kind_checks_count, 5);
        assert_eq!(telemetry.kind_cache_hit_rate, Some(0.75));
        assert_eq!(telemetry.const_evaluations, 2);
        assert_eq!(telemetry.type_level_cache_size, 4);
    }

    #[test]
    fn apply_constraint_solution_records_bounds_and_nullability() {
        let mut builder = TypeFactsBuilder::new();
        let predicate = BoundPredicate::Trait(TraitBound::simple("Comparable"));
        let bounds = GenericBounds::new(vec![BoundConstraint::new(TypeId::new(13), predicate)]);
        let mut solution = ConstraintSolution::new(SymbolId::from("pkg::Owner"));
        solution
            .resolved_bounds
            .push(ResolvedBound::new(TypeId::new(13), bounds.clone()));
        solution.nullability.mark_nullable(TypeId::new(13));

        builder.apply_constraint_solution(&solution);

        let snapshot = builder.build();
        assert_eq!(snapshot.recorded_bounds(TypeId::new(13)), Some(&bounds));
        assert_eq!(
            snapshot.nullability_override_for(TypeId::new(13).to_raw()),
            Some(NullabilityFlag::Nullable)
        );
    }

    #[test]
    fn function_signature_combines_metadata() {
        let mut builder = TypeFactsBuilder::new();
        let mut scheme = TypeScheme::new(vec![TypeId::new(11)], sample_type("Int"));
        scheme.set_variance(TypeId::new(11), Variance::Contravariant);
        builder.add_scheme("pkg::fn", scheme);
        builder.record_generic_arguments("pkg::fn", vec![sample_type("String")]);
        builder.record_variance(TypeId::new(11), Variance::Covariant);

        let snapshot = builder.build();
        let signature = snapshot
            .function_signature("pkg::fn")
            .expect("function signature");

        assert_eq!(signature.name, "pkg::fn");
        assert_eq!(signature.generics, vec![TypeId::new(11)]);
        assert_eq!(signature.arguments.len(), 1);
        assert_eq!(
            signature.variance.get(&TypeId::new(11)),
            Some(&Variance::Covariant)
        );
    }

    #[test]
    fn nullability_overrides_are_preserved() {
        let mut builder = TypeFactsBuilder::new();
        builder.record_nullability_override(42, NullabilityFlag::Nullable);
        let snapshot = builder.build();
        assert_eq!(
            snapshot.nullability_override_for(42),
            Some(NullabilityFlag::Nullable)
        );
    }

    fn sample_json_literal() -> JsonLiteral {
        JsonLiteral {
            value: JsonValue::Object {
                entries: vec![
                    JsonEntry {
                        key: "created_at".to_string(),
                        comments: Vec::new(),
                        value: JsonValue::Number {
                            literal: "1".to_string(),
                            grouping: NumberGrouping::None,
                            span: Span::dummy(),
                        },
                        span: Span::dummy(),
                    },
                    JsonEntry {
                        key: "user-name".to_string(),
                        comments: Vec::new(),
                        value: JsonValue::String {
                            value: "Alice".to_string(),
                            span: Span::dummy(),
                        },
                        span: Span::dummy(),
                    },
                    JsonEntry {
                        key: "userName".to_string(),
                        comments: Vec::new(),
                        value: JsonValue::Boolean {
                            value: true,
                            span: Span::dummy(),
                        },
                        span: Span::dummy(),
                    },
                ],
                span: Span::dummy(),
            },
            leading_comments: Vec::new(),
            trailing_comments: Vec::new(),
            span: Span::dummy(),
            inferred_schema: None,
        }
    }

    #[test]
    fn schema_cache_infers_and_caches() {
        let literal = sample_json_literal();
        let mut cache = SchemaCache::new();

        let schema1 = json_literal_to_schema(&mut cache, &literal);
        let schema2 = json_literal_to_schema(&mut cache, &literal);

        assert_eq!(cache.len(), 1);
        assert!(!cache.is_empty());

        match schema1 {
            Schema::Object {
                ref fields,
                ref required,
            } => {
                assert!(fields.contains_key("createdAt"));
                assert!(fields.contains_key("userName"));
                assert!(fields.contains_key("userName_"));
                assert!(required.contains("createdAt"));
            }
            other => panic!("expected object schema, found {:?}", other),
        }

        assert_eq!(schema1, schema2);
    }

    #[test]
    fn snake_case_conversion_and_sanitization() {
        assert_eq!(snake_to_camel("created_at"), "createdAt");
        assert_eq!(snake_to_camel("alreadyCamel"), "alreadyCamel");
        assert_eq!(sanitize_java_identifier("class"), "class_");
        assert_eq!(sanitize_java_identifier("9value"), "_9value");

        let mut used = HashSet::new();
        let first = canonical_field_name("user_name", &mut used);
        let second = canonical_field_name("userName", &mut used);
        assert_eq!(first, "userName");
        assert_eq!(second, "userName_");
    }
}
