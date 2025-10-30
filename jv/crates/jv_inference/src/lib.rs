//! jv_inference - Hindley-Milner style type inference engine for the jv language.
//!
//! This crate is responsible for hosting the upcoming type inference pipeline that
//! powers the `jv` compiler. The implementation is introduced incrementally and the
//! initial scaffold focuses on the public module layout and service-facing contracts.

pub mod cache;
pub mod config;
pub mod constraint;
pub mod diagnostics;
pub mod environment;
pub mod generalize;
pub mod kind;
pub mod service;
pub mod solver;
pub mod type_level;
pub mod types;

#[cfg(test)]
mod tests;

pub use cache::{
    CacheMetrics, CachedDiagnostic, CachedSignature, ConstraintCache, DependencyTracker,
    FingerprintHash, InferenceCache, SignatureUpdate,
};
pub use config::ParallelInferenceConfig;
pub use constraint::CompactConstraintGraph;
pub use service::{
    FactSpan, SchemaCache, TypeFacts, TypeFactsTelemetry, TypeLevelValue, json_literal_to_schema,
    sanitize_java_identifier, snake_to_camel,
};
pub use type_level::{
    EvaluationContext, EvaluationDependencies, TerminationConfig, TerminationError,
    TypeLevelBinding, TypeLevelDiagnostic, TypeLevelEvaluationOutcome, TypeLevelEvaluator,
};
pub use types::{
    BoundConstraint, BoundPredicate, BoundTypeReference, BoundsMatrix, CapabilityBound,
    CapabilityHints, CapabilitySolution, DispatchKind, FieldType, FunctionSignatureBound,
    NullabilityFlag, SymbolId, TraitBound, TypeId, TypeKind, TypeVariant,
};
