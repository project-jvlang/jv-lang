//! jv_inference - Hindley-Milner style type inference engine for the jv language.
//!
//! This crate is responsible for hosting the upcoming type inference pipeline that
//! powers the `jv` compiler. The implementation is introduced incrementally and the
//! initial scaffold focuses on the public module layout and service-facing contracts.

pub mod cache;
pub mod constraint;
pub mod diagnostics;
pub mod environment;
pub mod generalize;
pub mod service;
pub mod solver;
pub mod types;

pub use cache::{
    CacheMetrics, CachedDiagnostic, CachedSignature, DependencyTracker, FingerprintHash,
    InferenceCache, SignatureUpdate,
};
pub use constraint::CompactConstraintGraph;
pub use service::{FactSpan, TypeFacts};
pub use types::{
    BoundConstraint, BoundPredicate, BoundsMatrix, FieldType, NullabilityFlag, TypeId, TypeKind,
    TypeVariant,
};
