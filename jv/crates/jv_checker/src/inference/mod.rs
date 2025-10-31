//! 型推論エンジンのエントリーポイントと公開インターフェース。
//!
//! このモジュールはサブモジュールとして制約処理、型表現、推論ユーティリティを
//! 提供し、将来的に `TypeChecker` から呼び出されるエントリーポイントをまとめる。

pub mod compatibility;
pub mod constraint;
pub mod context_adaptation;
pub mod conversions;
pub mod diagnostics;
pub mod engine;
pub mod environment;
pub mod extensions;
pub mod imports;
pub mod iteration;
pub mod nullability;
pub mod prelude;
pub mod regex;
pub mod type_factory;
pub mod types;
pub mod unify;
pub mod utils;

pub use crate::java::{JavaBoxingTable, JavaNullabilityPolicy, JavaPrimitive};
pub use compatibility::CompatibilityChecker;
pub use constraint::{Constraint, ConstraintGenerator, ConstraintKind, ConstraintSet};
pub use context_adaptation::{CharToStringAdaptation, ContextAdaptation};
pub use conversions::{
    AppliedConversion, ConversionKind, ConversionMetadata, ConversionOutcome,
    ConversionRulesEngine, HelperSpec, NullableGuard, NullableGuardReason,
};
pub use engine::{InferenceEngine, InferenceError, InferenceResult};
pub use environment::{TypeEnvironment, TypeScheme};
pub use iteration::LoopClassification;
pub use nullability::NullabilityAnalyzer;
pub use regex::{RegexCommandIssue, RegexCommandTyping, RegexMatchTyping, RegexMatchWarning};
pub use type_factory::TypeFactory;
pub use types::{
    PrimitiveType, TypeBinding, TypeError, TypeId, TypeKind, TypeVariable, TypeVariableKind,
};
pub use unify::{ConstraintSolver, SolveError, SolveResult};
pub use utils::TypeIdGenerator;
