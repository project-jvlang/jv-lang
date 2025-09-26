//! 型推論エンジンのエントリーポイントと公開インターフェース。
//!
//! このモジュールはサブモジュールとして制約処理、型表現、推論ユーティリティを
//! 提供し、将来的に `TypeChecker` から呼び出されるエントリーポイントをまとめる。

pub mod constraint;
pub mod engine;
pub mod environment;
pub mod types;
pub mod unify;
pub mod utils;

pub use constraint::{Constraint, ConstraintGenerator, ConstraintKind, ConstraintSet};
pub use engine::{InferenceEngine, InferenceError, InferenceResult};
pub use environment::{TypeEnvironment, TypeScheme};
pub use types::{TypeBinding, TypeId, TypeKind, TypeVariable, TypeVariableKind};
pub use unify::{ConstraintSolver, SolveError, SolveResult};
pub use utils::TypeIdGenerator;
