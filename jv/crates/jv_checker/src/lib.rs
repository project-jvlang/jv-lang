// jv_checker - Static analysis and validation
pub mod compat;
pub mod diagnostics;
pub mod inference;

pub use inference::{
    InferenceEngine, InferenceError, InferenceResult, NullabilityAnalyzer, TypeBinding,
    TypeEnvironment, TypeId, TypeKind, TypeScheme,
};

use jv_ast::Program;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CheckError {
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Null safety violation: {0}")]
    NullSafetyError(String),
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("Invalid syntax: {0}")]
    SyntaxError(String),
    #[error("Validation error: {0}")]
    ValidationError(String),
}

/// 型推論の結果を外部コンシューマへ提供するためのスナップショット。
#[derive(Debug, Clone)]
pub struct InferenceSnapshot {
    environment: TypeEnvironment,
    bindings: Vec<TypeBinding>,
    function_schemes: HashMap<String, TypeScheme>,
    result_type: Option<TypeKind>,
}

impl InferenceSnapshot {
    fn from_engine(engine: &InferenceEngine) -> Self {
        Self {
            environment: engine.environment().clone(),
            bindings: engine.bindings().to_vec(),
            function_schemes: engine.function_schemes().clone(),
            result_type: engine.result_type().cloned(),
        }
    }
}

/// 推論済み情報へアクセスするためのサービスレイヤ。
pub trait TypeInferenceService {
    /// すべての型束縛を取得する。
    fn bindings(&self) -> &[TypeBinding];

    /// 型環境全体を参照する。
    fn environment(&self) -> &TypeEnvironment;

    /// 指定された関数シグネチャを取得する。
    fn function_scheme(&self, name: &str) -> Option<&TypeScheme>;

    /// 利用可能な関数シグネチャ一覧を返す。
    fn function_schemes(&self) -> &HashMap<String, TypeScheme>;

    /// 推論済みのトップレベル型を返す。
    fn result_type(&self) -> Option<&TypeKind>;
}

impl TypeInferenceService for InferenceSnapshot {
    fn bindings(&self) -> &[TypeBinding] {
        &self.bindings
    }

    fn environment(&self) -> &TypeEnvironment {
        &self.environment
    }

    fn function_scheme(&self, name: &str) -> Option<&TypeScheme> {
        self.function_schemes.get(name)
    }

    fn function_schemes(&self) -> &HashMap<String, TypeScheme> {
        &self.function_schemes
    }

    fn result_type(&self) -> Option<&TypeKind> {
        self.result_type.as_ref()
    }
}

/// TypeChecker orchestrates the inference engine and exposes analysis results.
#[derive(Debug)]
pub struct TypeChecker {
    engine: InferenceEngine,
    snapshot: Option<InferenceSnapshot>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            engine: InferenceEngine::new(),
            snapshot: None,
        }
    }

    /// 型推論と整合性検証を実行する。
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<CheckError>> {
        match self.engine.infer_program(program) {
            Ok(()) => {
                self.snapshot = Some(InferenceSnapshot::from_engine(&self.engine));
                Ok(())
            }
            Err(error) => {
                self.snapshot = None;
                Err(vec![CheckError::TypeError(error.to_string())])
            }
        }
    }

    /// null 安全診断を推論結果に基づかず AST から直接実行する。
    pub fn check_null_safety(&self, program: &Program) -> Vec<CheckError> {
        NullabilityAnalyzer::analyze(program)
    }

    /// 現在保持している推論スナップショットを取得する。
    pub fn inference_snapshot(&self) -> Option<&InferenceSnapshot> {
        self.snapshot.as_ref()
    }

    /// 推論サービスとしてアクセスする。
    pub fn inference_service(&self) -> Option<&dyn TypeInferenceService> {
        self.snapshot
            .as_ref()
            .map(|snapshot| snapshot as &dyn TypeInferenceService)
    }

    /// 推論スナップショットを引き渡し、内部状態からは破棄する。
    pub fn take_inference_snapshot(&mut self) -> Option<InferenceSnapshot> {
        self.snapshot.take()
    }

    /// Check for forbidden Java syntax or patterns.
    pub fn check_forbidden_syntax(&self, _program: &Program) -> Vec<String> {
        let violations = Vec::new();
        // This would check for patterns that shouldn't appear in jv code
        // For example: raw Java generics syntax, null checks without ?. operator, etc.
        violations
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
