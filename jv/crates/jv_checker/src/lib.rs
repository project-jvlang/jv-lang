// jv_checker - Static analysis and validation
pub mod compat;
pub mod diagnostics;
pub mod inference;
pub mod null_safety;
pub mod pattern;

pub use inference::{
    InferenceEngine, InferenceError, InferenceResult, NullabilityAnalyzer, TypeBinding,
    TypeEnvironment, TypeId, TypeKind, TypeScheme,
};
pub use jv_inference::ParallelInferenceConfig;

use jv_ast::{Program, Span};
use null_safety::{JavaLoweringHint, NullSafetyCoordinator};
use pattern::{PatternCacheMetrics, PatternMatchService};
use std::collections::HashMap;
use thiserror::Error;

use jv_inference::service::{TypeFactsBuilder, TypeFactsSnapshot, TypeScheme as FactsTypeScheme};
use jv_inference::solver::TypeBinding as FactsTypeBinding;
use jv_inference::types::{
    TypeId as FactsTypeId, TypeKind as FactsTypeKind, TypeVariant as FactsTypeVariant,
};

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
    #[error("Validation error: {message}")]
    ValidationError { message: String, span: Option<Span> },
}

/// 型推論の結果を外部コンシューマへ提供するためのスナップショット。
#[derive(Debug, Clone)]
pub struct InferenceSnapshot {
    environment: TypeEnvironment,
    bindings: Vec<TypeBinding>,
    function_schemes: HashMap<String, TypeScheme>,
    result_type: Option<TypeKind>,
    facts: TypeFactsSnapshot,
}

impl InferenceSnapshot {
    fn from_engine(engine: &InferenceEngine) -> Self {
        let environment = engine.environment().clone();
        let bindings = engine.bindings().to_vec();
        let function_schemes = engine.function_schemes().clone();
        let result_type = engine.result_type().cloned();

        let facts = build_type_facts(
            &environment,
            &bindings,
            &function_schemes,
            result_type.as_ref(),
        );

        Self {
            environment,
            bindings,
            function_schemes,
            result_type,
            facts,
        }
    }

    pub fn type_facts(&self) -> &TypeFactsSnapshot {
        &self.facts
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

/// Telemetry captured during the most recent inference run.
#[derive(Debug, Clone)]
pub struct InferenceTelemetry {
    pub constraints_emitted: usize,
    pub bindings_resolved: usize,
    pub inference_duration_ms: f64,
    pub preserved_constraints: usize,
    pub cache_hit_rate: Option<f64>,
    pub invalidation_cascade_depth: usize,
    pub pattern_cache_hits: u64,
    pub pattern_cache_misses: u64,
}

impl Default for InferenceTelemetry {
    fn default() -> Self {
        Self {
            constraints_emitted: 0,
            bindings_resolved: 0,
            inference_duration_ms: 0.0,
            preserved_constraints: 0,
            cache_hit_rate: None,
            invalidation_cascade_depth: 0,
            pattern_cache_hits: 0,
            pattern_cache_misses: 0,
        }
    }
}

/// TypeChecker orchestrates the inference engine and exposes analysis results.
#[derive(Debug)]
pub struct TypeChecker {
    engine: InferenceEngine,
    snapshot: Option<InferenceSnapshot>,
    parallel_config: ParallelInferenceConfig,
    null_safety_hints: Vec<JavaLoweringHint>,
    merged_facts: Option<TypeFactsSnapshot>,
    pattern_service: PatternMatchService,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self::with_parallel_config(ParallelInferenceConfig::default())
    }

    /// Constructs a type checker with an explicit parallel inference configuration.
    pub fn with_parallel_config(config: ParallelInferenceConfig) -> Self {
        let config = config.sanitized();
        let mut engine = InferenceEngine::new();
        engine.set_parallel_config(config);
        Self {
            engine,
            snapshot: None,
            parallel_config: config,
            null_safety_hints: Vec::new(),
            merged_facts: None,
            pattern_service: PatternMatchService::new(),
        }
    }

    /// Updates the parallel inference configuration and applies it to the engine.
    pub fn set_parallel_config(&mut self, config: ParallelInferenceConfig) {
        let config = config.sanitized();
        self.parallel_config = config;
        self.engine.set_parallel_config(config);
    }

    /// Returns the current parallel inference configuration.
    pub fn parallel_config(&self) -> ParallelInferenceConfig {
        self.parallel_config
    }

    /// Provides access to the latest telemetry captured during inference.
    pub fn telemetry(&self) -> &InferenceTelemetry {
        self.engine.telemetry()
    }

    /// 型推論と整合性検証を実行する。
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<CheckError>> {
        self.engine.set_parallel_config(self.parallel_config);
        self.null_safety_hints.clear();
        match self.engine.infer_program(program) {
            Ok(()) => {
                let validation_errors = self.pattern_service.validate_program(program);
                let metrics = self.pattern_service.take_cache_metrics();
                self.record_pattern_cache_metrics(metrics);
                if !validation_errors.is_empty() {
                    self.snapshot = None;
                    self.merged_facts = None;
                    return Err(validation_errors);
                }
                self.snapshot = Some(InferenceSnapshot::from_engine(&self.engine));
                self.merged_facts = self
                    .snapshot
                    .as_ref()
                    .map(|snapshot| snapshot.type_facts().clone());
                Ok(())
            }
            Err(error) => {
                self.snapshot = None;
                self.merged_facts = None;
                Err(vec![CheckError::TypeError(error.to_string())])
            }
        }
    }

    /// null 安全診断を実行し、推論スナップショットがあればそれを連携する。
    pub fn check_null_safety(
        &mut self,
        program: &Program,
        snapshot: Option<&InferenceSnapshot>,
    ) -> Vec<CheckError> {
        let snapshot = snapshot.or_else(|| self.inference_snapshot());
        let mut report = NullSafetyCoordinator::new(snapshot).run(program);
        self.null_safety_hints = report.take_java_hints();
        if let Some(facts) = report.take_type_facts() {
            self.merged_facts = Some(facts);
        }
        report.take_diagnostics()
    }

    /// 現在保持している推論スナップショットを取得する。
    pub fn inference_snapshot(&self) -> Option<&InferenceSnapshot> {
        self.snapshot.as_ref()
    }

    /// Returns Java lowering hints from the most recent null safety analysis.
    pub fn null_safety_hints(&self) -> &[JavaLoweringHint] {
        &self.null_safety_hints
    }

    /// 推論サービスとしてアクセスする。
    pub fn inference_service(&self) -> Option<&dyn TypeInferenceService> {
        self.snapshot
            .as_ref()
            .map(|snapshot| snapshot as &dyn TypeInferenceService)
    }

    /// 新しい TypeFacts スナップショットへアクセスする。
    pub fn type_facts(&self) -> Option<&TypeFactsSnapshot> {
        self.merged_facts
            .as_ref()
            .or_else(|| self.snapshot.as_ref().map(|snapshot| snapshot.type_facts()))
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

    fn record_pattern_cache_metrics(&mut self, metrics: PatternCacheMetrics) {
        let telemetry = self.engine.telemetry_mut();
        telemetry.pattern_cache_hits = metrics.hits;
        telemetry.pattern_cache_misses = metrics.misses;
    }
}

fn build_type_facts(
    environment: &TypeEnvironment,
    bindings: &[TypeBinding],
    function_schemes: &HashMap<String, TypeScheme>,
    result_type: Option<&TypeKind>,
) -> TypeFactsSnapshot {
    let mut builder = TypeFactsBuilder::new();

    let env_map = environment
        .flattened_bindings()
        .into_iter()
        .map(|(name, scheme)| (name, convert_type_kind(&scheme.ty)))
        .collect::<HashMap<_, _>>();
    builder.set_environment(env_map);

    for binding in bindings {
        builder.add_binding(convert_binding(binding));
    }

    for (name, scheme) in function_schemes {
        builder.add_scheme(name.clone(), convert_scheme(scheme));
    }

    if let Some(root) = result_type {
        builder.set_root_type(convert_type_kind(root));
    }

    builder.build()
}

fn convert_binding(binding: &TypeBinding) -> FactsTypeBinding {
    FactsTypeBinding {
        id: FactsTypeId::new(binding.variable.id.to_raw()),
        ty: convert_type_kind(&binding.ty),
    }
}

fn convert_scheme(scheme: &TypeScheme) -> FactsTypeScheme {
    let generics = scheme
        .quantifiers
        .iter()
        .map(|id| FactsTypeId::new(id.to_raw()))
        .collect::<Vec<_>>();
    FactsTypeScheme::new(generics, convert_type_kind(&scheme.ty))
}

fn convert_type_kind(ty: &TypeKind) -> FactsTypeKind {
    match ty {
        TypeKind::Primitive(name) => FactsTypeKind::new(FactsTypeVariant::Primitive(name)),
        TypeKind::Optional(inner) => FactsTypeKind::optional(convert_type_kind(inner)),
        TypeKind::Variable(id) => {
            FactsTypeKind::new(FactsTypeVariant::Variable(FactsTypeId::new(id.to_raw())))
        }
        TypeKind::Function(params, ret) => {
            let converted_params = params.iter().map(convert_type_kind).collect::<Vec<_>>();
            let converted_ret = convert_type_kind(ret);
            FactsTypeKind::function(converted_params, converted_ret)
        }
        TypeKind::Unknown => FactsTypeKind::default(),
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
