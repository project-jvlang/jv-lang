// jv_checker - Static analysis and validation
pub mod binding;
pub mod compat;
pub mod diagnostics;
pub mod imports;
pub mod inference;
pub mod java;
pub mod null_safety;
pub mod pattern;
pub mod regex;
pub mod symbol_index;
pub mod telemetry;

pub use inference::{
    CharToStringAdaptation, ContextAdaptation, InferenceEngine, InferenceError, InferenceResult,
    NullabilityAnalyzer, PrimitiveType, TypeBinding, TypeEnvironment, TypeId, TypeKind, TypeScheme,
};
pub use java::{JavaBoxingTable, JavaNullabilityPolicy, JavaPrimitive};
pub use jv_inference::ParallelInferenceConfig;
pub use regex::RegexAnalysis;

use crate::imports::ResolvedImport;
use binding::{BindingResolution, BindingUsageSummary, LateInitManifest, resolve_bindings};
use inference::conversions::{AppliedConversion, ConversionKind, HelperSpec, NullableGuard};
use inference::{RegexCommandTyping, RegexMatchTyping};
use jv_ast::{Program, RegexCommandMode, RegexGuardStrategy, Span};
use jv_build::metadata::SymbolIndex;
use null_safety::{JavaLoweringHint, NullSafetyCoordinator};
use pattern::{
    NarrowedNullability, PatternCacheMetrics, PatternMatchFacts, PatternMatchService, PatternTarget,
};
use regex::RegexValidator;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use thiserror::Error;

use jv_inference::service::{TypeFactsBuilder, TypeFactsSnapshot, TypeScheme as FactsTypeScheme};
use jv_inference::solver::TypeBinding as FactsTypeBinding;
use jv_inference::types::{
    NullabilityFlag, TypeId as FactsTypeId, TypeKind as FactsTypeKind,
    TypeVariant as FactsTypeVariant,
};
use jv_pm::JavaTarget;

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
    pattern_facts: HashMap<(u64, PatternTarget), PatternMatchFacts>,
    regex_typings: Vec<RegexMatchTyping>,
    regex_command_typings: Vec<RegexCommandTyping>,
    regex_analyses: Vec<RegexAnalysis>,
    late_init_manifest: LateInitManifest,
    context_adaptations: Vec<ContextAdaptation>,
}

impl InferenceSnapshot {
    fn from_engine(
        engine: &InferenceEngine,
        pattern_facts: HashMap<(u64, PatternTarget), PatternMatchFacts>,
        regex_analyses: Vec<RegexAnalysis>,
        late_init_manifest: LateInitManifest,
    ) -> Self {
        let environment = engine.environment().clone();
        let bindings = engine.bindings().to_vec();
        let function_schemes = engine.function_schemes().clone();
        let result_type = engine.result_type().cloned();
        let regex_typings = environment.regex_typings().to_vec();
        let regex_command_typings = environment.regex_command_typings().to_vec();

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
            pattern_facts,
            regex_typings,
            regex_command_typings,
            regex_analyses,
            late_init_manifest,
            context_adaptations: engine.context_adaptations().to_vec(),
        }
    }

    pub fn type_facts(&self) -> &TypeFactsSnapshot {
        &self.facts
    }

    pub fn pattern_facts(&self) -> &HashMap<(u64, PatternTarget), PatternMatchFacts> {
        &self.pattern_facts
    }

    pub fn pattern_fact(&self, node_id: u64, target: PatternTarget) -> Option<&PatternMatchFacts> {
        self.pattern_facts.get(&(node_id, target))
    }

    pub fn regex_analyses(&self) -> &[RegexAnalysis] {
        &self.regex_analyses
    }

    pub fn regex_typings(&self) -> &[RegexMatchTyping] {
        &self.regex_typings
    }

    pub fn regex_command_typings(&self) -> &[RegexCommandTyping] {
        &self.regex_command_typings
    }

    pub fn binding_scheme(&self, name: &str) -> Option<&TypeScheme> {
        self.environment.lookup(name)
    }

    pub fn late_init_manifest(&self) -> &LateInitManifest {
        &self.late_init_manifest
    }

    pub fn context_adaptations(&self) -> &[ContextAdaptation] {
        &self.context_adaptations
    }
}

/// Summary of nullability information for each branch within a `when` expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhenBranchNullability {
    arm_nullability: Vec<NullabilityFlag>,
    fallback_nullability: Option<NullabilityFlag>,
}

impl WhenBranchNullability {
    fn new(arm_count: usize, has_fallback: bool) -> Self {
        Self {
            arm_nullability: vec![NullabilityFlag::Unknown; arm_count],
            fallback_nullability: has_fallback.then_some(NullabilityFlag::Unknown),
        }
    }

    /// Returns the inferred nullability for each `when` arm in source order.
    pub fn arm_nullability(&self) -> &[NullabilityFlag] {
        &self.arm_nullability
    }

    /// Returns the inferred nullability for the fallback branch (`else`), when present.
    pub fn fallback_nullability(&self) -> Option<NullabilityFlag> {
        self.fallback_nullability
    }

    fn update_from_pattern_facts(&mut self, subject: &str, facts: &PatternMatchFacts) {
        let mut branch_flags: Vec<(usize, NullabilityFlag)> = facts
            .narrowing()
            .arms()
            .filter_map(|(arm_id, snapshot)| {
                snapshot
                    .on_match()
                    .iter()
                    .find(|binding| binding.variable == subject)
                    .map(|binding| (*arm_id as usize, to_nullability_flag(binding.nullability)))
            })
            .collect();
        branch_flags.sort_by_key(|(index, _)| *index);
        for (index, flag) in branch_flags {
            if index >= self.arm_nullability.len() {
                self.arm_nullability
                    .resize(index + 1, NullabilityFlag::Unknown);
            }
            if let Some(slot) = self.arm_nullability.get_mut(index) {
                *slot = flag;
            }
        }

        if let Some(snapshot) = facts.fallback_narrowing() {
            if let Some(binding) = snapshot
                .on_match()
                .iter()
                .find(|binding| binding.variable == subject)
            {
                if let Some(fallback) = self.fallback_nullability.as_mut() {
                    *fallback = to_nullability_flag(binding.nullability);
                }
            }
        }
    }
}

fn to_nullability_flag(value: NarrowedNullability) -> NullabilityFlag {
    match value {
        NarrowedNullability::NonNull => NullabilityFlag::NonNull,
        NarrowedNullability::Nullable => NullabilityFlag::Nullable,
        NarrowedNullability::Unknown => NullabilityFlag::Unknown,
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

    /// Late-init 用メタデータを返す。
    fn late_init_manifest(&self) -> &LateInitManifest;

    /// Retrieves branch-level nullability flags for a `when` expression.
    ///
    /// The caller must provide the node identifier used by the pattern service,
    /// together with context information about the subject identifier and the
    /// number of arms. Missing facts default to `Unknown`.
    fn when_branch_nullability(
        &self,
        node_id: u64,
        subject_name: Option<&str>,
        arm_count: usize,
        has_fallback: bool,
        target: PatternTarget,
    ) -> WhenBranchNullability;
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

    fn late_init_manifest(&self) -> &LateInitManifest {
        &self.late_init_manifest
    }

    fn when_branch_nullability(
        &self,
        node_id: u64,
        subject_name: Option<&str>,
        arm_count: usize,
        has_fallback: bool,
        target: PatternTarget,
    ) -> WhenBranchNullability {
        let mut summary = WhenBranchNullability::new(arm_count, has_fallback);
        if let (Some(subject), Some(facts)) = (subject_name, self.pattern_fact(node_id, target)) {
            summary.update_from_pattern_facts(subject, facts);
        }
        summary
    }
}

/// RegexCommand 専用テレメトリ。
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RegexCommandTelemetry {
    /// 記録された RegexCommand の合計数。
    pub total: usize,
    /// モード別の出現回数。
    pub modes: BTreeMap<String, usize>,
    /// 戻り値型別の出現回数。
    pub return_types: BTreeMap<String, usize>,
    /// ガード戦略別の出現回数。
    pub guard_strategies: BTreeMap<String, usize>,
    /// `Stream` を具現化する必要があるケースの数。
    pub materialize_streams: usize,
    /// 発生した診断コード別の回数。
    pub diagnostics: BTreeMap<String, usize>,
}

/// Telemetry captured during the most recent inference run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceTelemetry {
    pub constraints_emitted: usize,
    pub bindings_resolved: usize,
    pub inference_duration_ms: f64,
    pub preserved_constraints: usize,
    pub cache_hit_rate: Option<f64>,
    pub invalidation_cascade_depth: usize,
    pub pattern_cache_hits: u64,
    pub pattern_cache_misses: u64,
    pub pattern_bridge_ms: f64,
    pub generic_constraints_emitted: usize,
    pub bound_checks: usize,
    pub variance_conflicts: usize,
    pub sealed_hierarchy_checks: usize,
    pub generic_solver_ms: f64,
    pub variance_analysis_ms: f64,
    pub kind_checks_count: u64,
    pub kind_cache_hit_rate: Option<f64>,
    pub const_evaluations: u64,
    pub type_level_cache_size: usize,
    pub widening_conversions: usize,
    pub boxing_conversions: usize,
    pub unboxing_conversions: usize,
    pub string_conversions: usize,
    pub nullable_guards_generated: usize,
    pub method_invocation_conversions: usize,
    pub conversion_catalog_hits: u64,
    pub conversion_catalog_misses: u64,
    pub conversion_events: Vec<AppliedConversion>,
    pub nullable_guards: Vec<NullableGuard>,
    pub catalog_hits: Vec<HelperSpec>,
    pub regex_command: RegexCommandTelemetry,
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
            pattern_bridge_ms: 0.0,
            generic_constraints_emitted: 0,
            bound_checks: 0,
            variance_conflicts: 0,
            sealed_hierarchy_checks: 0,
            generic_solver_ms: 0.0,
            variance_analysis_ms: 0.0,
            kind_checks_count: 0,
            kind_cache_hit_rate: None,
            const_evaluations: 0,
            type_level_cache_size: 0,
            widening_conversions: 0,
            boxing_conversions: 0,
            unboxing_conversions: 0,
            string_conversions: 0,
            nullable_guards_generated: 0,
            method_invocation_conversions: 0,
            conversion_catalog_hits: 0,
            conversion_catalog_misses: 0,
            conversion_events: Vec::new(),
            nullable_guards: Vec::new(),
            catalog_hits: Vec::new(),
            regex_command: RegexCommandTelemetry::default(),
        }
    }
}

impl InferenceTelemetry {
    /// 変換適用結果を集計し、Telemetry カウンタへ反映する。
    pub fn record_conversions(&mut self, conversions: &[AppliedConversion]) {
        for conversion in conversions {
            match conversion.kind {
                ConversionKind::WideningPrimitive => self.widening_conversions += 1,
                ConversionKind::Boxing => self.boxing_conversions += 1,
                ConversionKind::Unboxing => self.unboxing_conversions += 1,
                ConversionKind::StringConversion => self.string_conversions += 1,
                ConversionKind::MethodInvocation => self.method_invocation_conversions += 1,
                ConversionKind::Identity => {}
            }

            if let Some(guard) = conversion.nullable_guard {
                self.nullable_guards_generated += 1;
                self.nullable_guards.push(guard);
            }

            if let Some(helper) = conversion.helper_method.as_ref() {
                self.catalog_hits.push(helper.clone());
            }

            self.conversion_events.push(conversion.clone());
        }
    }

    /// Enriches conversion events with span information derived from source map data.
    pub fn attach_conversion_spans(&mut self, mappings: &[jv_mapper::ConversionMapping]) {
        crate::telemetry::report::attach_conversion_spans(&mut self.conversion_events, mappings);
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
    regex_validator: RegexValidator,
    normalized_program: Option<Program>,
    binding_usage: BindingUsageSummary,
    late_init_manifest: LateInitManifest,
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
            regex_validator: RegexValidator::new(),
            normalized_program: None,
            binding_usage: BindingUsageSummary::default(),
            late_init_manifest: LateInitManifest::default(),
        }
    }

    /// Updates the parallel inference configuration and applies it to the engine.
    pub fn set_parallel_config(&mut self, config: ParallelInferenceConfig) {
        let config = config.sanitized();
        self.parallel_config = config;
        self.engine.set_parallel_config(config);
    }

    pub fn set_java_target(&mut self, target: JavaTarget) {
        self.engine.set_java_target(target);
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

    pub fn normalized_program(&self) -> Option<&Program> {
        self.normalized_program.as_ref()
    }

    pub fn take_normalized_program(&mut self) -> Option<Program> {
        self.normalized_program.take()
    }

    pub fn binding_usage(&self) -> &BindingUsageSummary {
        &self.binding_usage
    }

    pub fn late_init_manifest(&self) -> &LateInitManifest {
        &self.late_init_manifest
    }

    pub fn set_imports(&mut self, symbol_index: Arc<SymbolIndex>, imports: Vec<ResolvedImport>) {
        self.engine.set_imports(symbol_index, imports);
    }

    pub fn clear_imports(&mut self) {
        self.engine.clear_imports();
    }

    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<CheckError>> {
        self.engine.set_parallel_config(self.parallel_config);
        self.null_safety_hints.clear();
        self.clear_regex_command_telemetry();

        let binding_resolution = resolve_bindings(program);
        let BindingResolution {
            program: normalized,
            diagnostics,
            usage,
            late_init_manifest,
        } = binding_resolution;

        self.binding_usage = usage;
        self.normalized_program = Some(normalized);
        self.late_init_manifest = late_init_manifest;

        if !diagnostics.is_empty() {
            self.snapshot = None;
            self.merged_facts = None;
            return Err(diagnostics);
        }

        let inference_result = {
            let normalized_program = self
                .normalized_program
                .as_ref()
                .expect("normalized program should be available");
            self.engine.infer_program(normalized_program)
        };

        match inference_result {
            Ok(()) => {
                let (regex_errors, regex_analyses) = {
                    let normalized_program = self
                        .normalized_program
                        .as_ref()
                        .expect("normalized program should be available");
                    let errors = self.regex_validator.validate_program(normalized_program);
                    let analyses = self.regex_validator.take_analyses();
                    (errors, analyses)
                };
                if !regex_errors.is_empty() {
                    self.snapshot = None;
                    self.merged_facts = None;
                    return Err(regex_errors);
                }

                let validation_errors = {
                    let normalized_program = self
                        .normalized_program
                        .as_ref()
                        .expect("normalized program should be available");
                    self.pattern_service.validate_program(normalized_program)
                };
                let pattern_facts = self.pattern_service.take_recorded_facts();
                let metrics = self.pattern_service.take_cache_metrics();
                self.record_pattern_cache_metrics(metrics);
                if !validation_errors.is_empty() {
                    self.snapshot = None;
                    self.merged_facts = None;
                    self.update_type_facts_telemetry();
                    return Err(validation_errors);
                }
                self.snapshot = Some(InferenceSnapshot::from_engine(
                    &self.engine,
                    pattern_facts,
                    regex_analyses,
                    self.late_init_manifest.clone(),
                ));
                self.merged_facts = self
                    .snapshot
                    .as_ref()
                    .map(|snapshot| snapshot.type_facts().clone());
                self.update_type_facts_telemetry();

                let placement_errors = {
                    let normalized_program = self
                        .normalized_program
                        .as_ref()
                        .expect("normalized program should be available");
                    compat::annotation_targets::validate_program(normalized_program)
                };
                if !placement_errors.is_empty() {
                    self.snapshot = None;
                    self.merged_facts = None;
                    self.update_type_facts_telemetry();
                    return Err(placement_errors);
                }
                self.refresh_regex_command_telemetry();
                Ok(())
            }
            Err(error) => {
                let bindings = self.engine.bindings().to_vec();
                let function_schemes = self.engine.function_schemes().clone();
                let result_type = self.engine.result_type().cloned();
                let facts = build_type_facts(
                    self.engine.environment(),
                    bindings.as_slice(),
                    &function_schemes,
                    result_type.as_ref(),
                );
                self.snapshot = None;
                self.merged_facts = Some(facts);
                self.update_type_facts_telemetry();
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
        let snapshot_owned = match snapshot {
            Some(existing) => Some(existing.clone()),
            None => self.inference_snapshot().cloned(),
        };
        let mut coordinator =
            NullSafetyCoordinator::new(snapshot_owned, Some(&mut self.pattern_service));
        let mut report = coordinator.run(program);
        self.null_safety_hints = report.take_java_hints();
        if let Some(facts) = report.take_type_facts() {
            self.merged_facts = Some(facts);
        }
        self.update_type_facts_telemetry();
        if let Some(duration) = report.pattern_bridge_duration_ms() {
            self.engine.telemetry_mut().pattern_bridge_ms = duration;
        } else {
            self.engine.telemetry_mut().pattern_bridge_ms = 0.0;
        }
        let mut diagnostics = report.take_diagnostics();
        diagnostics.extend(report.take_warnings());
        diagnostics
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

    /// Regex analysis results gathered during the last checker run.
    pub fn regex_analyses(&self) -> Option<&[RegexAnalysis]> {
        self.snapshot
            .as_ref()
            .map(|snapshot| snapshot.regex_analyses())
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

    fn refresh_regex_command_telemetry(&mut self) {
        let typings: Vec<RegexCommandTyping> = if let Some(snapshot) = self.snapshot.as_ref() {
            snapshot.regex_command_typings().to_vec()
        } else {
            self.engine.environment().regex_command_typings().to_vec()
        };

        let mut metrics = RegexCommandTelemetry::default();

        for typing in &typings {
            metrics.total += 1;

            let mode_key = match typing.mode {
                RegexCommandMode::All => "all",
                RegexCommandMode::First => "first",
                RegexCommandMode::Match => "match",
                RegexCommandMode::Split => "split",
                RegexCommandMode::Iterate => "iterate",
            }
            .to_string();
            *metrics.modes.entry(mode_key).or_insert(0) += 1;

            let return_key = typing.return_type.describe();
            *metrics.return_types.entry(return_key).or_insert(0) += 1;

            let guard_key = match typing.guard_strategy {
                RegexGuardStrategy::None => "none",
                RegexGuardStrategy::CaptureAndGuard { .. } => "capture_and_guard",
            }
            .to_string();
            *metrics.guard_strategies.entry(guard_key).or_insert(0) += 1;

            if typing.requires_stream_materialization {
                metrics.materialize_streams += 1;
            }

            for diagnostic in &typing.diagnostics {
                *metrics
                    .diagnostics
                    .entry(diagnostic.code.clone())
                    .or_insert(0) += 1;
            }
        }

        self.engine.telemetry_mut().regex_command = metrics;
    }

    fn clear_regex_command_telemetry(&mut self) {
        self.engine.telemetry_mut().regex_command = RegexCommandTelemetry::default();
    }

    fn update_type_facts_telemetry(&mut self) {
        let facts_snapshot = {
            if let Some(facts) = self.merged_facts.as_ref() {
                Some(facts.clone())
            } else {
                self.snapshot
                    .as_ref()
                    .map(|snapshot| snapshot.type_facts().clone())
            }
        };

        let telemetry = self.engine.telemetry_mut();
        if let Some(facts) = facts_snapshot {
            let counters = facts.telemetry();
            telemetry.kind_checks_count = counters.kind_checks_count;
            telemetry.kind_cache_hit_rate = counters.kind_cache_hit_rate;
            telemetry.const_evaluations = counters.const_evaluations;
            telemetry.type_level_cache_size = counters.type_level_cache_size;
        } else {
            telemetry.kind_checks_count = 0;
            telemetry.kind_cache_hit_rate = None;
            telemetry.const_evaluations = 0;
            telemetry.type_level_cache_size = 0;
        }
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
        TypeKind::Primitive(primitive) => {
            FactsTypeKind::new(FactsTypeVariant::Primitive(primitive.jv_name()))
                .with_nullability(NullabilityFlag::NonNull)
        }
        TypeKind::Boxed(primitive) => {
            FactsTypeKind::new(FactsTypeVariant::Primitive(primitive.boxed_fqcn()))
                .with_nullability(NullabilityFlag::NonNull)
        }
        TypeKind::Reference(name) => {
            let interned: &'static str = Box::leak(name.clone().into_boxed_str());
            FactsTypeKind::new(FactsTypeVariant::Primitive(interned))
                .with_nullability(NullabilityFlag::NonNull)
        }
        TypeKind::Optional(inner) => FactsTypeKind::optional(convert_type_kind(inner)),
        TypeKind::Variable(id) => {
            FactsTypeKind::new(FactsTypeVariant::Variable(FactsTypeId::new(id.to_raw())))
        }
        TypeKind::Function(params, ret) => {
            let converted_params = params.iter().map(convert_type_kind).collect::<Vec<_>>();
            let converted_ret = convert_type_kind(ret);
            FactsTypeKind::function(converted_params, converted_ret)
                .with_nullability(NullabilityFlag::NonNull)
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
