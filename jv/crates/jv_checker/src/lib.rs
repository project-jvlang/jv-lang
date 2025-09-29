// jv_checker - Static analysis and validation
pub mod compat;
pub mod diagnostics;
pub mod inference;
pub mod null_safety;

pub use inference::{
    InferenceEngine, InferenceError, InferenceResult, NullabilityAnalyzer, TypeBinding,
    TypeEnvironment, TypeId, TypeKind, TypeScheme,
};
pub use jv_inference::ParallelInferenceConfig;

use jv_ast::{
    Argument, ConcurrencyConstruct, Expression, ForInStatement, LoopStrategy, NumericRangeLoop,
    Pattern, Program, Property, ResourceManagement, Span, Statement, StringPart, TypeAnnotation,
    WhenArm,
};
use null_safety::NullSafetyCoordinator;
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
        }
    }
}

/// TypeChecker orchestrates the inference engine and exposes analysis results.
#[derive(Debug)]
pub struct TypeChecker {
    engine: InferenceEngine,
    snapshot: Option<InferenceSnapshot>,
    parallel_config: ParallelInferenceConfig,
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
        match self.engine.infer_program(program) {
            Ok(()) => {
                let validation_errors = WhenUsageValidator::validate(program);
                if !validation_errors.is_empty() {
                    self.snapshot = None;
                    return Err(validation_errors);
                }
                self.snapshot = Some(InferenceSnapshot::from_engine(&self.engine));
                Ok(())
            }
            Err(error) => {
                self.snapshot = None;
                Err(vec![CheckError::TypeError(error.to_string())])
            }
        }
    }

    /// null 安全診断を実行し、推論スナップショットがあればそれを連携する。
    pub fn check_null_safety(
        &self,
        program: &Program,
        snapshot: Option<&InferenceSnapshot>,
    ) -> Vec<CheckError> {
        let snapshot = snapshot.or_else(|| self.inference_snapshot());
        NullSafetyCoordinator::new(snapshot)
            .run(program)
            .into_diagnostics()
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

    /// 新しい TypeFacts スナップショットへアクセスする。
    pub fn type_facts(&self) -> Option<&TypeFactsSnapshot> {
        self.snapshot.as_ref().map(|snapshot| snapshot.type_facts())
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

struct WhenUsageValidator {
    errors: Vec<CheckError>,
}

impl WhenUsageValidator {
    fn validate(program: &Program) -> Vec<CheckError> {
        let mut validator = Self { errors: Vec::new() };
        validator.visit_program(program);
        validator.errors
    }

    fn visit_program(&mut self, program: &Program) {
        for statement in &program.statements {
            self.visit_statement(statement, false);
        }
    }

    fn visit_statement(&mut self, statement: &Statement, expects_value: bool) {
        match statement {
            Statement::ValDeclaration { initializer, .. } => {
                self.visit_expression(initializer, true);
            }
            Statement::VarDeclaration { initializer, .. } => {
                if let Some(expr) = initializer {
                    self.visit_expression(expr, true);
                }
            }
            Statement::FunctionDeclaration {
                parameters,
                return_type,
                body,
                ..
            } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default, true);
                    }
                }
                let body_expects_value = return_type
                    .as_ref()
                    .map(|annotation| !type_annotation_is_unit(annotation))
                    .unwrap_or(false);
                self.visit_expression(body, body_expects_value);
            }
            Statement::ClassDeclaration {
                properties,
                methods,
                ..
            } => {
                for property in properties {
                    self.visit_property(property);
                }
                for method in methods {
                    self.visit_statement(method, false);
                }
            }
            Statement::InterfaceDeclaration {
                properties,
                methods,
                ..
            } => {
                for property in properties {
                    self.visit_property(property);
                }
                for method in methods {
                    self.visit_statement(method, false);
                }
            }
            Statement::ExtensionFunction(extension) => {
                self.visit_statement(&extension.function, false);
            }
            Statement::Expression { expr, .. } => {
                self.visit_expression(expr, expects_value);
            }
            Statement::Return { value, .. } => {
                if let Some(expr) = value {
                    self.visit_expression(expr, true);
                }
            }
            Statement::Assignment { target, value, .. } => {
                self.visit_expression(target, true);
                self.visit_expression(value, true);
            }
            Statement::ForIn(for_in) => self.visit_for_in(for_in),
            Statement::Concurrency(construct) => self.visit_concurrency_construct(construct),
            Statement::ResourceManagement(resource) => self.visit_resource_management(resource),
            Statement::DataClassDeclaration { parameters, .. } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default, true);
                    }
                }
            }
            Statement::Import { .. }
            | Statement::Package { .. }
            | Statement::Break(_)
            | Statement::Continue(_) => {}
        }
    }

    fn visit_expression(&mut self, expression: &Expression, expects_value: bool) {
        match expression {
            Expression::Literal(..)
            | Expression::Identifier(..)
            | Expression::This(..)
            | Expression::Super(..) => {}
            Expression::Binary { left, right, .. } => {
                self.visit_expression(left, true);
                self.visit_expression(right, true);
            }
            Expression::Unary { operand, .. } => {
                self.visit_expression(operand, true);
            }
            Expression::Call { function, args, .. } => {
                self.visit_expression(function, true);
                for arg in args {
                    match arg {
                        Argument::Positional(expr) => self.visit_expression(expr, true),
                        Argument::Named { value, .. } => self.visit_expression(value, true),
                    }
                }
            }
            Expression::MemberAccess { object, .. }
            | Expression::NullSafeMemberAccess { object, .. } => {
                self.visit_expression(object, true);
            }
            Expression::IndexAccess { object, index, .. }
            | Expression::NullSafeIndexAccess { object, index, .. } => {
                self.visit_expression(object, true);
                self.visit_expression(index, true);
            }
            Expression::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let StringPart::Expression(expr) = part {
                        self.visit_expression(expr, true);
                    }
                }
            }
            Expression::Array { elements, .. } => {
                for element in elements {
                    self.visit_expression(element, true);
                }
            }
            Expression::Lambda {
                parameters, body, ..
            } => {
                for parameter in parameters {
                    if let Some(default) = &parameter.default_value {
                        self.visit_expression(default, true);
                    }
                }
                self.visit_expression(body, true);
            }
            Expression::Try { expr, .. } => self.visit_expression(expr, expects_value),
            Expression::Block { statements, .. } => {
                for (index, statement) in statements.iter().enumerate() {
                    let is_last = index == statements.len().saturating_sub(1);
                    let stmt_expects_value = expects_value && is_last;
                    self.visit_statement(statement, stmt_expects_value);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                span,
                ..
            } => {
                self.visit_expression(condition, true);
                self.visit_expression(then_branch, expects_value);
                if let Some(else_expr) = else_branch {
                    self.visit_expression(else_expr, expects_value);
                } else if expects_value {
                    self.record_missing_else(span);
                }
            }
            Expression::When {
                expr,
                arms,
                else_arm,
                implicit_end,
                span,
                ..
            } => {
                if let Some(subject) = expr {
                    self.visit_expression(subject, true);
                }
                for arm in arms {
                    self.visit_when_arm(arm, expects_value);
                }
                if let Some(else_expr) = else_arm {
                    self.visit_expression(else_expr, expects_value);
                } else if expects_value && implicit_end.is_none() {
                    self.record_missing_else(span);
                }
            }
        }
    }

    fn visit_when_arm(&mut self, arm: &WhenArm, expects_value: bool) {
        self.visit_pattern(&arm.pattern);
        if let Some(guard) = &arm.guard {
            self.visit_expression(guard, true);
        }
        self.visit_expression(&arm.body, expects_value);
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Constructor { patterns, .. } => {
                for nested in patterns {
                    self.visit_pattern(nested);
                }
            }
            Pattern::Range { start, end, .. } => {
                self.visit_expression(start, true);
                self.visit_expression(end, true);
            }
            Pattern::Guard {
                pattern: nested,
                condition,
                ..
            } => {
                self.visit_pattern(nested);
                self.visit_expression(condition, true);
            }
            Pattern::Literal(..) | Pattern::Identifier(..) | Pattern::Wildcard(..) => {}
        }
    }

    fn visit_property(&mut self, property: &Property) {
        if let Some(initializer) = &property.initializer {
            self.visit_expression(initializer, true);
        }
        if let Some(getter) = &property.getter {
            self.visit_expression(getter, true);
        }
        if let Some(setter) = &property.setter {
            self.visit_expression(setter, false);
        }
    }

    fn visit_for_in(&mut self, for_in: &ForInStatement) {
        self.visit_expression(&for_in.iterable, true);
        self.visit_loop_strategy(&for_in.strategy);
        self.visit_expression(&for_in.body, false);
    }

    fn visit_loop_strategy(&mut self, strategy: &LoopStrategy) {
        if let LoopStrategy::NumericRange(NumericRangeLoop { start, end, .. }) = strategy {
            self.visit_expression(start, true);
            self.visit_expression(end, true);
        }
    }

    fn visit_concurrency_construct(&mut self, construct: &ConcurrencyConstruct) {
        match construct {
            ConcurrencyConstruct::Spawn { body, .. } | ConcurrencyConstruct::Async { body, .. } => {
                self.visit_expression(body, false);
            }
            ConcurrencyConstruct::Await { expr, .. } => {
                self.visit_expression(expr, true);
            }
        }
    }

    fn visit_resource_management(&mut self, resource: &ResourceManagement) {
        match resource {
            ResourceManagement::Use { resource, body, .. } => {
                self.visit_expression(resource, true);
                self.visit_expression(body, false);
            }
            ResourceManagement::Defer { body, .. } => {
                self.visit_expression(body, false);
            }
        }
    }

    fn record_missing_else(&mut self, span: &Span) {
        let message = "E_WHEN_002: when expression used as a value must declare an `else` branch. 値コンテキストでは `else` を追加し、docs/language-guide.md#when-expression / docs/language-guide-en.md#when-expression を参照してください.".to_string();
        self.errors.push(CheckError::ValidationError {
            message,
            span: Some(span.clone()),
        });
    }
}

fn type_annotation_is_unit(annotation: &TypeAnnotation) -> bool {
    match annotation {
        TypeAnnotation::Simple(name) => name == "Unit",
        TypeAnnotation::Nullable(inner) => type_annotation_is_unit(inner),
        _ => false,
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
