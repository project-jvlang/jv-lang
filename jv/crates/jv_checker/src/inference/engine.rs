//! 型推論エンジンの公開エントリーポイント。
//!
//! 制約ジェネレータと単一化ソルバを束ね、関数宣言のシグネチャを一般化した
//! `TypeScheme` として公開する。タスク5では特に関数パラメータ・デフォルト値・
//! return 経路を制約に反映し、解決結果から曖昧なシグネチャを検出する。

use crate::InferenceTelemetry;
use crate::imports::ResolvedImport;
use crate::inference::constraint::ConstraintGenerator;
use crate::inference::context_adaptation::{self, ContextAdaptation};
use crate::inference::conversions::ConversionHelperCatalog;
use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::imports::ImportRegistry;
use crate::inference::prelude;
use crate::inference::types::{TypeBinding, TypeId, TypeKind};
use crate::inference::unify::{ConstraintSolver, SolveError, SolveResult};
use jv_ast::{Program, Statement};
use jv_build::metadata::{ConversionCatalogCache, SymbolIndex};
use jv_inference::ParallelInferenceConfig;
use jv_pm::JavaTarget;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Instant;

/// 型推論処理で発生し得るエラーを表す。
#[derive(thiserror::Error, Debug)]
pub enum InferenceError {
    #[error("failed to solve constraints: {0}")]
    SolveFailure(#[from] SolveError),
    #[error(
        "ambiguous function signature for `{name}`; add annotations or defaults to disambiguate"
    )]
    AmbiguousFunction { name: String },
}

/// 型推論関数の共通戻り値エイリアス。
pub type InferenceResult<T> = Result<T, InferenceError>;

/// 推論ワークフローの状態を保持し、エントリーポイントを提供する。
#[derive(Debug)]
pub struct InferenceEngine {
    environment: TypeEnvironment,
    bindings: Vec<TypeBinding>,
    result_type: Option<TypeKind>,
    function_schemes: HashMap<String, TypeScheme>,
    parallel_config: ParallelInferenceConfig,
    telemetry: InferenceTelemetry,
    import_index: Option<Arc<SymbolIndex>>,
    resolved_imports: Vec<ResolvedImport>,
    java_target: JavaTarget,
    conversion_cache: ConversionCatalogCache,
    active_conversion_catalog: Option<Arc<ConversionHelperCatalog>>,
    context_adaptations: Vec<ContextAdaptation>,
}

impl Default for InferenceEngine {
    fn default() -> Self {
        Self {
            environment: TypeEnvironment::new(),
            bindings: Vec::new(),
            result_type: None,
            function_schemes: HashMap::new(),
            parallel_config: ParallelInferenceConfig::default(),
            telemetry: InferenceTelemetry::default(),
            import_index: None,
            resolved_imports: Vec::new(),
            java_target: JavaTarget::default(),
            conversion_cache: ConversionCatalogCache::new(),
            active_conversion_catalog: None,
            context_adaptations: Vec::new(),
        }
    }
}

impl InferenceEngine {
    /// 空の推論コンテキストを生成する。
    pub fn new() -> Self {
        Self::default()
    }

    /// Updates the parallel inference configuration. The value is sanitized to
    /// ensure deterministic fallbacks when parallel execution is disabled.
    pub fn set_parallel_config(&mut self, config: ParallelInferenceConfig) {
        self.parallel_config = config.sanitized();
    }

    /// Returns the currently configured parallel inference settings.
    pub fn parallel_config(&self) -> ParallelInferenceConfig {
        self.parallel_config
    }

    /// Returns telemetry for the most recent inference invocation.
    pub fn telemetry(&self) -> &InferenceTelemetry {
        &self.telemetry
    }

    /// Configures the Java target for catalog lookups.
    pub fn set_java_target(&mut self, target: JavaTarget) {
        self.java_target = target;
    }

    /// Provides mutable access to telemetry for downstream stages.
    pub fn telemetry_mut(&mut self) -> &mut InferenceTelemetry {
        &mut self.telemetry
    }

    /// Supplies the resolved import list and associated symbol index for the next inference run.
    pub fn set_imports(&mut self, symbol_index: Arc<SymbolIndex>, imports: Vec<ResolvedImport>) {
        self.import_index = Some(symbol_index);
        self.resolved_imports = imports;
        self.active_conversion_catalog = None;
    }

    /// Clears previously registered import context.
    pub fn clear_imports(&mut self) {
        self.import_index = None;
        self.resolved_imports.clear();
        self.active_conversion_catalog = None;
    }

    /// AST 全体に対する推論を実行し、各種結果を内部状態へ保持する。
    pub fn infer_program(&mut self, program: &Program) -> InferenceResult<()> {
        let mut environment = TypeEnvironment::new();
        let extensions = prelude::install_prelude(&mut environment);
        let mut import_registry = self.import_index.clone().map(|index| {
            let mut registry = ImportRegistry::new(index);
            registry.register_imports(&mut environment, &self.resolved_imports);
            registry
        });
        let mut catalog_hits = 0u64;
        let mut catalog_misses = 0u64;
        let conversion_catalog = self.import_index.as_ref().map(|index| {
            let access = self
                .conversion_cache
                .access(self.java_target, index.as_ref());
            if access.hit {
                catalog_hits += 1;
            } else {
                catalog_misses += 1;
            }
            Arc::new(ConversionHelperCatalog::new(access.catalog))
        });
        environment.set_conversion_catalog(conversion_catalog.clone());
        self.active_conversion_catalog = conversion_catalog.clone();
        let generator =
            ConstraintGenerator::new(&mut environment, &extensions, import_registry.as_mut());
        let constraints = generator.generate(program);
        let constraint_count = constraints.len();
        let inference_start = Instant::now();

        // 制約を解決し、型変数への束縛を得る。
        let mut solver = ConstraintSolver::with_config(self.parallel_config);
        solver.set_conversion_catalog(self.active_conversion_catalog.clone());
        let solve_result = match solver.solve(constraints) {
            Ok(result) => result,
            Err(error) => {
                self.telemetry = InferenceTelemetry::default();
                return Err(InferenceError::from(error));
            }
        };
        let substitutions = build_substitution_map(&solve_result.bindings);

        // 関数シグネチャを精算し、曖昧さを検出する。
        let mut function_schemes = HashMap::new();
        let mut ambiguous_functions = Vec::new();
        for name in collect_function_names(program) {
            if let Some(existing) = environment.lookup(&name).cloned() {
                let resolved = resolve_type(&existing.ty, &substitutions);
                if resolved.contains_unknown() {
                    ambiguous_functions.push(name);
                    continue;
                }
                let quantifiers = resolved.free_type_vars();
                let final_scheme = TypeScheme::new(quantifiers, resolved.clone());
                environment.define_scheme(name.clone(), final_scheme.clone());
                function_schemes.insert(name, final_scheme);
            }
        }

        let resolved_symbols: Vec<(String, TypeScheme)> = environment
            .flattened_bindings()
            .into_iter()
            .filter_map(|(name, scheme)| {
                let resolved = resolve_type(&scheme.ty, &substitutions);
                if resolved.contains_unknown() {
                    return None;
                }
                let quantifiers = resolved.free_type_vars();
                let final_scheme = TypeScheme::new(quantifiers, resolved);
                Some((name, final_scheme))
            })
            .collect();

        for (name, scheme) in resolved_symbols {
            environment.redefine_scheme(&name, scheme);
        }

        let SolveResult {
            bindings,
            remaining: _,
            conversions,
        } = solve_result;

        self.bindings = bindings;
        self.environment = environment;
        self.function_schemes = function_schemes;
        self.result_type = None;
        let duration_ms = inference_start.elapsed().as_secs_f64() * 1_000.0;
        let mut telemetry = InferenceTelemetry {
            constraints_emitted: constraint_count,
            bindings_resolved: self.bindings.len(),
            inference_duration_ms: duration_ms,
            ..InferenceTelemetry::default()
        };
        telemetry.conversion_catalog_hits = catalog_hits;
        telemetry.conversion_catalog_misses = catalog_misses;
        telemetry.record_conversions(&conversions);
        self.context_adaptations = context_adaptation::collect_context_adaptations(&conversions);
        self.telemetry = telemetry;
        if let Some(name) = ambiguous_functions.into_iter().next() {
            Err(InferenceError::AmbiguousFunction { name })
        } else {
            Ok(())
        }
    }

    /// 現在保持している型束縛一覧。
    pub fn bindings(&self) -> &[TypeBinding] {
        &self.bindings
    }

    /// 推論結果として得られたトップレベルの型を返す。
    pub fn result_type(&self) -> Option<&TypeKind> {
        self.result_type.as_ref()
    }

    /// 推論後の環境へアクセスする（テスト・後続フェーズ向け）。
    pub fn environment(&self) -> &TypeEnvironment {
        &self.environment
    }

    /// 推論済み関数シグネチャを取得する。
    pub fn function_scheme(&self, name: &str) -> Option<&TypeScheme> {
        self.function_schemes.get(name)
    }

    /// Returns planned context adaptations inferred during solving.
    pub fn context_adaptations(&self) -> &[ContextAdaptation] {
        &self.context_adaptations
    }

    /// 推論済み関数シグネチャの一覧を返す。
    pub fn function_schemes(&self) -> &HashMap<String, TypeScheme> {
        &self.function_schemes
    }
}

fn build_substitution_map(bindings: &[TypeBinding]) -> HashMap<TypeId, TypeKind> {
    bindings
        .iter()
        .map(|binding| (binding.variable.id, binding.ty.clone()))
        .collect()
}

fn resolve_type(ty: &TypeKind, substitutions: &HashMap<TypeId, TypeKind>) -> TypeKind {
    match ty {
        TypeKind::Primitive(primitive) => TypeKind::Primitive(*primitive),
        TypeKind::Boxed(primitive) => TypeKind::Boxed(*primitive),
        TypeKind::Reference(name) => TypeKind::reference(name.clone()),
        TypeKind::Optional(inner) => TypeKind::optional(resolve_type(inner, substitutions)),
        TypeKind::Variable(id) => {
            if let Some(resolved) = substitutions.get(id) {
                resolve_type(resolved, substitutions)
            } else {
                TypeKind::Variable(*id)
            }
        }
        TypeKind::Function(params, ret) => {
            let resolved_params = params
                .iter()
                .map(|param| resolve_type(param, substitutions))
                .collect();
            let resolved_return = resolve_type(ret, substitutions);
            TypeKind::function(resolved_params, resolved_return)
        }
        TypeKind::Unknown => TypeKind::Unknown,
    }
}

fn collect_function_names(program: &Program) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut names = Vec::new();
    for statement in &program.statements {
        if let Statement::FunctionDeclaration { name, .. } = statement {
            if seen.insert(name.clone()) {
                names.push(name.clone());
            }
        }
    }
    names
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inference::types::PrimitiveType;
    use jv_ast::{
        BinaryMetadata, BinaryOp, Expression, Literal, Modifiers, Parameter, ParameterModifiers,
        Span, Statement, TypeAnnotation,
    };

    fn dummy_span() -> Span {
        Span::dummy()
    }

    fn modifiers() -> Modifiers {
        Modifiers::default()
    }

    fn function_statement(
        name: &str,
        parameters: Vec<Parameter>,
        return_annotation: Option<TypeAnnotation>,
        body: Expression,
    ) -> Statement {
        Statement::FunctionDeclaration {
            name: name.into(),
            type_parameters: Vec::new(),
            generic_signature: None,
            where_clause: None,
            parameters,
            return_type: return_annotation,
            primitive_return: None,
            body: Box::new(body),
            modifiers: modifiers(),
            span: dummy_span(),
        }
    }

    #[test]
    fn infers_annotated_function_signature() {
        let parameters = vec![
            Parameter {
                name: "lhs".into(),
                type_annotation: Some(TypeAnnotation::Simple("Int".into())),
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            },
            Parameter {
                name: "rhs".into(),
                type_annotation: Some(TypeAnnotation::Simple("Int".into())),
                default_value: None,
                modifiers: ParameterModifiers::default(),
                span: dummy_span(),
            },
        ];

        let body = Expression::Binary {
            left: Box::new(Expression::Identifier("lhs".into(), dummy_span())),
            op: BinaryOp::Add,
            right: Box::new(Expression::Identifier("rhs".into(), dummy_span())),
            span: dummy_span(),
            metadata: BinaryMetadata::default(),
        };

        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![function_statement(
                "add",
                parameters,
                Some(TypeAnnotation::Simple("Int".into())),
                body,
            )],
            span: dummy_span(),
        };

        let mut engine = InferenceEngine::new();
        engine
            .infer_program(&program)
            .expect("inference must succeed");
        let scheme = engine
            .function_scheme("add")
            .expect("function scheme must exist");

        assert!(scheme.quantifiers.is_empty());
        match &scheme.ty {
            TypeKind::Function(params, ret) => {
                assert_eq!(params.len(), 2);
                assert_eq!(params[0], TypeKind::primitive(PrimitiveType::Int));
                assert_eq!(params[1], TypeKind::primitive(PrimitiveType::Int));
                assert_eq!(**ret, TypeKind::primitive(PrimitiveType::Int));
            }
            other => panic!("expected function type, found {other:?}"),
        }
    }

    #[test]
    fn generalizes_identity_function() {
        let parameters = vec![Parameter {
            name: "value".into(),
            type_annotation: None,
            default_value: None,
            modifiers: ParameterModifiers::default(),
            span: dummy_span(),
        }];
        let body = Expression::Identifier("value".into(), dummy_span());

        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![function_statement("identity", parameters, None, body)],
            span: dummy_span(),
        };

        let mut engine = InferenceEngine::new();
        engine
            .infer_program(&program)
            .expect("inference must succeed");
        let scheme = engine
            .function_scheme("identity")
            .expect("function scheme must exist");

        assert_eq!(scheme.quantifiers.len(), 1);
        match &scheme.ty {
            TypeKind::Function(params, ret) => {
                assert_eq!(params.len(), 1);
                match (&params[0], ret.as_ref()) {
                    (TypeKind::Variable(a), TypeKind::Variable(b)) => assert_eq!(a, b),
                    other => panic!("expected identical type variables, found {other:?}"),
                }
            }
            other => panic!("expected function type, found {other:?}"),
        }
    }

    #[test]
    fn uses_default_value_to_specialize_parameter() {
        let parameters = vec![Parameter {
            name: "message".into(),
            type_annotation: None,
            default_value: Some(Expression::Literal(
                Literal::String("hello".into()),
                dummy_span(),
            )),
            modifiers: ParameterModifiers::default(),
            span: dummy_span(),
        }];
        let body = Expression::Identifier("message".into(), dummy_span());

        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![function_statement("greet", parameters, None, body)],
            span: dummy_span(),
        };

        let mut engine = InferenceEngine::new();
        engine
            .infer_program(&program)
            .expect("inference must succeed");
        let scheme = engine
            .function_scheme("greet")
            .expect("function scheme must exist");

        assert!(scheme.quantifiers.is_empty());
        match &scheme.ty {
            TypeKind::Function(params, ret) => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], TypeKind::reference("java.lang.String"));
                assert_eq!(**ret, TypeKind::reference("java.lang.String"));
            }
            other => panic!("expected function type, found {other:?}"),
        }
    }

    #[test]
    fn reports_ambiguous_function_signature() {
        let parameters = vec![Parameter {
            name: "x".into(),
            type_annotation: None,
            default_value: None,
            modifiers: ParameterModifiers::default(),
            span: dummy_span(),
        }];
        // return 文を持たない関数: 型が Unknown のまま残る
        let body = Expression::Literal(Literal::Null, dummy_span());

        let program = Program {
            package: None,
            imports: Vec::new(),
            statements: vec![function_statement("ambiguous", parameters, None, body)],
            span: dummy_span(),
        };

        let mut engine = InferenceEngine::new();
        let err = engine
            .infer_program(&program)
            .expect_err("ambiguity must be reported");

        match err {
            InferenceError::AmbiguousFunction { name } => assert_eq!(name, "ambiguous"),
            other => panic!("unexpected error: {other:?}"),
        }
    }
}
