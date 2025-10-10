//! 型推論エンジンの公開エントリーポイント。
//!
//! 制約ジェネレータと単一化ソルバを束ね、関数宣言のシグネチャを一般化した
//! `TypeScheme` として公開する。タスク5では特に関数パラメータ・デフォルト値・
//! return 経路を制約に反映し、解決結果から曖昧なシグネチャを検出する。

use crate::inference::constraint::ConstraintGenerator;
use crate::inference::environment::{TypeEnvironment, TypeScheme};
use crate::inference::prelude;
use crate::inference::types::{TypeBinding, TypeId, TypeKind};
use crate::inference::unify::{ConstraintSolver, SolveError};
use crate::InferenceTelemetry;
use jv_ast::{Program, Statement};
use jv_inference::ParallelInferenceConfig;
use std::collections::{HashMap, HashSet};
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

    /// Provides mutable access to telemetry for downstream stages.
    pub fn telemetry_mut(&mut self) -> &mut InferenceTelemetry {
        &mut self.telemetry
    }

    /// AST 全体に対する推論を実行し、各種結果を内部状態へ保持する。
    pub fn infer_program(&mut self, program: &Program) -> InferenceResult<()> {
        let mut environment = TypeEnvironment::new();
        let extensions = prelude::install_prelude(&mut environment);
        let generator = ConstraintGenerator::new(&mut environment, &extensions);
        let constraints = generator.generate(program);
        let constraint_count = constraints.len();
        let inference_start = Instant::now();

        // 制約を解決し、型変数への束縛を得る。
        let solve_result =
            match ConstraintSolver::with_config(self.parallel_config).solve(constraints) {
                Ok(result) => result,
                Err(error) => {
                    self.telemetry = InferenceTelemetry::default();
                    return Err(InferenceError::from(error));
                }
            };
        let substitutions = build_substitution_map(&solve_result.bindings);

        // 関数シグネチャを精算し、曖昧さを検出する。
        let mut function_schemes = HashMap::new();
        for name in collect_function_names(program) {
            if let Some(existing) = environment.lookup(&name).cloned() {
                let resolved = resolve_type(&existing.ty, &substitutions);
                if resolved.contains_unknown() {
                    return Err(InferenceError::AmbiguousFunction { name });
                }
                let quantifiers = resolved.free_type_vars();
                let final_scheme = TypeScheme::new(quantifiers, resolved.clone());
                environment.define_scheme(name.clone(), final_scheme.clone());
                function_schemes.insert(name, final_scheme);
            }
        }

        self.bindings = solve_result.bindings;
        self.environment = environment;
        self.function_schemes = function_schemes;
        self.result_type = None;
        let duration_ms = inference_start.elapsed().as_secs_f64() * 1_000.0;
        self.telemetry = InferenceTelemetry {
            constraints_emitted: constraint_count,
            bindings_resolved: self.bindings.len(),
            inference_duration_ms: duration_ms,
            ..InferenceTelemetry::default()
        };
        Ok(())
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
        TypeKind::Primitive(name) => TypeKind::Primitive(name),
        TypeKind::Optional(inner) => {
            TypeKind::Optional(Box::new(resolve_type(inner, substitutions)))
        }
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
    use jv_ast::{
        BinaryOp, Expression, Literal, Modifiers, Parameter, Span, Statement, TypeAnnotation,
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
                span: dummy_span(),
            },
            Parameter {
                name: "rhs".into(),
                type_annotation: Some(TypeAnnotation::Simple("Int".into())),
                default_value: None,
                span: dummy_span(),
            },
        ];

        let body = Expression::Binary {
            left: Box::new(Expression::Identifier("lhs".into(), dummy_span())),
            op: BinaryOp::Add,
            right: Box::new(Expression::Identifier("rhs".into(), dummy_span())),
            span: dummy_span(),
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
                assert_eq!(params[0], TypeKind::Primitive("Int"));
                assert_eq!(params[1], TypeKind::Primitive("Int"));
                assert_eq!(**ret, TypeKind::Primitive("Int"));
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
                assert_eq!(params[0], TypeKind::Primitive("String"));
                assert_eq!(**ret, TypeKind::Primitive("String"));
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
