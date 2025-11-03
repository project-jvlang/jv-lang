use super::termination::{TerminationConfig, TerminationError, TerminationGuard};
use crate::cache::{CachedDiagnostic, DependencyTracker, FingerprintHash};
use crate::constraint::AstId;
use crate::service::TypeLevelValue;
use jv_ast::Span;
use jv_ast::types::{TypeLevelExpr, TypeLevelIdentifier, TypeLevelOp};
use serde_json::to_vec;
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Value recorded in the evaluation environment alongside its origin identifier.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeLevelBinding {
    value: TypeLevelValue,
    origin: Option<AstId>,
}

impl TypeLevelBinding {
    /// Creates a binding without origin metadata.
    pub fn new(value: TypeLevelValue) -> Self {
        Self {
            value,
            origin: None,
        }
    }

    /// Creates a binding referencing the originating AST node.
    pub fn with_origin(value: TypeLevelValue, origin: AstId) -> Self {
        Self {
            value,
            origin: Some(origin),
        }
    }

    /// Returns the stored value.
    pub fn value(&self) -> &TypeLevelValue {
        &self.value
    }

    /// Returns the origin metadata when present.
    pub fn origin(&self) -> Option<AstId> {
        self.origin
    }
}

/// Environment supplying identifiers used during evaluation.
#[derive(Debug, Default, Clone)]
pub struct EvaluationContext {
    bindings: HashMap<String, TypeLevelBinding>,
}

impl EvaluationContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a binding reference into the context.
    pub fn insert_binding<S: Into<String>>(
        &mut self,
        name: S,
        binding: TypeLevelBinding,
    ) -> Option<TypeLevelBinding> {
        self.bindings.insert(name.into(), binding)
    }

    /// Convenience helper inserting a binding without origin metadata.
    pub fn insert_value<S: Into<String>>(
        &mut self,
        name: S,
        value: TypeLevelValue,
    ) -> Option<TypeLevelBinding> {
        self.insert_binding(name, TypeLevelBinding::new(value))
    }

    pub fn get(&self, name: &str) -> Option<&TypeLevelBinding> {
        self.bindings.get(name)
    }

    pub fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    pub fn available_names(&self) -> impl Iterator<Item = &String> {
        self.bindings.keys()
    }
}

/// Collection of AST dependencies discovered while evaluating an expression.
#[derive(Debug, Default)]
pub struct EvaluationDependencies {
    inner: HashSet<AstId>,
}

impl EvaluationDependencies {
    pub fn new() -> Self {
        Self {
            inner: HashSet::new(),
        }
    }

    pub fn insert(&mut self, id: AstId) {
        self.inner.insert(id);
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &AstId> {
        self.inner.iter()
    }

    fn into_vec(self) -> Vec<AstId> {
        self.inner.into_iter().collect()
    }
}

/// Diagnostics emitted when evaluation fails.
#[derive(Debug, Clone)]
pub struct TypeLevelDiagnostic {
    pub code: &'static str,
    pub message: String,
    pub span: Option<Span>,
    pub notes: Vec<String>,
}

impl TypeLevelDiagnostic {
    pub fn new(code: &'static str, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            span: None,
            notes: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn into_cached(self) -> CachedDiagnostic {
        let mut diagnostic = CachedDiagnostic::new(self.code, self.message);
        if let Some(span) = self.span {
            diagnostic = diagnostic.with_span(span);
        }
        if !self.notes.is_empty() {
            diagnostic = diagnostic.with_notes(self.notes);
        }
        diagnostic
    }
}

/// Result of evaluating a single type-level expression.
#[derive(Debug, Clone)]
pub struct TypeLevelEvaluationOutcome {
    pub value: Option<TypeLevelValue>,
    pub diagnostics: Vec<TypeLevelDiagnostic>,
    pub cache_hit: bool,
}

impl TypeLevelEvaluationOutcome {
    pub fn success(value: TypeLevelValue, cache_hit: bool) -> Self {
        Self {
            value: Some(value),
            diagnostics: Vec::new(),
            cache_hit,
        }
    }

    pub fn failure(diagnostic: TypeLevelDiagnostic) -> Self {
        Self {
            value: None,
            diagnostics: vec![diagnostic],
            cache_hit: false,
        }
    }

    pub fn is_success(&self) -> bool {
        self.value.is_some()
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct TypeLevelMetrics {
    evaluations: u64,
    cache_hits: u64,
}

impl TypeLevelMetrics {
    fn record_evaluation(&mut self) {
        self.evaluations = self.evaluations.saturating_add(1);
    }

    fn record_cache_hit(&mut self) {
        self.cache_hits = self.cache_hits.saturating_add(1);
    }
}

#[derive(Debug, Clone)]
struct CachedEvaluation {
    fingerprint: FingerprintHash,
    value: TypeLevelValue,
}

impl CachedEvaluation {
    fn new(fingerprint: FingerprintHash, value: TypeLevelValue) -> Self {
        Self { fingerprint, value }
    }
}

/// Pure evaluator for type-level expressions with caching and dependency tracking.
#[derive(Debug)]
pub struct TypeLevelEvaluator {
    cache: HashMap<AstId, CachedEvaluation>,
    tracker: DependencyTracker,
    metrics: TypeLevelMetrics,
    termination: TerminationConfig,
}

impl Default for TypeLevelEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeLevelEvaluator {
    /// Creates an evaluator using the default termination configuration.
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            tracker: DependencyTracker::new(),
            metrics: TypeLevelMetrics::default(),
            termination: TerminationConfig::default(),
        }
    }

    /// Creates an evaluator using the provided termination configuration.
    pub fn with_termination_config(config: TerminationConfig) -> Self {
        Self {
            cache: HashMap::new(),
            tracker: DependencyTracker::new(),
            metrics: TypeLevelMetrics::default(),
            termination: config,
        }
    }

    /// Evaluates the provided expression returning either a cached result or a freshly
    /// computed value together with diagnostics on failure.
    pub fn evaluate(
        &mut self,
        expr_id: AstId,
        expr: &TypeLevelExpr,
        context: &EvaluationContext,
    ) -> TypeLevelEvaluationOutcome {
        let fingerprint = fingerprint_expression(expr);
        if let Some(entry) = self.cache.get(&expr_id) {
            if entry.fingerprint == fingerprint {
                self.metrics.record_cache_hit();
                return TypeLevelEvaluationOutcome::success(entry.value.clone(), true);
            }
        }

        let mut guard = TerminationGuard::new(self.termination);
        let mut dependencies = EvaluationDependencies::new();
        match guard.enter(Some(expr_id)) {
            Ok(()) => {
                let result =
                    self.evaluate_expr(expr_id, expr, context, &mut guard, &mut dependencies);
                guard.exit(Some(expr_id));
                self.finalize(expr_id, fingerprint, result, expr, context, dependencies)
            }
            Err(error) => self.finalize_error(error, None, expr, context),
        }
    }

    fn finalize(
        &mut self,
        expr_id: AstId,
        fingerprint: FingerprintHash,
        result: Result<TypeLevelValue, TypeLevelError>,
        expr: &TypeLevelExpr,
        context: &EvaluationContext,
        dependencies: EvaluationDependencies,
    ) -> TypeLevelEvaluationOutcome {
        self.metrics.record_evaluation();

        match result {
            Ok(value) => {
                self.cache
                    .insert(expr_id, CachedEvaluation::new(fingerprint, value.clone()));
                if dependencies.is_empty() {
                    // Remove any previous dependency edges for this expression.
                    self.tracker.set_dependencies(expr_id, std::iter::empty());
                } else {
                    self.tracker
                        .set_dependencies(expr_id, dependencies.into_vec());
                }
                TypeLevelEvaluationOutcome::success(value, false)
            }
            Err(error) => {
                let diagnostic = error.into_diagnostic(expr, context);
                if dependencies.is_empty() {
                    self.tracker.set_dependencies(expr_id, std::iter::empty());
                } else {
                    self.tracker
                        .set_dependencies(expr_id, dependencies.into_vec());
                }
                TypeLevelEvaluationOutcome::failure(diagnostic)
            }
        }
    }

    fn finalize_error(
        &self,
        error: TerminationError,
        span: Option<Span>,
        expr: &TypeLevelExpr,
        context: &EvaluationContext,
    ) -> TypeLevelEvaluationOutcome {
        let diagnostic =
            TypeLevelError::from_termination(error, span).into_diagnostic(expr, context);
        TypeLevelEvaluationOutcome::failure(diagnostic)
    }

    fn evaluate_expr(
        &mut self,
        expr_id: AstId,
        expr: &TypeLevelExpr,
        context: &EvaluationContext,
        guard: &mut TerminationGuard,
        dependencies: &mut EvaluationDependencies,
    ) -> Result<TypeLevelValue, TypeLevelError> {
        match expr {
            TypeLevelExpr::LiteralInt(value) => Ok(TypeLevelValue::Int(*value)),
            TypeLevelExpr::LiteralBool(value) => Ok(TypeLevelValue::Bool(*value)),
            TypeLevelExpr::LiteralString(value) => Ok(TypeLevelValue::String(value.clone())),
            TypeLevelExpr::Identifier(identifier) => {
                self.evaluate_identifier(expr_id, identifier, context, dependencies)
            }
            TypeLevelExpr::Apply { span, .. } => Err(TypeLevelError::Unsupported {
                feature: "function application",
                span: Some(span.clone()),
            }),
            TypeLevelExpr::BinaryOp { op, lhs, rhs, span } => {
                guard
                    .enter(None)
                    .map_err(|error| TypeLevelError::from_termination(error, Some(span.clone())))?;
                let left_result = self.evaluate_expr(expr_id, lhs, context, guard, dependencies);
                guard.exit(None);

                guard
                    .enter(None)
                    .map_err(|error| TypeLevelError::from_termination(error, Some(span.clone())))?;
                let right_result = self.evaluate_expr(expr_id, rhs, context, guard, dependencies);
                guard.exit(None);

                let left = left_result?;
                let right = right_result?;
                Self::evaluate_binary_op(op.clone(), left, right, span.clone())
            }
        }
    }

    fn evaluate_identifier(
        &self,
        expr_id: AstId,
        identifier: &TypeLevelIdentifier,
        context: &EvaluationContext,
        dependencies: &mut EvaluationDependencies,
    ) -> Result<TypeLevelValue, TypeLevelError> {
        match context.get(identifier.name.as_str()) {
            Some(binding) => {
                if let Some(origin) = binding.origin() {
                    if origin == expr_id {
                        return Err(TypeLevelError::from_termination(
                            TerminationError::CycleDetected {
                                cycle: vec![expr_id, origin],
                            },
                            Some(identifier.span.clone()),
                        ));
                    }
                    dependencies.insert(origin);
                }
                Ok(binding.value().clone())
            }
            None => Err(TypeLevelError::UnknownIdentifier {
                name: identifier.name.clone(),
                span: identifier.span.clone(),
            }),
        }
    }

    fn evaluate_binary_op(
        op: TypeLevelOp,
        lhs: TypeLevelValue,
        rhs: TypeLevelValue,
        span: Span,
    ) -> Result<TypeLevelValue, TypeLevelError> {
        use TypeLevelOp::*;

        match op {
            Add | Subtract | Multiply | Divide | Modulo => {
                Self::evaluate_integer_op(op, lhs, rhs, span)
            }
            Less | LessEqual | Greater | GreaterEqual => {
                Self::evaluate_comparison(op, lhs, rhs, span)
            }
            Equal | NotEqual => Self::evaluate_equality(op, lhs, rhs, span),
            And | Or => Self::evaluate_boolean_op(op, lhs, rhs, span),
        }
    }

    fn evaluate_integer_op(
        op: TypeLevelOp,
        lhs: TypeLevelValue,
        rhs: TypeLevelValue,
        span: Span,
    ) -> Result<TypeLevelValue, TypeLevelError> {
        match (lhs, rhs) {
            (TypeLevelValue::Int(left), TypeLevelValue::Int(right)) => {
                let value = match op {
                    TypeLevelOp::Add => left.wrapping_add(right),
                    TypeLevelOp::Subtract => left.wrapping_sub(right),
                    TypeLevelOp::Multiply => left.wrapping_mul(right),
                    TypeLevelOp::Divide => {
                        if right == 0 {
                            return Err(TypeLevelError::DivisionByZero { span });
                        }
                        left.wrapping_div(right)
                    }
                    TypeLevelOp::Modulo => {
                        if right == 0 {
                            return Err(TypeLevelError::DivisionByZero { span });
                        }
                        left.wrapping_rem(right)
                    }
                    _ => unreachable!("non-integer op routed to integer handler"),
                };
                Ok(TypeLevelValue::Int(value))
            }
            (lhs, rhs) => Err(TypeLevelError::TypeMismatch { op, span, lhs, rhs }),
        }
    }

    fn evaluate_comparison(
        op: TypeLevelOp,
        lhs: TypeLevelValue,
        rhs: TypeLevelValue,
        span: Span,
    ) -> Result<TypeLevelValue, TypeLevelError> {
        match (lhs, rhs) {
            (TypeLevelValue::Int(left), TypeLevelValue::Int(right)) => {
                let value = match op {
                    TypeLevelOp::Less => left < right,
                    TypeLevelOp::LessEqual => left <= right,
                    TypeLevelOp::Greater => left > right,
                    TypeLevelOp::GreaterEqual => left >= right,
                    _ => unreachable!("comparison handler only accepts comparison ops"),
                };
                Ok(TypeLevelValue::Bool(value))
            }
            (lhs, rhs) => Err(TypeLevelError::TypeMismatch { op, span, lhs, rhs }),
        }
    }

    fn evaluate_equality(
        op: TypeLevelOp,
        lhs: TypeLevelValue,
        rhs: TypeLevelValue,
        span: Span,
    ) -> Result<TypeLevelValue, TypeLevelError> {
        let result = match (&lhs, &rhs) {
            (TypeLevelValue::Int(left), TypeLevelValue::Int(right)) => left == right,
            (TypeLevelValue::Bool(left), TypeLevelValue::Bool(right)) => left == right,
            (TypeLevelValue::String(left), TypeLevelValue::String(right)) => left == right,
            _ => {
                return Err(TypeLevelError::TypeMismatch { op, span, lhs, rhs });
            }
        };
        if matches!(op, TypeLevelOp::NotEqual) {
            Ok(TypeLevelValue::Bool(!result))
        } else {
            Ok(TypeLevelValue::Bool(result))
        }
    }

    fn evaluate_boolean_op(
        op: TypeLevelOp,
        lhs: TypeLevelValue,
        rhs: TypeLevelValue,
        span: Span,
    ) -> Result<TypeLevelValue, TypeLevelError> {
        match (lhs, rhs) {
            (TypeLevelValue::Bool(left), TypeLevelValue::Bool(right)) => {
                let value = match op {
                    TypeLevelOp::And => left && right,
                    TypeLevelOp::Or => left || right,
                    _ => unreachable!("boolean handler only accepts And/Or"),
                };
                Ok(TypeLevelValue::Bool(value))
            }
            (lhs, rhs) => Err(TypeLevelError::TypeMismatch { op, span, lhs, rhs }),
        }
    }

    /// Returns the current cache size.
    pub fn cache_size(&self) -> usize {
        self.cache.len()
    }

    /// Returns the internal dependency tracker.
    pub fn dependency_tracker(&self) -> &DependencyTracker {
        &self.tracker
    }

    /// Mutable access to the dependency tracker (primarily for tests).
    pub fn dependency_tracker_mut(&mut self) -> &mut DependencyTracker {
        &mut self.tracker
    }

    /// Returns `(evaluations, cache_hits)` counters collected so far.
    pub fn metrics(&self) -> (u64, u64) {
        (self.metrics.evaluations, self.metrics.cache_hits)
    }

    /// Updates the termination configuration for subsequent evaluations.
    pub fn set_termination_config(&mut self, config: TerminationConfig) {
        self.termination = config;
    }
}

#[derive(Debug, Clone)]
enum TypeLevelError {
    UnknownIdentifier {
        name: String,
        span: Span,
    },
    Unsupported {
        feature: &'static str,
        span: Option<Span>,
    },
    TypeMismatch {
        op: TypeLevelOp,
        span: Span,
        lhs: TypeLevelValue,
        rhs: TypeLevelValue,
    },
    DivisionByZero {
        span: Span,
    },
    Termination {
        error: TerminationError,
        span: Option<Span>,
    },
}

impl TypeLevelError {
    fn into_diagnostic(
        self,
        expr: &TypeLevelExpr,
        context: &EvaluationContext,
    ) -> TypeLevelDiagnostic {
        match self {
            Self::UnknownIdentifier { name, span } => {
                let mut diagnostic = TypeLevelDiagnostic::new(
                    "JV3101",
                    format!("型レベル式で識別子 `{name}` を解決できません。"),
                )
                .with_span(span);
                if !context.is_empty() {
                    let available: Vec<_> = context.available_names().map(|s| s.as_str()).collect();
                    if !available.is_empty() {
                        diagnostic = diagnostic
                            .with_note(format!("利用可能な識別子: {}", available.join(", ")));
                    }
                }
                diagnostic.with_note(format!("式: {}", describe_expr(expr)))
            }
            Self::Unsupported { feature, span } => {
                let message =
                    format!("Phase 2 の型レベル評価では `{feature}` はサポートされていません。");
                let mut diagnostic = TypeLevelDiagnostic::new("JV3101", message);
                if let Some(span) = span {
                    diagnostic = diagnostic.with_span(span);
                }
                diagnostic.with_note(format!("式: {}", describe_expr(expr)))
            }
            Self::TypeMismatch { op, span, lhs, rhs } => {
                let mut diagnostic = TypeLevelDiagnostic::new(
                    "JV3101",
                    format!(
                        "型レベル演算 `{}` には互換性のあるオペランドが必要です。",
                        op_symbol(&op)
                    ),
                )
                .with_span(span);
                diagnostic = diagnostic.with_note(format!("左項: {}", lhs));
                diagnostic = diagnostic.with_note(format!("右項: {}", rhs));
                diagnostic.with_note(format!("式: {}", describe_expr(expr)))
            }
            Self::DivisionByZero { span } => {
                let diagnostic = TypeLevelDiagnostic::new(
                    "JV3101",
                    "0 での除算または剰余演算は許可されていません。",
                )
                .with_span(span);
                diagnostic.with_note(format!("式: {}", describe_expr(expr)))
            }
            Self::Termination { error, span } => {
                let diagnostic =
                    TypeLevelDiagnostic::new("JV3102", error.to_string()).with_span_opt(span);
                diagnostic.with_note(format!("式: {}", describe_expr(expr)))
            }
        }
    }
}

impl TypeLevelError {
    fn from_termination(error: TerminationError, span: Option<Span>) -> Self {
        Self::Termination { error, span }
    }
}

impl TypeLevelDiagnostic {
    fn with_span_opt(mut self, span: Option<Span>) -> Self {
        self.span = span;
        self
    }
}

fn fingerprint_expression(expr: &TypeLevelExpr) -> FingerprintHash {
    to_vec(expr)
        .map(|bytes| FingerprintHash::of(&bytes))
        .unwrap_or(FingerprintHash::ZERO)
}

fn describe_expr(expr: &TypeLevelExpr) -> String {
    match expr {
        TypeLevelExpr::LiteralInt(value) => value.to_string(),
        TypeLevelExpr::LiteralBool(value) => value.to_string(),
        TypeLevelExpr::LiteralString(value) => format!("\"{}\"", value),
        TypeLevelExpr::Identifier(identifier) => identifier.name.clone(),
        TypeLevelExpr::Apply {
            callee, arguments, ..
        } => {
            let args: Vec<String> = arguments.iter().map(describe_expr).collect();
            format!("{}({})", describe_expr(callee), args.join(", "))
        }
        TypeLevelExpr::BinaryOp { op, lhs, rhs, .. } => {
            format!(
                "({} {} {})",
                describe_expr(lhs),
                op_symbol(op),
                describe_expr(rhs)
            )
        }
    }
}

fn op_symbol(op: &TypeLevelOp) -> &'static str {
    use TypeLevelOp::*;
    match op {
        Add => "+",
        Subtract => "-",
        Multiply => "*",
        Divide => "/",
        Modulo => "%",
        Equal => "==",
        NotEqual => "!=",
        Less => "<",
        LessEqual => "<=",
        Greater => ">",
        GreaterEqual => ">=",
        And => "&&",
        Or => "||",
    }
}

impl fmt::Display for TypeLevelError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownIdentifier { name, .. } => {
                write!(f, "未解決の識別子 `{name}`")
            }
            Self::Unsupported { feature, .. } => {
                write!(f, "`{feature}` は未対応です")
            }
            Self::TypeMismatch { op, .. } => {
                write!(f, "演算 `{}` のオペランド型が一致しません", op_symbol(op))
            }
            Self::DivisionByZero { .. } => write!(f, "0 で除算できません"),
            Self::Termination { error, .. } => write!(f, "{error}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_ast::types::{TypeLevelExpr, TypeLevelIdentifier, TypeLevelOp};

    fn dummy_span() -> Span {
        Span::dummy()
    }

    fn literal_int(value: i64) -> TypeLevelExpr {
        TypeLevelExpr::LiteralInt(value)
    }

    fn literal_bool(value: bool) -> TypeLevelExpr {
        TypeLevelExpr::LiteralBool(value)
    }

    fn identifier(name: &str) -> TypeLevelExpr {
        TypeLevelExpr::Identifier(TypeLevelIdentifier {
            name: name.into(),
            span: dummy_span(),
        })
    }

    fn binary(op: TypeLevelOp, lhs: TypeLevelExpr, rhs: TypeLevelExpr) -> TypeLevelExpr {
        TypeLevelExpr::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: dummy_span(),
        }
    }

    #[test]
    fn evaluates_arithmetic_expression() {
        let mut evaluator = TypeLevelEvaluator::new();
        let expr_id = 1;
        let expr = binary(
            TypeLevelOp::Add,
            literal_int(2),
            binary(TypeLevelOp::Multiply, literal_int(3), literal_int(4)),
        );

        let context = EvaluationContext::new();
        let outcome = evaluator.evaluate(expr_id, &expr, &context);

        assert!(outcome.is_success());
        assert_eq!(outcome.value, Some(TypeLevelValue::Int(14)));
        assert!(!outcome.cache_hit);
        let (evaluations, cache_hits) = evaluator.metrics();
        assert_eq!(evaluations, 1);
        assert_eq!(cache_hits, 0);
    }

    #[test]
    fn reuses_cached_result() {
        let mut evaluator = TypeLevelEvaluator::new();
        let expr_id = 2;
        let expr = binary(TypeLevelOp::Subtract, literal_int(10), literal_int(3));
        let context = EvaluationContext::new();

        let first = evaluator.evaluate(expr_id, &expr, &context);
        assert!(first.is_success());

        let second = evaluator.evaluate(expr_id, &expr, &context);
        assert!(second.is_success());
        assert!(second.cache_hit);

        let (evaluations, cache_hits) = evaluator.metrics();
        assert_eq!(evaluations, 1, "only one evaluation should execute");
        assert_eq!(cache_hits, 1, "second lookup should hit cache");
    }

    #[test]
    fn reports_unknown_identifier() {
        let mut evaluator = TypeLevelEvaluator::new();
        let expr_id = 3;
        let expr = identifier("SIZE");
        let context = EvaluationContext::new();

        let outcome = evaluator.evaluate(expr_id, &expr, &context);
        assert!(!outcome.is_success());
        let diagnostic = outcome.diagnostics.first().expect("diagnostic");
        assert_eq!(diagnostic.code, "JV3101");
        assert!(diagnostic.message.contains("SIZE"));
    }

    #[test]
    fn reports_type_mismatch() {
        let mut evaluator = TypeLevelEvaluator::new();
        let expr_id = 4;
        let expr = binary(TypeLevelOp::Add, literal_bool(true), literal_int(1));
        let context = EvaluationContext::new();

        let outcome = evaluator.evaluate(expr_id, &expr, &context);
        assert!(!outcome.is_success());
        let diagnostic = outcome.diagnostics.first().expect("diagnostic");
        assert_eq!(diagnostic.code, "JV3101");
        assert!(diagnostic.message.contains("演算 `+`"));
        assert_eq!(outcome.value, None);
    }

    #[test]
    fn reports_division_by_zero() {
        let mut evaluator = TypeLevelEvaluator::new();
        let expr_id = 5;
        let expr = binary(TypeLevelOp::Divide, literal_int(10), literal_int(0));
        let context = EvaluationContext::new();

        let outcome = evaluator.evaluate(expr_id, &expr, &context);
        assert!(!outcome.is_success());
        let diagnostic = outcome.diagnostics.first().expect("diagnostic");
        assert_eq!(diagnostic.code, "JV3101");
        assert!(diagnostic.message.contains("0 での除算"));
    }

    #[test]
    fn enforces_recursion_depth_limit() {
        let mut evaluator = TypeLevelEvaluator::new();
        evaluator.set_termination_config(TerminationConfig::new(10));

        let expr_id = 6;
        let mut expr = literal_int(1);
        for _ in 0..11 {
            expr = binary(TypeLevelOp::Add, expr, literal_int(1));
        }

        let context = EvaluationContext::new();
        let outcome = evaluator.evaluate(expr_id, &expr, &context);
        assert!(!outcome.is_success());
        let diagnostic = &outcome.diagnostics[0];
        assert_eq!(diagnostic.code, "JV3102");
        assert!(diagnostic.message.contains("最大深度"));
    }

    #[test]
    fn detects_simple_cycle_via_origin() {
        let mut evaluator = TypeLevelEvaluator::new();
        let expr_id = 7;

        let mut context = EvaluationContext::new();
        context.insert_binding(
            "N",
            TypeLevelBinding::with_origin(TypeLevelValue::Int(42), expr_id),
        );

        let expr = identifier("N");
        let outcome = evaluator.evaluate(expr_id, &expr, &context);
        assert!(!outcome.is_success());
        let diagnostic = &outcome.diagnostics[0];
        assert_eq!(diagnostic.code, "JV3102");
        assert!(diagnostic.message.contains("循環参照"));
    }

    #[test]
    fn records_dependencies_for_cache_invalidation() {
        let mut evaluator = TypeLevelEvaluator::new();
        let expr_id = 8;
        let origin_id = 11;

        let mut context = EvaluationContext::new();
        context.insert_binding(
            "WIDTH",
            TypeLevelBinding::with_origin(TypeLevelValue::Int(5), origin_id),
        );
        let expr = binary(TypeLevelOp::Multiply, identifier("WIDTH"), literal_int(2));

        let outcome = evaluator.evaluate(expr_id, &expr, &context);
        assert!(outcome.is_success());

        let dependents = evaluator.dependency_tracker().dependents_of(origin_id);
        assert_eq!(dependents, [expr_id]);

        // Ensure the dependency tracker cascades invalidate roots correctly.
        let cascade = evaluator
            .dependency_tracker_mut()
            .invalidate_from([origin_id]);
        assert!(cascade.contains(&expr_id));
    }
}
