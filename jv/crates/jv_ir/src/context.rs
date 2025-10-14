use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

use crate::types::{
    DataFormat, JavaType, MethodOverload, SampleMode, StaticMethodCall, UtilityClass,
};
use jv_ast::Span;
use jv_support::arena::{
    PoolMetrics as TransformPoolMetrics, PoolSessionMetrics as TransformPoolSessionMetrics,
    TransformPools, TransformPoolsGuard,
};

/// Transformation context for desugaring
#[derive(Debug)]
pub struct TransformContext {
    /// Type information gathered from analysis
    pub type_info: HashMap<String, JavaType>,
    /// Recorded Java signatures for top-level functions
    pub function_signatures: HashMap<String, Vec<JavaType>>,
    /// Current scope for variable resolution
    pub scope_stack: Vec<HashMap<String, JavaType>>,
    /// Generated utility classes
    pub utility_classes: Vec<UtilityClass>,
    /// Generated method overloads
    pub method_overloads: Vec<MethodOverload>,
    /// Extension function mappings
    pub extension_methods: HashMap<String, StaticMethodCall>,
    /// Current package
    pub current_package: Option<String>,
    /// Options controlling @Sample transformation behaviour
    pub sample_options: SampleOptions,
    /// Cache tracking whitespace-delimited sequence element types to avoid recomputation
    pub sequence_style_cache: SequenceStyleCache,
    /// Counter for synthesised local identifiers
    temp_counter: usize,
    /// Optional arena pools shared across lowering sessions
    pool_state: Option<TransformPoolState>,
    /// Recorded lowering strategies for `when` expressions (telemetry & debugging)
    when_strategies: Vec<WhenStrategyRecord>,
}

impl TransformContext {
    /// Create a fresh transformation context with an initial scope.
    ///
    /// ```
    /// use jv_ir::{TransformContext, JavaType};
    ///
    /// let mut ctx = TransformContext::new();
    /// let int_type = JavaType::int();
    /// ctx.add_variable("answer".to_string(), int_type.clone());
    /// assert_eq!(ctx.lookup_variable("answer"), Some(&int_type));
    ///
    /// ctx.enter_scope();
    /// let string_type = JavaType::string();
    /// ctx.add_variable("message".to_string(), string_type.clone());
    /// assert!(ctx.lookup_variable("message").is_some());
    /// ctx.exit_scope();
    /// assert!(ctx.lookup_variable("message").is_none());
    /// ```
    pub fn new() -> Self {
        Self {
            type_info: HashMap::new(),
            function_signatures: HashMap::new(),
            scope_stack: vec![HashMap::new()],
            utility_classes: Vec::new(),
            method_overloads: Vec::new(),
            extension_methods: HashMap::new(),
            current_package: None,
            sample_options: SampleOptions::default(),
            sequence_style_cache: SequenceStyleCache::with_capacity(),
            temp_counter: 0,
            pool_state: None,
            when_strategies: Vec::new(),
        }
    }

    pub fn enter_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn add_variable(&mut self, name: String, java_type: JavaType) {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope.insert(name, java_type);
        }
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&JavaType> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(java_type) = scope.get(name) {
                return Some(java_type);
            }
        }
        self.type_info.get(name)
    }

    pub fn register_function_signature(&mut self, name: String, params: Vec<JavaType>) {
        self.function_signatures.insert(name, params);
    }

    pub fn function_signature(&self, name: &str) -> Option<&[JavaType]> {
        self.function_signatures
            .get(name)
            .map(|types| types.as_slice())
    }

    pub fn fresh_identifier(&mut self, prefix: &str) -> String {
        let name = format!("{}{}", prefix, self.temp_counter);
        self.temp_counter += 1;
        name
    }

    pub fn sample_options(&self) -> &SampleOptions {
        &self.sample_options
    }

    pub fn sample_options_mut(&mut self) -> &mut SampleOptions {
        &mut self.sample_options
    }

    pub fn sequence_style_cache(&self) -> &SequenceStyleCache {
        &self.sequence_style_cache
    }

    pub fn sequence_style_cache_mut(&mut self) -> &mut SequenceStyleCache {
        &mut self.sequence_style_cache
    }

    /// Creates a transformation context that owns arena-backed pools.
    pub fn with_pools(pools: TransformPools) -> Self {
        let mut ctx = Self::new();
        ctx.pool_state = Some(TransformPoolState::new(pools));
        ctx
    }

    /// Enables pooling for an existing context.
    pub fn configure_pools(&mut self, pools: TransformPools) {
        self.pool_state = Some(TransformPoolState::new(pools));
    }

    /// Disables pooling for the context.
    pub fn disable_pools(&mut self) {
        self.pool_state = None;
    }

    /// Returns true when pools are configured.
    pub fn pools_enabled(&self) -> bool {
        self.pool_state.is_some()
    }

    /// Starts a lowering session, clearing cached state and borrowing pools if available.
    pub fn begin_lowering_session(&mut self) -> Option<TransformPoolsGuard> {
        self.sequence_style_cache.clear();
        self.pool_state.as_mut().map(|state| state.acquire())
    }

    /// Captures metrics produced by the most recent lowering session.
    pub fn finish_lowering_session(&mut self) {
        // Metrics are materialised when the guard drops; nothing further required here.
    }

    /// Returns metrics for the most recent session, if pooling is enabled and a session ran.
    pub fn last_pool_session(&self) -> Option<TransformPoolSessionMetrics> {
        self.pool_state
            .as_ref()
            .and_then(|state| state.last_session())
    }

    /// Indicates whether the most recent session reused arena capacity.
    pub fn last_pool_warm_start(&self) -> Option<bool> {
        self.pool_state
            .as_ref()
            .and_then(|state| state.last_warm_start())
    }

    /// Returns cumulative pool metrics collected so far.
    pub fn pool_metrics(&self) -> Option<TransformPoolMetrics> {
        self.pool_state.as_ref().map(|state| state.metrics())
    }

    /// Convenience helper exposing the current reuse ratio across sessions.
    pub fn pool_reuse_ratio(&self) -> Option<f64> {
        self.pool_state
            .as_ref()
            .map(|state| state.metrics().reuse_ratio())
    }

    /// Records the lowering strategy applied to a `when` expression for later inspection.
    pub fn record_when_strategy(&mut self, span: Span, description: impl Into<String>) {
        self.when_strategies.push(WhenStrategyRecord {
            span,
            description: description.into(),
        });
    }

    /// Returns the list of recorded lowering strategies without consuming them.
    pub fn when_strategies(&self) -> &[WhenStrategyRecord] {
        &self.when_strategies
    }

    /// Consumes and returns the recorded lowering strategies.
    pub fn take_when_strategies(&mut self) -> Vec<WhenStrategyRecord> {
        std::mem::take(&mut self.when_strategies)
    }
}

// Helper implementations
impl Default for TransformContext {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for TransformContext {
    fn clone(&self) -> Self {
        Self {
            type_info: self.type_info.clone(),
            function_signatures: self.function_signatures.clone(),
            scope_stack: self.scope_stack.clone(),
            utility_classes: self.utility_classes.clone(),
            method_overloads: self.method_overloads.clone(),
            extension_methods: self.extension_methods.clone(),
            current_package: self.current_package.clone(),
            sample_options: self.sample_options.clone(),
            sequence_style_cache: self.sequence_style_cache.clone(),
            temp_counter: self.temp_counter,
            pool_state: self
                .pool_state
                .as_ref()
                .map(TransformPoolState::shallow_clone),
            when_strategies: self.when_strategies.clone(),
        }
    }
}

/// Metadata describing the lowering strategy chosen for a `when` expression.
#[derive(Debug, Clone)]
pub struct WhenStrategyRecord {
    pub span: Span,
    pub description: String,
}

#[derive(Debug, Clone)]
pub struct SampleOptions {
    pub base_dir: Option<PathBuf>,
    pub allow_network: bool,
    pub default_mode: SampleMode,
    pub default_format: Option<DataFormat>,
    pub cache_dir: Option<PathBuf>,
    pub aws_cli_path: Option<PathBuf>,
    pub git_cli_path: Option<PathBuf>,
    pub timeout: Duration,
    pub embed_max_bytes: Option<u64>,
}

impl Default for SampleOptions {
    fn default() -> Self {
        Self {
            base_dir: None,
            allow_network: false,
            default_mode: SampleMode::Embed,
            default_format: None,
            cache_dir: None,
            aws_cli_path: None,
            git_cli_path: None,
            timeout: Duration::from_secs(30),
            embed_max_bytes: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SequenceStyleCache {
    array_elements: HashMap<SpanKey, JavaType>,
    call_arguments: HashMap<SpanKey, JavaType>,
}

impl SequenceStyleCache {
    pub fn with_capacity() -> Self {
        Self {
            array_elements: HashMap::new(),
            call_arguments: HashMap::new(),
        }
    }

    pub fn lookup_or_insert_array(
        &mut self,
        span: &Span,
        element_type: JavaType,
    ) -> Option<JavaType> {
        let key = SpanKey::from(span);
        match self.array_elements.get(&key) {
            Some(existing) => Some(existing.clone()),
            None => {
                self.array_elements.insert(key, element_type);
                None
            }
        }
    }

    pub fn lookup_or_insert_call(
        &mut self,
        span: &Span,
        element_type: JavaType,
    ) -> Option<JavaType> {
        let key = SpanKey::from(span);
        match self.call_arguments.get(&key) {
            Some(existing) => Some(existing.clone()),
            None => {
                self.call_arguments.insert(key, element_type);
                None
            }
        }
    }

    pub fn clear(&mut self) {
        self.array_elements.clear();
        self.call_arguments.clear();
    }
}

#[derive(Debug)]
struct TransformPoolState {
    pools: TransformPools,
    last_warm_start: Option<bool>,
}

impl TransformPoolState {
    fn new(pools: TransformPools) -> Self {
        Self {
            pools,
            last_warm_start: None,
        }
    }

    fn acquire(&mut self) -> TransformPoolsGuard {
        let guard = self.pools.acquire();
        self.last_warm_start = Some(guard.is_warm_start());
        guard
    }

    fn last_session(&self) -> Option<TransformPoolSessionMetrics> {
        self.pools.last_session()
    }

    fn last_warm_start(&self) -> Option<bool> {
        self.last_warm_start
    }

    fn metrics(&self) -> TransformPoolMetrics {
        self.pools.metrics()
    }

    fn shallow_clone(&self) -> Self {
        let chunk = self.pools.chunk_bytes();
        let mut cloned = Self::new(TransformPools::with_chunk_capacity(chunk));
        cloned.last_warm_start = self.last_warm_start;
        cloned
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SpanKey {
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize,
}

impl From<&Span> for SpanKey {
    fn from(span: &Span) -> Self {
        Self {
            start_line: span.start_line,
            start_column: span.start_column,
            end_line: span.end_line,
            end_column: span.end_column,
        }
    }
}
