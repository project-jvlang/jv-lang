use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

use crate::types::{
    DataFormat, IrExpression, IrImport, IrImportDetail, IrResolvedMethodTarget, IrStatement,
    JavaType, LogLevel, LoggerFieldId, LoggerFieldSpec, LoggingFrameworkKind, LoggingMetadata,
    MethodOverload, PrimitiveReturnMetadata, SampleMode, UtilityClass,
};
use jv_ast::{CallArgumentStyle, Span};
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
    /// Preloaded signature hints sourced from TypeFacts
    signature_hints: HashMap<String, FunctionSignatureHint>,
    /// Current scope for variable resolution
    pub scope_stack: Vec<HashMap<String, JavaType>>,
    /// Generated utility classes
    pub utility_classes: Vec<UtilityClass>,
    /// Generated method overloads
    pub method_overloads: Vec<MethodOverload>,
    /// Registered method declarations collected during lowering
    pub method_declarations: Vec<RegisteredMethodDeclaration>,
    /// Registered method call sites collected during lowering
    pub method_calls: Vec<RegisteredMethodCall>,
    /// Registered extension function metadata keyed by function name.
    pub extension_registry: HashMap<String, Vec<ExtensionMetadata>>,
    /// Stack tracking the extension function currently being lowered (if any).
    extension_scope_stack: Vec<ExtensionScope>,
    /// Current package
    pub current_package: Option<String>,
    /// Record component metadata keyed by type name (supports FQCN and simple names)
    pub record_components: HashMap<String, HashMap<String, JavaType>>,
    /// Options controlling @Sample transformation behaviour
    pub sample_options: SampleOptions,
    /// Cache tracking whitespace-delimited sequence element types to avoid recomputation
    pub sequence_style_cache: SequenceStyleCache,
    /// Tuple record plan metadata keyed by tuple literal span
    tuple_plan_usages: HashMap<SpanKey, TuplePlanRegistration>,
    /// Counter for synthesised local identifiers
    temp_counter: usize,
    /// Optional arena pools shared across lowering sessions
    pool_state: Option<TransformPoolState>,
    /// Recorded lowering strategies for `when` expressions (telemetry & debugging)
    when_strategies: Vec<WhenStrategyRecord>,
    /// Resolved import plan used to enrich IR import statements
    planned_imports: Vec<IrImport>,
    /// Alias lookup for resolved import entries
    import_aliases: HashMap<String, JavaType>,
    /// Logging transformation state and configuration
    logging_state: LoggingState,
    /// Tuple return types keyed by function name (for tuple literals in returns).
    tuple_return_types: HashMap<String, JavaType>,
    /// Tuple component names keyed by record name for tuple returns.
    tuple_return_components: HashMap<String, Vec<String>>,
}

/// Hint describing a function signature sourced from TypeFacts prior to lowering.
#[derive(Debug, Clone, Default)]
pub struct FunctionSignatureHint {
    pub parameters: Vec<JavaType>,
    pub return_type: Option<JavaType>,
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
            signature_hints: HashMap::new(),
            scope_stack: vec![HashMap::new()],
            utility_classes: Vec::new(),
            method_overloads: Vec::new(),
            method_declarations: Vec::new(),
            method_calls: Vec::new(),
            extension_registry: HashMap::new(),
            extension_scope_stack: Vec::new(),
            current_package: None,
            record_components: HashMap::new(),
            sample_options: SampleOptions::default(),
            sequence_style_cache: SequenceStyleCache::with_capacity(),
            tuple_plan_usages: HashMap::new(),
            temp_counter: 0,
            pool_state: None,
            when_strategies: Vec::new(),
            planned_imports: Vec::new(),
            import_aliases: HashMap::new(),
            logging_state: LoggingState::default(),
            tuple_return_types: HashMap::new(),
            tuple_return_components: HashMap::new(),
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
        if let Some(java_type) = self.import_aliases.get(name) {
            return Some(java_type);
        }
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

    /// Registers a preloaded function signature hint sourced from upstream analysis.
    pub fn preload_function_signature(
        &mut self,
        name: impl Into<String>,
        parameters: Vec<JavaType>,
        return_type: Option<JavaType>,
    ) {
        let name = name.into();
        let hint = FunctionSignatureHint {
            parameters,
            return_type,
        };
        for key in Self::function_name_keys(&name) {
            self.signature_hints.insert(key, hint.clone());
        }
    }

    /// Retrieves a preloaded signature hint for the provided function name, if any.
    pub fn function_signature_hint(&self, name: &str) -> Option<&FunctionSignatureHint> {
        self.signature_hints.get(name)
    }

    fn function_name_keys(name: &str) -> Vec<String> {
        let mut keys = Vec::new();
        let mut push_key = |candidate: &str| {
            if candidate.is_empty() {
                return;
            }
            if !keys.iter().any(|existing| existing == candidate) {
                keys.push(candidate.to_string());
            }
        };

        push_key(name);

        if let Some(simple) = name.rsplit("::").next() {
            push_key(simple);
        }

        let dotted = name.replace("::", ".");
        if dotted != name {
            push_key(&dotted);
        }
        if let Some(simple_dot) = dotted.rsplit('.').next() {
            push_key(simple_dot);
        }

        if let Some(simple_dollar) = dotted.rsplit('$').next() {
            push_key(simple_dollar);
        }

        keys
    }

    pub fn register_record_components(
        &mut self,
        type_name: String,
        components: Vec<(String, JavaType)>,
    ) {
        let component_map: HashMap<String, JavaType> = components
            .into_iter()
            .map(|(name, ty)| (name, ty))
            .collect();
        for key in Self::record_type_keys(&type_name) {
            self.record_components
                .entry(key)
                .or_insert_with(HashMap::new)
                .extend(
                    component_map
                        .iter()
                        .map(|(name, ty)| (name.clone(), ty.clone())),
                );
        }
    }

    pub fn record_component_type(&self, type_name: &str, component: &str) -> Option<JavaType> {
        self.record_components
            .get(type_name)
            .and_then(|components| components.get(component))
            .cloned()
            .or_else(|| {
                type_name
                    .rsplit('.')
                    .next()
                    .filter(|simple| *simple != type_name)
                    .and_then(|simple| {
                        self.record_components
                            .get(simple)
                            .and_then(|components| components.get(component))
                            .cloned()
                    })
            })
            .or_else(|| {
                type_name
                    .rsplit('$')
                    .next()
                    .filter(|simple| *simple != type_name)
                    .and_then(|simple| {
                        self.record_components
                            .get(simple)
                            .and_then(|components| components.get(component))
                            .cloned()
                    })
            })
    }

    fn record_type_keys(type_name: &str) -> Vec<String> {
        let mut keys = vec![type_name.to_string()];
        if let Some(simple) = type_name.rsplit('.').next() {
            if simple != type_name {
                keys.push(simple.to_string());
            }
        }
        if let Some(simple) = type_name.rsplit('$').next() {
            if simple != type_name {
                keys.push(simple.to_string());
            }
        }
        keys
    }

    pub fn register_tuple_plan_usage(
        &mut self,
        span: Span,
        record_name: Option<String>,
        component_names: Vec<String>,
        component_spans: Vec<Option<Span>>,
        type_hints: Vec<Option<JavaType>>,
    ) {
        let key = SpanKey::from(&span);
        self.tuple_plan_usages.insert(
            key,
            TuplePlanRegistration {
                record_name,
                component_names,
                component_spans,
                type_hints,
            },
        );
    }

    pub fn tuple_component_metadata(
        &self,
        span: &Span,
        index: usize,
    ) -> Option<TupleComponentMetadata> {
        let key = SpanKey::from(span);
        let registration = self.tuple_plan_usages.get(&key)?;
        let field_name = registration
            .component_names
            .get(index)
            .cloned()
            .unwrap_or_else(|| format!("_{}", index + 1));
        let java_type = registration
            .type_hints
            .get(index)
            .and_then(|hint| hint.clone())
            .or_else(|| {
                registration
                    .record_name
                    .as_ref()
                    .and_then(|name| self.record_component_type(name, &field_name))
            })
            .unwrap_or_else(JavaType::object);
        let source_span = registration
            .component_spans
            .get(index)
            .and_then(|entry| entry.clone());
        Some(TupleComponentMetadata {
            field_name,
            java_type,
            source_span,
        })
    }

    pub fn tuple_record_java_type(&self, span: &Span) -> Option<JavaType> {
        let key = SpanKey::from(span);
        let registration = self.tuple_plan_usages.get(&key)?;
        registration
            .record_name
            .as_ref()
            .map(|name| JavaType::Reference {
                name: name.clone(),
                generic_args: vec![],
            })
    }

    pub fn register_tuple_return(&mut self, function: &str, record_name: &str, components: Vec<String>) {
        self.tuple_return_types.insert(
            function.to_string(),
            JavaType::Reference {
                name: record_name.to_string(),
                generic_args: vec![],
            },
        );
        self.tuple_return_components
            .entry(record_name.to_string())
            .or_insert(components);
    }

    pub fn tuple_return_type(&self, function: &str) -> Option<JavaType> {
        self.tuple_return_types.get(function).cloned()
    }

    pub fn tuple_record_components(&self, record: &str) -> Option<&Vec<String>> {
        self.tuple_return_components.get(record)
    }

    /// Registers a lowered method declaration so that later passes can resolve Java naming.
    pub fn bind_method_declaration(&mut self, method: &mut IrStatement, owner: Option<String>) {
        if let IrStatement::MethodDeclaration {
            name,
            java_name,
            parameters,
            primitive_return,
            return_type,
            modifiers,
            span,
            ..
        } = method
        {
            let canonical_java_name = java_name.clone().unwrap_or_else(|| name.clone());

            if java_name.is_none() {
                *java_name = Some(canonical_java_name.clone());
            }

            self.method_declarations.push(RegisteredMethodDeclaration {
                owner,
                name: name.clone(),
                java_name: canonical_java_name,
                parameter_types: parameters
                    .iter()
                    .map(|param| param.java_type.clone())
                    .collect(),
                return_type: return_type.clone(),
                is_static: modifiers.is_static,
                primitive_return: primitive_return.clone(),
                span: span.clone(),
            });
        }
    }

    /// Registers a lowered method call expression for subsequent Java name resolution.
    pub fn bind_method_call(
        &mut self,
        call: &mut IrExpression,
        owner: Option<String>,
        receiver_type: Option<JavaType>,
        argument_types: Vec<JavaType>,
    ) {
        let mut argument_types = argument_types;
        if let IrExpression::MethodCall {
            args,
            method_name,
            java_name,
            resolved_target,
            argument_style,
            java_type,
            span,
            ..
        } = call
        {
            if let Some(receiver) = receiver_type.as_ref() {
                if let Some(inferred) = infer_known_method_return_type(method_name, Some(receiver))
                {
                    *java_type = inferred;
                } else if let Some(inferred) = infer_map_method_return_type(method_name, receiver) {
                    *java_type = inferred;
                }

                complete_map_method_call(
                    method_name,
                    args,
                    &mut argument_types,
                    receiver,
                    java_type,
                );
            } else if let Some(inferred) = infer_known_method_return_type(method_name, None) {
                *java_type = inferred;
            }

            let canonical_java_name = java_name.clone().unwrap_or_else(|| method_name.clone());

            if java_name.is_none() {
                *java_name = Some(canonical_java_name.clone());
            }

            let target = resolved_target.get_or_insert_with(IrResolvedMethodTarget::default);
            if target.original_name.is_none() {
                target.original_name = Some(method_name.clone());
            }
            if target.java_name.is_none() {
                target.java_name = Some(canonical_java_name.clone());
            }
            if target.owner.is_none() {
                target.owner = owner.clone();
            }
            target.erased_parameters.clear();

            self.method_calls.push(RegisteredMethodCall {
                owner,
                original_name: method_name.clone(),
                java_name: canonical_java_name,
                receiver_type,
                argument_types,
                return_type: java_type.clone(),
                argument_style: *argument_style,
                span: span.clone(),
            });
        }
    }

    pub fn register_extension_method(
        &mut self,
        method_name: String,
        receiver_type: JavaType,
        parameter_types: Vec<JavaType>,
        return_type: JavaType,
    ) {
        if let Some(receiver_canonical) = Self::canonical_type_name(&receiver_type) {
            let metadata = ExtensionMetadata {
                receiver_canonical,
                parameter_types,
                return_type,
            };
            self.extension_registry
                .entry(method_name)
                .or_default()
                .push(metadata);
        }
    }

    pub fn lookup_extension_method(
        &self,
        method_name: &str,
        receiver_type: &JavaType,
        argument_count: usize,
    ) -> Option<&ExtensionMetadata> {
        let actual = Self::canonical_type_name(receiver_type)?;
        let candidates = self.extension_registry.get(method_name)?;

        let mut fallback = None;
        for candidate in candidates {
            if candidate.parameter_types.len() != argument_count + 1 {
                continue;
            }
            if candidate.receiver_canonical == actual {
                return Some(candidate);
            }
            if fallback.is_none()
                && Self::receiver_type_assignable(&actual, &candidate.receiver_canonical)
            {
                fallback = Some(candidate);
            }
        }
        fallback
    }

    pub fn push_extension_scope(&mut self, method_name: String, receiver_type: JavaType) {
        if let Some(receiver_canonical) = Self::canonical_type_name(&receiver_type) {
            self.extension_scope_stack.push(ExtensionScope {
                method_name,
                receiver_canonical,
            });
        }
    }

    pub fn pop_extension_scope(&mut self) {
        self.extension_scope_stack.pop();
    }

    pub fn current_extension_scope(&self) -> Option<&ExtensionScope> {
        self.extension_scope_stack.last()
    }

    pub fn is_current_extension_scope(&self, method_name: &str, receiver_type: &JavaType) -> bool {
        match self.current_extension_scope() {
            Some(scope) => {
                if let Some(actual) = Self::canonical_type_name(receiver_type) {
                    scope.method_name == method_name && scope.receiver_canonical == actual
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn canonical_type_name(java_type: &JavaType) -> Option<String> {
        match java_type {
            JavaType::Reference { name, .. } => Some(name.clone()),
            JavaType::Primitive(name) => Some(name.clone()),
            JavaType::Array {
                element_type,
                dimensions,
            } => Self::canonical_type_name(element_type).map(|base| {
                let mut result = base;
                for _ in 0..*dimensions {
                    result.push_str("[]");
                }
                result
            }),
            JavaType::Functional { interface_name, .. } => Some(interface_name.clone()),
            JavaType::Void => Some("void".to_string()),
            JavaType::Wildcard { .. } => None,
        }
    }

    fn receiver_type_assignable(actual: &str, expected: &str) -> bool {
        if actual == expected {
            return true;
        }

        match expected {
            "java.lang.Iterable" => matches!(
                actual,
                "java.util.Collection"
                    | "java.util.List"
                    | "java.util.Set"
                    | "java.util.Deque"
                    | "java.util.Queue"
                    | "java.util.LinkedList"
                    | "java.util.ArrayDeque"
                    | "java.util.ArrayList"
            ),
            "java.util.Collection" => matches!(
                actual,
                "java.util.List"
                    | "java.util.Set"
                    | "java.util.Deque"
                    | "java.util.Queue"
                    | "java.util.LinkedList"
                    | "java.util.ArrayDeque"
                    | "java.util.ArrayList"
            ),
            _ => false,
        }
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

    pub fn logging_options(&self) -> &LoggingOptions {
        self.logging_state.options()
    }

    pub fn logging_options_mut(&mut self) -> &mut LoggingOptions {
        self.logging_state.options_mut()
    }

    pub fn allocate_logger_field(&mut self, owner_hint: Option<String>) -> LoggerFieldId {
        self.logging_state.allocate_field(owner_hint)
    }

    pub fn logger_field_spec(&self, id: LoggerFieldId) -> Option<&LoggerFieldSpec> {
        self.logging_state.field(id)
    }

    pub fn take_logging_metadata(&mut self) -> LoggingMetadata {
        self.logging_state.take_metadata()
    }

    pub fn set_logging_framework(&mut self, framework: LoggingFrameworkKind) {
        self.logging_state.framework = framework;
    }

    pub fn logging_framework(&self) -> &LoggingFrameworkKind {
        &self.logging_state.framework
    }

    pub fn set_trace_context_enabled(&mut self, enabled: bool) {
        self.logging_state.trace_context = enabled;
    }

    pub fn trace_context_enabled(&self) -> bool {
        self.logging_state.trace_context
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

    /// Injects the resolved import plan to be consumed during lowering.
    pub fn set_resolved_imports(&mut self, imports: Vec<IrImport>) {
        let mut aliases = HashMap::new();
        for import in &imports {
            if let IrImportDetail::Type { fqcn } = &import.detail {
                let java_type = JavaType::Reference {
                    name: fqcn.clone(),
                    generic_args: vec![],
                };

                if let Some(alias) = &import.alias {
                    aliases.insert(alias.clone(), java_type.clone());
                } else if let Some(simple_name) = fqcn.rsplit('.').next() {
                    aliases.insert(simple_name.to_string(), java_type.clone());
                }
            }
        }
        self.import_aliases = aliases;
        self.planned_imports = imports;
    }

    /// Returns true when the context currently holds a resolved import plan.
    pub fn has_resolved_imports(&self) -> bool {
        !self.planned_imports.is_empty()
    }

    /// Takes ownership of the planned imports, leaving the context empty.
    pub fn take_resolved_imports(&mut self) -> Vec<IrImport> {
        std::mem::take(&mut self.planned_imports)
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

fn infer_known_method_return_type(
    method_name: &str,
    receiver_type: Option<&JavaType>,
) -> Option<JavaType> {
    match method_name {
        "toString" => Some(JavaType::string()),
        "codePointAt" => {
            if receiver_type.map(is_string_like_reference).unwrap_or(false) {
                Some(JavaType::int())
            } else {
                None
            }
        }
        "intValue" if receiver_type.map(is_number_like_reference).unwrap_or(false) => {
            Some(JavaType::int())
        }
        "longValue" if receiver_type.map(is_number_like_reference).unwrap_or(false) => {
            Some(JavaType::Primitive("long".to_string()))
        }
        "floatValue" if receiver_type.map(is_number_like_reference).unwrap_or(false) => {
            Some(JavaType::Primitive("float".to_string()))
        }
        "doubleValue" if receiver_type.map(is_number_like_reference).unwrap_or(false) => {
            Some(JavaType::Primitive("double".to_string()))
        }
        "shortValue" if receiver_type.map(is_number_like_reference).unwrap_or(false) => {
            Some(JavaType::Primitive("short".to_string()))
        }
        "byteValue" if receiver_type.map(is_number_like_reference).unwrap_or(false) => {
            Some(JavaType::Primitive("byte".to_string()))
        }
        "charValue" if receiver_type.map(is_number_like_reference).unwrap_or(false) => {
            Some(JavaType::Primitive("char".to_string()))
        }
        "booleanValue" if receiver_type.map(is_number_like_reference).unwrap_or(false) => {
            Some(JavaType::Primitive("boolean".to_string()))
        }
        "size"
            if receiver_type
                .map(is_collection_like_reference)
                .unwrap_or(false)
                || receiver_type.map(is_map_like_reference).unwrap_or(false) =>
        {
            Some(JavaType::int())
        }
        _ => None,
    }
}

fn is_string_like_reference(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => matches!(
            name.as_str(),
            "String" | "java.lang.String" | "CharSequence" | "java.lang.CharSequence"
        ),
        _ => false,
    }
}

fn is_number_like_reference(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => matches!(
            name.as_str(),
            "Number"
                | "java.lang.Number"
                | "Integer"
                | "java.lang.Integer"
                | "Long"
                | "java.lang.Long"
                | "Float"
                | "java.lang.Float"
                | "Double"
                | "java.lang.Double"
                | "Short"
                | "java.lang.Short"
                | "Byte"
                | "java.lang.Byte"
                | "Character"
                | "java.lang.Character"
                | "Boolean"
                | "java.lang.Boolean"
        ),
        _ => false,
    }
}

fn is_collection_like_reference(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => matches!(
            name.as_str(),
            "java.util.Collection"
                | "Collection"
                | "java.util.List"
                | "List"
                | "java.util.Set"
                | "Set"
                | "java.util.Queue"
                | "Queue"
                | "java.util.Deque"
                | "Deque"
                | "java.lang.Iterable"
                | "Iterable"
        ),
        _ => false,
    }
}

fn is_map_like_reference(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => matches!(name.as_str(), "java.util.Map" | "Map"),
        _ => false,
    }
}

fn infer_map_method_return_type(method_name: &str, receiver: &JavaType) -> Option<JavaType> {
    match method_name {
        "get" | "computeIfAbsent" | "computeIfPresent" | "compute" | "put" | "remove"
            if is_map_type(receiver) =>
        {
            map_value_type(receiver)
        }
        _ => None,
    }
}

fn complete_map_method_call(
    method_name: &str,
    args: &mut [IrExpression],
    argument_types: &mut Vec<JavaType>,
    receiver: &JavaType,
    return_type: &mut JavaType,
) {
    let Some((key_type, value_type)) = map_key_value_types(receiver) else {
        return;
    };

    match method_name {
        "get" | "containsKey" => {
            ensure_argument_type(args, argument_types, 0, &key_type);
        }
        "containsValue" => {
            ensure_argument_type(args, argument_types, 0, &value_type);
        }
        "put" | "putIfAbsent" => {
            ensure_argument_type(args, argument_types, 0, &key_type);
            ensure_argument_type(args, argument_types, 1, &value_type);
        }
        "remove" => {
            ensure_argument_type(args, argument_types, 0, &key_type);
            if args.len() > 1 {
                ensure_argument_type(args, argument_types, 1, &value_type);
            }
        }
        "computeIfAbsent" => {
            ensure_argument_type(args, argument_types, 0, &key_type);
            ensure_unary_lambda_argument(args, argument_types, 1, &key_type, &value_type);
            *return_type = value_type;
        }
        "compute" | "computeIfPresent" => {
            ensure_argument_type(args, argument_types, 0, &key_type);
            ensure_bifunction_lambda_argument(
                args,
                argument_types,
                1,
                &key_type,
                &value_type,
                &value_type,
            );
            *return_type = value_type;
        }
        "merge" => {
            ensure_argument_type(args, argument_types, 0, &key_type);
            ensure_argument_type(args, argument_types, 1, &value_type);
            ensure_bifunction_lambda_argument(
                args,
                argument_types,
                2,
                &value_type,
                &value_type,
                &value_type,
            );
            *return_type = value_type;
        }
        _ => {}
    }
}

fn is_map_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => {
            let simple = name.rsplit('.').next().unwrap_or(name);
            matches!(
                simple,
                "Map" | "HashMap" | "LinkedHashMap" | "ConcurrentHashMap"
            ) || matches!(
                name.as_str(),
                "java.util.Map"
                    | "java.util.HashMap"
                    | "java.util.LinkedHashMap"
                    | "java.util.concurrent.ConcurrentHashMap"
            )
        }
        _ => false,
    }
}

fn map_value_type(java_type: &JavaType) -> Option<JavaType> {
    match java_type {
        JavaType::Reference { generic_args, .. } => generic_args.get(1).cloned(),
        _ => None,
    }
}

fn map_key_value_types(java_type: &JavaType) -> Option<(JavaType, JavaType)> {
    match java_type {
        JavaType::Reference { generic_args, .. } if generic_args.len() >= 2 => {
            Some((generic_args[0].clone(), generic_args[1].clone()))
        }
        _ => None,
    }
}

fn ensure_argument_type(
    args: &mut [IrExpression],
    argument_types: &mut Vec<JavaType>,
    index: usize,
    expected: &JavaType,
) {
    if index >= args.len() || index >= argument_types.len() {
        return;
    }

    if argument_types[index] != *expected {
        argument_types[index] = expected.clone();
    }

    set_expression_type(&mut args[index], expected.clone());
}

fn ensure_lambda_param_names(param_names: &mut Vec<String>, required: usize) {
    if param_names.len() == required {
        return;
    }

    param_names.clear();
    for idx in 0..required {
        if idx == 0 {
            param_names.push("it".to_string());
        } else {
            param_names.push(format!("it{}", idx + 1));
        }
    }
}

fn ensure_unary_lambda_argument(
    args: &mut [IrExpression],
    argument_types: &mut Vec<JavaType>,
    index: usize,
    param_type: &JavaType,
    return_type: &JavaType,
) {
    if index >= args.len() || index >= argument_types.len() {
        return;
    }

    if let IrExpression::Lambda {
        functional_interface,
        param_types,
        java_type,
        param_names,
        ..
    } = &mut args[index]
    {
        ensure_lambda_param_names(param_names, 1);

        if param_types.len() != 1 || param_types[0] != *param_type {
            param_types.clear();
            param_types.push(param_type.clone());
        }

        let functional_type = JavaType::Functional {
            interface_name: functional_interface.clone(),
            param_types: vec![param_type.clone()],
            return_type: Box::new(return_type.clone()),
        };

        if *java_type != functional_type {
            *java_type = functional_type.clone();
        }

        if argument_types[index] != functional_type {
            argument_types[index] = functional_type;
        }
    } else {
        ensure_argument_type(args, argument_types, index, return_type);
    }
}

fn ensure_bifunction_lambda_argument(
    args: &mut [IrExpression],
    argument_types: &mut Vec<JavaType>,
    index: usize,
    first_param: &JavaType,
    second_param: &JavaType,
    return_type: &JavaType,
) {
    if index >= args.len() || index >= argument_types.len() {
        return;
    }

    if let IrExpression::Lambda {
        functional_interface,
        param_types,
        java_type,
        param_names,
        ..
    } = &mut args[index]
    {
        ensure_lambda_param_names(param_names, 2);

        let expected_params = [first_param.clone(), second_param.clone()];

        if param_types.len() != expected_params.len()
            || param_types[0] != expected_params[0]
            || param_types[1] != expected_params[1]
        {
            param_types.clear();
            param_types.extend(expected_params.into_iter());
        }

        let functional_type = JavaType::Functional {
            interface_name: functional_interface.clone(),
            param_types: vec![first_param.clone(), second_param.clone()],
            return_type: Box::new(return_type.clone()),
        };

        if *java_type != functional_type {
            *java_type = functional_type.clone();
        }

        if argument_types[index] != functional_type {
            argument_types[index] = functional_type;
        }
    } else {
        ensure_argument_type(args, argument_types, index, return_type);
    }
}

fn set_expression_type(expr: &mut IrExpression, java_type: JavaType) {
    match expr {
        IrExpression::Identifier { java_type: ty, .. }
        | IrExpression::MethodCall { java_type: ty, .. }
        | IrExpression::FieldAccess { java_type: ty, .. }
        | IrExpression::ArrayAccess { java_type: ty, .. }
        | IrExpression::Binary { java_type: ty, .. }
        | IrExpression::Unary { java_type: ty, .. }
        | IrExpression::Assignment { java_type: ty, .. }
        | IrExpression::Conditional { java_type: ty, .. }
        | IrExpression::Block { java_type: ty, .. }
        | IrExpression::ObjectCreation { java_type: ty, .. }
        | IrExpression::TupleLiteral { java_type: ty, .. }
        | IrExpression::SequencePipeline { java_type: ty, .. }
        | IrExpression::Switch { java_type: ty, .. }
        | IrExpression::NullSafeOperation { java_type: ty, .. }
        | IrExpression::CompletableFuture { java_type: ty, .. }
        | IrExpression::VirtualThread { java_type: ty, .. }
        | IrExpression::TryWithResources { java_type: ty, .. }
        | IrExpression::LogInvocation { java_type: ty, .. }
        | IrExpression::RegexCommand { java_type: ty, .. }
        | IrExpression::This { java_type: ty, .. }
        | IrExpression::Super { java_type: ty, .. } => {
            *ty = java_type;
        }
        IrExpression::Cast { target_type, .. } => {
            *target_type = java_type;
        }
        IrExpression::Lambda { .. }
        | IrExpression::Literal(_, _)
        | IrExpression::TextBlock { .. }
        | IrExpression::RegexPattern { .. }
        | IrExpression::ArrayCreation { .. }
        | IrExpression::StringFormat { .. }
        | IrExpression::InstanceOf { .. } => {}
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
            signature_hints: self.signature_hints.clone(),
            scope_stack: self.scope_stack.clone(),
            utility_classes: self.utility_classes.clone(),
            method_overloads: self.method_overloads.clone(),
            method_declarations: self.method_declarations.clone(),
            method_calls: self.method_calls.clone(),
            extension_registry: self.extension_registry.clone(),
            extension_scope_stack: self.extension_scope_stack.clone(),
            current_package: self.current_package.clone(),
            record_components: self.record_components.clone(),
            sample_options: self.sample_options.clone(),
            sequence_style_cache: self.sequence_style_cache.clone(),
            tuple_plan_usages: self.tuple_plan_usages.clone(),
            temp_counter: self.temp_counter,
            pool_state: self
                .pool_state
                .as_ref()
                .map(TransformPoolState::shallow_clone),
            when_strategies: self.when_strategies.clone(),
            planned_imports: self.planned_imports.clone(),
            import_aliases: self.import_aliases.clone(),
            logging_state: self.logging_state.clone(),
            tuple_return_types: self.tuple_return_types.clone(),
            tuple_return_components: self.tuple_return_components.clone(),
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
pub struct ExtensionMetadata {
    pub receiver_canonical: String,
    pub parameter_types: Vec<JavaType>,
    pub return_type: JavaType,
}

#[derive(Debug, Clone)]
pub struct ExtensionScope {
    pub method_name: String,
    pub receiver_canonical: String,
}

/// Metadata captured for each method declaration produced during lowering.
#[derive(Debug, Clone)]
pub struct RegisteredMethodDeclaration {
    pub owner: Option<String>,
    pub name: String,
    pub java_name: String,
    pub parameter_types: Vec<JavaType>,
    pub return_type: JavaType,
    pub is_static: bool,
    pub primitive_return: Option<PrimitiveReturnMetadata>,
    pub span: Span,
}

/// Metadata captured for each method call expression produced during lowering.
#[derive(Debug, Clone)]
pub struct RegisteredMethodCall {
    pub owner: Option<String>,
    pub original_name: String,
    pub java_name: String,
    pub receiver_type: Option<JavaType>,
    pub argument_types: Vec<JavaType>,
    pub return_type: JavaType,
    pub argument_style: CallArgumentStyle,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LoggingOptions {
    pub active_level: LogLevel,
    pub default_level: LogLevel,
}

impl Default for LoggingOptions {
    fn default() -> Self {
        Self {
            active_level: LogLevel::Info,
            default_level: LogLevel::Info,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct LoggingState {
    options: LoggingOptions,
    next_field_id: u32,
    fields: Vec<LoggerFieldSpec>,
    framework: LoggingFrameworkKind,
    trace_context: bool,
}

impl LoggingState {
    fn options(&self) -> &LoggingOptions {
        &self.options
    }

    fn options_mut(&mut self) -> &mut LoggingOptions {
        &mut self.options
    }

    fn allocate_field(&mut self, owner_hint: Option<String>) -> LoggerFieldId {
        let id = LoggerFieldId(self.next_field_id);
        self.next_field_id = self.next_field_id.wrapping_add(1);
        let spec = LoggerFieldSpec {
            id,
            owner_hint,
            field_name: "LOGGER".to_string(),
            class_id: None,
        };
        self.fields.push(spec);
        id
    }

    fn field(&self, id: LoggerFieldId) -> Option<&LoggerFieldSpec> {
        self.fields.iter().find(|spec| spec.id == id)
    }

    fn take_metadata(&mut self) -> LoggingMetadata {
        LoggingMetadata {
            logger_fields: std::mem::take(&mut self.fields),
            framework: self.framework.clone(),
            trace_context: self.trace_context,
        }
    }
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

#[derive(Debug, Clone)]
pub struct TupleComponentMetadata {
    pub field_name: String,
    pub java_type: JavaType,
    pub source_span: Option<Span>,
}

#[derive(Debug, Clone)]
struct TuplePlanRegistration {
    record_name: Option<String>,
    component_names: Vec<String>,
    component_spans: Vec<Option<Span>>,
    type_hints: Vec<Option<JavaType>>,
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
