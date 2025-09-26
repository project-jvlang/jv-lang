use std::collections::HashMap;

use crate::types::{
    DataFormat, JavaType, MethodOverload, SampleMode, StaticMethodCall, UtilityClass,
};
use jv_ast::Span;
use std::path::PathBuf;
use std::time::Duration;

/// Transformation context for desugaring
#[derive(Debug, Clone)]
pub struct TransformContext {
    /// Type information gathered from analysis
    pub type_info: HashMap<String, JavaType>,
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
            scope_stack: vec![HashMap::new()],
            utility_classes: Vec::new(),
            method_overloads: Vec::new(),
            extension_methods: HashMap::new(),
            current_package: None,
            sample_options: SampleOptions::default(),
            sequence_style_cache: SequenceStyleCache::with_capacity(),
            temp_counter: 0,
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
}

// Helper implementations
impl Default for TransformContext {
    fn default() -> Self {
        Self::new()
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
