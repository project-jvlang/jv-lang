use jv_ast::Span;

// Error types
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum TransformError {
    #[error("Type inference failed: {message}")]
    TypeInferenceError { message: String, span: Span },

    #[error("Unsupported construct: {construct}")]
    UnsupportedConstruct { construct: String, span: Span },

    #[error("Invalid pattern: {message}")]
    InvalidPattern { message: String, span: Span },

    #[error("Null safety violation: {message}")]
    NullSafetyError { message: String, span: Span },

    #[error("Scope resolution error: {message}")]
    ScopeError { message: String, span: Span },

    #[error("Invalid default parameter: {message}")]
    DefaultParameterError { message: String, span: Span },

    #[error("Extension function error: {message}")]
    ExtensionFunctionError { message: String, span: Span },

    #[error("Concurrency construct error: {message}")]
    ConcurrencyError { message: String, span: Span },

    #[error("Resource management error: {message}")]
    ResourceManagementError { message: String, span: Span },
}

impl TransformError {
    pub fn span(&self) -> &Span {
        match self {
            TransformError::TypeInferenceError { span, .. }
            | TransformError::UnsupportedConstruct { span, .. }
            | TransformError::InvalidPattern { span, .. }
            | TransformError::NullSafetyError { span, .. }
            | TransformError::ScopeError { span, .. }
            | TransformError::DefaultParameterError { span, .. }
            | TransformError::ExtensionFunctionError { span, .. }
            | TransformError::ConcurrencyError { span, .. }
            | TransformError::ResourceManagementError { span, .. } => span,
        }
    }
}
