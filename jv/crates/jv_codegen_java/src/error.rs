use jv_ast::Span;
use thiserror::Error;

/// Error variants produced while lowering IR to Java source.
#[derive(Debug, Clone, PartialEq, Error)]
pub enum CodeGenError {
    #[error("Unsupported IR construct: {construct}")]
    UnsupportedConstruct {
        construct: String,
        span: Option<Span>,
    },

    #[error("Type generation error: {message}")]
    TypeGenerationError { message: String, span: Option<Span> },

    #[error("Invalid method signature: {message}")]
    InvalidMethodSignature { message: String, span: Option<Span> },

    #[error("Null safety error: {message}")]
    NullSafetyError { message: String, span: Option<Span> },

    #[error("Generic type error: {message}")]
    GenericTypeError { message: String, span: Option<Span> },

    #[error("Import resolution error: {message}")]
    ImportError { message: String },

    #[error("Source formatting error: {message}")]
    FormattingError { message: String },

    #[error("Java 25 feature error: {message}")]
    Java25FeatureError { message: String, span: Option<Span> },

    #[error("Record generation error: {message}")]
    RecordGenerationError { message: String, span: Option<Span> },

    #[error("Pattern matching error: {message}")]
    PatternMatchingError { message: String, span: Option<Span> },

    #[error("Unsupported type: {type_name} - {message}")]
    UnsupportedType { type_name: String, message: String },

    #[error("Invalid generic arity: expected {expected}, got {actual} for type {type_name}")]
    InvalidGenericArity {
        expected: usize,
        actual: usize,
        type_name: String,
    },

    #[error("Invalid switch cases: {message}")]
    InvalidSwitchCases { message: String },

    #[error("Invalid pattern: {message}")]
    InvalidPattern { message: String },
}
