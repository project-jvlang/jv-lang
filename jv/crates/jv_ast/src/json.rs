use crate::expression::SequenceDelimiter;
use crate::types::Span;
use serde::{Deserialize, Serialize};

/// Identifier assigned to an inferred JSON schema within the compiler pipeline.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SchemaId(pub String);

/// Numeric grouping metadata propagated from the lexer into JSON number literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum NumberGrouping {
    /// No explicit grouping characters were present.
    None,
    /// Digits were grouped using commas (1,234 style).
    Comma,
    /// Digits were grouped using underscores (1_234 style).
    Underscore,
    /// A mix of grouping characters was observed.
    Mixed,
}

impl Default for NumberGrouping {
    fn default() -> Self {
        NumberGrouping::None
    }
}

/// Classification for JSON comments that are preserved as trivia.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum JsonCommentKind {
    /// Line comments introduced with `//`.
    Line,
    /// Block comments delimited by `/* ... */`.
    Block,
}

/// Comment trivia associated with JSON literals.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JsonComment {
    pub kind: JsonCommentKind,
    pub text: String,
    pub span: Span,
}

/// Key-value entry inside a JSON object literal.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JsonEntry {
    pub key: String,
    #[serde(default)]
    pub comments: Vec<JsonComment>,
    pub value: JsonValue,
    pub span: Span,
}

/// JSON literal value captured by the AST.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum JsonValue {
    Object {
        entries: Vec<JsonEntry>,
        span: Span,
    },
    Array {
        elements: Vec<JsonValue>,
        #[serde(default)]
        delimiter: SequenceDelimiter,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    Number {
        literal: String,
        #[serde(default)]
        grouping: NumberGrouping,
        span: Span,
    },
    Boolean {
        value: bool,
        span: Span,
    },
    Null {
        span: Span,
    },
}

/// Root node for JSON literals.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JsonLiteral {
    pub value: JsonValue,
    #[serde(default)]
    pub leading_comments: Vec<JsonComment>,
    #[serde(default)]
    pub trailing_comments: Vec<JsonComment>,
    pub span: Span,
    #[serde(default)]
    pub inferred_schema: Option<SchemaId>,
}
