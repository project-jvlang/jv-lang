// jv_ast/expression - Expression types and related constructs
use crate::json::JsonLiteral;
use crate::strings::MultilineStringLiteral;
use crate::types::*;
use serde::de::Deserializer;
use serde::{Deserialize, Serialize};

/// AST Expression node representing all types of expressions in jv
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    // Literals
    Literal(Literal, Span),

    /// Regex literal with raw + normalized pattern metadata.
    RegexLiteral(RegexLiteral),

    // Identifiers
    Identifier(String, Span),

    // Binary operations
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
        span: Span,
    },

    // Unary operations
    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
        span: Span,
    },

    // Function calls
    Call {
        function: Box<Expression>,
        args: Vec<Argument>,
        #[serde(default)]
        type_arguments: Vec<TypeAnnotation>,
        #[serde(default, alias = "argument_style")]
        argument_metadata: CallArgumentMetadata,
        span: Span,
    },

    // Member access: obj.property
    MemberAccess {
        object: Box<Expression>,
        property: String,
        span: Span,
    },

    // Null-safe member access: obj?.property
    NullSafeMemberAccess {
        object: Box<Expression>,
        property: String,
        span: Span,
    },

    // Array/Index access: arr[index]
    IndexAccess {
        object: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },

    // Null-safe index access: arr?[index]
    NullSafeIndexAccess {
        object: Box<Expression>,
        index: Box<Expression>,
        span: Span,
    },

    // Explicit type casts: expr as TargetType
    TypeCast {
        expr: Box<Expression>,
        target: TypeAnnotation,
        span: Span,
    },

    // String interpolation
    StringInterpolation {
        parts: Vec<StringPart>,
        span: Span,
    },

    // Multiline string literals
    MultilineString(MultilineStringLiteral),

    // JSON literals
    JsonLiteral(JsonLiteral),

    // When expressions
    When {
        expr: Option<Box<Expression>>,
        arms: Vec<WhenArm>,
        else_arm: Option<Box<Expression>>,
        #[serde(default)]
        implicit_end: Option<ImplicitWhenEnd>,
        span: Span,
    },

    // if expressions
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
        span: Span,
    },

    // Block expressions
    Block {
        statements: Vec<crate::Statement>,
        span: Span,
    },

    /// Doublebrace 初期化式。
    DoublebraceInit(DoublebraceInit),

    // Array literals
    Array {
        elements: Vec<Expression>,
        delimiter: SequenceDelimiter,
        span: Span,
    },

    // Lambda expressions
    Lambda {
        parameters: Vec<Parameter>,
        body: Box<Expression>,
        span: Span,
    },

    // Try expressions for error handling
    Try {
        body: Box<Expression>,
        #[serde(default)]
        catch_clauses: Vec<TryCatchClause>,
        #[serde(default)]
        finally_block: Option<Box<Expression>>,
        span: Span,
    },

    // This/super references
    This(Span),
    Super(Span),
}

/// Doublebrace 初期化式のASTノード。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DoublebraceInit {
    #[serde(default)]
    pub base: Option<Box<Expression>>,
    #[serde(default)]
    pub receiver_hint: Option<TypeAnnotation>,
    #[serde(default)]
    pub statements: Vec<crate::Statement>,
    pub span: Span,
}

/// Delimiter metadata describing how a sequence literal separated its elements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SequenceDelimiter {
    /// Elements are comma-delimited (default behaviour).
    Comma,
    /// Elements are separated through layout-aware trivia such as whitespace.
    Whitespace,
}

impl Default for SequenceDelimiter {
    fn default() -> Self {
        SequenceDelimiter::Comma
    }
}

/// Metadata describing how arguments were grouped for a function call.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CallArgumentStyle {
    /// Arguments are comma-delimited (current default).
    Comma,
    /// Arguments rely on layout-aware grouping (whitespace informed).
    Whitespace,
}

impl Default for CallArgumentStyle {
    fn default() -> Self {
        CallArgumentStyle::Comma
    }
}

/// Additional metadata captured for function call arguments.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CallArgumentMetadata {
    pub style: CallArgumentStyle,
    #[serde(default)]
    pub homogeneous_kind: Option<ArgumentElementKind>,
    #[serde(default)]
    pub separator_diagnostics: Vec<CallArgumentIssue>,
    #[serde(default)]
    pub used_commas: bool,
}

impl Default for CallArgumentMetadata {
    fn default() -> Self {
        Self {
            style: CallArgumentStyle::Comma,
            homogeneous_kind: None,
            separator_diagnostics: Vec::new(),
            used_commas: false,
        }
    }
}

impl CallArgumentMetadata {
    pub fn with_style(style: CallArgumentStyle) -> Self {
        Self {
            style,
            ..Self::default()
        }
    }
}

/// Element classification used when arguments were grouped without commas.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ArgumentElementKind {
    Number,
    String,
    Boolean,
    Identifier,
    Json,
    Lambda,
    Other,
}

/// Diagnostics emitted while normalizing argument separators.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CallArgumentIssue {
    pub message: String,
    #[serde(default)]
    pub span: Option<Span>,
}

impl<'de> Deserialize<'de> for CallArgumentMetadata {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum MetadataFormat {
            Full {
                #[serde(default)]
                style: Option<CallArgumentStyle>,
                #[serde(default)]
                homogeneous_kind: Option<ArgumentElementKind>,
                #[serde(default)]
                separator_diagnostics: Vec<CallArgumentIssue>,
                #[serde(default)]
                used_commas: Option<bool>,
            },
            Legacy(CallArgumentStyle),
        }

        match MetadataFormat::deserialize(deserializer)? {
            MetadataFormat::Full {
                style,
                homogeneous_kind,
                separator_diagnostics,
                used_commas,
            } => Ok(CallArgumentMetadata {
                style: style.unwrap_or_default(),
                homogeneous_kind,
                separator_diagnostics,
                used_commas: used_commas.unwrap_or(false),
            }),
            MetadataFormat::Legacy(style) => Ok(CallArgumentMetadata::with_style(style)),
        }
    }
}

/// Function call arguments (supports named arguments)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Argument {
    Positional(Expression),
    Named {
        name: String,
        value: Expression,
        span: Span,
    },
}

/// String interpolation parts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StringPart {
    Text(String),
    Expression(Expression),
}

/// When expression arms with pattern matching
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhenArm {
    pub pattern: Pattern,
    #[serde(default)]
    pub guard: Option<Expression>,
    pub body: Expression,
    pub span: Span,
}

/// Metadata describing implicit termination inserted for a `when` expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ImplicitWhenEnd {
    /// Represents an implicit `else -> Unit` branch materialised by later stages.
    Unit { span: Span },
}

/// Parameter property binding (e.g. primary constructor `val`/`var`).
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum ParameterProperty {
    /// No property binding; parameter is local to the callable.
    None,
    /// `val` parameter; exposes a read-only property.
    Val,
    /// `var` parameter; exposes a mutable property.
    Var,
}

impl Default for ParameterProperty {
    fn default() -> Self {
        Self::None
    }
}

/// Modifier flags attached to a function or lambda parameter.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParameterModifiers {
    #[serde(default)]
    pub property: ParameterProperty,
    #[serde(default)]
    pub is_mut: bool,
    #[serde(default)]
    pub is_ref: bool,
}

impl Default for ParameterModifiers {
    fn default() -> Self {
        Self {
            property: ParameterProperty::None,
            is_mut: false,
            is_ref: false,
        }
    }
}

/// Function parameters with default values and named parameter support
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub default_value: Option<Expression>,
    #[serde(default)]
    pub modifiers: ParameterModifiers,
    pub span: Span,
}

/// Catch clause used within try expressions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TryCatchClause {
    pub parameter: Option<Parameter>,
    pub body: Box<Expression>,
    pub span: Span,
}

impl Expression {
    /// Doublebrace 初期化式であれば参照を返す。
    pub fn as_doublebrace_init(&self) -> Option<&DoublebraceInit> {
        match self {
            Expression::DoublebraceInit(node) => Some(node),
            _ => None,
        }
    }

    /// Doublebrace 初期化式であれば可変参照を返す。
    pub fn as_doublebrace_init_mut(&mut self) -> Option<&mut DoublebraceInit> {
        match self {
            Expression::DoublebraceInit(node) => Some(node),
            _ => None,
        }
    }
}
