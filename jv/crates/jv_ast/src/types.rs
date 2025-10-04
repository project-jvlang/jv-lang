// jv_ast/types - Basic types, operators, and position information
use crate::annotation::Annotation;
use serde::{Deserialize, Serialize};

/// Position information for AST nodes
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Span {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl Span {
    pub fn new(start_line: usize, start_column: usize, end_line: usize, end_column: usize) -> Self {
        Self {
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }

    pub fn dummy() -> Self {
        Self::default()
    }
}

/// Literal values
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    String(String),
    Number(String), // Keep as string for precision
    Boolean(bool),
    Null,
    Character(char),
}

/// Binary operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    // Comparison
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    // Logical
    And,
    Or,
    // Null safety
    Elvis, // ?:
    // Range operators
    RangeExclusive,
    RangeInclusive,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    // Assignment compound
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,    // !
    Minus,  // -
    Plus,   // +
    BitNot, // ~
}

/// Type annotations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeAnnotation {
    Simple(String),
    Nullable(Box<TypeAnnotation>),
    Generic {
        name: String,
        type_args: Vec<TypeAnnotation>,
    },
    Function {
        params: Vec<TypeAnnotation>,
        return_type: Box<TypeAnnotation>,
    },
    Array(Box<TypeAnnotation>),
}

/// Qualified name used for traits, capabilities, and other declaration references.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct QualifiedName {
    pub segments: Vec<String>,
    pub span: Span,
}

impl QualifiedName {
    pub fn new(segments: Vec<String>, span: Span) -> Self {
        Self { segments, span }
    }

    pub fn simple_name(&self) -> Option<&str> {
        self.segments.last().map(String::as_str)
    }

    pub fn qualified(&self) -> String {
        self.segments.join(".")
    }
}

/// Additional hints used when resolving capabilities.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct CapabilityHints {
    #[serde(default)]
    pub preferred_impl: Option<String>,
    #[serde(default)]
    pub inline_only: bool,
}

/// Capability requirement analogous to type class constraints.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapabilityRequirement {
    pub name: QualifiedName,
    pub target: TypeAnnotation,
    #[serde(default)]
    pub hints: CapabilityHints,
    pub span: Span,
}

/// Function signature-style predicate for advanced constraints.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionConstraintSignature {
    pub parameters: Vec<TypeAnnotation>,
    pub return_type: Option<TypeAnnotation>,
    pub span: Span,
}

/// where句で指定される制約の集合。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
    pub span: Span,
}

/// where句内の個別制約。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum WherePredicate {
    TraitBound {
        type_param: String,
        trait_name: QualifiedName,
        type_args: Vec<TypeAnnotation>,
        span: Span,
    },
    Capability {
        type_param: String,
        capability: CapabilityRequirement,
        span: Span,
    },
    FunctionSignature {
        type_param: String,
        signature: FunctionConstraintSignature,
        span: Span,
    },
}

impl WherePredicate {
    pub fn span(&self) -> &Span {
        match self {
            WherePredicate::TraitBound { span, .. }
            | WherePredicate::Capability { span, .. }
            | WherePredicate::FunctionSignature { span, .. } => span,
        }
    }
}

/// Pattern matching constructs
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Pattern {
    Literal(Literal, Span),
    Identifier(String, Span),
    Wildcard(Span), // _
    Constructor {
        name: String,
        patterns: Vec<Pattern>,
        span: Span,
    },
    Range {
        start: Box<crate::Expression>,
        end: Box<crate::Expression>,
        inclusive_end: bool,
        span: Span,
    },
    Guard {
        pattern: Box<Pattern>,
        condition: crate::Expression,
        span: Span,
    },
}

/// Visibility modifiers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Private,
    Internal,
    Protected,
}

/// Class/data class modifiers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Modifiers {
    pub visibility: Visibility,
    pub is_abstract: bool,
    pub is_final: bool,
    pub is_static: bool,
    pub is_override: bool,
    pub is_open: bool,
    #[serde(default)]
    pub annotations: Vec<Annotation>,
}

impl Default for Modifiers {
    fn default() -> Self {
        Self {
            visibility: Visibility::Private,
            is_abstract: false,
            is_final: false,
            is_static: false,
            is_override: false,
            is_open: false,
            annotations: Vec::new(),
        }
    }
}
