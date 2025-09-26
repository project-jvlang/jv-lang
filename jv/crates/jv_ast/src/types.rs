// jv_ast/types - Basic types, operators, and position information
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

/// Annotation attached to declarations (e.g., @Sample)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Annotation {
    pub name: String,
    #[serde(default)]
    pub arguments: Vec<crate::expression::AnnotationArgument>,
    pub span: Span,
}
