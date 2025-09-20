// jv_ast/expression - Expression types and related constructs
use crate::types::*;
use serde::{Deserialize, Serialize};

/// AST Expression node representing all types of expressions in jv
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    // Literals
    Literal(Literal, Span),

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

    // String interpolation
    StringInterpolation {
        parts: Vec<StringPart>,
        span: Span,
    },

    // When expressions
    When {
        expr: Option<Box<Expression>>,
        arms: Vec<WhenArm>,
        else_arm: Option<Box<Expression>>,
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

    // Array literals
    Array {
        elements: Vec<Expression>,
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
        expr: Box<Expression>,
        span: Span,
    },

    // This/super references
    This(Span),
    Super(Span),
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
    pub body: Expression,
    pub span: Span,
}

/// Function parameters with default values and named parameter support
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub default_value: Option<Expression>,
    pub span: Span,
}
