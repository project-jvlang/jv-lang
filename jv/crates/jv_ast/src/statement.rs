// jv_ast/statement - Statement types and program structure
use crate::expression::*;
use crate::types::*;
use serde::{Deserialize, Serialize};

/// Class/interface property
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Property {
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
    pub modifiers: Modifiers,
    pub getter: Option<Box<Expression>>,
    pub setter: Option<Box<Expression>>,
    pub span: Span,
}

/// Extension function context
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExtensionFunction {
    pub receiver_type: TypeAnnotation,
    pub function: Box<Statement>, // Must be FunctionDeclaration
    pub span: Span,
}

/// Async/spawn constructs
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConcurrencyConstruct {
    Spawn { body: Box<Expression>, span: Span },
    Async { body: Box<Expression>, span: Span },
    Await { expr: Box<Expression>, span: Span },
}

/// Resource management constructs
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ResourceManagement {
    Use {
        resource: Box<Expression>,
        body: Box<Expression>,
        span: Span,
    },
    Defer {
        body: Box<Expression>,
        span: Span,
    },
}

/// Statements in jv language
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    // Variable declarations
    ValDeclaration {
        name: String,
        type_annotation: Option<TypeAnnotation>,
        initializer: Expression,
        modifiers: Modifiers,
        span: Span,
    },

    VarDeclaration {
        name: String,
        type_annotation: Option<TypeAnnotation>,
        initializer: Option<Expression>,
        modifiers: Modifiers,
        span: Span,
    },

    // Function declarations
    FunctionDeclaration {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        body: Box<Expression>,
        modifiers: Modifiers,
        span: Span,
    },

    // Class declarations
    ClassDeclaration {
        name: String,
        type_parameters: Vec<String>,
        superclass: Option<TypeAnnotation>,
        interfaces: Vec<TypeAnnotation>,
        properties: Vec<Property>,
        methods: Vec<Box<Statement>>, // Must be FunctionDeclaration
        modifiers: Modifiers,
        span: Span,
    },

    // Data class declarations (immutable by default)
    DataClassDeclaration {
        name: String,
        parameters: Vec<Parameter>,
        type_parameters: Vec<String>,
        is_mutable: bool,
        modifiers: Modifiers,
        span: Span,
    },

    // Interface declarations
    InterfaceDeclaration {
        name: String,
        type_parameters: Vec<String>,
        superinterfaces: Vec<TypeAnnotation>,
        methods: Vec<Box<Statement>>, // Abstract function declarations
        properties: Vec<Property>,
        modifiers: Modifiers,
        span: Span,
    },

    // Extension functions
    ExtensionFunction(ExtensionFunction),

    // Expression statements
    Expression {
        expr: Expression,
        span: Span,
    },

    // Return statements
    Return {
        value: Option<Expression>,
        span: Span,
    },

    // Assignment statements
    Assignment {
        target: Expression, // Could be identifier or member access
        value: Expression,
        span: Span,
    },

    // While loops
    While {
        condition: Expression,
        body: Box<Expression>,
        span: Span,
    },

    // For loops
    For {
        variable: String,
        iterable: Expression,
        body: Box<Expression>,
        span: Span,
    },

    // Break/continue
    Break(Span),
    Continue(Span),

    // Import statements
    Import {
        path: String,
        alias: Option<String>,
        is_wildcard: bool,
        span: Span,
    },

    // Package declaration
    Package {
        name: String,
        span: Span,
    },

    // Concurrency constructs
    Concurrency(ConcurrencyConstruct),

    // Resource management
    ResourceManagement(ResourceManagement),
}

/// Top-level program
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub package: Option<String>,
    pub imports: Vec<Statement>, // Must be Import statements
    pub statements: Vec<Statement>,
    pub span: Span,
}
