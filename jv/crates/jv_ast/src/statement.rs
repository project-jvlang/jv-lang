// jv_ast/statement - Statement types and program structure
use crate::annotation::{Annotation, AnnotationArgument};
use crate::binding_pattern::BindingPatternKind;
use crate::comments::*;
use crate::expression::*;
use crate::types::*;
use serde::{Deserialize, Serialize};

/// Origin of a `val` binding indicating how the declaration was introduced.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValBindingOrigin {
    /// Declaration explicitly used the `val` keyword.
    ExplicitKeyword,
    /// Declaration was inferred without a type annotation.
    Implicit,
    /// Declaration was inferred while providing an explicit type annotation.
    ImplicitTyped,
}

impl Default for ValBindingOrigin {
    fn default() -> Self {
        ValBindingOrigin::ExplicitKeyword
    }
}

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

/// Loop binding metadata describing the identifier used in iteration and optional annotation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LoopBinding {
    pub name: String,
    #[serde(default)]
    pub pattern: Option<BindingPatternKind>,
    pub type_annotation: Option<TypeAnnotation>,
    pub span: Span,
}

/// Numeric range bounds for `for-in` loops.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NumericRangeLoop {
    pub start: Expression,
    pub end: Expression,
    pub inclusive: bool,
    pub span: Span,
}

/// Loop strategy describing how the iterable expression should be interpreted.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LoopStrategy {
    NumericRange(NumericRangeLoop),
    Iterable,
    LazySequence { needs_cleanup: bool },
    Unknown,
}

/// Test declaration metadata captured at the AST level.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TestDeclaration {
    /// Display name provided by the DSL (e.g., `"renders dataset"`).
    pub display_name: String,
    /// Normalized identifier applied during lowering.
    #[serde(default)]
    pub normalized: Option<String>,
    /// Dataset bound to the test (inline rows or @Sample reference).
    #[serde(default)]
    pub dataset: Option<TestDataset>,
    /// Parameter bindings exposed to the test body.
    #[serde(default)]
    pub parameters: Vec<TestParameter>,
    /// Attached annotations such as lifecycle hooks.
    #[serde(default)]
    pub annotations: Vec<Annotation>,
    /// Body expression to execute.
    pub body: Expression,
    /// Source span for the entire declaration.
    pub span: Span,
}

/// Dataset specification for a test declaration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TestDataset {
    /// Inline array dataset rows.
    InlineArray {
        rows: Vec<TestDatasetRow>,
        span: Span,
    },
    /// External dataset reference via @Sample annotation.
    Sample(TestSampleMetadata),
}

/// Single row within an inline dataset.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TestDatasetRow {
    /// Values within the row.
    pub values: Vec<Expression>,
    /// Span for the row.
    pub span: Span,
}

/// Metadata describing an @Sample dataset reference.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TestSampleMetadata {
    /// Source path passed to @Sample.
    pub source: String,
    /// Additional annotation arguments.
    #[serde(default)]
    pub arguments: Vec<AnnotationArgument>,
    /// Span covering the annotation usage.
    pub span: Span,
}

/// Describes a single parameter exposed to the test body.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TestParameter {
    /// Pattern describing the binding (identifier, tuple, etc.).
    pub pattern: BindingPatternKind,
    /// Optional type annotation for the binding.
    #[serde(default)]
    pub type_annotation: Option<TypeAnnotation>,
    /// Span covering the entire parameter declaration.
    pub span: Span,
}

/// Structured representation of a `for-in` loop.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ForInStatement {
    pub binding: LoopBinding,
    pub iterable: Expression,
    pub strategy: LoopStrategy,
    pub body: Box<Expression>,
    pub span: Span,
}

/// `:=` および `->` による単位定義の関係演算子。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnitRelation {
    /// `:=` による値定義。
    DefinitionAssign,
    /// `->` による変換先参照。
    ConversionArrow,
}

/// 単位依存定義1件を表現する構造体。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitDependency {
    /// 左辺の単位名。
    pub name: String,
    /// 使用された関係演算子。
    pub relation: UnitRelation,
    /// `:=` の右辺に出現した式。
    #[serde(default)]
    pub value: Option<Expression>,
    /// `->` の右側に表記された単位名。
    #[serde(default)]
    pub target: Option<String>,
    /// ソース位置。
    pub span: Span,
}

/// 単位変換ブロックの種別。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnitConversionKind {
    /// `@Conversion` ブロック。
    Conversion,
    /// `@ReverseConversion` ブロック。
    ReverseConversion,
}

/// `@Conversion` / `@ReverseConversion` ブロックを表す構造体。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitConversionBlock {
    pub kind: UnitConversionKind,
    #[serde(default)]
    pub body: Vec<Statement>,
    pub span: Span,
}

/// 単位定義ブロックに格納されるメンバー列挙体。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnitTypeMember {
    Dependency(UnitDependency),
    Conversion(UnitConversionBlock),
    NestedStatement(Box<Statement>),
}

/// トップレベル単位定義を表現する構造体。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitTypeDefinition {
    /// カテゴリ名（例: 「単位系」「Currency」）。
    pub category: String,
    /// 基底型注釈。
    pub base_type: TypeAnnotation,
    /// 単位シンボル情報。
    pub name: UnitSymbol,
    /// ブロック内のメンバー。
    #[serde(default)]
    pub members: Vec<UnitTypeMember>,
    /// ソース位置。
    pub span: Span,
}

/// Statements in jv language
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    // Comments
    Comment(CommentStatement),

    /// 単位型定義。
    UnitTypeDefinition(UnitTypeDefinition),

    // Variable declarations
    ValDeclaration {
        name: String,
        #[serde(default)]
        binding: Option<BindingPatternKind>,
        type_annotation: Option<TypeAnnotation>,
        initializer: Expression,
        modifiers: Modifiers,
        #[serde(default)]
        origin: ValBindingOrigin,
        span: Span,
    },

    VarDeclaration {
        name: String,
        #[serde(default)]
        binding: Option<BindingPatternKind>,
        type_annotation: Option<TypeAnnotation>,
        initializer: Option<Expression>,
        modifiers: Modifiers,
        span: Span,
    },

    // Function declarations
    FunctionDeclaration {
        name: String,
        type_parameters: Vec<String>,
        #[serde(default)]
        generic_signature: Option<GenericSignature>,
        where_clause: Option<WhereClause>,
        parameters: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        #[serde(default)]
        primitive_return: Option<PrimitiveReturnMetadata>,
        body: Box<Expression>,
        modifiers: Modifiers,
        span: Span,
    },

    // Test declarations
    TestDeclaration(TestDeclaration),

    // Class declarations
    ClassDeclaration {
        name: String,
        type_parameters: Vec<String>,
        #[serde(default)]
        generic_signature: Option<GenericSignature>,
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
        #[serde(default)]
        generic_signature: Option<GenericSignature>,
        is_mutable: bool,
        modifiers: Modifiers,
        span: Span,
    },

    // Interface declarations
    InterfaceDeclaration {
        name: String,
        type_parameters: Vec<String>,
        #[serde(default)]
        generic_signature: Option<GenericSignature>,
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

    // Throw statements
    Throw {
        expr: Expression,
        span: Span,
    },

    // Assignment statements
    Assignment {
        target: Expression, // Could be identifier or member access
        #[serde(default)]
        binding_pattern: Option<BindingPatternKind>,
        value: Expression,
        span: Span,
    },

    // For-in loops with strategy metadata
    ForIn(ForInStatement),

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
