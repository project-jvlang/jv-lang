// jv_ir - Intermediate representation for desugaring jv language constructs
use crate::sequence_pipeline::SequencePipeline;
use jv_ast::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::time::Duration;
use thiserror::Error;

/// Java-compatible type representation after desugaring
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum JavaType {
    /// Primitive types: int, boolean, char, etc.
    Primitive(String),
    /// Reference types: String, Object, custom classes
    Reference {
        name: String,
        generic_args: Vec<JavaType>,
    },
    /// Array types: int[], String[][]
    Array {
        element_type: Box<JavaType>,
        dimensions: usize,
    },
    /// Function types represented as functional interfaces
    Functional {
        interface_name: String,
        param_types: Vec<JavaType>,
        return_type: Box<JavaType>,
    },
    /// Wildcard generic (e.g., `? extends Number`).
    Wildcard {
        kind: JavaWildcardKind,
        bound: Option<Box<JavaType>>,
    },
    /// Void type
    Void,
}

/// Variance kind for Java wildcards.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum JavaWildcardKind {
    Unbounded,
    Extends,
    Super,
}

/// Desugared expressions - all jv sugar removed
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrExpression {
    // Basic expressions that map directly to Java
    Literal(Literal, Span),
    /// Compiled regular expression pattern handle.
    RegexPattern {
        pattern: String,
        java_type: JavaType,
        span: Span,
    },
    Identifier {
        name: String,
        java_type: JavaType,
        span: Span,
    },

    // Method calls (function calls become method calls or static calls)
    MethodCall {
        receiver: Option<Box<IrExpression>>, // None for static calls
        method_name: String,
        args: Vec<IrExpression>,
        argument_style: CallArgumentStyle,
        java_type: JavaType,
        span: Span,
    },

    // Field access
    FieldAccess {
        receiver: Box<IrExpression>,
        field_name: String,
        java_type: JavaType,
        span: Span,
    },

    // Array access
    ArrayAccess {
        array: Box<IrExpression>,
        index: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Binary operations
    Binary {
        left: Box<IrExpression>,
        op: BinaryOp,
        right: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Unary operations
    Unary {
        op: UnaryOp,
        operand: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Assignment
    Assignment {
        target: Box<IrExpression>,
        value: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Conditional expression (ternary)
    Conditional {
        condition: Box<IrExpression>,
        then_expr: Box<IrExpression>,
        else_expr: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Block expression
    Block {
        statements: Vec<IrStatement>,
        java_type: JavaType,
        span: Span,
    },

    // Array creation
    ArrayCreation {
        element_type: JavaType,
        dimensions: Vec<Option<IrExpression>>, // None for unsized dimensions
        initializer: Option<Vec<IrExpression>>,
        delimiter: SequenceDelimiter,
        span: Span,
    },

    // Object creation (new Constructor(args))
    ObjectCreation {
        class_name: String,
        generic_args: Vec<JavaType>,
        args: Vec<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Lambda expression (desugared to anonymous class or method reference)
    Lambda {
        functional_interface: String,
        param_names: Vec<String>,
        param_types: Vec<JavaType>,
        body: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Sequence pipeline expression constructed from Kotlin-style collection chains.
    SequencePipeline {
        pipeline: SequencePipeline,
        java_type: JavaType,
        span: Span,
    },

    // Switch expression (Java 14+ switch expressions)
    Switch {
        discriminant: Box<IrExpression>,
        cases: Vec<IrSwitchCase>,
        java_type: JavaType,
        #[serde(default)]
        implicit_end: Option<IrImplicitWhenEnd>,
        #[serde(default)]
        strategy_description: Option<String>,
        span: Span,
    },

    // Cast expression
    Cast {
        expr: Box<IrExpression>,
        target_type: JavaType,
        span: Span,
    },

    // instanceof check
    InstanceOf {
        expr: Box<IrExpression>,
        target_type: JavaType,
        span: Span,
    },

    // This reference
    This {
        java_type: JavaType,
        span: Span,
    },

    // Super reference
    Super {
        java_type: JavaType,
        span: Span,
    },

    // Null-safe operations become explicit null checks
    NullSafeOperation {
        expr: Box<IrExpression>,
        operation: Box<IrExpression>, // The operation to perform if not null
        default_value: Option<Box<IrExpression>>, // Default if null
        java_type: JavaType,
        span: Span,
    },

    // String formatting (from string interpolation)
    StringFormat {
        format_string: String,
        args: Vec<IrExpression>,
        span: Span,
    },

    // CompletableFuture operations (from async/await)
    CompletableFuture {
        operation: CompletableFutureOp,
        args: Vec<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Virtual thread operations (from spawn)
    VirtualThread {
        operation: VirtualThreadOp,
        args: Vec<IrExpression>,
        java_type: JavaType,
        span: Span,
    },

    // Try-with-resources (from use blocks)
    TryWithResources {
        resources: Vec<IrResource>,
        body: Box<IrExpression>,
        java_type: JavaType,
        span: Span,
    },
}

/// Sample data mode selected via @Sample annotation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SampleMode {
    Embed,
    Load,
}

/// Supported external data formats for @Sample.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DataFormat {
    Json,
    Csv,
    Tsv,
}

impl std::fmt::Display for DataFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataFormat::Json => write!(f, "JSON"),
            DataFormat::Csv => write!(f, "CSV"),
            DataFormat::Tsv => write!(f, "TSV"),
        }
    }
}

/// Source classification for fetched sample data.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SampleSourceKind {
    LocalFile,
    Http,
    S3,
    GitSsh,
    CachedFile,
    Inline,
}

/// Request parameters controlling sample data fetching.
#[derive(Debug, Clone)]
pub struct SampleFetchRequest {
    pub source: String,
    pub base_dir: Option<PathBuf>,
    pub allow_network: bool,
    pub expected_sha256: Option<String>,
    pub max_bytes: Option<u64>,
    pub cache_dir: Option<PathBuf>,
    pub aws_cli_path: Option<PathBuf>,
    pub git_cli_path: Option<PathBuf>,
    pub timeout: Duration,
}

impl SampleFetchRequest {
    pub fn new(source: impl Into<String>) -> Self {
        Self {
            source: source.into(),
            base_dir: None,
            allow_network: false,
            expected_sha256: None,
            max_bytes: None,
            cache_dir: None,
            aws_cli_path: None,
            git_cli_path: None,
            timeout: Duration::from_secs(30),
        }
    }
}

/// Result payload containing fetched bytes and metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SampleFetchResult {
    pub bytes: Vec<u8>,
    pub sha256: String,
    pub source_kind: SampleSourceKind,
    pub origin: String,
    pub cache_path: Option<PathBuf>,
}

#[derive(Debug, Error)]
pub enum SampleFetchError {
    #[error("無効なURIです: {uri} ({message})")]
    InvalidUri { uri: String, message: String },

    #[error("ネットワークアクセスが許可されていません: {uri}")]
    NetworkDisabled { uri: String },

    #[error("HTTPリクエストに失敗しました: {uri} ({message})")]
    HttpRequest { uri: String, message: String },

    #[error("HTTPレスポンスエラーです: {uri} (status={status})")]
    HttpResponse { uri: String, status: u16 },

    #[error("ファイルアクセスに失敗しました: {path} ({error})")]
    Io {
        path: PathBuf,
        #[source]
        error: std::io::Error,
    },

    #[error("CLIが見つかりません: {command}")]
    CommandMissing { command: String },

    #[error("CLI実行に失敗しました: {command} (status={status:?}) {stderr}")]
    CommandFailed {
        command: String,
        status: Option<i32>,
        stderr: String,
    },

    #[error("サイズ上限を超えています (limit={limit} bytes, actual={actual} bytes)")]
    SizeLimitExceeded { limit: u64, actual: u64 },

    #[error("SHA256ハッシュが一致しません (expected={expected}, actual={actual})")]
    Sha256Mismatch { expected: String, actual: String },

    #[error("git+ssh URIにpathクエリがありません: {uri}")]
    GitPathMissing { uri: String },

    #[error("サポートされていないスキームです: {scheme}")]
    UnsupportedScheme { scheme: String },
}

/// サポートされているデータフォーマット。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PrimitiveType {
    String,
    Boolean,
    Integer,
    Long,
    BigInteger,
    Double,
    BigDecimal,
    Null,
}

impl PrimitiveType {
    pub fn is_integral(self) -> bool {
        matches!(
            self,
            PrimitiveType::Integer | PrimitiveType::Long | PrimitiveType::BigInteger
        )
    }

    pub fn is_decimal(self) -> bool {
        matches!(self, PrimitiveType::Double | PrimitiveType::BigDecimal)
    }

    pub fn is_numeric(self) -> bool {
        self.is_integral() || self.is_decimal()
    }
}

/// 推論されたスキーマ表現。
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Schema {
    Primitive(PrimitiveType),
    Object {
        fields: BTreeMap<String, Schema>,
        required: BTreeSet<String>,
    },
    Array {
        element_type: Box<Schema>,
    },
    Optional(Box<Schema>),
    Union(Vec<Schema>),
}

#[derive(Debug, Error)]
pub enum SchemaError {
    #[error("JSONデータの解析に失敗しました: {message}")]
    InvalidJson { message: String },

    #[error("{format}データの解析に失敗しました: {message}")]
    InvalidTabular { format: DataFormat, message: String },

    #[error("カラムヘッダーが見つかりません")]
    MissingHeaders,

    #[error("データセットが空です")]
    EmptyDataset,
}

/// Record field descriptor derived from schema.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SampleRecordField {
    pub name: String,
    pub java_type: JavaType,
    pub is_optional: bool,
}

/// Record description used for Java code generation of @Sample data structures.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SampleRecordDescriptor {
    pub name: String,
    pub fields: Vec<SampleRecordField>,
}

/// IR representation for @Sample annotated declarations.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrSampleDeclaration {
    pub variable_name: String,
    pub java_type: JavaType,
    pub format: DataFormat,
    pub mode: SampleMode,
    pub source: String,
    pub source_kind: SampleSourceKind,
    pub sha256: String,
    pub cache_path: Option<PathBuf>,
    pub limit_bytes: Option<u64>,
    pub embedded_data: Option<Vec<u8>>,
    pub schema: Schema,
    pub records: Vec<SampleRecordDescriptor>,
    pub root_record_name: Option<String>,
    pub span: Span,
}

/// Metadata describing implicitly inserted termination for when expressions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrImplicitWhenEnd {
    /// Represents an implicit `else -> Unit` branch.
    Unit { span: Span },
}

/// Switch cases for desugared when expressions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrSwitchCase {
    /// Patterns become case labels or guards
    pub labels: Vec<IrCaseLabel>,
    /// Guard condition (for pattern guards)
    pub guard: Option<IrExpression>,
    /// Case body
    pub body: IrExpression,
    pub span: Span,
}

/// Case labels in switch expressions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrCaseLabel {
    /// Literal case: case 42:
    Literal(Literal),
    /// Type pattern: case String s:
    TypePattern {
        type_name: String,
        variable: String,
        #[serde(default)]
        deconstruction: Option<IrDeconstructionPattern>,
    },
    /// Range pattern using guard semantics with explicit bounds
    Range {
        type_name: String,
        variable: String,
        lower: Box<IrExpression>,
        upper: Box<IrExpression>,
        inclusive_end: bool,
    },
    /// Default case
    Default,
}

/// Nested deconstruction pattern attached to a type pattern label.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrDeconstructionPattern {
    pub components: Vec<IrDeconstructionComponent>,
}

/// Component-level pattern used for nested destructuring.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrDeconstructionComponent {
    Wildcard,
    Binding {
        name: String,
    },
    Literal(Literal),
    Type {
        type_name: String,
        #[serde(default)]
        pattern: Option<Box<IrDeconstructionPattern>>,
    },
}

/// CompletableFuture operations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CompletableFutureOp {
    /// CompletableFuture.supplyAsync(supplier)
    SupplyAsync,
    /// future.thenApply(function)
    ThenApply,
    /// future.thenCompose(function)
    ThenCompose,
    /// future.get() or future.join()
    Get,
    /// CompletableFuture.completedFuture(value)
    CompletedFuture,
}

/// Virtual thread operations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VirtualThreadOp {
    /// Thread.ofVirtual().start(runnable)
    Start,
    /// Thread.ofVirtual().factory()
    Factory,
}

/// Resource for try-with-resources
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrResource {
    pub name: String,
    pub initializer: IrExpression,
    pub java_type: JavaType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrCommentKind {
    Line,
    Block,
}

/// Describes the resolved target of an import statement after analysis.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrImportDetail {
    /// Regular type import (`import package.Type;`).
    Type { fqcn: String },
    /// Package wildcard import (`import package.*;`).
    Package { name: String },
    /// Static member import (`import static owner.member;`).
    Static { owner: String, member: String },
    /// Java module import (`import module java.sql;`).
    Module { name: String },
}

/// Canonical IR representation of an import statement.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrImport {
    /// Path exactly as written by the user (may include aliases or wildcards).
    pub original: String,
    /// Optional alias supplied via `as` in the source import.
    pub alias: Option<String>,
    /// Resolved target describing how the import should be rendered downstream.
    pub detail: IrImportDetail,
    /// Java module dependency inferred for the import (if any).
    pub module_dependency: Option<String>,
    /// Source span for diagnostics and tooling attribution.
    pub span: Span,
}

/// Desugared statements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrStatement {
    // Comments propagated from AST
    Comment {
        kind: IrCommentKind,
        text: String,
        span: Span,
    },

    // Statement decorated with a trailing inline comment
    Commented {
        statement: Box<IrStatement>,
        comment: String,
        kind: IrCommentKind,
        comment_span: Span,
    },

    // Variable declarations with explicit types
    VariableDeclaration {
        name: String,
        java_type: JavaType,
        initializer: Option<IrExpression>,
        is_final: bool,
        modifiers: IrModifiers,
        span: Span,
    },

    // @Sample annotated declaration descriptor
    SampleDeclaration(IrSampleDeclaration),

    // Method declarations
    MethodDeclaration {
        name: String,
        #[serde(default)]
        type_parameters: Vec<IrTypeParameter>,
        parameters: Vec<IrParameter>,
        return_type: JavaType,
        body: Option<IrExpression>, // None for abstract methods
        modifiers: IrModifiers,
        throws: Vec<String>, // Exception types
        span: Span,
    },

    // Class declarations
    ClassDeclaration {
        name: String,
        type_parameters: Vec<IrTypeParameter>,
        superclass: Option<JavaType>,
        interfaces: Vec<JavaType>,
        fields: Vec<IrStatement>,         // FieldDeclaration statements
        methods: Vec<IrStatement>,        // MethodDeclaration statements
        nested_classes: Vec<IrStatement>, // ClassDeclaration statements
        modifiers: IrModifiers,
        span: Span,
    },

    // Interface declarations
    InterfaceDeclaration {
        name: String,
        type_parameters: Vec<IrTypeParameter>,
        superinterfaces: Vec<JavaType>,
        methods: Vec<IrStatement>, // MethodDeclaration statements (abstract)
        default_methods: Vec<IrStatement>, // MethodDeclaration statements (default)
        fields: Vec<IrStatement>,  // Field declarations (public static final)
        nested_types: Vec<IrStatement>,
        modifiers: IrModifiers,
        span: Span,
    },

    // Record declarations (from data classes)
    RecordDeclaration {
        name: String,
        type_parameters: Vec<IrTypeParameter>,
        components: Vec<IrRecordComponent>,
        interfaces: Vec<JavaType>,
        methods: Vec<IrStatement>, // Additional methods
        modifiers: IrModifiers,
        span: Span,
    },

    // Field declarations
    FieldDeclaration {
        name: String,
        java_type: JavaType,
        initializer: Option<IrExpression>,
        modifiers: IrModifiers,
        span: Span,
    },

    // Expression statements
    Expression {
        expr: IrExpression,
        span: Span,
    },

    // Return statements
    Return {
        value: Option<IrExpression>,
        span: Span,
    },

    // If statements
    If {
        condition: IrExpression,
        then_stmt: Box<IrStatement>,
        else_stmt: Option<Box<IrStatement>>,
        span: Span,
    },

    // While loops
    While {
        condition: IrExpression,
        body: Box<IrStatement>,
        span: Span,
    },

    // For loops (enhanced for)
    ForEach {
        variable: String,
        variable_type: JavaType,
        iterable: IrExpression,
        body: Box<IrStatement>,
        iterable_kind: IrForEachKind,
        span: Span,
    },

    // Traditional for loops
    For {
        init: Option<Box<IrStatement>>,
        condition: Option<IrExpression>,
        update: Option<IrExpression>,
        body: Box<IrStatement>,
        metadata: Option<IrForLoopMetadata>,
        span: Span,
    },

    // Switch statements
    Switch {
        discriminant: IrExpression,
        cases: Vec<IrSwitchCase>,
        span: Span,
    },

    // Try-catch-finally
    Try {
        body: Box<IrStatement>,
        catch_clauses: Vec<IrCatchClause>,
        finally_block: Option<Box<IrStatement>>,
        span: Span,
    },

    // Try-with-resources
    TryWithResources {
        resources: Vec<IrResource>,
        body: Box<IrStatement>,
        catch_clauses: Vec<IrCatchClause>,
        finally_block: Option<Box<IrStatement>>,
        span: Span,
    },

    // Throw statements
    Throw {
        expr: IrExpression,
        span: Span,
    },

    // Break/continue
    Break {
        label: Option<String>,
        span: Span,
    },
    Continue {
        label: Option<String>,
        span: Span,
    },

    // Block statements
    Block {
        statements: Vec<IrStatement>,
        span: Span,
    },

    // Import statements
    Import(IrImport),

    // Package declaration
    Package {
        name: String,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrForEachKind {
    Iterable,
    LazySequence { needs_cleanup: bool },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrForLoopMetadata {
    NumericRange(IrNumericRangeLoop),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrNumericRangeLoop {
    pub binding: String,
    pub binding_type: JavaType,
    pub end_variable: String,
    pub end_type: JavaType,
    pub inclusive: bool,
    pub span: Span,
}

/// Method/constructor parameters
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrParameter {
    pub name: String,
    pub java_type: JavaType,
    pub modifiers: IrModifiers,
    pub span: Span,
}

/// Record components (for data classes -> records)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrRecordComponent {
    pub name: String,
    pub java_type: JavaType,
    pub span: Span,
}

/// Variance annotation recorded for IR type parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum IrVariance {
    #[default]
    Invariant,
    Covariant,
    Contravariant,
    Bivariant,
}

/// Value captured from type-level computation or const bindings.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrTypeLevelValue {
    Int(i64),
    Bool(bool),
    String(String),
}

impl IrTypeLevelValue {
    /// Returns the contained integer when the value represents an `Int`.
    pub fn as_int(&self) -> Option<i64> {
        if let Self::Int(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the contained boolean when the value represents a `Bool`.
    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    /// Returns the contained string slice when the value represents a `String`.
    pub fn as_str(&self) -> Option<&str> {
        if let Self::String(value) = self {
            Some(value.as_str())
        } else {
            None
        }
    }
}

/// Aggregated generic metadata recorded per declaration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct IrGenericMetadata {
    /// Kind assignments for declared type parameters.
    #[serde(default)]
    pub type_parameter_kinds: BTreeMap<String, Kind>,
    /// Resolved const parameter bindings produced by inference.
    #[serde(default)]
    pub const_parameter_values: BTreeMap<String, IrTypeLevelValue>,
    /// Cached results for type-level expressions.
    #[serde(default)]
    pub type_level_bindings: BTreeMap<String, IrTypeLevelValue>,
}

/// Mapping from declaration identifiers to generic metadata.
pub type GenericMetadataMap = BTreeMap<String, IrGenericMetadata>;

/// Type parameters for generics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrTypeParameter {
    pub name: String,
    pub bounds: Vec<JavaType>,
    #[serde(default)]
    pub variance: IrVariance,
    #[serde(default)]
    pub permits: Vec<String>,
    #[serde(default)]
    pub kind: Option<Kind>,
    pub span: Span,
}

impl IrTypeParameter {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            bounds: Vec::new(),
            variance: IrVariance::default(),
            permits: Vec::new(),
            kind: None,
            span,
        }
    }
}

/// Catch clause for exception handling
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrCatchClause {
    pub exception_type: JavaType,
    pub variable_name: String,
    pub body: IrStatement,
    pub span: Span,
}

/// Java modifiers (public, private, static, etc.)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrAnnotation {
    pub name: AnnotationName,
    pub arguments: Vec<IrAnnotationArgument>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrAnnotationArgument {
    Positional(IrAnnotationValue),
    Named {
        name: String,
        value: IrAnnotationValue,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IrAnnotationValue {
    Literal(Literal),
    EnumConstant { type_name: String, constant: String },
    Array(Vec<IrAnnotationValue>),
    ClassLiteral(String),
    Nested(IrAnnotation),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct IrModifiers {
    pub visibility: IrVisibility,
    pub is_static: bool,
    pub is_final: bool,
    pub is_abstract: bool,
    pub is_synchronized: bool,
    pub is_native: bool,
    pub is_strictfp: bool,
    pub annotations: Vec<IrAnnotation>,
    #[serde(default)]
    pub is_sealed: bool,
    #[serde(default)]
    pub permitted_types: Vec<String>,
}

/// Java visibility modifiers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub enum IrVisibility {
    Public,
    Protected,
    #[default]
    Package, // Default (no modifier)
    Private,
}

/// Complete desugared program
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IrProgram {
    pub package: Option<String>,
    pub imports: Vec<IrStatement>,
    pub type_declarations: Vec<IrStatement>, // Classes, interfaces, records
    #[serde(default)]
    pub generic_metadata: GenericMetadataMap,
    pub span: Span,
}

/// Method overload generated from default/named parameters
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MethodOverload {
    pub name: String,
    pub parameters: Vec<IrParameter>,
    pub return_type: JavaType,
    pub body: IrExpression,
    pub modifiers: IrModifiers,
    pub span: Span,
}

/// Utility class generated from top-level functions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UtilityClass {
    pub name: String,
    pub methods: Vec<IrStatement>, // MethodDeclaration statements
    pub modifiers: IrModifiers,
    pub span: Span,
}

/// Static method call generated from extension functions
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StaticMethodCall {
    pub class_name: String,
    pub method_name: String,
    pub args: Vec<IrExpression>, // First arg is the receiver
    pub java_type: JavaType,
    pub span: Span,
}

impl JavaType {
    pub fn int() -> Self {
        JavaType::Primitive("int".to_string())
    }

    pub fn boolean() -> Self {
        JavaType::Primitive("boolean".to_string())
    }

    pub fn string() -> Self {
        JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        }
    }

    pub fn pattern() -> Self {
        JavaType::Reference {
            name: "java.util.regex.Pattern".to_string(),
            generic_args: vec![],
        }
    }

    pub fn object() -> Self {
        JavaType::Reference {
            name: "Object".to_string(),
            generic_args: vec![],
        }
    }

    pub fn list() -> Self {
        JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: vec![],
        }
    }

    pub fn sequence() -> Self {
        JavaType::Reference {
            name: "jv.collections.SequenceCore".to_string(),
            generic_args: vec![],
        }
    }

    pub fn wildcard_extends(bound: JavaType) -> Self {
        JavaType::Wildcard {
            kind: JavaWildcardKind::Extends,
            bound: Some(Box::new(bound)),
        }
    }

    pub fn wildcard_super(bound: JavaType) -> Self {
        JavaType::Wildcard {
            kind: JavaWildcardKind::Super,
            bound: Some(Box::new(bound)),
        }
    }

    pub fn wildcard_unbounded() -> Self {
        JavaType::Wildcard {
            kind: JavaWildcardKind::Unbounded,
            bound: None,
        }
    }

    pub fn void() -> Self {
        JavaType::Void
    }

    pub fn is_nullable(&self) -> bool {
        match self {
            JavaType::Primitive(_) => false,
            _ => true,
        }
    }
}

// Include test module
