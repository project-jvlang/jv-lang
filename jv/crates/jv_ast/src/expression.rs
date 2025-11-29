// jv_ast/expression - Expression types and related constructs
use crate::json::JsonLiteral;
use crate::strings::MultilineStringLiteral;
use crate::types::*;
use serde::de::Deserializer;
use serde::{Deserialize, Serialize};

/// ログブロックの出力レベルを表す。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LogBlockLevel {
    /// `LOG { ... }` で宣言された、設定の `default_level` を利用するブロック。
    Default,
    /// `TRACE { ... }` ブロック。
    Trace,
    /// `DEBUG { ... }` ブロック。
    Debug,
    /// `INFO { ... }` ブロック。
    Info,
    /// `WARN { ... }` ブロック。
    Warn,
    /// `ERROR { ... }` ブロック。
    Error,
}

/// ログブロック内で保持される要素。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LogItem {
    /// ログ前に評価されるステートメント。
    Statement(crate::Statement),
    /// ログメッセージや引数を生成する式。
    Expression(Expression),
    /// ネストされたログブロック。
    Nested(LogBlock),
}

/// ログブロック式のデータモデル。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LogBlock {
    /// ブロックに割り当てられたログレベル。
    pub level: LogBlockLevel,
    /// ブロック本体に出現するステートメント・メッセージ。
    #[serde(default)]
    pub items: Vec<LogItem>,
    /// ブロック全体のソース位置。
    pub span: Span,
}

/// AST Expression node representing all types of expressions in jv
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    // Literals
    Literal(Literal, Span),

    /// Regex literal with raw + normalized pattern metadata.
    RegexLiteral(RegexLiteral),

    /// Concise regex command expression.
    RegexCommand(Box<RegexCommand>),

    /// 単位付きリテラル。
    UnitLiteral {
        /// 元となるリテラル式。
        value: Box<Expression>,
        /// 付随する単位情報。
        unit: UnitSymbol,
        /// `@` 前後の空白スタイル。
        spacing: UnitSpacingStyle,
        /// ソース上の位置。
        span: Span,
    },

    // Identifiers
    Identifier(String, Span),

    // Binary operations
    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
        span: Span,
        #[serde(default)]
        metadata: BinaryMetadata,
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

    /// ログブロック式。レベルおよび本体要素を保持する。
    LogBlock(LogBlock),

    // Array literals
    Array {
        elements: Vec<Expression>,
        delimiter: SequenceDelimiter,
        span: Span,
    },

    /// Tuple literals `(a b c)` grouped without commas.
    Tuple {
        elements: Vec<Expression>,
        #[serde(default)]
        fields: Vec<TupleFieldMeta>,
        #[serde(default)]
        context: TupleContextFlags,
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

/// Regex command expression data.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegexCommand {
    pub mode: RegexCommandMode,
    pub mode_origin: RegexCommandModeOrigin,
    pub subject: Box<Expression>,
    pub pattern: RegexLiteral,
    #[serde(default)]
    pub pattern_expr: Option<Box<Expression>>,
    #[serde(default)]
    pub replacement: Option<RegexReplacement>,
    #[serde(default)]
    pub flags: Vec<RegexFlag>,
    #[serde(default)]
    pub raw_flags: Option<String>,
    pub span: Span,
}

/// Regex command execution mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RegexCommandMode {
    /// Replace all matches with the replacement.
    All,
    /// Replace only the first match.
    First,
    /// Perform a boolean match test.
    Match,
    /// Split the subject into an array of strings.
    Split,
    /// Iterate over matches as a stream.
    Iterate,
}

/// Origin of the resolved regex command mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RegexCommandModeOrigin {
    /// Mode provided via long-form token such as `[match]`.
    ExplicitToken,
    /// Mode provided via short single-character prefix such as `m`.
    ShortMode,
    /// Mode inferred from presence of replacement section.
    DefaultReplacement,
    /// Mode inferred due to absence of replacement section.
    DefaultMatch,
}

/// Replacement description for a regex command.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RegexReplacement {
    /// Literal replacement with optional embedded expressions/back-references.
    Literal(RegexLiteralReplacement),
    /// Lambda replacement capturing parameters and body.
    Lambda(RegexLambdaReplacement),
    /// Fallback expression replacement (e.g. helper function returning `String`).
    Expression(Expression),
}

/// Literal replacement metadata.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegexLiteralReplacement {
    pub raw: String,
    pub normalized: String,
    #[serde(default)]
    pub template_segments: Vec<RegexTemplateSegment>,
    pub span: Span,
}

/// Lambda replacement metadata.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegexLambdaReplacement {
    pub params: Vec<Parameter>,
    pub body: Box<Expression>,
    pub span: Span,
}

/// Template segment in a literal replacement.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RegexTemplateSegment {
    Text(String),
    BackReference(u32),
    Expression(Expression),
}

/// Regex flag identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RegexFlag {
    CaseInsensitive,
    Multiline,
    DotAll,
    UnicodeCase,
    UnixLines,
    Comments,
    Literal,
    CanonEq,
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

/// `@` 周囲の空白スタイルを記録する補助構造体。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct UnitSpacingStyle {
    /// `@` の直前に空白が存在するか。
    pub space_before_at: bool,
    /// `@` の直後に空白が存在するか。
    pub space_after_at: bool,
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

/// Source span metadata for user-provided tuple field labels.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LabeledSpan {
    pub name: String,
    pub span: Span,
}

/// Metadata captured for each tuple element.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TupleFieldMeta {
    #[serde(default)]
    pub primary_label: Option<String>,
    #[serde(default)]
    pub secondary_labels: Vec<LabeledSpan>,
    #[serde(default)]
    pub identifier_hint: Option<String>,
    pub fallback_index: usize,
    pub span: Span,
}

impl TupleFieldMeta {
    /// Create an empty metadata placeholder for a tuple element.
    pub fn empty(fallback_index: usize, span: Span) -> Self {
        Self {
            primary_label: None,
            secondary_labels: Vec::new(),
            identifier_hint: None,
            fallback_index,
            span,
        }
    }
}

/// Flags describing how a tuple expression is being used.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct TupleContextFlags {
    #[serde(default)]
    pub in_destructuring_pattern: bool,
    #[serde(default)]
    pub is_function_return: bool,
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

/// 2項演算子に付随する補助メタデータ。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct BinaryMetadata {
    /// `is` 演算子に関するメタデータ。
    #[serde(default)]
    pub is_test: Option<IsTestMetadata>,
}

/// `is` 演算子の種別と関連情報。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IsTestMetadata {
    /// 解析された `is` 演算子の判定種別。
    pub kind: IsTestKind,
    /// 正規表現リテラルが右辺に現れた場合のリテラル情報。
    #[serde(default)]
    pub regex: Option<RegexLiteral>,
    /// `Pattern` 型を返す式が右辺に現れた場合の式ツリー。
    #[serde(default)]
    pub pattern_expr: Option<Box<Expression>>,
    /// 事前検証で得られた診断情報。
    #[serde(default)]
    pub diagnostics: Vec<RegexTestDiagnostic>,
    /// 左辺に適用するガード戦略ヒント。
    #[serde(default)]
    pub guard_strategy: RegexGuardStrategy,
    /// `is` 判定全体のソーススパン。
    pub span: Span,
}

/// `is` 判定で利用される分類。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IsTestKind {
    /// 型テスト（従来の `is Type`）。
    Type,
    /// 正規表現リテラル。
    RegexLiteral,
    /// `Pattern` 型を返す任意の式。
    PatternExpression,
}

/// 正規表現判定に付随する簡易診断。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RegexTestDiagnostic {
    /// 日本語での診断メッセージ。
    pub message: String,
    /// 任意の診断コード。
    #[serde(default)]
    pub code: Option<String>,
    /// 該当箇所のスパン。
    #[serde(default)]
    pub span: Option<Span>,
}

/// 左辺に適用するガード戦略。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RegexGuardStrategy {
    /// ガード不要。
    None,
    /// 一時変数へ退避しつつ null ガードを行う。
    CaptureAndGuard {
        /// 生成済みの一時変数名（未定義時は `None`）。
        #[serde(default)]
        temp_name: Option<String>,
    },
}

impl Default for RegexGuardStrategy {
    fn default() -> Self {
        RegexGuardStrategy::None
    }
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
