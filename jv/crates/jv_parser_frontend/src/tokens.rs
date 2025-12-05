//! Token type definitions for jv language.
//!
//! This module provides the core token types used throughout the parser and related tooling.

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Token types for jv language
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TokenType {
    // Literals
    String(String),
    StringInterpolation(String), // "${...}" content
    Number(String),              // Store as string to avoid f64 Eq issues, parse later
    Character(char),
    Identifier(String),
    Underscore,
    ImplicitParam(u32),
    Boolean(bool),
    RegexLiteral(String),

    // Keywords
    Val,
    Var,
    When,
    Data,
    Class,
    Fun,
    Where,
    If,
    Else,
    For,
    In,
    While,
    Do,
    Return,
    Throw,
    Break,
    Continue,
    True,
    False,
    Null,
    Package,
    Import,
    Log,
    Trace,
    Debug,
    Info,
    Warn,
    Error,

    // Operators
    Assign,         // =
    Plus,           // +
    Minus,          // -
    Multiply,       // *
    Divide,         // /
    Modulo,         // %
    Equal,          // ==
    NotEqual,       // !=
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    And,            // &&
    Or,             // ||
    Not,            // !
    RangeExclusive, // ..
    RangeInclusive, // ..=

    // Null safety operators
    Question, // ?
    NullSafe, // ?.
    Elvis,    // ?:

    // Arrow operators
    Arrow,    // ->
    FatArrow, // =>

    // Punctuation
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    LayoutComma,  // synthetic comma for layout-delimited sequences
    Dot,          // .
    Semicolon,    // ;
    Colon,        // :
    DoubleColon,  // ::
    At,           // @

    // String interpolation tokens
    StringStart, // "...${
    StringMid,   // }...${
    StringEnd,   // }..."

    // Comments
    LineComment(String),
    BlockComment(String),
    JavaDocComment(String),
    FieldNameLabel(FieldNameLabelToken),

    // Whitespace (usually ignored but useful for formatting)
    Whitespace(String),
    Newline,

    // Special
    Eof,
    Invalid(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JsonCommentTriviaKind {
    Line,
    Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct JsonCommentTrivia {
    pub kind: JsonCommentTriviaKind,
    pub text: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SourceCommentKind {
    Line,
    Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SourceCommentTrivia {
    pub kind: SourceCommentKind,
    pub text: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct TokenTrivia {
    pub spaces: u16,
    pub newlines: u16,
    pub comments: bool,
    #[serde(default)]
    pub json_comments: Vec<JsonCommentTrivia>,
    #[serde(default)]
    pub doc_comment: Option<String>,
    #[serde(default)]
    pub passthrough_comments: Vec<SourceCommentTrivia>,
    #[serde(default)]
    pub jv_comments: Vec<SourceCommentTrivia>,
}

impl TokenTrivia {
    pub fn merge_line_breaks(&mut self, count: u16) {
        self.newlines = self.newlines.saturating_add(count);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JsonConfidence {
    None,
    Low,
    Medium,
    High,
}

impl Default for JsonConfidence {
    fn default() -> Self {
        JsonConfidence::None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StringDelimiterKind {
    DoubleQuote,
    TripleQuote,
    BacktickBlock,
    SingleQuote,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StringLiteralMetadata {
    pub delimiter: StringDelimiterKind,
    pub allows_interpolation: bool,
    pub normalize_indentation: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum NumberGroupingKind {
    None,
    Comma,
    Underscore,
    Mixed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NumberLiteralMetadata {
    pub grouping: NumberGroupingKind,
    pub original_lexeme: String,
    #[serde(default)]
    pub suffix: Option<char>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct UnderscoreInfoMetadata {
    pub raw: String,
    pub is_implicit: bool,
    #[serde(default)]
    pub number: Option<u32>,
    pub line: usize,
    pub column: usize,
    pub length: usize,
    pub in_non_code_region: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct CommentCarryOverMetadata {
    #[serde(default)]
    pub passthrough: Vec<SourceCommentTrivia>,
    #[serde(default)]
    pub jv_only: Vec<SourceCommentTrivia>,
    #[serde(default)]
    pub json: Vec<JsonCommentTrivia>,
    #[serde(default)]
    pub doc_comment: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FieldNameLabelKind {
    LineComment,
    BlockComment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FieldNameLabelErrorKind {
    InvalidIdentifier,
    ExtraText,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FieldNameLabelIssue {
    pub reason: FieldNameLabelErrorKind,
    pub text: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FieldNameLabelCandidate {
    pub name: String,
    pub line: usize,
    pub column: usize,
    pub length: usize,
    #[serde(default)]
    pub token_distance: Option<usize>,
    pub kind: FieldNameLabelKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LabeledSpan {
    pub name: String,
    pub line: usize,
    pub column: usize,
    pub length: usize,
    pub kind: FieldNameLabelKind,
    #[serde(default)]
    pub token_distance: Option<usize>,
}

impl From<&FieldNameLabelCandidate> for LabeledSpan {
    fn from(candidate: &FieldNameLabelCandidate) -> Self {
        Self {
            name: candidate.name.clone(),
            line: candidate.line,
            column: candidate.column,
            length: candidate.length,
            kind: candidate.kind,
            token_distance: candidate.token_distance,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FieldNameLabelToken {
    pub primary: Option<String>,
    #[serde(default)]
    pub primary_span: Option<LabeledSpan>,
    #[serde(default)]
    pub secondary: Vec<LabeledSpan>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TokenMetadata {
    PotentialJsonStart {
        confidence: JsonConfidence,
    },
    StringLiteral(StringLiteralMetadata),
    NumberLiteral(NumberLiteralMetadata),
    UnderscoreInfo(UnderscoreInfoMetadata),
    LayoutComma(LayoutCommaMetadata),
    StringInterpolation {
        segments: Vec<StringInterpolationSegment>,
    },
    RegexLiteral {
        raw: String,
        pattern: String,
    },
    CommentCarryOver(CommentCarryOverMetadata),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LayoutSequenceKind {
    Array,
    Call,
    When,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StringInterpolationSegment {
    Literal(String),
    Expression(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExplicitSeparatorLocation {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LayoutCommaMetadata {
    pub sequence: LayoutSequenceKind,
    #[serde(default)]
    pub explicit_separator: Option<ExplicitSeparatorLocation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LegacyLoopKeyword {
    While,
    Do,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InvalidImplicitParamReason {
    LeadingZero,
    Overflow,
    NonDigit,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TokenDiagnostic {
    LegacyLoop {
        keyword: LegacyLoopKeyword,
    },
    InvalidImplicitParam {
        reason: InvalidImplicitParamReason,
        #[serde(default)]
        suggested: Option<String>,
    },
    InvalidFieldNameLabel {
        reason: FieldNameLabelErrorKind,
        text: String,
        line: usize,
        column: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
    pub leading_trivia: TokenTrivia,
    pub diagnostic: Option<TokenDiagnostic>,
    #[serde(default)]
    pub metadata: Vec<TokenMetadata>,
}

#[derive(Error, Debug)]
pub enum LexError {
    #[error("Unexpected character '{0}' at line {1}, column {2}")]
    UnexpectedChar(char, usize, usize),
    #[error("Unterminated string at line {0}, column {1}")]
    UnterminatedString(usize, usize),
    #[error("Unterminated regex literal at line {line}, column {column}")]
    UnterminatedRegex { line: usize, column: usize },
    #[error("Invalid character {character:?} in regex literal at line {line}, column {column}")]
    InvalidRegexCharacter {
        character: char,
        line: usize,
        column: usize,
    },
    #[error("Lookahead buffer overflow (requested {requested} bytes)")]
    LookaheadOverflow { requested: usize },
}
