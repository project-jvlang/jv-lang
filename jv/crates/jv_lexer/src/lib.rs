// P0-001: lexer tokens定義・パース - TDD Red Phase
// Reference: 作業指示-20250830.md:19-20

pub mod pipeline;
pub mod plugins;

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

impl StringDelimiterKind {
    fn allows_interpolation(self) -> bool {
        !matches!(self, StringDelimiterKind::SingleQuote)
    }

    fn normalize_indentation(self) -> bool {
        matches!(self, StringDelimiterKind::TripleQuote)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StringLiteralMetadata {
    pub delimiter: StringDelimiterKind,
    pub allows_interpolation: bool,
    pub normalize_indentation: bool,
}

impl StringLiteralMetadata {
    fn from_kind(kind: StringDelimiterKind) -> Self {
        Self {
            delimiter: kind,
            allows_interpolation: kind.allows_interpolation(),
            normalize_indentation: kind.normalize_indentation(),
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LayoutMode {
    Enabled,
    Disabled,
}

pub struct Lexer {
    input: String,
    layout_mode: LayoutMode,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input,
            layout_mode: LayoutMode::Enabled,
        }
    }

    pub fn with_layout_mode(input: String, layout_mode: LayoutMode) -> Self {
        Self { input, layout_mode }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        use crate::pipeline::{
            CharScanner, Classifier, Emitter, LexerContext, LexerPipeline, Normalizer,
            PipelineStages, TokenPluginManager,
        };

        let mut context = LexerContext::with_layout_mode(&self.input, self.layout_mode);
        let stages = PipelineStages::new(
            CharScanner::new(),
            Normalizer::new(),
            Classifier::new(),
            Emitter::new(),
        );
        let plugins = TokenPluginManager::with_default_plugins();
        let mut pipeline = LexerPipeline::new(stages, plugins);
        let mut tokens = Vec::new();
        pipeline.run(&mut context, &mut tokens)?;
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests;
