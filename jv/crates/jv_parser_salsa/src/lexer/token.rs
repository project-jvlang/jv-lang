use super::Span;

pub use jv_lexer::{
    CommentCarryOverMetadata, FieldNameLabelCandidate, FieldNameLabelKind, FieldNameLabelToken,
    InvalidImplicitParamReason, JsonCommentTrivia, JsonCommentTriviaKind, JsonConfidence,
    LayoutCommaMetadata, LayoutSequenceKind, LegacyLoopKeyword, NumberGroupingKind,
    NumberLiteralMetadata, SourceCommentKind, SourceCommentTrivia, StringDelimiterKind,
    StringInterpolationSegment, StringLiteralMetadata, TokenDiagnostic, TokenMetadata, TokenTrivia,
};

/// jv トークン種別（u8 で保持し salsa のキー容量を抑える）。
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Literals
    String,
    StringInterpolation,
    Number,
    Character,
    Identifier,
    Underscore,
    ImplicitParam,
    BooleanTrue,
    BooleanFalse,
    RegexLiteral,

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
    Assign,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Not,
    RangeExclusive,
    RangeInclusive,

    // Null safety
    Question,
    NullSafe,
    Elvis,

    // Arrows
    Arrow,
    FatArrow,

    // Punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    LayoutComma,
    Dot,
    Semicolon,
    Colon,
    DoubleColon,
    At,

    // String interpolation tokens
    StringStart,
    StringMid,
    StringEnd,

    // Comments / trivia
    LineComment,
    BlockComment,
    JavaDocComment,
    FieldNameLabel,
    Whitespace,
    Newline,

    // Special
    Eof,
    Invalid,
}

/// ゼロコピーで lexeme を保持するトークン。
#[derive(Clone, Debug)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: &'src str,
    pub leading_trivia: TokenTrivia,
    pub metadata: Vec<TokenMetadata>,
    pub diagnostic: Option<TokenDiagnostic>,
}
