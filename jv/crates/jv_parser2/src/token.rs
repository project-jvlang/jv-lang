//! トークン種別の軽量表現。既存レキサー（jv_lexer::TokenType）の全バリアントを1バイトに押し込む。

use crate::span::Span;

/// 1バイトで収まるトークン種別。
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // 基本
    Eof = 0,
    Invalid,
    Whitespace,
    Newline,
    Identifier,
    Underscore,
    ImplicitParam,

    // リテラル
    Number,
    String,
    StringInterpolation,
    Character,
    Boolean,
    Regex,

    // 演算子・記号
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
    NullSafe, // ?.
    Elvis,    // ?:
    Question,
    Arrow,
    FatArrow,
    PipeLeft,  // |{
    PipeRight, // }|

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

    // キーワード（ここから連続で並べること）
    Val,
    Var,
    Test,
    When,
    Data,
    Class,
    Fun,
    Where,
    If,
    Else,
    For,
    In,
    Is,
    While,
    Do,
    Return,
    Throw,
    Break,
    Continue,
    TrueKw,
    FalseKw,
    NullKw,
    Package,
    Import,
    Log,
    Trace,
    Debug,
    Info,
    Warn,
    Error,
    Assert,
    // キーワード終端の次に番兵を置き、総数チェックに利用する。
    KeywordSentinel,

    // 文字列補間トークン
    StringStart,
    StringMid,
    StringEnd,

    // コメント/ラベル
    LineComment,
    BlockComment,
    JavaDocComment,
    FieldNameLabel,

    __Count,
}

impl TokenKind {
    /// キーワード領域の開始・終了を返す。
    const KEYWORD_START: TokenKind = TokenKind::Val;
    const KEYWORD_END: TokenKind = TokenKind::Assert;

    /// キーワードかどうか。
    pub const fn is_keyword(self) -> bool {
        (self as u8) >= (Self::KEYWORD_START as u8) && (self as u8) <= (Self::KEYWORD_END as u8)
    }

    /// 演算子かどうか。
    pub const fn is_operator(self) -> bool {
        matches!(
            self,
            TokenKind::Assign
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Multiply
                | TokenKind::Divide
                | TokenKind::Modulo
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::And
                | TokenKind::Or
                | TokenKind::Not
                | TokenKind::RangeExclusive
                | TokenKind::RangeInclusive
                | TokenKind::NullSafe
                | TokenKind::Elvis
                | TokenKind::Question
                | TokenKind::Arrow
                | TokenKind::FatArrow
                | TokenKind::PipeLeft
                | TokenKind::PipeRight
        )
    }

    /// トークン総数（__Countのインデックス）。
    pub const VARIANT_COUNT: usize = TokenKind::__Count as usize;
}

/// トークン本体（kind + span）。
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// バリアント数が200未満であることを保証。
const _: () = assert!(TokenKind::VARIANT_COUNT < 200);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_block_is_contiguous() {
        assert!(TokenKind::Val.is_keyword());
        assert!(TokenKind::Assert.is_keyword());
        assert!(!TokenKind::Identifier.is_keyword());
    }

    #[test]
    fn operator_detection_covers_jv_specific_tokens() {
        assert!(TokenKind::Elvis.is_operator());
        assert!(TokenKind::NullSafe.is_operator());
        assert!(TokenKind::RangeInclusive.is_operator());
        assert!(!TokenKind::Identifier.is_operator());
    }

    #[test]
    fn variant_count_under_limit() {
        assert!(TokenKind::VARIANT_COUNT < 200);
    }
}
