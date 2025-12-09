//! 再帰下降パーサーのエントリポイント。
//!
//! lexer で生成されたトークン列を保持する ParseResult の土台のみをここで提供する。

use crate::lexer::{Span, TokenDiagnostic, TokenKind, TokenMetadata, TokenTrivia};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OwnedToken {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
    pub leading_trivia: TokenTrivia,
    pub metadata: Vec<TokenMetadata>,
    pub diagnostic: Option<TokenDiagnostic>,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ParseResult {
    pub tokens: Vec<OwnedToken>,
    pub errors: Vec<String>,
}
