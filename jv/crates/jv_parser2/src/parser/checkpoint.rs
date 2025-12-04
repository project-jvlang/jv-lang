use super::Parser;
use crate::lexer::Mode;
use crate::token::{Token, TokenKind};

/// パース状態のスナップショット。
#[derive(Debug, Clone, Copy)]
pub struct Checkpoint {
    pub(crate) cursor: usize,
    pub(crate) token_index: usize,
    pub(crate) tokens_len: usize,
    pub(crate) diagnostics_len: usize,
    pub(crate) context: crate::context::JvContext,
    pub(crate) recovered: bool,
    pub(crate) lexer_current: Token,
    pub(crate) lexer_mode: Mode,
    pub(crate) lexer_last_token_kind: TokenKind,
}

impl Checkpoint {
    pub(crate) fn new(
        cursor: usize,
        token_index: usize,
        tokens_len: usize,
        diagnostics_len: usize,
        context: crate::context::JvContext,
        recovered: bool,
        lexer_current: Token,
        lexer_mode: Mode,
        lexer_last_token_kind: TokenKind,
    ) -> Self {
        Self {
            cursor,
            token_index,
            tokens_len,
            diagnostics_len,
            context,
            recovered,
            lexer_current,
            lexer_mode,
            lexer_last_token_kind,
        }
    }

    pub(crate) fn from_parser<'src, 'alloc>(parser: &Parser<'src, 'alloc>) -> Self {
        let (cursor, current, mode, last_token_kind) = parser.lexer.state();
        Self::new(
            cursor,
            parser.position,
            parser.tokens.len(),
            parser.diagnostics.len(),
            parser.context,
            parser.recovered,
            current,
            mode,
            last_token_kind,
        )
    }
}
