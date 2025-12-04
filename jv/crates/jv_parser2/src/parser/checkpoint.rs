use super::Parser;

/// パース状態のスナップショット。
#[derive(Debug, Clone, Copy)]
pub struct Checkpoint {
    pub(crate) cursor: usize,
    pub(crate) token_index: usize,
    pub(crate) tokens_len: usize,
    pub(crate) diagnostics_len: usize,
    pub(crate) context: crate::context::JvContext,
    pub(crate) recovered: bool,
}

impl Checkpoint {
    pub(crate) fn new(
        cursor: usize,
        token_index: usize,
        tokens_len: usize,
        diagnostics_len: usize,
        context: crate::context::JvContext,
        recovered: bool,
    ) -> Self {
        Self {
            cursor,
            token_index,
            tokens_len,
            diagnostics_len,
            context,
            recovered,
        }
    }

    pub(crate) fn from_parser<'src, 'alloc>(parser: &Parser<'src, 'alloc>) -> Self {
        Self::new(
            parser.lexer.current_offset(),
            parser.position,
            parser.tokens.len(),
            parser.diagnostics.len(),
            parser.context,
            parser.recovered,
        )
    }
}
