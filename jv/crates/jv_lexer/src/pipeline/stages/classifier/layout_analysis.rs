use crate::{
    pipeline::{
        context::LexerContext,
        types::{NormalizedToken, RawTokenKind},
    },
    LexError, TokenMetadata, TokenType,
};

use super::{ClassificationModule, ClassificationState};

pub struct LayoutAnalysisModule;

impl LayoutAnalysisModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for LayoutAnalysisModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if state.token_type().is_some() {
            return Ok(());
        }

        if !matches!(token.raw.kind, RawTokenKind::Whitespace) {
            return Ok(());
        }

        if state.metadata_contains(|meta| matches!(meta, TokenMetadata::LayoutComma(_))) {
            state.set_token_type(TokenType::LayoutComma);
        }

        Ok(())
    }
}
