use crate::{
    LexError, TokenType,
    pipeline::{
        context::LexerContext,
        types::{NormalizedToken, RawTokenKind},
    },
};

use super::{ClassificationModule, ClassificationState};

pub struct NumberLiteralModule;

impl NumberLiteralModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for NumberLiteralModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if state.token_type().is_some() {
            return Ok(());
        }

        if !matches!(token.raw.kind, RawTokenKind::NumberCandidate) {
            return Ok(());
        }

        state.set_token_type(TokenType::Number(token.normalized_text.clone()));
        Ok(())
    }
}
