use crate::{
    LexError, TokenType,
    pipeline::{
        context::LexerContext,
        types::{NormalizedToken, RawTokenKind},
    },
};

use super::{ClassificationModule, ClassificationState};

pub struct RegexLiteralModule;

impl RegexLiteralModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for RegexLiteralModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if !matches!(token.raw.kind, RawTokenKind::RegexCandidate) {
            return Ok(());
        }

        if state.token_type().is_some() {
            return Ok(());
        }

        state.set_token_type(TokenType::RegexLiteral(token.normalized_text.clone()));

        Ok(())
    }
}
