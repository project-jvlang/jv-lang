use crate::{
    pipeline::{
        context::LexerContext,
        types::{NormalizedToken, RawTokenKind},
    },
    LexError, TokenMetadata, TokenType,
};

use super::{ClassificationModule, ClassificationState};

pub struct StringInterpolationModule;

impl StringInterpolationModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for StringInterpolationModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if state.token_type().is_some() {
            return Ok(());
        }

        if !matches!(token.raw.kind, RawTokenKind::Symbol) {
            return Ok(());
        }

        let has_string_metadata = token
            .metadata
            .provisional_metadata
            .iter()
            .any(|meta| matches!(meta, TokenMetadata::StringLiteral(_)));

        if !has_string_metadata {
            return Ok(());
        }

        state.set_token_type(TokenType::String(token.normalized_text.clone()));
        Ok(())
    }
}
