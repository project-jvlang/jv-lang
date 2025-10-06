use crate::{
    pipeline::{
        context::LexerContext,
        types::{EmissionPlan, NormalizedToken, RawTokenKind},
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

        let has_literal_meta = state
            .metadata()
            .iter()
            .any(|meta| matches!(meta, TokenMetadata::StringLiteral(_)));

        if !has_literal_meta {
            return Ok(());
        }

        let interpolation_segments = state.metadata().iter().find_map(|meta| {
            if let TokenMetadata::StringInterpolation { segments } = meta {
                Some(segments.clone())
            } else {
                None
            }
        });

        if let Some(segments) = interpolation_segments {
            state.set_token_type(TokenType::StringInterpolation(
                token.normalized_text.clone(),
            ));
            state.set_emission_plan(EmissionPlan::StringInterpolation { segments });
        } else {
            state.set_token_type(TokenType::String(token.normalized_text.clone()));
        }

        Ok(())
    }
}
