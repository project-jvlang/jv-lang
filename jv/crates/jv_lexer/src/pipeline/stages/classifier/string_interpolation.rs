use crate::{
    LexError, TokenMetadata, TokenType,
    pipeline::{
        context::LexerContext,
        types::{EmissionPlan, NormalizedToken, RawTokenKind},
    },
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

        let string_metadata = state.metadata().iter().find_map(|meta| {
            if let TokenMetadata::StringLiteral(data) = meta {
                Some(data.clone())
            } else {
                None
            }
        });

        if let Some(meta) = &string_metadata {
            if meta.is_raw {
                match meta.char_length {
                    1 => {
                        let mut chars = token.normalized_text.chars();
                        if let (Some(ch), None) = (chars.next(), chars.next()) {
                            state.set_token_type(TokenType::Character(ch));
                        } else {
                            state.set_token_type(TokenType::Invalid(token.raw.text.to_string()));
                        }
                    }
                    _ => {
                        state.set_token_type(TokenType::String(token.normalized_text.clone()));
                    }
                }
                return Ok(());
            }
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
