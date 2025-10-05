use crate::{
    pipeline::{context::LexerContext, types::NormalizedToken},
    LexError, TokenMetadata,
};

use super::{ClassificationModule, ClassificationState};
use crate::pipeline::stages::json_utils::{detect_array_confidence, detect_object_confidence};

pub struct JsonDetectionModule;

impl JsonDetectionModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for JsonDetectionModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        let offset = token.raw.span.byte_range.end;
        let source = ctx.source;
        let metadata = state.metadata();
        if metadata
            .iter()
            .any(|meta| matches!(meta, TokenMetadata::PotentialJsonStart { .. }))
        {
            return Ok(());
        }

        match token.raw.text {
            "{" => {
                if let Some(confidence) = detect_object_confidence(source, offset) {
                    metadata.push(TokenMetadata::PotentialJsonStart { confidence });
                }
            }
            "[" => {
                if let Some(confidence) = detect_array_confidence(source, offset) {
                    metadata.push(TokenMetadata::PotentialJsonStart { confidence });
                }
            }
            _ => {}
        }
        Ok(())
    }
}
