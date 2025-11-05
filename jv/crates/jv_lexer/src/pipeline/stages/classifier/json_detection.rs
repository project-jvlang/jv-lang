use crate::{
    LexError, TokenMetadata,
    pipeline::{context::LexerContext, types::NormalizedToken},
};

use super::{ClassificationModule, ClassificationState};

pub struct JsonDetectionModule;

impl JsonDetectionModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for JsonDetectionModule {
    fn apply<'source>(
        &mut self,
        _token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if state
            .metadata()
            .iter()
            .any(|meta| matches!(meta, TokenMetadata::PotentialJsonStart { .. }))
        {
            return Ok(());
        }

        // Normalizer is responsible for attaching PotentialJsonStart metadata to brace tokens.
        // If it is absent we skip recomputation here to avoid duplicating the detection logic.
        Ok(())
    }
}
