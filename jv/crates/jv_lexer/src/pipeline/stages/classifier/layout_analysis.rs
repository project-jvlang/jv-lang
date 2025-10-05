use crate::{
    pipeline::{context::LexerContext, types::NormalizedToken},
    LexError,
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
        _token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        _state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        // TODO: Requirement 2 Phase 4 ではレイアウト駆動のコンマ挿入を検討する。
        Ok(())
    }
}
