use crate::{
    pipeline::{context::LexerContext, types::NormalizedToken},
    LexError,
};

use super::{ClassificationModule, ClassificationState};

pub struct CommentModule;

impl CommentModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for CommentModule {
    fn apply<'source>(
        &mut self,
        _token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        _state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        // コメントは CharScanner でトリビアとして処理されるため、現段階では分類を行わない。
        Ok(())
    }
}
