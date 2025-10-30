use crate::{
    LexError, TokenType,
    pipeline::{
        context::LexerContext,
        types::{NormalizedToken, RawTokenKind},
    },
};

use super::{ClassificationModule, ClassificationState};

pub struct CommentModule;

impl CommentModule {
    pub fn new() -> Self {
        Self
    }
}

enum CommentKind {
    Line,
    Block,
    JavaDoc,
}

fn classify_comment(raw_text: &str) -> CommentKind {
    let trimmed = raw_text.trim_start();
    if trimmed.starts_with("/**") {
        CommentKind::JavaDoc
    } else if trimmed.starts_with("/*") {
        CommentKind::Block
    } else {
        CommentKind::Line
    }
}

impl ClassificationModule for CommentModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if state.token_type().is_some() {
            return Ok(());
        }

        if !matches!(token.raw.kind, RawTokenKind::CommentCandidate) {
            return Ok(());
        }

        let comment_text = token.normalized_text.clone();
        let raw_text = token.raw.text;

        let token_type = match classify_comment(raw_text) {
            CommentKind::Line => TokenType::LineComment(comment_text),
            CommentKind::Block => TokenType::BlockComment(comment_text),
            CommentKind::JavaDoc => TokenType::JavaDocComment(comment_text),
        };

        state.overwrite_token_type(token_type);
        Ok(())
    }
}
