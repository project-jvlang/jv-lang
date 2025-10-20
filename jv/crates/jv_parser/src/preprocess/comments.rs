use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use jv_lexer::{Token, TokenType};

use super::shared::StageSharedState;
use super::stage::{PreprocessStage, StageContext, StageStatus};

pub struct CommentsStage {
    shared: Rc<RefCell<StageSharedState>>,
}

impl CommentsStage {
    pub fn new(shared: Rc<RefCell<StageSharedState>>) -> Self {
        Self { shared }
    }

    fn process(
        tokens: Vec<Token>,
        origins: Vec<usize>,
        call_paren_origins: &HashSet<usize>,
    ) -> (Vec<Token>, Vec<usize>) {
        let mut result_tokens = Vec::with_capacity(tokens.len());
        let mut result_origins = Vec::with_capacity(tokens.len());
        let mut stack: Vec<SequenceContext> = Vec::new();
        let mut iter = tokens.into_iter().zip(origins.into_iter());

        while let Some((mut token, origin)) = iter.next() {
            match token.token_type {
                TokenType::JavaDocComment(_) => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.pending_comment = true;
                    }
                    continue;
                }
                TokenType::LineComment(_) | TokenType::BlockComment(_) => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.pending_comment = true;
                        continue;
                    }

                    result_tokens.push(token);
                    result_origins.push(origin);
                    continue;
                }
                _ => {}
            }

            if let Some(ctx) = stack.last_mut() {
                if ctx.pending_comment {
                    token.leading_trivia.comments = true;
                    ctx.pending_comment = false;
                }
            }

            match token.token_type {
                TokenType::LeftBracket => {
                    stack.push(SequenceContext::new_array());
                }
                TokenType::RightBracket => {
                    if matches!(
                        stack.last().map(|ctx| ctx.kind),
                        Some(SequenceContextKind::Array)
                    ) {
                        stack.pop();
                    }
                    if let Some(ctx) = stack.last_mut() {
                        ctx.pending_comment = false;
                    }
                }
                TokenType::LeftParen => {
                    if call_paren_origins.contains(&origin) {
                        stack.push(SequenceContext::new_call());
                    }
                    if let Some(ctx) = stack.last_mut() {
                        ctx.pending_comment = false;
                    }
                }
                TokenType::RightParen => {
                    if matches!(
                        stack.last().map(|ctx| ctx.kind),
                        Some(SequenceContextKind::Call)
                    ) {
                        stack.pop();
                    }
                    if let Some(ctx) = stack.last_mut() {
                        ctx.pending_comment = false;
                    }
                }
                TokenType::Comma => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.pending_comment = false;
                    }
                }
                _ => {}
            }

            result_tokens.push(token);
            result_origins.push(origin);
        }

        (result_tokens, result_origins)
    }
}

impl PreprocessStage for CommentsStage {
    fn name(&self) -> &'static str {
        "stage0-comments"
    }

    fn run(&self, context: &mut StageContext<'_>) -> StageStatus {
        let (call_paren_origins, current_origins) = {
            let shared = self.shared.borrow();
            (
                shared.call_paren_origins.clone(),
                shared.origin_indices.clone(),
            )
        };

        let tokens = std::mem::take(context.tokens_mut());
        let (processed, updated_origins) =
            Self::process(tokens, current_origins, &call_paren_origins);

        *context.tokens_mut() = processed;
        self.shared
            .borrow_mut()
            .update_origin_indices(updated_origins);

        StageStatus::Continue
    }
}

#[derive(Clone, Copy)]
enum SequenceContextKind {
    Array,
    Call,
}

struct SequenceContext {
    kind: SequenceContextKind,
    pending_comment: bool,
}

impl SequenceContext {
    fn new_array() -> Self {
        Self {
            kind: SequenceContextKind::Array,
            pending_comment: false,
        }
    }

    fn new_call() -> Self {
        Self {
            kind: SequenceContextKind::Call,
            pending_comment: false,
        }
    }
}
