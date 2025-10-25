use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::when_tracker::{PendingWhenTracker, WhenTrackerEvent};
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
        let mut pending_when = PendingWhenTracker::new();
        let mut iter = tokens.into_iter().zip(origins.into_iter());

        while let Some((mut token, origin)) = iter.next() {
            let when_event = pending_when.observe(&token.token_type);
            let starts_when_block = matches!(when_event, WhenTrackerEvent::EnterBlock);

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
                TokenType::LeftBrace => {
                    if starts_when_block {
                        stack.push(SequenceContext::new_when());
                    } else if let Some(ctx) = stack.last_mut() {
                        if let SequenceContextKind::When = ctx.kind {
                            ctx.when_brace_depth = ctx.when_brace_depth.saturating_add(1);
                        }
                    }
                    if let Some(ctx) = stack.last_mut() {
                        ctx.pending_comment = false;
                    }
                }
                TokenType::RightBrace => {
                    let mut handled_when = false;
                    let mut popped_when = false;

                    if let Some(ctx) = stack.last_mut() {
                        if let SequenceContextKind::When = ctx.kind {
                            handled_when = true;
                            if ctx.when_brace_depth > 1 {
                                ctx.when_brace_depth -= 1;
                                ctx.pending_comment = false;
                            } else {
                                stack.pop();
                                popped_when = true;
                            }
                        }
                    }

                    if popped_when || !handled_when {
                        if let Some(ctx) = stack.last_mut() {
                            ctx.pending_comment = false;
                        }
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
    When,
}

struct SequenceContext {
    kind: SequenceContextKind,
    pending_comment: bool,
    when_brace_depth: usize,
}

impl SequenceContext {
    fn new_array() -> Self {
        Self {
            kind: SequenceContextKind::Array,
            pending_comment: false,
            when_brace_depth: 0,
        }
    }

    fn new_call() -> Self {
        Self {
            kind: SequenceContextKind::Call,
            pending_comment: false,
            when_brace_depth: 0,
        }
    }

    fn new_when() -> Self {
        Self {
            kind: SequenceContextKind::When,
            pending_comment: false,
            when_brace_depth: 1,
        }
    }
}
