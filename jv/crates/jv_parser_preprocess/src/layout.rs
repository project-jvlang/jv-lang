use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use crate::when_tracker::{PendingWhenTracker, WhenTrackerEvent};
use jv_lexer::{
    ExplicitSeparatorLocation, LayoutCommaMetadata, LayoutSequenceKind, Token, TokenMetadata,
    TokenTrivia, TokenType,
};

use super::shared::StageSharedState;
use super::stage::{PreprocessStage, StageContext, StageStatus};

pub struct LayoutStage {
    shared: Rc<RefCell<StageSharedState>>,
}

impl LayoutStage {
    pub fn new(shared: Rc<RefCell<StageSharedState>>) -> Self {
        Self { shared }
    }

    fn apply(
        tokens: Vec<Token>,
        origins: Vec<usize>,
        call_paren_origins: &HashSet<usize>,
    ) -> (Vec<Token>, Vec<usize>) {
        let mut result_tokens = Vec::with_capacity(tokens.len());
        let mut result_origins = Vec::with_capacity(tokens.len());
        let mut stack: Vec<SequenceContext> = Vec::new();
        let mut prev_token_type: Option<TokenType> = None;
        let mut pending_when = PendingWhenTracker::new();

        let mut iter = tokens.into_iter().zip(origins.into_iter()).peekable();

        while let Some((token, origin)) = iter.next() {
            let next_token = iter.peek().map(|(token, _)| token);
            let token_type_ref = &token.token_type;
            let current_token_type = token.token_type.clone();
            let when_event = pending_when.observe(token_type_ref);
            let starts_when_block = matches!(when_event, WhenTrackerEvent::EnterBlock);

            if let Some(ctx) = stack.last_mut() {
                if matches!(ctx.kind, SequenceContextKind::When)
                    && ctx.when_brace_depth == 1
                    && ctx.when_paren_depth == 0
                    && ctx.when_bracket_depth == 0
                    && token.leading_trivia.newlines > 0
                {
                    ctx.when_in_branch_body = false;
                }

                let eligible = match ctx.kind {
                    SequenceContextKind::Array => {
                        !matches!(token.token_type, TokenType::Comma | TokenType::RightBracket)
                    }
                    SequenceContextKind::Call => {
                        !matches!(token.token_type, TokenType::Comma | TokenType::RightParen)
                    }
                    SequenceContextKind::When => {
                        ctx.when_brace_depth == 1
                            && ctx.when_paren_depth == 0
                            && ctx.when_bracket_depth == 0
                            && !ctx.when_in_branch_body
                            && is_when_layout_candidate(&token.token_type)
                    }
                } && is_sequence_layout_candidate(
                    prev_token_type.as_ref(),
                    token_type_ref,
                    next_token,
                );

                if eligible {
                    let mut layout_needed =
                        !ctx.prev_was_separator && has_layout_trivia(&token.leading_trivia);

                    if layout_needed
                        && matches!(ctx.kind, SequenceContextKind::When)
                        && token.leading_trivia.newlines == 0
                        && !token.leading_trivia.comments
                    {
                        layout_needed = false;
                    }

                    if layout_needed {
                        match ctx.kind {
                            SequenceContextKind::Array => {
                                let metadata = LayoutCommaMetadata {
                                    sequence: LayoutSequenceKind::Array,
                                    explicit_separator: ctx.last_explicit_separator.take(),
                                };
                                let mut synthetic = make_layout_comma_token(&token);
                                synthetic
                                    .metadata
                                    .push(TokenMetadata::LayoutComma(metadata));
                                result_origins.push(origin);
                                result_tokens.push(synthetic);
                            }
                            SequenceContextKind::Call => {
                                ctx.last_explicit_separator = None;
                            }
                            SequenceContextKind::When => {
                                let metadata = LayoutCommaMetadata {
                                    sequence: LayoutSequenceKind::When,
                                    explicit_separator: None,
                                };
                                let mut synthetic = make_layout_comma_token(&token);
                                synthetic
                                    .metadata
                                    .push(TokenMetadata::LayoutComma(metadata));
                                result_origins.push(origin);
                                result_tokens.push(synthetic);
                                ctx.when_in_branch_body = false;
                            }
                        }

                        ctx.prev_was_separator = true;
                    }
                }
            }

            match token.token_type {
                TokenType::LeftBracket => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.prev_was_separator = false;
                        ctx.last_explicit_separator = None;
                        if let SequenceContextKind::When = ctx.kind {
                            ctx.when_bracket_depth += 1;
                        }
                    }
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
                        if let SequenceContextKind::When = ctx.kind {
                            if ctx.when_bracket_depth > 0 {
                                ctx.when_bracket_depth -= 1;
                            }
                        }
                        ctx.prev_was_separator = false;
                        ctx.last_explicit_separator = None;
                    }
                }
                TokenType::LeftParen => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.prev_was_separator = false;
                        ctx.last_explicit_separator = None;
                        if let SequenceContextKind::When = ctx.kind {
                            ctx.when_paren_depth += 1;
                        }
                    }
                    if call_paren_origins.contains(&origin) {
                        stack.push(SequenceContext::new_call());
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
                        if let SequenceContextKind::When = ctx.kind {
                            if ctx.when_paren_depth > 0 {
                                ctx.when_paren_depth -= 1;
                            }
                        }
                        ctx.prev_was_separator = false;
                        ctx.last_explicit_separator = None;
                    }
                }
                TokenType::Arrow | TokenType::FatArrow => {
                    if let Some(ctx) = stack.last_mut() {
                        if let SequenceContextKind::When = ctx.kind {
                            if ctx.when_brace_depth == 1
                                && ctx.when_paren_depth == 0
                                && ctx.when_bracket_depth == 0
                            {
                                ctx.when_in_branch_body = true;
                            }
                        }
                        ctx.prev_was_separator = false;
                        ctx.last_explicit_separator = None;
                    }
                }
                TokenType::LayoutComma => {
                    if let Some(ctx) = stack.last_mut() {
                        if let SequenceContextKind::When = ctx.kind {
                            ctx.when_in_branch_body = false;
                        }
                        ctx.prev_was_separator = true;
                        ctx.last_explicit_separator = None;
                    }
                }
                TokenType::Comma => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.prev_was_separator = true;
                        ctx.last_explicit_separator = Some(ExplicitSeparatorLocation {
                            line: token.line,
                            column: token.column,
                        });
                    }
                }
                TokenType::LeftBrace => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.prev_was_separator = false;
                        ctx.last_explicit_separator = None;
                    }

                    if starts_when_block {
                        stack.push(SequenceContext::new_when());
                    } else if let Some(ctx) = stack.last_mut() {
                        if let SequenceContextKind::When = ctx.kind {
                            ctx.when_brace_depth = ctx.when_brace_depth.saturating_add(1);
                        }
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
                                ctx.prev_was_separator = false;
                                ctx.last_explicit_separator = None;
                            } else {
                                stack.pop();
                                popped_when = true;
                            }
                        }
                    }

                    if popped_when || !handled_when {
                        if let Some(ctx) = stack.last_mut() {
                            ctx.prev_was_separator = false;
                            ctx.last_explicit_separator = None;
                        }
                    }
                }
                TokenType::StringStart | TokenType::StringMid | TokenType::StringEnd => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.prev_was_separator = false;
                        ctx.last_explicit_separator = None;
                    }
                }
                _ => {
                    if let Some(ctx) = stack.last_mut() {
                        ctx.prev_was_separator = false;
                        ctx.last_explicit_separator = None;
                    }
                }
            }

            result_origins.push(origin);
            result_tokens.push(token);
            prev_token_type = Some(current_token_type);
        }

        (result_tokens, result_origins)
    }
}

impl PreprocessStage for LayoutStage {
    fn name(&self) -> &'static str {
        "stage0-layout"
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
            Self::apply(tokens, current_origins, &call_paren_origins);
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
    prev_was_separator: bool,
    last_explicit_separator: Option<ExplicitSeparatorLocation>,
    when_brace_depth: usize,
    when_paren_depth: usize,
    when_bracket_depth: usize,
    when_in_branch_body: bool,
}

impl SequenceContext {
    fn new_array() -> Self {
        Self {
            kind: SequenceContextKind::Array,
            prev_was_separator: true,
            last_explicit_separator: None,
            when_brace_depth: 0,
            when_paren_depth: 0,
            when_bracket_depth: 0,
            when_in_branch_body: false,
        }
    }

    fn new_call() -> Self {
        Self {
            kind: SequenceContextKind::Call,
            prev_was_separator: true,
            last_explicit_separator: None,
            when_brace_depth: 0,
            when_paren_depth: 0,
            when_bracket_depth: 0,
            when_in_branch_body: false,
        }
    }

    fn new_when() -> Self {
        Self {
            kind: SequenceContextKind::When,
            prev_was_separator: false,
            last_explicit_separator: None,
            when_brace_depth: 1,
            when_paren_depth: 0,
            when_bracket_depth: 0,
            when_in_branch_body: false,
        }
    }
}

fn requires_right_operand(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Plus
            | TokenType::Minus
            | TokenType::Multiply
            | TokenType::Divide
            | TokenType::Modulo
            | TokenType::Equal
            | TokenType::NotEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::And
            | TokenType::Or
            | TokenType::RangeExclusive
            | TokenType::RangeInclusive
            | TokenType::Elvis
            | TokenType::Arrow
            | TokenType::FatArrow
    )
}

fn is_sequence_layout_candidate(
    prev_token: Option<&TokenType>,
    current: &TokenType,
    next_token: Option<&Token>,
) -> bool {
    if let Some(prev) = prev_token {
        if requires_right_operand(prev) {
            return false;
        }
    }

    match current {
        TokenType::Plus | TokenType::Minus => {
            if let Some(next) = next_token {
                matches!(next.token_type, TokenType::Number(_))
                    && next.leading_trivia.spaces == 0
                    && next.leading_trivia.newlines == 0
                    && !next.leading_trivia.comments
            } else {
                false
            }
        }
        _ => !matches!(
            current,
            TokenType::Comma
                | TokenType::LayoutComma
                | TokenType::RightBracket
                | TokenType::RightParen
                | TokenType::Assign
                | TokenType::Colon
                | TokenType::Dot
                | TokenType::Arrow
                | TokenType::FatArrow
        ),
    }
}

fn is_when_layout_candidate(token_type: &TokenType) -> bool {
    !matches!(
        token_type,
        TokenType::Comma
            | TokenType::LayoutComma
            | TokenType::Arrow
            | TokenType::FatArrow
            | TokenType::RightBrace
    )
}

fn has_layout_trivia(trivia: &TokenTrivia) -> bool {
    trivia.spaces > 0 || trivia.newlines > 0 || trivia.comments
}

fn make_layout_comma_token(reference: &Token) -> Token {
    Token {
        token_type: TokenType::LayoutComma,
        lexeme: ",".to_string(),
        line: reference.line,
        column: reference.column,
        leading_trivia: TokenTrivia::default(),
        diagnostic: None,
        metadata: Vec::new(),
    }
}
