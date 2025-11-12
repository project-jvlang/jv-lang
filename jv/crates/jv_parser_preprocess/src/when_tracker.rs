use jv_lexer::TokenType;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PendingWhenState {
    AwaitingSubjectOrBrace,
    InSubject { paren_depth: usize },
    AwaitingBrace,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum WhenTrackerEvent {
    None,
    EnterBlock,
}

/// 状態機械を用いて `when` ブロック開始の `{` を検出する。
#[derive(Default, Debug)]
pub(crate) struct PendingWhenTracker {
    state: Option<PendingWhenState>,
}

impl PendingWhenTracker {
    pub(crate) fn new() -> Self {
        Self { state: None }
    }

    /// 現在のトークンを観測し、`when {` の `{` であれば EnterBlock を返す。
    pub(crate) fn observe(&mut self, token_type: &TokenType) -> WhenTrackerEvent {
        match token_type {
            TokenType::When => {
                self.state = Some(PendingWhenState::AwaitingSubjectOrBrace);
            }
            TokenType::LeftParen => match self.state {
                Some(PendingWhenState::AwaitingSubjectOrBrace) => {
                    self.state = Some(PendingWhenState::InSubject { paren_depth: 1 });
                }
                Some(PendingWhenState::InSubject {
                    ref mut paren_depth,
                }) => {
                    *paren_depth = paren_depth.saturating_add(1);
                }
                _ => {}
            },
            TokenType::RightParen => match self.state {
                Some(PendingWhenState::InSubject {
                    ref mut paren_depth,
                }) => {
                    if *paren_depth > 1 {
                        *paren_depth -= 1;
                    } else {
                        self.state = Some(PendingWhenState::AwaitingBrace);
                    }
                }
                _ => {}
            },
            TokenType::LeftBrace => {
                let should_enter = matches!(
                    self.state,
                    Some(
                        PendingWhenState::AwaitingSubjectOrBrace | PendingWhenState::AwaitingBrace
                    )
                );
                self.state = None;
                if should_enter {
                    return WhenTrackerEvent::EnterBlock;
                }
            }
            _ if is_ignorable_between_when_and_brace(token_type) => {}
            _ => {
                if matches!(
                    self.state,
                    Some(
                        PendingWhenState::AwaitingSubjectOrBrace | PendingWhenState::AwaitingBrace
                    )
                ) {
                    self.state = None;
                }
            }
        }

        WhenTrackerEvent::None
    }
}

fn is_ignorable_between_when_and_brace(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::LineComment(_)
            | TokenType::BlockComment(_)
            | TokenType::JavaDocComment(_)
            | TokenType::FieldNameLabel(_)
            | TokenType::Whitespace(_)
            | TokenType::Newline
            | TokenType::LayoutComma
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::preprocess_tokens;
    use jv_lexer::{LayoutSequenceKind, TokenMetadata};

    fn replay(tokens: &[TokenType]) -> Vec<WhenTrackerEvent> {
        let mut tracker = PendingWhenTracker::new();
        tokens.iter().map(|token| tracker.observe(token)).collect()
    }

    #[test]
    fn detects_block_without_subject_with_trivia() {
        let events = replay(&[
            TokenType::When,
            TokenType::Whitespace(" ".into()),
            TokenType::Newline,
            TokenType::LeftBrace,
        ]);

        assert!(matches!(events.as_slice(), [WhenTrackerEvent::None, ..]));
        assert!(
            events
                .last()
                .is_some_and(|event| *event == WhenTrackerEvent::EnterBlock)
        );
    }

    #[test]
    fn detects_block_with_subject_after_whitespace() {
        let events = replay(&[
            TokenType::When,
            TokenType::Whitespace("\t".into()),
            TokenType::LeftParen,
            TokenType::Identifier("subject".into()),
            TokenType::RightParen,
            TokenType::Newline,
            TokenType::LeftBrace,
        ]);

        assert!(
            events
                .last()
                .is_some_and(|event| *event == WhenTrackerEvent::EnterBlock)
        );
    }

    #[test]
    fn integrates_with_lexer_output() {
        let source = "when (candidate is Character) { true -> 0 }";
        let mut lexer = jv_lexer::Lexer::new(source.into());
        let tokens = lexer.tokenize().expect("lexing succeeds");

        let mut tracker = PendingWhenTracker::new();
        let mut saw_enter = false;

        for token in tokens {
            if matches!(
                tracker.observe(&token.token_type),
                WhenTrackerEvent::EnterBlock
            ) {
                saw_enter = true;
                break;
            }
        }

        assert!(
            saw_enter,
            "expected lexer-driven tokens to trigger EnterBlock"
        );
    }

    #[test]
    fn layout_stage_emits_when_layout_commas_after_whitespace() {
        let source = r#"
fun demo(candidate: Any, stream: Stream<Any>): Int {
    return stream
        .mapToInt { candidate ->
            when (candidate is Character) {
                true -> candidate.toString().codePointAt(0)
                else -> (candidate as Number).intValue()
            }
        }
        .sum()
}
"#;

        let mut lexer = jv_lexer::Lexer::new(source.into());
        let tokens = lexer.tokenize().expect("lexing succeeds");
        let processed = preprocess_tokens(tokens);

        let when_commas = processed
            .iter()
            .filter(|token| {
                token.metadata.iter().any(|metadata| match metadata {
                    TokenMetadata::LayoutComma(info)
                        if matches!(info.sequence, LayoutSequenceKind::When) =>
                    {
                        true
                    }
                    _ => false,
                })
            })
            .count();

        assert!(
            when_commas >= 2,
            "expected layout commas for when branches, found {}",
            when_commas
        );
    }

    #[test]
    fn stdlib_sum_int_family_produces_when_layout_commas() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../stdlib/collections/sequence.jv"
        );
        let source = std::fs::read_to_string(path).expect("read stdlib sequence module");
        let mut lexer = jv_lexer::Lexer::new(source);
        let tokens = lexer.tokenize().expect("lexing succeeds");
        let processed = preprocess_tokens(tokens);

        let when_commas = processed
            .iter()
            .filter(|token| {
                token.metadata.iter().any(|metadata| match metadata {
                    TokenMetadata::LayoutComma(info)
                        if matches!(info.sequence, LayoutSequenceKind::When) =>
                    {
                        true
                    }
                    _ => false,
                })
            })
            .count();

        assert!(
            when_commas >= 2,
            "expected stdlib when branches to receive layout commas"
        );

        let mut tracker = PendingWhenTracker::new();
        let mut enters = 0;
        for token in &processed {
            if matches!(
                tracker.observe(&token.token_type),
                WhenTrackerEvent::EnterBlock
            ) {
                enters += 1;
            }
        }

        assert!(
            enters >= 1,
            "expected at least one when block entry in stdlib sequence module"
        );
    }
}
