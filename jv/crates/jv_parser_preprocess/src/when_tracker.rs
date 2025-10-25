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
            | TokenType::LayoutComma
    )
}
