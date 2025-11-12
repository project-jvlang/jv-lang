use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;

use jv_lexer::{Token, TokenType};

use super::shared::StageSharedState;
use super::stage::{PreprocessStage, StageContext, StageStatus};

pub struct CallStage {
    shared: Rc<RefCell<StageSharedState>>,
}

impl CallStage {
    pub fn new(shared: Rc<RefCell<StageSharedState>>) -> Self {
        Self { shared }
    }

    fn allows_call_suffix(token_type: &TokenType) -> bool {
        matches!(
            token_type,
            TokenType::Identifier(_)
                | TokenType::Number(_)
                | TokenType::Character(_)
                | TokenType::String(_)
                | TokenType::StringInterpolation(_)
                | TokenType::Boolean(_)
                | TokenType::Null
                | TokenType::RightParen
                | TokenType::RightBracket
                | TokenType::RightBrace
                | TokenType::StringEnd
                | TokenType::Greater
        )
    }

    fn compute_call_parens(tokens: &[Token]) -> HashSet<usize> {
        let mut call_eligible = false;
        let mut suppress_definition_call = false;
        let mut call_paren_indices = HashSet::new();

        for (index, token) in tokens.iter().enumerate() {
            match token.token_type {
                TokenType::JavaDocComment(_)
                | TokenType::LineComment(_)
                | TokenType::BlockComment(_)
                | TokenType::FieldNameLabel(_) => continue,
                _ => {}
            }

            let token_type_ref = &token.token_type;

            if matches!(
                token_type_ref,
                TokenType::Fun | TokenType::Class | TokenType::Data
            ) {
                suppress_definition_call = true;
            }

            let mut next_call_state = Self::allows_call_suffix(token_type_ref);

            let mut is_call_left_paren =
                matches!(token_type_ref, TokenType::LeftParen) && call_eligible;
            if is_call_left_paren && suppress_definition_call {
                is_call_left_paren = false;
                suppress_definition_call = false;
            }
            if matches!(token_type_ref, TokenType::LeftParen) {
                suppress_definition_call = false;
                next_call_state = false;
            }

            if is_call_left_paren {
                call_paren_indices.insert(index);
            }

            if matches!(token_type_ref, TokenType::LeftBracket | TokenType::Comma) {
                next_call_state = false;
            }

            call_eligible = next_call_state;
        }

        call_paren_indices
    }
}

impl PreprocessStage for CallStage {
    fn name(&self) -> &'static str {
        "stage0-call"
    }

    fn run(&self, context: &mut StageContext<'_>) -> StageStatus {
        let tokens = context.tokens();
        let call_paren_indices = Self::compute_call_parens(tokens);

        let mut shared = self.shared.borrow_mut();
        shared.reset();
        shared.set_origin_indices(tokens.len());
        shared.call_paren_origins = call_paren_indices;

        StageStatus::Continue
    }
}
