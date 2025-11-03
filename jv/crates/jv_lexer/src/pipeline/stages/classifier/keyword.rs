use crate::{
    LexError, TokenType,
    pipeline::{
        context::LexerContext,
        types::{NormalizedToken, RawTokenKind},
    },
};

use super::{ClassificationModule, ClassificationState};

pub struct KeywordModule;

impl KeywordModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for KeywordModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if state.token_type().is_some() {
            return Ok(());
        }

        if !matches!(token.raw.kind, RawTokenKind::Identifier) {
            return Ok(());
        }

        let keyword = match token.normalized_text.as_str() {
            "val" => Some(TokenType::Val),
            "var" => Some(TokenType::Var),
            "when" => Some(TokenType::When),
            "data" => Some(TokenType::Data),
            "class" => Some(TokenType::Class),
            "fun" => Some(TokenType::Fun),
            "where" => Some(TokenType::Where),
            "if" => Some(TokenType::If),
            "else" => Some(TokenType::Else),
            "for" => Some(TokenType::For),
            "in" => Some(TokenType::In),
            "while" => Some(TokenType::While),
            "do" => Some(TokenType::Do),
            "return" => Some(TokenType::Return),
            "throw" => Some(TokenType::Throw),
            "break" => Some(TokenType::Break),
            "continue" => Some(TokenType::Continue),
            "true" => Some(TokenType::Boolean(true)),
            "false" => Some(TokenType::Boolean(false)),
            "null" => Some(TokenType::Null),
            "package" => Some(TokenType::Package),
            "import" => Some(TokenType::Import),
            _ => None,
        };

        if let Some(token_type) = keyword {
            state.set_token_type(token_type);
        }

        Ok(())
    }
}
