use crate::{
    pipeline::{
        context::LexerContext,
        types::{NormalizedToken, RawTokenKind},
    },
    LexError, TokenType,
};

use super::{ClassificationModule, ClassificationState};

pub struct OperatorModule;

impl OperatorModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for OperatorModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if !matches!(token.raw.kind, RawTokenKind::Symbol) {
            return Ok(());
        }

        if state.token_type().is_some() {
            return Ok(());
        }

        let token_type = match token.raw.text {
            "(" => Some(TokenType::LeftParen),
            ")" => Some(TokenType::RightParen),
            "{" => Some(TokenType::LeftBrace),
            "}" => Some(TokenType::RightBrace),
            "[" => Some(TokenType::LeftBracket),
            "]" => Some(TokenType::RightBracket),
            "," => Some(TokenType::Comma),
            ";" => Some(TokenType::Semicolon),
            "." => Some(TokenType::Dot),
            ":" => Some(TokenType::Colon),
            "::" => Some(TokenType::DoubleColon),
            "+" => Some(TokenType::Plus),
            "-" => Some(TokenType::Minus),
            "*" => Some(TokenType::Multiply),
            "/" => Some(TokenType::Divide),
            "%" => Some(TokenType::Modulo),
            "=" => Some(TokenType::Assign),
            "==" => Some(TokenType::Equal),
            "!" => Some(TokenType::Not),
            "!=" => Some(TokenType::NotEqual),
            "<" => Some(TokenType::Less),
            "<=" => Some(TokenType::LessEqual),
            ">" => Some(TokenType::Greater),
            ">=" => Some(TokenType::GreaterEqual),
            "&&" => Some(TokenType::And),
            "||" => Some(TokenType::Or),
            "->" => Some(TokenType::Arrow),
            "=>" => Some(TokenType::FatArrow),
            ".." => Some(TokenType::RangeExclusive),
            "..=" => Some(TokenType::RangeInclusive),
            "?." => Some(TokenType::NullSafe),
            "?:" => Some(TokenType::Elvis),
            "?" => Some(TokenType::Question),
            "@" => Some(TokenType::At),
            _ => None,
        };

        if let Some(token_type) = token_type {
            state.set_token_type(token_type);
        }

        Ok(())
    }
}
