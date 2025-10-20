use chumsky::error::Simple;
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::Span;
use jv_lexer::{Token, TokenType};

use super::spans::{merge_spans, span_from_token};

pub fn token_val() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Val))
}

pub fn token_var() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Var))
}

pub fn token_fun() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Fun))
}

pub fn token_where_keyword() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Where))
}

pub fn token_for() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::For))
}

pub fn token_in_keyword() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::In))
}

pub fn token_is() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| {
        matches!(&token.token_type, TokenType::Identifier(name) if name == "is")
            && token.leading_trivia.newlines == 0
    })
}

pub fn token_as() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| {
        matches!(&token.token_type, TokenType::Identifier(name) if name == "as")
            && token.leading_trivia.newlines == 0
    })
}

pub fn token_while_keyword() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::While))
}

pub fn token_do_keyword() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Do))
}

pub fn token_data() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Data))
}

pub fn token_class() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Class))
}

pub fn token_package() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Package))
}

pub fn token_import() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Import))
}

pub fn token_return() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| match &token.token_type {
        TokenType::Return => true,
        TokenType::Identifier(name) => name == "return",
        _ => false,
    })
}

pub fn token_throw() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| match &token.token_type {
        TokenType::Throw => true,
        TokenType::Identifier(name) => name == "throw",
        _ => false,
    })
}

pub fn token_use() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    keyword("use")
}

pub fn token_defer() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    keyword("defer")
}

pub fn token_spawn() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    keyword("spawn")
}

pub fn token_when() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::When))
}

pub fn token_else() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Else))
}

pub fn token_assign() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Assign))
}

pub fn token_left_paren() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::LeftParen))
}

pub fn token_right_paren() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::RightParen))
}

pub fn token_left_brace() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::LeftBrace))
}

pub fn token_right_brace() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::RightBrace))
}

pub fn token_comma() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Comma))
}

pub fn token_layout_comma() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::LayoutComma))
}

pub fn token_any_comma() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    choice((token_comma(), token_layout_comma()))
}

pub fn token_colon() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Colon))
}

pub fn token_at() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::At))
}

pub fn token_dot() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Dot))
}

pub fn token_range_exclusive() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::RangeExclusive))
}

pub fn token_range_inclusive() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::RangeInclusive))
}

pub fn token_null_safe() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::NullSafe))
}

pub fn token_arrow() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Arrow))
}

pub fn token_plus() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Plus))
}

pub fn token_minus() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Minus))
}

pub fn token_multiply() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Multiply))
}

pub fn token_divide() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Divide))
}

pub fn token_modulo() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Modulo))
}

pub fn token_equal() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Equal))
}

pub fn token_not_equal() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::NotEqual))
}

pub fn token_less() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Less))
}

pub fn token_less_equal() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::LessEqual))
}

pub fn token_greater() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Greater))
}

pub fn token_greater_equal() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::GreaterEqual))
}

pub fn token_and() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::And))
}

pub fn token_or() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Or))
}

pub fn token_not() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Not))
}

pub fn token_question() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Question))
}

pub fn token_elvis() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Elvis))
}

pub fn token_if() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::If))
}

pub fn token_left_bracket() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::LeftBracket))
}

pub fn token_right_bracket() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::RightBracket))
}

pub fn token_string_start() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::StringStart))
}

pub fn token_string_mid() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::StringMid))
}

pub fn token_string_end() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::StringEnd))
}

pub fn identifier() -> impl ChumskyParser<Token, String, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| match token.token_type {
        TokenType::Identifier(name) => Ok(name),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
}

pub fn identifier_with_span(
) -> impl ChumskyParser<Token, (String, Span), Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| {
        let token_span = span_from_token(&token);
        match token.token_type {
            TokenType::Identifier(name) => Ok((name, token_span)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }
    })
}

pub fn qualified_name_with_span(
) -> impl ChumskyParser<Token, (Vec<String>, Span), Error = Simple<Token>> + Clone {
    identifier_with_span()
        .then(token_dot().ignore_then(identifier_with_span()).repeated())
        .map(|((first_name, first_span), rest)| {
            let mut segments = Vec::with_capacity(rest.len() + 1);
            segments.push(first_name);
            let mut span = first_span;
            for (segment, segment_span) in rest {
                span = merge_spans(&span, &segment_span);
                segments.push(segment);
            }
            (segments, span)
        })
}

pub fn keyword(
    expected: &'static str,
) -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(move |token: &Token| match &token.token_type {
        TokenType::Identifier(name) => name == expected,
        _ => false,
    })
}
