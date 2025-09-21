use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Expression, Literal, Pattern, Span};
use jv_lexer::{Token, TokenType};

use super::support::{
    expression_span, identifier_with_span, keyword, merge_spans, span_from_token, token_comma,
    token_if, token_left_paren, token_right_paren,
};

/// Parser for patterns used in `when` expressions.
pub(crate) fn when_pattern_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Pattern, Error = Simple<Token>> + Clone {
    recursive(move |pattern| {
        let wildcard = keyword("_").map(|token| {
            let span = span_from_token(&token);
            Pattern::Wildcard(span)
        });

        let literal = filter_map(|span, token: Token| {
            let token_span = span_from_token(&token);
            match token.token_type {
                TokenType::String(value) => {
                    Ok(Pattern::Literal(Literal::String(value), token_span))
                }
                TokenType::Number(value) => {
                    Ok(Pattern::Literal(Literal::Number(value), token_span))
                }
                TokenType::Boolean(value) => {
                    Ok(Pattern::Literal(Literal::Boolean(value), token_span))
                }
                TokenType::Null => Ok(Pattern::Literal(Literal::Null, token_span)),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
            }
        });

        let is_pattern = keyword("is")
            .map(|token| span_from_token(&token))
            .then(identifier_with_span())
            .map(|(is_span, (type_name, type_span))| Pattern::Constructor {
                name: type_name,
                patterns: Vec::new(),
                span: merge_spans(&is_span, &type_span),
            });

        let identifier_or_constructor = identifier_with_span()
            .then(
                token_left_paren()
                    .map(|token| span_from_token(&token))
                    .then(pattern.clone().separated_by(token_comma()).allow_trailing())
                    .then(token_right_paren().map(|token| span_from_token(&token)))
                    .map(|((left_span, arguments), right_span)| {
                        let span = merge_spans(&left_span, &right_span);
                        (arguments, span)
                    })
                    .or_not(),
            )
            .map(|((name, name_span), args)| match args {
                Some((arguments, args_span)) => Pattern::Constructor {
                    name,
                    patterns: arguments,
                    span: merge_spans(&name_span, &args_span),
                },
                None => Pattern::Identifier(name, name_span),
            });

        let base = choice((wildcard, literal, is_pattern, identifier_or_constructor)).boxed();

        base.clone()
            .then(token_if().ignore_then(expr.clone()).or_not())
            .map(|(base_pattern, guard)| match guard {
                Some(condition) => {
                    let base_span = pattern_span(&base_pattern);
                    let condition_span = expression_span(&condition);
                    let span = merge_spans(&base_span, &condition_span);
                    Pattern::Guard {
                        pattern: Box::new(base_pattern),
                        condition,
                        span,
                    }
                }
                None => base_pattern,
            })
            .boxed()
    })
}

pub(crate) fn pattern_span(pattern: &Pattern) -> Span {
    match pattern {
        Pattern::Literal(_, span)
        | Pattern::Identifier(_, span)
        | Pattern::Wildcard(span)
        | Pattern::Constructor { span, .. }
        | Pattern::Range { span, .. }
        | Pattern::Guard { span, .. } => span.clone(),
    }
}
