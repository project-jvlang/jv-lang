use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{BinaryOp, Expression, Literal, Pattern, Span};
use jv_lexer::{Token, TokenType};

use jv_parser_syntax_support::{
    expression_span, identifier_with_span, keyword, merge_spans, regex_literal_from_token,
    span_from_token, token_and, token_any_comma, token_in_keyword, token_left_paren,
    token_right_paren,
};

/// Parser for patterns used in `when` expressions.
pub fn when_pattern_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Pattern, Error = Simple<Token>> + Clone {
    recursive(move |pattern| {
        let wildcard = keyword("_").map(|token| {
            let span = span_from_token(&token);
            Pattern::Wildcard(span)
        });

        let literal = filter(|token: &Token| {
            matches!(
                token.token_type,
                TokenType::String(_)
                    | TokenType::Number(_)
                    | TokenType::Character(_)
                    | TokenType::Boolean(_)
                    | TokenType::Null
                    | TokenType::RegexLiteral(_)
            )
        })
        .map(|token: Token| {
            let token_span = span_from_token(&token);
            match token.token_type {
                TokenType::String(value) => Pattern::Literal(Literal::String(value), token_span),
                TokenType::Number(value) => Pattern::Literal(Literal::Number(value), token_span),
                TokenType::Character(value) => {
                    Pattern::Literal(Literal::Character(value), token_span)
                }
                TokenType::Boolean(value) => Pattern::Literal(Literal::Boolean(value), token_span),
                TokenType::Null => Pattern::Literal(Literal::Null, token_span),
                TokenType::RegexLiteral(_) => {
                    let literal = regex_literal_from_token(&token, token_span);
                    Pattern::Literal(Literal::Regex(literal.clone()), literal.span.clone())
                }
                _ => unreachable!("filter ensures only literal tokens reach this branch"),
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
                    .then(
                        pattern
                            .clone()
                            .separated_by(token_any_comma())
                            .allow_trailing(),
                    )
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

        let range_pattern = token_in_keyword()
            .map(|token| span_from_token(&token))
            .then(expr.clone())
            .try_map(|(in_span, range_expr), span| match range_expr {
                Expression::Binary {
                    left,
                    op: BinaryOp::RangeExclusive,
                    right,
                    span: range_span,
                } => {
                    let span = merge_spans(&in_span, &range_span);
                    Ok(Pattern::Range {
                        start: left,
                        end: right,
                        inclusive_end: false,
                        span,
                    })
                }
                Expression::Binary {
                    left,
                    op: BinaryOp::RangeInclusive,
                    right,
                    span: range_span,
                } => {
                    let span = merge_spans(&in_span, &range_span);
                    Ok(Pattern::Range {
                        start: left,
                        end: right,
                        inclusive_end: true,
                        span,
                    })
                }
                other => {
                    let message = "JV3104: `in` パターンには範囲式が必要です / `in` patterns require a range expression";
                    let detail = format!("invalid range pattern: {:?}", other);
                    Err(Simple::custom(span, format!("{} ({})", message, detail)))
                }
            })
            .boxed();

        let base = choice((
            wildcard,
            is_pattern,
            literal,
            identifier_or_constructor,
            range_pattern,
        ))
        .boxed();

        base.clone()
            .then(token_and().ignore_then(expr.clone()).or_not())
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

pub fn pattern_span(pattern: &Pattern) -> Span {
    match pattern {
        Pattern::Literal(_, span)
        | Pattern::Identifier(_, span)
        | Pattern::Wildcard(span)
        | Pattern::Constructor { span, .. }
        | Pattern::Range { span, .. }
        | Pattern::Guard { span, .. } => span.clone(),
    }
}
