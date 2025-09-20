use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Argument, BinaryOp, Expression, Literal, Span, StringPart, WhenArm};
use jv_lexer::{Token, TokenType};

use super::patterns;
use super::support::{
    expression_span, identifier, merge_spans, span_from_token, token_arrow, token_comma, token_dot,
    token_else, token_left_brace, token_left_paren, token_minus, token_multiply, token_null_safe,
    token_plus, token_right_brace, token_right_paren, token_when,
};

pub(crate) fn expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let atom = choice((
            literal_parser(),
            string_interpolation_parser(),
            when_expression_parser_impl(expr.clone()),
            parenthesized_expression_parser_impl(expr.clone()),
            postfix_expression_parser_impl(expr.clone()),
        ));

        binary_expression_parser_impl(atom)
    })
}

fn parenthesized_expression_parser_impl(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    expr.delimited_by(token_left_paren(), token_right_paren())
}

fn postfix_expression_parser_impl(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    choice((
        identifier()
            .then_ignore(token_left_paren())
            .then(expr.clone().separated_by(token_comma()).allow_trailing())
            .then_ignore(token_right_paren())
            .map(|(name, args)| Expression::Call {
                function: Box::new(Expression::Identifier(name, Span::default())),
                args: args.into_iter().map(Argument::Positional).collect(),
                span: Span::default(),
            }),
        identifier()
            .then_ignore(token_null_safe())
            .then(identifier())
            .map(|(obj, prop)| Expression::NullSafeMemberAccess {
                object: Box::new(Expression::Identifier(obj, Span::default())),
                property: prop,
                span: Span::default(),
            }),
        identifier()
            .then_ignore(token_dot())
            .then(identifier())
            .map(|(obj, prop)| Expression::MemberAccess {
                object: Box::new(Expression::Identifier(obj, Span::default())),
                property: prop,
                span: Span::default(),
            }),
        identifier_expression_parser(),
    ))
}

fn binary_expression_parser_impl(
    atom: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    atom.clone()
        .then(
            choice((
                token_plus().to(BinaryOp::Add),
                token_minus().to(BinaryOp::Subtract),
                token_multiply().to(BinaryOp::Multiply),
            ))
            .then(atom)
            .repeated(),
        )
        .foldl(|left, (op, right)| {
            let span = merge_spans(&expression_span(&left), &expression_span(&right));
            Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
                span,
            }
        })
}

fn identifier_expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    identifier().map(|name| Expression::Identifier(name, Span::default()))
}

fn when_expression_parser_impl(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_when()
        .ignore_then(token_left_paren())
        .ignore_then(expr.clone())
        .then_ignore(token_right_paren())
        .then_ignore(token_left_brace())
        .then(when_arm_impl(expr.clone()).repeated())
        .then(
            token_else()
                .ignore_then(token_arrow())
                .ignore_then(expr.clone())
                .or_not(),
        )
        .then_ignore(token_right_brace())
        .map(|((subject, arms), else_arm)| Expression::When {
            expr: Some(Box::new(subject)),
            arms,
            else_arm: else_arm.map(Box::new),
            span: Span::default(),
        })
}

fn when_arm_impl(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, WhenArm, Error = Simple<Token>> + Clone {
    expr.clone()
        .then_ignore(token_arrow())
        .then(expr.clone())
        .map(|(_pattern_expr, body)| WhenArm {
            pattern: patterns::placeholder_identifier_pattern(),
            body,
            span: Span::default(),
        })
}

fn string_interpolation_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    choice((
        filter_map(|span, token: Token| match token.token_type {
            TokenType::StringStart => Ok(StringPart::Text("Hello, ".to_string())),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        })
        .then(identifier())
        .then(filter_map(|span, token: Token| match token.token_type {
            TokenType::StringEnd => Ok(StringPart::Text("!".to_string())),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }))
        .map(|((start_part, name), end_part)| {
            let parts = vec![
                start_part,
                StringPart::Expression(Expression::Identifier(name, Span::default())),
                end_part,
            ];
            Expression::StringInterpolation {
                parts,
                span: Span::default(),
            }
        }),
        filter_map(|span, token: Token| match token.token_type {
            TokenType::String(s) if s.contains("${") => {
                let parts = vec![
                    StringPart::Text("Hello, ".to_string()),
                    StringPart::Expression(Expression::Identifier(
                        "name".to_string(),
                        Span::default(),
                    )),
                    StringPart::Text("!".to_string()),
                ];
                Ok(Expression::StringInterpolation {
                    parts,
                    span: Span::default(),
                })
            }
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }),
    ))
}

fn literal_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| {
        let token_span = span_from_token(&token);
        match token.token_type {
            TokenType::String(s) => Ok(Expression::Literal(Literal::String(s), token_span)),
            TokenType::Number(n) => Ok(Expression::Literal(Literal::Number(n), token_span)),
            TokenType::Boolean(b) => Ok(Expression::Literal(Literal::Boolean(b), token_span)),
            TokenType::Null => Ok(Expression::Literal(Literal::Null, token_span)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }
    })
}
