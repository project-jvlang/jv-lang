use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Expression, RegexLiteral, Span, Statement, TypeAnnotation};
use jv_lexer::{Token, TokenMetadata, TokenType};

use super::spans::{merge_spans, span_from_token};
use super::tokens::{
    qualified_name_with_span, token_any_comma, token_arrow, token_greater, token_left_brace,
    token_left_paren, token_less, token_question, token_right_brace, token_right_paren,
};

pub fn block_expression_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_left_brace()
        .map(|token| span_from_token(&token))
        .then(statement.repeated())
        .then(token_right_brace().map(|token| span_from_token(&token)))
        .map(|((left_span, statements), right_span)| {
            let span = merge_spans(&left_span, &right_span);
            Expression::Block { statements, span }
        })
}

pub fn expression_level_block_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    block_expression_parser(statement).boxed()
}

pub fn regex_literal_from_token(token: &Token, span: Span) -> RegexLiteral {
    let normalized = match &token.token_type {
        TokenType::RegexLiteral(value) => value.clone(),
        _ => token.lexeme.clone(),
    };

    let (raw, pattern) = token
        .metadata
        .iter()
        .find_map(|metadata| match metadata {
            TokenMetadata::RegexLiteral { raw, pattern } => Some((raw.clone(), pattern.clone())),
            _ => None,
        })
        .unwrap_or_else(|| (format!("/{normalized}/"), normalized.clone()));

    let mut computed_span = span;
    computed_span.end_column = computed_span.start_column + raw.chars().count();

    RegexLiteral {
        pattern,
        raw,
        span: computed_span,
    }
}

pub fn type_annotation() -> impl ChumskyParser<Token, TypeAnnotation, Error = Simple<Token>> + Clone
{
    recursive(|annotation| {
        let qualified = qualified_name_with_span();

        let function = annotation
            .clone()
            .separated_by(token_any_comma())
            .allow_trailing()
            .delimited_by(token_left_paren(), token_right_paren())
            .then_ignore(token_arrow())
            .then(annotation.clone())
            .map(|(params, return_type)| TypeAnnotation::Function {
                params,
                return_type: Box::new(return_type),
            });

        let type_arguments = annotation
            .clone()
            .separated_by(token_any_comma())
            .allow_trailing()
            .delimited_by(token_less(), token_greater())
            .or_not();

        let base = qualified
            .map(|(segments, span)| (segments.join("."), span))
            .then(type_arguments)
            .map(|((name, _span), args)| {
                if let Some(arguments) = args {
                    TypeAnnotation::Generic {
                        name,
                        type_args: arguments,
                    }
                } else {
                    TypeAnnotation::Simple(name)
                }
            });

        function
            .or(base)
            .then(token_question().or_not())
            .map(|(ty, nullable)| {
                if nullable.is_some() {
                    TypeAnnotation::Nullable(Box::new(ty))
                } else {
                    ty
                }
            })
    })
}
