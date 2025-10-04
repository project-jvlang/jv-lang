use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Expression, Parameter};
use jv_lexer::Token;

use super::support::{
    identifier_with_span, token_any_comma, token_assign, token_colon, token_val, token_var,
    type_annotation,
};

pub(crate) fn parameter_list(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Vec<Parameter>, Error = Simple<Token>> + Clone {
    parameter(expr)
        .separated_by(token_any_comma())
        .allow_trailing()
        .collect()
}

pub(crate) fn parameter(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Parameter, Error = Simple<Token>> + Clone {
    token_val()
        .or(token_var())
        .repeated()
        .ignore_then(identifier_with_span())
        .then(token_colon().ignore_then(type_annotation()).or_not())
        .then(token_assign().ignore_then(expr).or_not())
        .map(
            |(((name, span), type_annotation), default_value)| Parameter {
                name,
                type_annotation,
                default_value,
                span,
            },
        )
}
