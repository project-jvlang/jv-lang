use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Expression, Parameter, Span};
use jv_lexer::Token;

use super::support::{
    identifier, token_assign, token_colon, token_comma, token_val, token_var,
    type_annotation_simple,
};

pub(crate) fn parameter_list(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Vec<Parameter>, Error = Simple<Token>> + Clone {
    parameter(expr)
        .separated_by(token_comma())
        .allow_trailing()
        .collect()
}

pub(crate) fn parameter(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Parameter, Error = Simple<Token>> + Clone {
    token_val()
        .or(token_var())
        .repeated()
        .ignore_then(identifier())
        .then(token_colon().ignore_then(type_annotation_simple()).or_not())
        .then(token_assign().ignore_then(expr).or_not())
        .map(|((name, type_annotation), default_value)| Parameter {
            name,
            type_annotation,
            default_value,
            span: Span::default(),
        })
}
