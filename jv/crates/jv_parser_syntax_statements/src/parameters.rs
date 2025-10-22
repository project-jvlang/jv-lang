use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Expression, Parameter, ParameterModifiers, ParameterProperty};
use jv_lexer::Token;

use jv_parser_syntax_support::{
    identifier_with_span, token_any_comma, token_assign, token_colon, token_val, token_var,
    type_annotation,
};

pub fn parameter_list(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Vec<Parameter>, Error = Simple<Token>> + Clone {
    parameter(expr)
        .separated_by(token_any_comma())
        .allow_trailing()
        .collect()
        .boxed()
}

pub fn parameter(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Parameter, Error = Simple<Token>> + Clone {
    token_val()
        .map(|_| ParameterProperty::Val)
        .or(token_var().map(|_| ParameterProperty::Var))
        .repeated()
        .then(identifier_with_span())
        .then(token_colon().ignore_then(type_annotation()).or_not())
        .then(token_assign().ignore_then(expr).or_not())
        .map(
            |(((properties, (name, span)), type_annotation), default_value)| {
                let mut modifiers = ParameterModifiers::default();
                if properties
                    .iter()
                    .any(|property| matches!(property, ParameterProperty::Var))
                {
                    modifiers.property = ParameterProperty::Var;
                } else if properties
                    .iter()
                    .any(|property| matches!(property, ParameterProperty::Val))
                {
                    modifiers.property = ParameterProperty::Val;
                }

                Parameter {
                    name,
                    type_annotation,
                    default_value,
                    modifiers,
                    span,
                }
            },
        )
        .boxed()
}
