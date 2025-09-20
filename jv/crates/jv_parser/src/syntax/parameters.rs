use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Parameter, Span, TypeAnnotation};
use jv_lexer::Token;

use super::support::{identifier, token_colon, token_comma};

pub(crate) fn parameter_list(
) -> impl ChumskyParser<Token, Vec<Parameter>, Error = Simple<Token>> + Clone {
    parameter()
        .separated_by(token_comma())
        .allow_trailing()
        .collect()
}

pub(crate) fn parameter() -> impl ChumskyParser<Token, Parameter, Error = Simple<Token>> + Clone {
    identifier()
        .then_ignore(token_colon())
        .then(identifier())
        .map(|(name, type_name)| Parameter {
            name: name.clone(),
            type_annotation: Some(TypeAnnotation::Simple(type_name)),
            default_value: None,
            span: Span {
                start_line: 1,
                start_column: 1,
                end_line: 1,
                end_column: 1,
            },
        })
}
