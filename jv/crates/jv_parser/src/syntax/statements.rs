use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Modifiers, Statement, TypeAnnotation};
use jv_lexer::Token;

use super::expressions;
use super::parameters::parameter_list;
use super::support::{
    expression_span, identifier, merge_spans, span_from_token, token_assign, token_class,
    token_colon, token_data, token_fun, token_left_paren, token_right_paren, token_val, token_var,
};

pub(crate) fn statement_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    choice((
        val_declaration_parser(),
        var_declaration_parser(),
        function_declaration_parser(),
        data_class_declaration_parser(),
    ))
}

fn val_declaration_parser() -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_val()
        .then(identifier())
        .then_ignore(token_assign())
        .then(expressions::expression_parser())
        .map(|((val_token, name), initializer)| {
            let span = merge_spans(&span_from_token(&val_token), &expression_span(&initializer));
            Statement::ValDeclaration {
                name,
                type_annotation: None,
                initializer,
                modifiers: Modifiers::default(),
                span,
            }
        })
}

fn var_declaration_parser() -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_var()
        .then(identifier())
        .then_ignore(token_assign())
        .then(expressions::expression_parser())
        .map(|((var_token, name), initializer)| {
            let span = merge_spans(&span_from_token(&var_token), &expression_span(&initializer));
            Statement::VarDeclaration {
                name,
                type_annotation: None,
                initializer: Some(initializer),
                modifiers: Modifiers::default(),
                span,
            }
        })
}

fn function_declaration_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_fun()
        .then(identifier())
        .then_ignore(token_left_paren())
        .then(parameter_list())
        .then_ignore(token_right_paren())
        .then(token_colon().ignore_then(identifier()).or_not())
        .then_ignore(token_assign())
        .then(expressions::expression_parser())
        .map(|((((fun_token, name), parameters), return_type), body)| {
            let span = merge_spans(&span_from_token(&fun_token), &expression_span(&body));
            Statement::FunctionDeclaration {
                name,
                parameters,
                return_type: return_type.map(TypeAnnotation::Simple),
                body: Box::new(body),
                modifiers: Modifiers::default(),
                span,
            }
        })
}

fn data_class_declaration_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_data()
        .then_ignore(token_class())
        .then(identifier())
        .then_ignore(token_left_paren())
        .then(parameter_list())
        .then_ignore(token_right_paren())
        .map(|((data_token, name), parameters)| {
            let end_span = if parameters.is_empty() {
                span_from_token(&data_token)
            } else {
                parameters.last().unwrap().span.clone()
            };
            let span = merge_spans(&span_from_token(&data_token), &end_span);
            Statement::DataClassDeclaration {
                name,
                parameters,
                is_mutable: false,
                modifiers: Modifiers::default(),
                type_parameters: Vec::new(),
                span,
            }
        })
}
