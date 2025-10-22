use chumsky::error::Simple;
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    BindingPatternKind, CommentKind, CommentStatement, CommentVisibility, Expression,
    ExtensionFunction, Span, Statement, ValBindingOrigin,
};
use jv_lexer::{Token, TokenType};

use super::signatures::{
    extract_type_parameters_and_signature, function_signature, modifiers_parser,
    type_annotation_clause, type_parameter_list,
};
use crate::parameters::parameter_list;
use jv_parser_syntax_support::{
    block_expression_parser, expression_span, identifier, identifier_with_span,
    keyword as support_keyword, merge_spans, qualified_name_with_span, span_from_token,
    token_assign, token_class, token_colon, token_data, token_dot, token_fun, token_import,
    token_left_brace, token_left_paren, token_multiply, token_package, token_right_brace,
    token_right_paren, token_val, token_var, type_annotation,
};

pub(super) fn comment_statement_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| {
        let token_span = span_from_token(&token);
        match token.token_type.clone() {
            TokenType::LineComment(text) => {
                let visibility = if is_jv_only_line_comment(&text) {
                    CommentVisibility::JvOnly
                } else {
                    CommentVisibility::Passthrough
                };
                let rendered = if text.starts_with("/*") {
                    format!("//{}", &text[1..])
                } else if text.starts_with('/') {
                    text
                } else {
                    format!("/{}", text)
                };
                Ok(Statement::Comment(CommentStatement {
                    kind: CommentKind::Line,
                    visibility,
                    text: rendered,
                    span: token_span,
                }))
            }
            TokenType::BlockComment(text) => Ok(Statement::Comment(CommentStatement {
                kind: CommentKind::Block,
                visibility: CommentVisibility::Passthrough,
                text: format!("/*{}*/", text),
                span: token_span,
            })),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }
    })
}

pub(super) fn package_declaration_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_package()
        .map(|token| span_from_token(&token))
        .then(qualified_name_with_span())
        .map(|(package_span, (segments, path_span))| {
            let span = merge_spans(&package_span, &path_span);
            let name = segments.join(".");

            Statement::Package { name, span }
        })
}

pub(super) fn import_statement_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    #[derive(Clone)]
    enum ImportSuffix {
        Wildcard(Span),
        Alias { name: String, span: Span },
    }

    let wildcard = token_dot()
        .ignore_then(token_multiply())
        .map(|token| ImportSuffix::Wildcard(span_from_token(&token)));

    let alias = support_keyword("as")
        .map(|token| span_from_token(&token))
        .then(identifier_with_span())
        .map(|(as_span, (alias, alias_span))| {
            let span = merge_spans(&as_span, &alias_span);
            ImportSuffix::Alias { name: alias, span }
        });

    token_import()
        .map(|token| span_from_token(&token))
        .then(qualified_name_with_span())
        .then(choice((wildcard, alias)).or_not())
        .map(|((import_span, (segments, path_span)), suffix)| {
            let mut span = merge_spans(&import_span, &path_span);
            let mut alias = None;
            let mut is_wildcard = false;

            if let Some(suffix) = suffix {
                match suffix {
                    ImportSuffix::Wildcard(suffix_span) => {
                        span = merge_spans(&span, &suffix_span);
                        is_wildcard = true;
                    }
                    ImportSuffix::Alias {
                        name,
                        span: alias_span,
                    } => {
                        span = merge_spans(&span, &alias_span);
                        alias = Some(name);
                    }
                }
            }

            let path = segments.join(".");

            Statement::Import {
                path,
                alias,
                is_wildcard,
                span,
            }
        })
}

fn is_jv_only_line_comment(raw: &str) -> bool {
    let trimmed = raw.trim_start();
    trimmed.starts_with("///") || trimmed.starts_with("//*") || raw.contains("*//")
}

pub(super) fn val_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_val())
        .then(identifier_with_span())
        .then(type_annotation_clause())
        .then_ignore(token_assign())
        .then(expr)
        .map(
            |(((modifiers, (name, binding_span)), type_annotation), initializer)| {
                let span = expression_span(&initializer);
                let binding = BindingPatternKind::identifier(name.clone(), binding_span);
                Statement::ValDeclaration {
                    name,
                    binding: Some(binding),
                    type_annotation,
                    initializer,
                    modifiers,
                    origin: ValBindingOrigin::ExplicitKeyword,
                    span,
                }
            },
        )
}

pub(super) fn implicit_typed_val_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then(identifier_with_span())
        .then(token_colon().ignore_then(type_annotation()))
        .then_ignore(token_assign())
        .then(expr)
        .map(
            |(((modifiers, (name, binding_span)), type_annotation), initializer)| {
                let span = expression_span(&initializer);
                let binding = BindingPatternKind::identifier(name.clone(), binding_span);
                Statement::ValDeclaration {
                    name,
                    binding: Some(binding),
                    type_annotation: Some(type_annotation),
                    initializer,
                    modifiers,
                    origin: ValBindingOrigin::ImplicitTyped,
                    span,
                }
            },
        )
}

pub(super) fn var_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_var())
        .then(identifier_with_span())
        .then(type_annotation_clause())
        .then(token_assign().ignore_then(expr).or_not())
        .map(
            |(((modifiers, (name, binding_span)), type_annotation), initializer)| {
                let span = initializer
                    .as_ref()
                    .map(expression_span)
                    .unwrap_or_else(Span::dummy);
                let binding = BindingPatternKind::identifier(name.clone(), binding_span);
                Statement::VarDeclaration {
                    name,
                    binding: Some(binding),
                    type_annotation,
                    initializer,
                    modifiers,
                    span,
                }
            },
        )
}

pub(super) fn function_declaration_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_fun())
        .then(function_signature(expr.clone()))
        .then(function_body(statement, expr).or_not())
        .map(|((modifiers, signature), body)| {
            let body_expr = body.unwrap_or_else(|| Expression::Block {
                statements: Vec::new(),
                span: Span::dummy(),
            });
            let span = expression_span(&body_expr);

            if let Some(receiver) = signature.receiver_type {
                let inner_body = body_expr.clone();
                let inner = Statement::FunctionDeclaration {
                    name: signature.name,
                    type_parameters: signature.type_parameters.clone(),
                    generic_signature: signature.generic_signature.clone(),
                    where_clause: signature.where_clause.clone(),
                    parameters: signature.parameters,
                    return_type: signature.return_type,
                    primitive_return: signature.primitive_return.clone(),
                    body: Box::new(inner_body),
                    modifiers: modifiers.clone(),
                    span: span.clone(),
                };

                Statement::ExtensionFunction(ExtensionFunction {
                    receiver_type: receiver,
                    function: Box::new(inner),
                    span,
                })
            } else {
                Statement::FunctionDeclaration {
                    name: signature.name,
                    type_parameters: signature.type_parameters,
                    generic_signature: signature.generic_signature,
                    where_clause: signature.where_clause,
                    parameters: signature.parameters,
                    return_type: signature.return_type,
                    primitive_return: signature.primitive_return,
                    body: Box::new(body_expr),
                    modifiers,
                    span,
                }
            }
        })
        .boxed()
}

pub(super) fn data_class_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_data())
        .then(token_class().ignore_then(identifier()).or(identifier()))
        .then(type_parameter_list().or_not())
        .then_ignore(token_left_paren())
        .then(parameter_list(expr))
        .then_ignore(token_right_paren())
        .map(|(((modifiers, name), generics), parameters)| {
            let (type_parameters, generic_signature) =
                extract_type_parameters_and_signature(generics);

            Statement::DataClassDeclaration {
                name,
                parameters,
                is_mutable: false,
                modifiers,
                type_parameters,
                generic_signature,
                span: Span::dummy(),
            }
        })
        .boxed()
}

pub(super) fn class_declaration_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    let member_parser = comment_statement_parser()
        .or(function_declaration_parser(statement.clone(), expr.clone()))
        .boxed();

    modifiers_parser()
        .then_ignore(token_class())
        .then(identifier())
        .then(type_parameter_list().or_not())
        .then_ignore(token_left_brace())
        .then(member_parser.repeated())
        .then_ignore(token_right_brace())
        .map(|(((modifiers, name), generics), members)| {
            let (type_parameters, generic_signature) =
                extract_type_parameters_and_signature(generics);

            let methods = members
                .into_iter()
                .filter(|member| matches!(member, Statement::FunctionDeclaration { .. }))
                .map(Box::new)
                .collect();

            Statement::ClassDeclaration {
                name,
                type_parameters,
                generic_signature,
                superclass: None,
                interfaces: Vec::new(),
                properties: Vec::new(),
                methods,
                modifiers,
                span: Span::dummy(),
            }
        })
        .boxed()
}

fn function_body(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    block_expression_parser(statement.clone())
        .or(token_assign().ignore_then(expr))
        .boxed()
}
