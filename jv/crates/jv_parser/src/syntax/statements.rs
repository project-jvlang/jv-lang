use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    ConcurrencyConstruct, Expression, ExtensionFunction, Modifiers, Parameter, ResourceManagement,
    Span, Statement, TypeAnnotation, Visibility,
};
use jv_lexer::Token;

use super::expressions;
use super::parameters::parameter_list;
use super::support::{
    expression_span, identifier, keyword as support_keyword, merge_spans, span_from_token,
    statement_span, token_assign, token_class, token_colon, token_data, token_defer, token_dot,
    token_fun, token_left_brace, token_left_paren, token_return, token_right_brace,
    token_right_paren, token_spawn, token_use, token_val, token_var, type_annotation_simple,
};

pub(crate) fn statement_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    recursive(|statement| {
        let expr = expressions::expression_parser();

        let val_decl = val_declaration_parser(expr.clone());
        let var_decl = var_declaration_parser(expr.clone());
        let assignment = assignment_statement_parser(expr.clone());
        let function_decl = function_declaration_parser(statement.clone(), expr.clone());
        let data_class_decl = data_class_declaration_parser(expr.clone());
        let use_stmt = use_statement_parser(statement.clone(), expr.clone());
        let defer_stmt = defer_statement_parser(statement.clone());
        let spawn_stmt = spawn_statement_parser(statement.clone());
        let return_stmt = return_statement_parser(expr.clone());
        let expression_stmt = expression_statement_parser(expr);

        choice((
            val_decl,
            var_decl,
            assignment,
            function_decl,
            data_class_decl,
            use_stmt,
            defer_stmt,
            spawn_stmt,
            return_stmt,
            expression_stmt,
        ))
        .boxed()
    })
}

fn val_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_val())
        .then(identifier())
        .then(type_annotation_clause())
        .then_ignore(token_assign())
        .then(expr)
        .map(|(((modifiers, name), type_annotation), initializer)| {
            let span = expression_span(&initializer);
            Statement::ValDeclaration {
                name,
                type_annotation,
                initializer,
                modifiers,
                span,
            }
        })
}

fn var_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_var())
        .then(identifier())
        .then(type_annotation_clause())
        .then(token_assign().ignore_then(expr).or_not())
        .map(|(((modifiers, name), type_annotation), initializer)| {
            let span = initializer
                .as_ref()
                .map(expression_span)
                .unwrap_or_else(Span::dummy);
            Statement::VarDeclaration {
                name,
                type_annotation,
                initializer,
                modifiers,
                span,
            }
        })
}

fn assignment_statement_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    assignment_target_parser()
        .then_ignore(token_assign())
        .then(expr)
        .map(|(target, value)| {
            let span = merge_spans(&expression_span(&target), &expression_span(&value));
            Statement::Assignment {
                target,
                value,
                span,
            }
        })
}

fn function_declaration_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
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
                    parameters: signature.parameters,
                    return_type: signature.return_type,
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
                    parameters: signature.parameters,
                    return_type: signature.return_type,
                    body: Box::new(body_expr),
                    modifiers,
                    span,
                }
            }
        })
}

fn data_class_declaration_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    modifiers_parser()
        .then_ignore(token_data())
        .then_ignore(token_class())
        .then(identifier())
        .then_ignore(token_left_paren())
        .then(parameter_list(expr))
        .then_ignore(token_right_paren())
        .map(
            |((modifiers, name), parameters)| Statement::DataClassDeclaration {
                name,
                parameters,
                is_mutable: false,
                modifiers,
                type_parameters: Vec::new(),
                span: Span::dummy(),
            },
        )
}

fn use_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_use()
        .ignore_then(token_left_paren())
        .ignore_then(expr)
        .then_ignore(token_right_paren())
        .then(block_expression_parser(statement.clone()))
        .map(|(resource, body)| {
            let span = merge_spans(&expression_span(&resource), &expression_span(&body));
            Statement::ResourceManagement(ResourceManagement::Use {
                resource: Box::new(resource),
                body: Box::new(body.clone()),
                span,
            })
        })
}

fn defer_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_defer()
        .ignore_then(block_expression_parser(statement.clone()))
        .map(|body| {
            Statement::ResourceManagement(ResourceManagement::Defer {
                body: Box::new(body.clone()),
                span: expression_span(&body),
            })
        })
}

fn spawn_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_spawn()
        .ignore_then(block_expression_parser(statement.clone()))
        .map(|body| {
            Statement::Concurrency(ConcurrencyConstruct::Spawn {
                body: Box::new(body.clone()),
                span: expression_span(&body),
            })
        })
}

fn return_statement_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_return()
        .then(expr.or_not())
        .map(|(ret_token, value)| {
            let span = value
                .as_ref()
                .map(expression_span)
                .unwrap_or_else(|| span_from_token(&ret_token));
            Statement::Return { value, span }
        })
}

fn expression_statement_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    expr.map(|expression| {
        let span = expression_span(&expression);
        Statement::Expression {
            expr: expression,
            span,
        }
    })
}

fn assignment_target_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone
{
    identifier()
        .then(token_dot().ignore_then(identifier()).repeated())
        .map(|(base, properties)| {
            let mut expr = Expression::Identifier(base, Span::dummy());
            for property in properties {
                expr = Expression::MemberAccess {
                    object: Box::new(expr),
                    property,
                    span: Span::dummy(),
                };
            }
            expr
        })
}

fn block_expression_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_left_brace()
        .ignore_then(statement.repeated())
        .then_ignore(token_right_brace())
        .map(|statements| {
            let span = if statements.is_empty() {
                Span::dummy()
            } else {
                let start = statement_span(&statements[0]);
                let end = statement_span(statements.last().unwrap());
                merge_spans(&start, &end)
            };

            Expression::Block { statements, span }
        })
}

fn function_body(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    block_expression_parser(statement.clone()).or(token_assign().ignore_then(expr))
}

fn function_signature(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, FunctionSignature, Error = Simple<Token>> + Clone {
    let receiver_and_name = type_annotation_simple()
        .then_ignore(token_dot())
        .then(identifier())
        .map(|(receiver, name)| (Some(receiver), name))
        .or(identifier().map(|name| (None, name)));

    receiver_and_name
        .then_ignore(token_left_paren())
        .then(parameter_list(expr.clone()))
        .then_ignore(token_right_paren())
        .then(type_annotation_clause())
        .map(
            |(((receiver, name), parameters), return_type)| FunctionSignature {
                receiver_type: receiver,
                name,
                parameters,
                return_type,
            },
        )
}

fn type_annotation_clause(
) -> impl ChumskyParser<Token, Option<TypeAnnotation>, Error = Simple<Token>> + Clone {
    token_colon().ignore_then(type_annotation_simple()).or_not()
}

fn modifiers_parser() -> impl ChumskyParser<Token, Modifiers, Error = Simple<Token>> + Clone {
    modifier_keyword().repeated().map(|keywords| {
        let mut modifiers = Modifiers::default();
        for keyword in keywords {
            match keyword {
                ModifierToken::Visibility(vis) => modifiers.visibility = vis,
                ModifierToken::Abstract => modifiers.is_abstract = true,
                ModifierToken::Final => modifiers.is_final = true,
                ModifierToken::Static => modifiers.is_static = true,
                ModifierToken::Override => modifiers.is_override = true,
                ModifierToken::Open => modifiers.is_open = true,
            }
        }
        modifiers
    })
}

fn modifier_keyword() -> impl ChumskyParser<Token, ModifierToken, Error = Simple<Token>> + Clone {
    choice((
        support_keyword("public")
            .to(())
            .map(|_| ModifierToken::Visibility(Visibility::Public)),
        support_keyword("private")
            .to(())
            .map(|_| ModifierToken::Visibility(Visibility::Private)),
        support_keyword("internal")
            .to(())
            .map(|_| ModifierToken::Visibility(Visibility::Internal)),
        support_keyword("protected")
            .to(())
            .map(|_| ModifierToken::Visibility(Visibility::Protected)),
        support_keyword("abstract")
            .to(())
            .map(|_| ModifierToken::Abstract),
        support_keyword("final")
            .to(())
            .map(|_| ModifierToken::Final),
        support_keyword("static")
            .to(())
            .map(|_| ModifierToken::Static),
        support_keyword("override")
            .to(())
            .map(|_| ModifierToken::Override),
        support_keyword("open").to(()).map(|_| ModifierToken::Open),
    ))
}

#[derive(Debug, Clone)]
struct FunctionSignature {
    receiver_type: Option<TypeAnnotation>,
    name: String,
    parameters: Vec<Parameter>,
    return_type: Option<TypeAnnotation>,
}

#[derive(Debug, Clone)]
enum ModifierToken {
    Visibility(Visibility),
    Abstract,
    Final,
    Static,
    Override,
    Open,
}
