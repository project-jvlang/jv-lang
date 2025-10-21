use chumsky::error::Simple;
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    BinaryOp, ConcurrencyConstruct, Expression, ForInStatement, LoopBinding, LoopStrategy,
    NumericRangeLoop, ResourceManagement, Span, Statement,
};
use jv_lexer::{Token, TokenType};

use super::signatures::type_annotation_clause;
use jv_parser_syntax_support::{
    block_expression_parser, expression_span, identifier, identifier_with_span, merge_spans,
    span_from_token, token_assign, token_defer, token_do_keyword, token_dot, token_for,
    token_in_keyword, token_left_paren, token_return, token_right_paren, token_spawn, token_throw,
    token_use, token_while_keyword,
};

pub(super) fn assignment_statement_parser(
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

pub(super) fn use_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_use()
        .ignore_then(token_left_paren())
        .ignore_then(expr)
        .then_ignore(token_right_paren())
        .then(block_expression_parser(statement.clone()))
        .boxed()
        .map(|(resource, body)| {
            let span = merge_spans(&expression_span(&resource), &expression_span(&body));
            Statement::ResourceManagement(ResourceManagement::Use {
                resource: Box::new(resource),
                body: Box::new(body.clone()),
                span,
            })
        })
}

pub(super) fn defer_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_defer()
        .ignore_then(block_expression_parser(statement.clone()))
        .boxed()
        .map(|body| {
            Statement::ResourceManagement(ResourceManagement::Defer {
                body: Box::new(body.clone()),
                span: expression_span(&body),
            })
        })
}

pub(super) fn spawn_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_spawn()
        .ignore_then(block_expression_parser(statement.clone()))
        .boxed()
        .map(|body| {
            Statement::Concurrency(ConcurrencyConstruct::Spawn {
                body: Box::new(body.clone()),
                span: expression_span(&body),
            })
        })
}

pub(super) fn return_statement_parser(
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

pub(super) fn throw_statement_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_throw().ignore_then(expr).map(|value| {
        let span = expression_span(&value);
        Statement::Throw { expr: value, span }
    })
}

pub(super) fn expression_statement_parser(
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

fn loop_binding_parser() -> impl ChumskyParser<Token, LoopBinding, Error = Simple<Token>> + Clone {
    identifier_with_span()
        .then(type_annotation_clause())
        .map(|((name, span), type_annotation)| LoopBinding {
            name,
            type_annotation,
            span,
        })
}

fn infer_loop_strategy(iterable: &Expression) -> LoopStrategy {
    match iterable {
        Expression::Binary {
            left,
            right,
            op: BinaryOp::RangeExclusive,
            span,
        } => LoopStrategy::NumericRange(NumericRangeLoop {
            start: (*left.clone()),
            end: (*right.clone()),
            inclusive: false,
            span: span.clone(),
        }),
        Expression::Binary {
            left,
            right,
            op: BinaryOp::RangeInclusive,
            span,
        } => LoopStrategy::NumericRange(NumericRangeLoop {
            start: (*left.clone()),
            end: (*right.clone()),
            inclusive: true,
            span: span.clone(),
        }),
        _ => LoopStrategy::Iterable,
    }
}

pub(super) fn for_in_statement_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    token_for()
        .map(|token| span_from_token(&token))
        .then(
            token_left_paren()
                .ignore_then(loop_binding_parser())
                .then_ignore(token_in_keyword())
                .then(expr.clone())
                .then_ignore(token_right_paren()),
        )
        .then(block_expression_parser(statement.clone()))
        .boxed()
        .map(|((for_span, (binding, iterable)), body)| {
            let strategy = infer_loop_strategy(&iterable);
            let body_span = expression_span(&body);
            let span = merge_spans(&for_span, &body_span);

            Statement::ForIn(ForInStatement {
                binding,
                iterable,
                strategy,
                body: Box::new(body),
                span,
            })
        })
}

pub(super) fn legacy_loop_parser(
) -> impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone {
    choice((token_while_keyword(), token_do_keyword())).try_map(|token, span| {
        let keyword = match token.token_type {
            TokenType::While => "while",
            TokenType::Do => "do-while",
            _ => "legacy loop",
        };
        let message = format!(
            "E_LOOP_001: `{}` loops are no longer supported. Use `for (item in ...)` with ranges or iterables instead.",
            keyword
        );
        Err(Simple::custom(span, message))
    })
}
