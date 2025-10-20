use chumsky::error::Simple;
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{Expression, Statement};
use jv_lexer::Token;

use super::json::json_expression_parser;

mod operators;
mod postfix;
mod primary;

pub(crate) fn expression_parser<B, S>(
    block: B,
    statement: S,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone
where
    B: ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
    S: ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
{
    recursive(move |expr| {
        let block_expr = block.clone();
        let statement_parser = statement.clone();
        let primary_expr = choice((
            block_expr,
            json_expression_parser(),
            primary::when_expression_parser(expr.clone()),
            primary::forbidden_if_expression_parser(),
            primary::lambda_literal_parser(statement_parser.clone()),
            primary::array_literal_parser(expr.clone()),
            primary::string_interpolation_parser(expr.clone()),
            primary::parenthesized_expression_parser(expr.clone()),
            primary::literal_parser(),
            primary::this_expression_parser(),
            primary::super_expression_parser(),
            primary::identifier_expression_parser(),
        ))
        .boxed();

        let postfix_expr = postfix::postfix_expression_parser(
            statement_parser.clone(),
            primary_expr,
            expr.clone(),
        );
        let cast_expr = postfix::cast_expression_parser(postfix_expr.clone());
        let unary_expr = operators::unary_expression_parser(cast_expr.clone());
        let multiplicative_expr = operators::multiplicative_expression_parser(unary_expr.clone());
        let additive_expr = operators::additive_expression_parser(multiplicative_expr.clone());
        let range_expr = operators::range_expression_parser(additive_expr.clone());
        let comparison_expr = operators::comparison_expression_parser(range_expr.clone());
        let equality_expr = operators::equality_expression_parser(comparison_expr.clone());
        let logical_and_expr = operators::logical_and_expression_parser(equality_expr.clone());
        let logical_or_expr = operators::logical_or_expression_parser(logical_and_expr.clone());

        operators::elvis_expression_parser(logical_or_expr, expr.clone())
    })
}
