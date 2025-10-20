use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{BinaryOp, Expression, UnaryOp};
use jv_lexer::Token;

use crate::syntax::support::{
    expression_span, merge_spans, token_and, token_divide, token_elvis, token_equal, token_greater,
    token_greater_equal, token_is, token_less, token_less_equal, token_minus, token_modulo,
    token_multiply, token_not, token_not_equal, token_or, token_plus, token_range_exclusive,
    token_range_inclusive,
};

pub(super) fn unary_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    choice((
        token_not().to(UnaryOp::Not),
        token_minus().to(UnaryOp::Minus),
        token_plus().to(UnaryOp::Plus),
    ))
    .repeated()
    .then(operand)
    .map(|(ops, mut expr)| {
        for op in ops.into_iter().rev() {
            let span = expression_span(&expr);
            expr = Expression::Unary {
                op,
                operand: Box::new(expr),
                span,
            };
        }
        expr
    })
}

pub(super) fn multiplicative_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(
            choice((
                token_multiply().to(BinaryOp::Multiply),
                token_divide().to(BinaryOp::Divide),
                token_modulo().to(BinaryOp::Modulo),
            ))
            .then(operand)
            .repeated(),
        )
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

pub(super) fn additive_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(
            choice((
                token_plus().to(BinaryOp::Add),
                token_minus().to(BinaryOp::Subtract),
            ))
            .then(operand)
            .repeated(),
        )
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

pub(super) fn range_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(
            choice((
                token_range_exclusive().to(BinaryOp::RangeExclusive),
                token_range_inclusive().to(BinaryOp::RangeInclusive),
            ))
            .then(operand)
            .repeated(),
        )
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

pub(super) fn comparison_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(
            choice((
                token_less().to(BinaryOp::Less),
                token_less_equal().to(BinaryOp::LessEqual),
                token_greater().to(BinaryOp::Greater),
                token_greater_equal().to(BinaryOp::GreaterEqual),
                token_is().to(BinaryOp::Is),
            ))
            .then(operand)
            .repeated(),
        )
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

pub(super) fn equality_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(
            choice((
                token_equal().to(BinaryOp::Equal),
                token_not_equal().to(BinaryOp::NotEqual),
            ))
            .then(operand)
            .repeated(),
        )
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

pub(super) fn logical_and_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(token_and().to(BinaryOp::And).then(operand).repeated())
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

pub(super) fn logical_or_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(token_or().to(BinaryOp::Or).then(operand).repeated())
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

pub(super) fn elvis_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(token_elvis().ignore_then(expr).or_not())
        .map(|(left, right)| match right {
            Some(right_expr) => build_binary(left, BinaryOp::Elvis, right_expr),
            None => left,
        })
}

fn build_binary(left: Expression, op: BinaryOp, right: Expression) -> Expression {
    let span = merge_spans(&expression_span(&left), &expression_span(&right));
    Expression::Binary {
        left: Box::new(left),
        op,
        right: Box::new(right),
        span,
    }
}
