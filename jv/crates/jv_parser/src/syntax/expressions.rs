use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    Argument, BinaryOp, Expression, Literal, Parameter, Span, StringPart, UnaryOp, WhenArm,
};
use jv_lexer::{Token, TokenType};

use super::patterns;
use super::support::{
    expression_span, identifier, keyword, merge_spans, span_from_token, token_and, token_arrow,
    token_assign, token_colon, token_comma, token_divide, token_dot, token_else, token_elvis,
    token_equal, token_greater, token_greater_equal, token_if, token_left_brace,
    token_left_bracket, token_left_paren, token_less, token_less_equal, token_minus, token_modulo,
    token_multiply, token_not, token_not_equal, token_null_safe, token_or, token_plus,
    token_question, token_right_brace, token_right_bracket, token_right_paren, token_string_end,
    token_string_mid, token_string_start, token_when, type_annotation_simple,
};

pub(crate) fn expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let primary = choice((
            when_expression_parser(expr.clone()),
            if_expression_parser(expr.clone()),
            lambda_literal_parser(expr.clone()),
            array_literal_parser(expr.clone()),
            string_interpolation_parser(expr.clone()),
            parenthesized_expression_parser(expr.clone()),
            literal_parser(),
            this_expression_parser(),
            super_expression_parser(),
            identifier_expression_parser(),
        ))
        .boxed();

        let postfix = postfix_expression_parser(primary, expr.clone());
        let unary = unary_expression_parser(postfix.clone());
        let multiplicative = multiplicative_expression_parser(unary.clone());
        let additive = additive_expression_parser(multiplicative.clone());
        let comparison = comparison_expression_parser(additive.clone());
        let equality = equality_expression_parser(comparison.clone());
        let logical_and = logical_and_expression_parser(equality.clone());
        let logical_or = logical_or_expression_parser(logical_and.clone());

        elvis_expression_parser(logical_or, expr.clone())
    })
}

fn parenthesized_expression_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    expr.delimited_by(token_left_paren(), token_right_paren())
}

fn when_expression_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_when()
        .ignore_then(
            token_left_paren()
                .ignore_then(expr.clone())
                .then_ignore(token_right_paren())
                .or_not(),
        )
        .then_ignore(token_left_brace())
        .then(when_arm_parser(expr.clone()).repeated())
        .then(
            token_else()
                .ignore_then(token_arrow())
                .ignore_then(expr.clone())
                .map(Box::new)
                .or_not(),
        )
        .then_ignore(token_right_brace())
        .map(|((subject, arms), else_arm)| Expression::When {
            expr: subject.map(|s| Box::new(s)),
            arms,
            else_arm,
            span: Span::default(),
        })
}

fn when_arm_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, WhenArm, Error = Simple<Token>> + Clone {
    patterns::when_pattern_parser(expr.clone())
        .then_ignore(token_arrow())
        .then(expr)
        .map(|(pattern, body)| WhenArm {
            pattern,
            body,
            span: Span::default(),
        })
}

fn if_expression_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_if()
        .ignore_then(token_left_paren())
        .ignore_then(expr.clone())
        .then_ignore(token_right_paren())
        .then(expr.clone())
        .then(token_else().ignore_then(expr.clone()).or_not())
        .map(|((condition, then_branch), else_branch)| Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
            span: Span::default(),
        })
}

fn lambda_literal_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_left_brace()
        .ignore_then(
            lambda_parameter_clause()
                .then_ignore(token_arrow())
                .then(expr)
                .map(|(parameters, body)| Expression::Lambda {
                    parameters,
                    body: Box::new(body),
                    span: Span::default(),
                }),
        )
        .then_ignore(token_right_brace())
}

fn lambda_parameter_clause(
) -> impl ChumskyParser<Token, Vec<Parameter>, Error = Simple<Token>> + Clone {
    let parameter = identifier()
        .then(token_colon().ignore_then(type_annotation_simple()).or_not())
        .map(|(name, type_annotation)| Parameter {
            name,
            type_annotation,
            default_value: None,
            span: Span::default(),
        });

    choice((
        token_left_paren()
            .ignore_then(
                parameter
                    .clone()
                    .separated_by(token_comma())
                    .allow_trailing(),
            )
            .then_ignore(token_right_paren()),
        parameter.map(|param| vec![param]),
    ))
}

fn array_literal_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_left_bracket()
        .ignore_then(expr.clone().separated_by(token_comma()).allow_trailing())
        .then_ignore(token_right_bracket())
        .map(|elements| Expression::Array {
            elements,
            span: Span::default(),
        })
}

fn string_interpolation_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_string_start()
        .map(|token| (token.lexeme.clone(), span_from_token(&token)))
        .then(expr.clone())
        .then(
            token_string_mid()
                .map(|token| (token.lexeme.clone(), span_from_token(&token)))
                .then(expr.clone())
                .repeated(),
        )
        .then(token_string_end().map(|token| (token.lexeme.clone(), span_from_token(&token))))
        .map(|(((start_text, first_expr), segments), end_text)| {
            let mut parts = Vec::new();

            if !start_text.0.is_empty() {
                parts.push(StringPart::Text(start_text.0));
            }

            parts.push(StringPart::Expression(first_expr));

            for ((text, _text_span), expr_part) in segments {
                if !text.is_empty() {
                    parts.push(StringPart::Text(text));
                }
                parts.push(StringPart::Expression(expr_part));
            }

            if !end_text.0.is_empty() {
                parts.push(StringPart::Text(end_text.0));
            }

            Expression::StringInterpolation {
                parts,
                span: Span::default(),
            }
        })
}

fn literal_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| {
        let token_span = span_from_token(&token);
        match token.token_type {
            TokenType::String(value) => Ok(Expression::Literal(Literal::String(value), token_span)),
            TokenType::Number(value) => Ok(Expression::Literal(Literal::Number(value), token_span)),
            TokenType::Boolean(value) => {
                Ok(Expression::Literal(Literal::Boolean(value), token_span))
            }
            TokenType::Null => Ok(Expression::Literal(Literal::Null, token_span)),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }
    })
}

fn this_expression_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone
{
    keyword("this").map(|token| Expression::This(span_from_token(&token)))
}

fn super_expression_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone
{
    keyword("super").map(|token| Expression::Super(span_from_token(&token)))
}

fn identifier_expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    identifier().map(|name| Expression::Identifier(name, Span::default()))
}

#[derive(Clone)]
enum PostfixOp {
    Call { args: Vec<Argument> },
    Member { property: String },
    NullSafeMember { property: String },
    Index { index: Expression },
    NullSafeIndex { index: Expression },
    TrailingLambda { lambda: Expression },
}

fn postfix_expression_parser(
    primary: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    primary
        .then(
            choice((
                call_suffix(expr.clone()),
                null_safe_member_suffix(),
                member_suffix(),
                null_safe_index_suffix(expr.clone()),
                index_suffix(expr.clone()),
                trailing_lambda_suffix(expr),
            ))
            .repeated(),
        )
        .foldl(|base, op| apply_postfix(base, op))
}

fn call_suffix(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    token_left_paren()
        .ignore_then(argument_list(expr))
        .then_ignore(token_right_paren())
        .map(|args| PostfixOp::Call { args })
}

fn argument_list(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Vec<Argument>, Error = Simple<Token>> + Clone {
    argument(expr)
        .separated_by(token_comma())
        .allow_trailing()
        .collect()
}

fn argument(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Argument, Error = Simple<Token>> + Clone {
    let named = identifier()
        .then_ignore(token_assign())
        .then(expr.clone())
        .map(|(name, value)| Argument::Named {
            name,
            value,
            span: Span::default(),
        });

    named.or(expr.map(Argument::Positional))
}

fn member_suffix() -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    token_dot()
        .ignore_then(identifier())
        .map(|property| PostfixOp::Member { property })
}

fn null_safe_member_suffix() -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone
{
    token_null_safe()
        .ignore_then(identifier())
        .map(|property| PostfixOp::NullSafeMember { property })
}

fn index_suffix(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    token_left_bracket()
        .ignore_then(expr.clone())
        .then_ignore(token_right_bracket())
        .map(|index| PostfixOp::Index { index })
}

fn null_safe_index_suffix(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    token_question()
        .ignore_then(token_left_bracket())
        .ignore_then(expr.clone())
        .then_ignore(token_right_bracket())
        .map(|index| PostfixOp::NullSafeIndex { index })
}

fn trailing_lambda_suffix(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    lambda_literal_parser(expr).map(|lambda| PostfixOp::TrailingLambda { lambda })
}

fn apply_postfix(base: Expression, op: PostfixOp) -> Expression {
    match op {
        PostfixOp::Call { args } => {
            let span = args
                .last()
                .map(|argument| match argument {
                    Argument::Positional(expr) => expression_span(expr),
                    Argument::Named { value, .. } => expression_span(value),
                })
                .map(|end_span| merge_spans(&expression_span(&base), &end_span))
                .unwrap_or_else(|| expression_span(&base));

            Expression::Call {
                function: Box::new(base),
                args,
                span,
            }
        }
        PostfixOp::Member { property } => {
            let span = expression_span(&base);
            Expression::MemberAccess {
                object: Box::new(base),
                property,
                span,
            }
        }
        PostfixOp::NullSafeMember { property } => {
            let span = expression_span(&base);
            Expression::NullSafeMemberAccess {
                object: Box::new(base),
                property,
                span,
            }
        }
        PostfixOp::Index { index } => {
            let span = merge_spans(&expression_span(&base), &expression_span(&index));
            Expression::IndexAccess {
                object: Box::new(base),
                index: Box::new(index),
                span,
            }
        }
        PostfixOp::NullSafeIndex { index } => {
            let span = merge_spans(&expression_span(&base), &expression_span(&index));
            Expression::NullSafeIndexAccess {
                object: Box::new(base),
                index: Box::new(index),
                span,
            }
        }
        PostfixOp::TrailingLambda { lambda } => match base {
            Expression::Call {
                function,
                mut args,
                span,
            } => {
                let new_span = merge_spans(&span, &expression_span(&lambda));
                args.push(Argument::Positional(lambda));
                Expression::Call {
                    function,
                    args,
                    span: new_span,
                }
            }
            other => {
                let span = merge_spans(&expression_span(&other), &expression_span(&lambda));
                Expression::Call {
                    function: Box::new(other),
                    args: vec![Argument::Positional(lambda)],
                    span,
                }
            }
        },
    }
}

fn unary_expression_parser(
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

fn multiplicative_expression_parser(
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

fn additive_expression_parser(
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

fn comparison_expression_parser(
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
            ))
            .then(operand)
            .repeated(),
        )
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

fn equality_expression_parser(
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

fn logical_and_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(token_and().to(BinaryOp::And).then(operand).repeated())
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

fn logical_or_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(token_or().to(BinaryOp::Or).then(operand).repeated())
        .foldl(|left, (op, right)| build_binary(left, op, right))
}

fn elvis_expression_parser(
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
