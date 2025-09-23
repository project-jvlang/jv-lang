use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    Argument, BinaryOp, CallArgumentStyle, Expression, Literal, Parameter, SequenceDelimiter,
    Span, StringPart, UnaryOp, WhenArm,
};
use jv_lexer::{Token, TokenType};

use super::patterns::{self, pattern_span};
use super::support::{
    expression_span, identifier, identifier_with_span, keyword, merge_spans, span_from_token,
    token_and, token_arrow, token_assign, token_colon, token_comma, token_divide, token_dot,
    token_else, token_elvis, token_equal, token_greater, token_greater_equal, token_if,
    token_left_brace, token_left_bracket, token_left_paren, token_less, token_less_equal,
    token_minus, token_modulo, token_multiply, token_not, token_not_equal, token_null_safe,
    token_or, token_plus, token_question, token_right_brace, token_right_bracket,
    token_right_paren, token_string_end, token_string_mid, token_string_start, token_when,
    type_annotation_simple,
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
        .map(|token| span_from_token(&token))
        .then(
            token_left_paren()
                .ignore_then(expr.clone())
                .then_ignore(token_right_paren())
                .or_not(),
        )
        .then_ignore(token_left_brace())
        .then(when_arm_parser(expr.clone()).repeated().at_least(1))
        .then(
            token_else()
                .ignore_then(token_arrow())
                .ignore_then(expr.clone())
                .map(Box::new)
                .or_not(),
        )
        .then(token_right_brace().map(|token| span_from_token(&token)))
        .map(|((((when_span, subject), arms), else_arm), end_span)| {
            let span = merge_spans(&when_span, &end_span);
            Expression::When {
                expr: subject.map(Box::new),
                arms,
                else_arm,
                span,
            }
        })
}

fn when_arm_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, WhenArm, Error = Simple<Token>> + Clone {
    patterns::when_pattern_parser(expr.clone())
        .then_ignore(token_arrow())
        .then(expr)
        .map(|(pattern, body)| {
            let pattern_span = pattern_span(&pattern);
            let body_span = expression_span(&body);
            let span = merge_spans(&pattern_span, &body_span);
            WhenArm {
                pattern,
                body,
                span,
            }
        })
}

fn if_expression_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_if()
        .map(|token| span_from_token(&token))
        .then(
            token_left_paren()
                .ignore_then(expr.clone())
                .then_ignore(token_right_paren()),
        )
        .then(expr.clone())
        .then(token_else().ignore_then(expr.clone()).or_not())
        .map(|(((if_span, condition), then_branch), else_branch)| {
            let then_span = expression_span(&then_branch);
            let mut span = merge_spans(&if_span, &then_span);

            let else_branch = else_branch.map(|branch| {
                span = merge_spans(&span, &expression_span(&branch));
                Box::new(branch)
            });

            Expression::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch,
                span,
            }
        })
}

fn lambda_literal_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_left_brace()
        .map(|token| span_from_token(&token))
        .then(
            lambda_parameter_clause()
                .then_ignore(token_arrow())
                .then(expr.clone()),
        )
        .then(token_right_brace().map(|token| span_from_token(&token)))
        .map(|((left_span, (parameters, body)), right_span)| {
            let span = merge_spans(&left_span, &right_span);
            Expression::Lambda {
                parameters,
                body: Box::new(body),
                span,
            }
        })
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
        .map(|token| span_from_token(&token))
        .then(expr.clone().separated_by(token_comma()).allow_trailing())
        .then(token_right_bracket().map(|token| span_from_token(&token)))
        .map(|((left_span, elements), right_span)| {
            let span = merge_spans(&left_span, &right_span);
            Expression::Array {
                elements,
                delimiter: SequenceDelimiter::Comma,
                span,
            }
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
        .map(|(((start_segment, first_expr), segments), end_segment)| {
            let (start_text, mut start_span) = start_segment;
            let (end_text, mut end_span) = end_segment;

            let mut parts = Vec::new();

            if !start_text.is_empty() {
                parts.push(StringPart::Text(start_text));
            }

            parts.push(StringPart::Expression(first_expr));

            for ((text, _text_span), expr_part) in segments {
                if !text.is_empty() {
                    parts.push(StringPart::Text(text));
                }
                parts.push(StringPart::Expression(expr_part));
            }

            if !end_text.is_empty() {
                parts.push(StringPart::Text(end_text));
            }

            if start_span.end_column == start_span.start_column {
                start_span.end_column += 1;
            }
            if end_span.end_column == end_span.start_column {
                end_span.end_column += 1;
            }

            let span = merge_spans(&start_span, &end_span);

            Expression::StringInterpolation { parts, span }
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
    identifier_with_span().map(|(name, span)| Expression::Identifier(name, span))
}

#[derive(Clone)]
enum PostfixOp {
    Call { args: Vec<Argument>, span: Span },
    Member { property: String, span: Span },
    NullSafeMember { property: String, span: Span },
    Index { index: Expression, span: Span },
    NullSafeIndex { index: Expression, span: Span },
    TrailingLambda { lambda: Expression, span: Span },
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
        .map(|token| span_from_token(&token))
        .then(argument_list(expr.clone()))
        .then(token_right_paren().map(|token| span_from_token(&token)))
        .map(|((left_span, args), right_span)| {
            let span = merge_spans(&left_span, &right_span);
            PostfixOp::Call { args, span }
        })
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
    let named = identifier_with_span()
        .then_ignore(token_assign())
        .then(expr.clone())
        .map(|((name, name_span), value)| {
            let value_span = expression_span(&value);
            let span = merge_spans(&name_span, &value_span);
            Argument::Named { name, value, span }
        });

    named.or(expr.map(Argument::Positional))
}

fn member_suffix() -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    token_dot()
        .map(|token| span_from_token(&token))
        .then(identifier_with_span())
        .map(|(dot_span, (property, property_span))| {
            let span = merge_spans(&dot_span, &property_span);
            PostfixOp::Member { property, span }
        })
}

fn null_safe_member_suffix() -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone
{
    token_null_safe()
        .map(|token| span_from_token(&token))
        .then(identifier_with_span())
        .map(|(op_span, (property, property_span))| {
            let span = merge_spans(&op_span, &property_span);
            PostfixOp::NullSafeMember { property, span }
        })
}

fn index_suffix(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    token_left_bracket()
        .map(|token| span_from_token(&token))
        .then(expr.clone())
        .then(token_right_bracket().map(|token| span_from_token(&token)))
        .map(|((left_span, index), right_span)| {
            let span = merge_spans(&left_span, &right_span);
            PostfixOp::Index { index, span }
        })
}

fn null_safe_index_suffix(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    token_question()
        .map(|token| span_from_token(&token))
        .then(token_left_bracket().map(|token| span_from_token(&token)))
        .then(expr.clone())
        .then(token_right_bracket().map(|token| span_from_token(&token)))
        .map(|(((question_span, left_span), index), right_span)| {
            let prefix_span = merge_spans(&question_span, &left_span);
            let span = merge_spans(&prefix_span, &right_span);
            PostfixOp::NullSafeIndex { index, span }
        })
}

fn trailing_lambda_suffix(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    lambda_literal_parser(expr).map(|lambda| {
        let span = expression_span(&lambda);
        PostfixOp::TrailingLambda { lambda, span }
    })
}

fn apply_postfix(base: Expression, op: PostfixOp) -> Expression {
    match op {
        PostfixOp::Call {
            args,
            span: suffix_span,
        } => {
            let span = merge_spans(&expression_span(&base), &suffix_span);
            Expression::Call {
                function: Box::new(base),
                args,
                argument_style: CallArgumentStyle::Comma,
                span,
            }
        }
        PostfixOp::Member {
            property,
            span: suffix_span,
        } => {
            let span = merge_spans(&expression_span(&base), &suffix_span);
            Expression::MemberAccess {
                object: Box::new(base),
                property,
                span,
            }
        }
        PostfixOp::NullSafeMember {
            property,
            span: suffix_span,
        } => {
            let span = merge_spans(&expression_span(&base), &suffix_span);
            Expression::NullSafeMemberAccess {
                object: Box::new(base),
                property,
                span,
            }
        }
        PostfixOp::Index {
            index,
            span: suffix_span,
        } => {
            let span = merge_spans(&expression_span(&base), &suffix_span);
            Expression::IndexAccess {
                object: Box::new(base),
                index: Box::new(index),
                span,
            }
        }
        PostfixOp::NullSafeIndex {
            index,
            span: suffix_span,
        } => {
            let span = merge_spans(&expression_span(&base), &suffix_span);
            Expression::NullSafeIndexAccess {
                object: Box::new(base),
                index: Box::new(index),
                span,
            }
        }
        PostfixOp::TrailingLambda {
            lambda,
            span: lambda_span,
        } => match base {
            Expression::Call {
                function,
                mut args,
                argument_style,
                span,
            } => {
                let new_span = merge_spans(&span, &lambda_span);
                args.push(Argument::Positional(lambda));
                Expression::Call {
                    function,
                    args,
                    argument_style,
                    span: new_span,
                }
            }
            other => {
                let span = merge_spans(&expression_span(&other), &lambda_span);
                Expression::Call {
                    function: Box::new(other),
                    args: vec![Argument::Positional(lambda)],
                    argument_style: CallArgumentStyle::Comma,
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
