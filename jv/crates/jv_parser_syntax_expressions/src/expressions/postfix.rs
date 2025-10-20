use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    Argument, ArgumentElementKind, CallArgumentIssue, CallArgumentMetadata, CallArgumentStyle,
    Expression, Literal, Span, Statement, TypeAnnotation,
};
use jv_lexer::Token;

use super::primary::lambda_literal_parser;
use jv_parser_syntax_support::{
    expression_span, identifier_with_span, merge_spans, span_from_token, token_any_comma, token_as,
    token_assign, token_comma, token_dot, token_greater, token_left_bracket, token_left_paren,
    token_less, token_null_safe, token_question, token_right_bracket, token_right_paren,
    type_annotation,
};

enum PostfixOp {
    Call {
        args: Vec<Argument>,
        metadata: CallArgumentMetadata,
        type_arguments: Vec<TypeAnnotation>,
        span: Span,
    },
    Member {
        property: String,
        span: Span,
    },
    NullSafeMember {
        property: String,
        span: Span,
    },
    Index {
        index: Expression,
        span: Span,
    },
    NullSafeIndex {
        index: Expression,
        span: Span,
    },
    TrailingLambda {
        lambda: Expression,
        span: Span,
    },
}

pub(super) fn postfix_expression_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
    primary: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    primary
        .then(
            choice((
                call_suffix(expr.clone()),
                null_safe_member_suffix(),
                member_suffix(),
                null_safe_index_suffix(expr.clone()),
                index_suffix(expr.clone()),
                trailing_lambda_suffix(statement),
            ))
            .repeated(),
        )
        .foldl(|base, op| apply_postfix(base, op))
}

pub(super) fn cast_expression_parser(
    operand: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    operand
        .clone()
        .then(
            token_as()
                .map(|token| span_from_token(&token))
                .then(type_annotation())
                .map(|(as_span, ty)| (ty, as_span))
                .repeated(),
        )
        .map(|(base, casts)| {
            casts.into_iter().fold(base, |expr, (target, target_span)| {
                let expr_span = expression_span(&expr);
                let span = merge_spans(&expr_span, &target_span);
                Expression::TypeCast {
                    expr: Box::new(expr),
                    target,
                    span,
                }
            })
        })
}

fn call_suffix(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    let arguments = argument_list(expr.clone());

    let with_type_arguments = call_type_arguments()
        .then(
            token_left_paren()
                .map(|token| span_from_token(&token))
                .then(arguments.clone())
                .then(token_right_paren().map(|token| span_from_token(&token))),
        )
        .map(
            |(type_arguments, ((left_span, (args, metadata)), right_span))| {
                let span = merge_spans(&left_span, &right_span);
                PostfixOp::Call {
                    args,
                    metadata,
                    type_arguments,
                    span,
                }
            },
        );

    let without_type_arguments = token_left_paren()
        .map(|token| span_from_token(&token))
        .then(arguments)
        .then(token_right_paren().map(|token| span_from_token(&token)))
        .map(|((left_span, (args, metadata)), right_span)| {
            let span = merge_spans(&left_span, &right_span);
            PostfixOp::Call {
                args,
                metadata,
                type_arguments: Vec::new(),
                span,
            }
        });

    choice((with_type_arguments, without_type_arguments))
}

fn call_type_arguments(
) -> impl ChumskyParser<Token, Vec<TypeAnnotation>, Error = Simple<Token>> + Clone {
    token_less()
        .ignore_then(
            type_annotation()
                .separated_by(token_any_comma())
                .allow_trailing(),
        )
        .then_ignore(token_greater())
}

fn call_argument_comma_error_message() -> String {
    "JV2102: 関数呼び出しでカンマ区切りはサポートされません。位置引数は空白または改行で区切ってください。\nJV2102: Function calls do not support comma separators. Separate positional arguments with whitespace or newlines.\nQuick Fix: calls.whitespace.remove-commas -> func a b c（例: plot(1, 2, 3) => plot(1 2 3))\nQuick Fix: calls.whitespace.remove-commas -> func a b c (Example: plot(1, 2, 3) => plot(1 2 3))\nDoc: docs/whitespace-arrays.md#function-calls"
        .to_string()
}

fn call_argument_named_argument_error() -> String {
    "JV1009: 空白区切りの引数リストでは名前付き引数を使用できません。カンマ区切りに戻すか、すべて位置引数にしてください。\nJV1009: Whitespace-delimited argument lists cannot include named arguments. Switch to comma-separated form or stick to positional arguments only.\nDoc: docs/whitespace-arrays.md#function-calls"
        .to_string()
}

fn argument_list(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, (Vec<Argument>, CallArgumentMetadata), Error = Simple<Token>> + Clone
{
    let comma_argument = token_comma()
        .ignore_then(argument(expr.clone()))
        .map(|argument| (CallArgumentStyle::Comma, argument));

    let whitespace_argument =
        argument(expr.clone()).map(|argument| (CallArgumentStyle::Whitespace, argument));

    argument(expr.clone())
        .then(choice((comma_argument, whitespace_argument)).repeated())
        .then(token_comma().or_not())
        .try_map(|((first, rest), trailing_comma), span| {
            let mut args = Vec::with_capacity(rest.len() + 1);
            let mut saw_comma = false;
            let mut saw_whitespace = false;
            let mut saw_named = matches!(first, Argument::Named { .. });

            args.push(first);

            for (separator_kind, argument) in rest {
                match separator_kind {
                    CallArgumentStyle::Comma => saw_comma = true,
                    CallArgumentStyle::Whitespace => saw_whitespace = true,
                }

                if matches!(argument, Argument::Named { .. }) {
                    saw_named = true;
                }

                args.push(argument);
            }

            if trailing_comma.is_some() {
                saw_comma = true;
            }

            if saw_comma {
                let message = call_argument_comma_error_message();
                return Err(Simple::custom(span, message));
            }

            if saw_whitespace {
                if saw_named {
                    let message = call_argument_named_argument_error();
                    return Err(Simple::custom(span, message));
                }
            }

            let style = if saw_whitespace {
                CallArgumentStyle::Whitespace
            } else {
                CallArgumentStyle::Comma
            };

            let metadata = build_call_argument_metadata(&args, style, saw_comma);

            Ok((args, metadata))
        })
        .or_not()
        .map(|result| result.unwrap_or_else(|| (Vec::new(), CallArgumentMetadata::default())))
}

fn build_call_argument_metadata(
    args: &[Argument],
    style: CallArgumentStyle,
    used_commas: bool,
) -> CallArgumentMetadata {
    let mut metadata = CallArgumentMetadata::with_style(style);
    metadata.used_commas = used_commas;

    if matches!(style, CallArgumentStyle::Whitespace) {
        let mut base_kind: Option<ArgumentElementKind> = None;
        let mut mismatch = false;

        for argument in args {
            let kind = match argument {
                Argument::Positional(expr) => argument_element_kind(expr),
                Argument::Named { .. } => ArgumentElementKind::Other,
            };

            if kind == ArgumentElementKind::Other {
                continue;
            }

            match base_kind {
                None => base_kind = Some(kind),
                Some(existing) if existing == kind => {}
                Some(_) => {
                    mismatch = true;
                    break;
                }
            }
        }

        if mismatch {
            metadata.separator_diagnostics.push(CallArgumentIssue {
                message: "JV1010: 空白区切りの引数では同じ種類の要素のみを並べられます。異なる型が混在する場合はカンマ区切りに戻してください。\nJV1010: Whitespace-delimited argument lists must remain homogeneous. Switch back to comma-separated arguments when mixing element kinds.\nDoc: docs/whitespace-arrays.md#function-calls".to_string(),
                span: combined_argument_span(args),
            });
        } else if let Some(kind) = base_kind {
            metadata.homogeneous_kind = Some(kind);
        }
    }

    metadata
}

fn argument_element_kind(expr: &Expression) -> ArgumentElementKind {
    match expr {
        Expression::Literal(literal, _) => match literal {
            Literal::Number(_) => ArgumentElementKind::Number,
            Literal::String(_) => ArgumentElementKind::String,
            Literal::Boolean(_) => ArgumentElementKind::Boolean,
            _ => ArgumentElementKind::Other,
        },
        Expression::MultilineString(_) | Expression::StringInterpolation { .. } => {
            ArgumentElementKind::String
        }
        Expression::JsonLiteral(_) => ArgumentElementKind::Json,
        Expression::Lambda { .. } => ArgumentElementKind::Lambda,
        _ => ArgumentElementKind::Other,
    }
}

fn argument_span(argument: &Argument) -> Option<Span> {
    match argument {
        Argument::Positional(expr) => Some(expression_span(expr)),
        Argument::Named { span, .. } => Some(span.clone()),
    }
}

fn combined_argument_span(args: &[Argument]) -> Option<Span> {
    let mut iter = args.iter().filter_map(argument_span);
    let first = iter.next()?;
    let mut last = first.clone();
    for span in iter {
        last = span;
    }
    Some(merge_spans(&first, &last))
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
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, PostfixOp, Error = Simple<Token>> + Clone {
    lambda_literal_parser(statement).map(|lambda| {
        let span = expression_span(&lambda);
        PostfixOp::TrailingLambda { lambda, span }
    })
}

fn apply_postfix(base: Expression, op: PostfixOp) -> Expression {
    match op {
        PostfixOp::Call {
            args,
            metadata,
            type_arguments,
            span: suffix_span,
        } => {
            let span = merge_spans(&expression_span(&base), &suffix_span);
            Expression::Call {
                function: Box::new(base),
                args,
                type_arguments,
                argument_metadata: metadata,
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
                type_arguments,
                argument_metadata,
                span,
            } => {
                let new_span = merge_spans(&span, &lambda_span);
                args.push(Argument::Positional(lambda));
                Expression::Call {
                    function,
                    args,
                    type_arguments,
                    argument_metadata,
                    span: new_span,
                }
            }
            other => {
                let span = merge_spans(&expression_span(&other), &lambda_span);
                Expression::Call {
                    function: Box::new(other),
                    args: vec![Argument::Positional(lambda)],
                    type_arguments: Vec::new(),
                    argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
                    span,
                }
            }
        },
    }
}
