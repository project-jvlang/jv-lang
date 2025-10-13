use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    Argument, ArgumentElementKind, BinaryOp, CallArgumentIssue, CallArgumentMetadata,
    CallArgumentStyle, Expression, Literal, MultilineKind, MultilineStringLiteral, Parameter,
    Pattern, SequenceDelimiter, Span, StringPart, UnaryOp, WhenArm,
};
use jv_lexer::{StringDelimiterKind, StringLiteralMetadata, Token, TokenMetadata, TokenType};

use super::json::json_expression_parser;
use super::patterns::{self, pattern_span};
use super::support::{
    expression_span, identifier, identifier_with_span, keyword, merge_spans,
    regex_literal_from_token, span_from_token, token_and, token_any_comma, token_arrow,
    token_assign, token_colon, token_comma, token_divide, token_dot, token_else, token_elvis,
    token_equal, token_greater, token_greater_equal, token_if, token_is, token_layout_comma,
    token_left_brace, token_left_bracket, token_left_paren, token_less, token_less_equal,
    token_minus, token_modulo, token_multiply, token_not, token_not_equal, token_null_safe,
    token_or, token_plus, token_question, token_range_exclusive, token_range_inclusive,
    token_right_brace, token_right_bracket, token_right_paren, token_string_end, token_string_mid,
    token_string_start, token_when, type_annotation,
};

pub(crate) fn expression_parser<B>(
    block: B,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone
where
    B: ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
{
    recursive(move |expr| {
        let block_expr = block.clone();
        let primary = choice((
            block_expr,
            json_expression_parser(),
            when_expression_parser(expr.clone()),
            forbidden_if_expression_parser(),
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
        let range = range_expression_parser(additive.clone());
        let comparison = comparison_expression_parser(range.clone());
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
    choice((
        when_expression_with_subject_parser(expr.clone()),
        when_expression_subjectless_parser(expr),
    ))
    .boxed()
}

fn when_expression_with_subject_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_when()
        .map(|token| span_from_token(&token))
        .then(
            token_left_paren()
                .ignore_then(expr.clone())
                .then_ignore(token_right_paren())
                .map(Box::new),
        )
        .then_ignore(token_left_brace())
        .then(
            when_arm_with_subject_parser(expr.clone())
                .repeated()
                .at_least(1),
        )
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
                expr: Some(subject),
                arms,
                else_arm,
                implicit_end: None,
                span,
            }
        })
}

fn when_expression_subjectless_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_when()
        .map(|token| span_from_token(&token))
        .then_ignore(token_left_brace())
        .then(
            when_arm_subjectless_parser(expr.clone())
                .repeated()
                .at_least(1),
        )
        .then(
            token_else()
                .ignore_then(token_arrow())
                .ignore_then(expr.clone())
                .map(Box::new)
                .or_not(),
        )
        .then(token_right_brace().map(|token| span_from_token(&token)))
        .map(|(((when_span, arms), else_arm), end_span)| {
            let span = merge_spans(&when_span, &end_span);
            Expression::When {
                expr: None,
                arms,
                else_arm,
                implicit_end: None,
                span,
            }
        })
}

fn split_when_guard(pattern: Pattern) -> (Pattern, Option<Expression>) {
    match pattern {
        Pattern::Guard {
            pattern, condition, ..
        } => (*pattern, Some(condition)),
        other => (other, None),
    }
}

fn when_arm_with_subject_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, WhenArm, Error = Simple<Token>> + Clone {
    patterns::when_pattern_parser(expr.clone())
        .then_ignore(token_arrow())
        .then(expr)
        .map(|(pattern, body)| {
            let pattern_span = pattern_span(&pattern);
            let body_span = expression_span(&body);
            let span = merge_spans(&pattern_span, &body_span);
            let (pattern, guard) = split_when_guard(pattern);
            WhenArm {
                pattern,
                guard,
                body,
                span,
            }
        })
        .boxed()
}

fn when_arm_subjectless_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, WhenArm, Error = Simple<Token>> + Clone {
    token_layout_comma()
        .repeated()
        .ignore_then(expr.clone())
        .then_ignore(token_layout_comma().repeated())
        .then_ignore(token_arrow())
        .then(expr)
        .map(|(condition, body)| {
            let condition_span = expression_span(&condition);
            let body_span = expression_span(&body);
            let span = merge_spans(&condition_span, &body_span);
            let pattern = Pattern::Guard {
                pattern: Box::new(Pattern::Wildcard(condition_span.clone())),
                condition,
                span: condition_span.clone(),
            };
            let (pattern, guard) = split_when_guard(pattern);

            WhenArm {
                pattern,
                guard,
                body,
                span,
            }
        })
        .boxed()
}

fn forbidden_if_expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_if()
        .map(|token| span_from_token(&token))
        .try_map(|_span, error_span| {
            let message = "JV3103: if式はサポートされていません。when式を使用してください。\nJV3103: if expressions are not supported; use when instead.\nQuick Fix: when.convert.if -> when { 条件 -> 真分岐; else -> 偽分岐 } (例: if (x > 0) a else b => when { x > 0 -> a; else -> b })\nQuick Fix: when.convert.if -> when { condition -> thenBranch; else -> elseBranch } (Example: if (x > 0) a else b => when { x > 0 -> a; else -> b })";
            Err(Simple::custom(error_span, message.to_string()))
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
        .then(token_colon().ignore_then(type_annotation()).or_not())
        .map(|(name, type_annotation)| Parameter {
            name,
            type_annotation,
            default_value: None,
            span: Span::default(),
        });

    let parenthesized = token_left_paren()
        .ignore_then(
            parameter
                .clone()
                .then(
                    token_any_comma()
                        .ignored()
                        .or_not()
                        .then(parameter.clone())
                        .map(|(_, param)| param)
                        .repeated(),
                )
                .map(|(first, rest)| {
                    let mut params = Vec::with_capacity(rest.len() + 1);
                    params.push(first);
                    params.extend(rest);
                    params
                })
                .or_not()
                .map(|params| params.unwrap_or_default()),
        )
        .then_ignore(token_any_comma().or_not())
        .then_ignore(token_right_paren());

    let bare = parameter.repeated().at_least(1);

    choice((parenthesized, bare))
}

fn array_literal_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_left_bracket()
        .map(|token| span_from_token(&token))
        .then(array_elements(expr.clone()))
        .then(token_right_bracket().map(|token| span_from_token(&token)))
        .map(|((left_span, (elements, delimiter)), right_span)| {
            let span = merge_spans(&left_span, &right_span);
            Expression::Array {
                elements,
                delimiter,
                span,
            }
        })
}

fn array_comma_error_message() -> String {
    "JV2101: 配列リテラルでカンマ区切りはサポートされません。空白または改行のみで要素を分けてください。\nJV2101: Array literals do not support comma separators. Use whitespace or newlines between elements.\nQuick Fix: arrays.whitespace.remove-commas -> [a b c]（例: [1, 2, 3] => [1 2 3])\nQuick Fix: arrays.whitespace.remove-commas -> [a b c] (Example: [1, 2, 3] => [1 2 3])\nDoc: docs/whitespace-arrays.md"
        .to_string()
}

fn array_elements<P>(
    expr: P,
) -> impl ChumskyParser<Token, (Vec<Expression>, SequenceDelimiter), Error = Simple<Token>> + Clone
where
    P: ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
{
    let separator = choice((
        token_comma().to(SequenceDelimiter::Comma),
        token_layout_comma().to(SequenceDelimiter::Whitespace),
    ));

    expr.clone()
        .then(separator.clone().then(expr.clone()).repeated())
        .then(token_comma().or_not())
        .try_map(|((first, rest), trailing_comma), span| {
            let mut elements = Vec::with_capacity(rest.len() + 1);
            let mut saw_comma = false;
            let mut saw_layout = false;

            elements.push(first);

            for (separator_kind, expr) in rest {
                match separator_kind {
                    SequenceDelimiter::Comma => saw_comma = true,
                    SequenceDelimiter::Whitespace => saw_layout = true,
                }
                elements.push(expr);
            }

            if trailing_comma.is_some() {
                saw_comma = true;
            }

            if saw_comma {
                let message = array_comma_error_message();
                return Err(Simple::custom(span, message));
            }

            let delimiter = if saw_layout {
                SequenceDelimiter::Whitespace
            } else {
                SequenceDelimiter::Comma
            };

            Ok((elements, delimiter))
        })
        .or_not()
        .map(|result| result.unwrap_or_else(|| (Vec::new(), SequenceDelimiter::Comma)))
}

fn string_interpolation_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_string_start()
        .map(|token| (token.clone(), span_from_token(&token)))
        .then(expr.clone())
        .then(
            token_string_mid()
                .map(|token| (token.clone(), span_from_token(&token)))
                .then(expr.clone())
                .repeated(),
        )
        .then(token_string_end().map(|token| (token.clone(), span_from_token(&token))))
        .map(|(((start_segment, first_expr), segments), end_segment)| {
            let (start_token, mut start_span) = start_segment;
            let (end_token, mut end_span) = end_segment;

            let mut parts = Vec::new();
            let mut raw_components = Vec::new();

            if !start_token.lexeme.is_empty() {
                parts.push(StringPart::Text(start_token.lexeme.clone()));
            }
            raw_components.push(start_token.lexeme.clone());

            parts.push(StringPart::Expression(first_expr));

            for ((text_token, _text_span), expr_part) in segments {
                if !text_token.lexeme.is_empty() {
                    parts.push(StringPart::Text(text_token.lexeme.clone()));
                }
                raw_components.push(text_token.lexeme.clone());
                parts.push(StringPart::Expression(expr_part));
            }

            if !end_token.lexeme.is_empty() {
                parts.push(StringPart::Text(end_token.lexeme.clone()));
            }
            raw_components.push(end_token.lexeme.clone());

            if start_span.end_column == start_span.start_column {
                start_span.end_column += 1;
            }
            if end_span.end_column == end_span.start_column {
                end_span.end_column += 1;
            }

            let span = merge_spans(&start_span, &end_span);

            if let Some(metadata) = string_literal_metadata(&start_token) {
                if let Some(kind) = multiline_kind_from_metadata(metadata) {
                    let raw = raw_components.concat();
                    let literal = build_multiline_literal(kind, raw, parts, span);
                    return Expression::MultilineString(literal);
                }
            }

            Expression::StringInterpolation { parts, span }
        })
}

fn literal_parser() -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| match &token.token_type {
        TokenType::String(_)
        | TokenType::Number(_)
        | TokenType::Boolean(_)
        | TokenType::Null
        | TokenType::RegexLiteral(_) => Ok(token),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
    .map(build_literal_expression)
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
    Call {
        args: Vec<Argument>,
        metadata: CallArgumentMetadata,
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
        .map(|((left_span, (args, metadata)), right_span)| {
            let span = merge_spans(&left_span, &right_span);
            PostfixOp::Call {
                args,
                metadata,
                span,
            }
        })
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

fn build_literal_expression(token: Token) -> Expression {
    let span = span_from_token(&token);
    match &token.token_type {
        TokenType::String(value) => build_string_literal(&token, value.clone(), span),
        TokenType::Number(value) => Expression::Literal(Literal::Number(value.clone()), span),
        TokenType::Boolean(value) => Expression::Literal(Literal::Boolean(*value), span),
        TokenType::Null => Expression::Literal(Literal::Null, span),
        TokenType::RegexLiteral(_) => {
            let literal = regex_literal_from_token(&token, span);
            Expression::RegexLiteral(literal)
        }
        _ => unreachable!("literal_parser filtered non-literal tokens"),
    }
}

fn build_string_literal(token: &Token, value: String, span: Span) -> Expression {
    if let Some(metadata) = string_literal_metadata(token) {
        if let Some(kind) = multiline_kind_from_metadata(metadata) {
            let literal = build_multiline_literal(kind, token.lexeme.clone(), Vec::new(), span);
            return Expression::MultilineString(literal);
        }
    }

    Expression::Literal(Literal::String(value), span)
}

fn build_multiline_literal(
    kind: MultilineKind,
    raw: String,
    parts: Vec<StringPart>,
    span: Span,
) -> MultilineStringLiteral {
    MultilineStringLiteral {
        kind,
        normalized: raw.clone(),
        raw,
        parts,
        indent: None,
        span,
    }
}

fn string_literal_metadata(token: &Token) -> Option<&StringLiteralMetadata> {
    token.metadata.iter().find_map(|metadata| match metadata {
        TokenMetadata::StringLiteral(data) => Some(data),
        _ => None,
    })
}

fn multiline_kind_from_metadata(metadata: &StringLiteralMetadata) -> Option<MultilineKind> {
    match metadata.delimiter {
        StringDelimiterKind::TripleQuote => Some(MultilineKind::TripleQuote),
        StringDelimiterKind::BacktickBlock => Some(MultilineKind::Backtick),
        _ => None,
    }
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
            metadata,
            span: suffix_span,
        } => {
            let span = merge_spans(&expression_span(&base), &suffix_span);
            Expression::Call {
                function: Box::new(base),
                args,
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
                argument_metadata,
                span,
            } => {
                let new_span = merge_spans(&span, &lambda_span);
                args.push(Argument::Positional(lambda));
                Expression::Call {
                    function,
                    args,
                    argument_metadata,
                    span: new_span,
                }
            }
            other => {
                let span = merge_spans(&expression_span(&other), &lambda_span);
                Expression::Call {
                    function: Box::new(other),
                    args: vec![Argument::Positional(lambda)],
                    argument_metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
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

fn range_expression_parser(
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
                token_is().to(BinaryOp::Is),
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
