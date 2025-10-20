use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::{
    Expression, Literal, MultilineKind, MultilineStringLiteral, Parameter, Pattern,
    SequenceDelimiter, Span, Statement, StringPart, WhenArm,
};
use jv_lexer::{StringDelimiterKind, StringLiteralMetadata, Token, TokenMetadata, TokenType};

use crate::patterns::{self, pattern_span};
use crate::support::{
    expression_span, identifier, identifier_with_span, keyword, merge_spans,
    regex_literal_from_token, span_from_token, statement_span, token_any_comma, token_arrow,
    token_colon, token_comma, token_else, token_if, token_layout_comma, token_left_brace,
    token_left_bracket, token_left_paren, token_right_brace, token_right_bracket,
    token_right_paren, token_string_end, token_string_mid, token_string_start, token_when,
    type_annotation,
};

pub(super) fn parenthesized_expression_parser(
    expr: impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    expr.delimited_by(token_left_paren(), token_right_paren())
}

pub(super) fn when_expression_parser(
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

pub(super) fn forbidden_if_expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_if()
        .map(|token| span_from_token(&token))
        .try_map(|_span, error_span| {
            let message = "JV3103: if式はサポートされていません。when式を使用してください。\nJV3103: if expressions are not supported; use when instead.\nQuick Fix: when.convert.if -> when { 条件 -> 真分岐; else -> 偽分岐 } (例: if (x > 0) a else b => when { x > 0 -> a; else -> b })\nQuick Fix: when.convert.if -> when { condition -> thenBranch; else -> elseBranch } (Example: if (x > 0) a else b => when { x > 0 -> a; else -> b })";
            Err(Simple::custom(error_span, message.to_string()))
        })
}

pub(super) fn lambda_literal_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    token_left_brace()
        .map(|token| span_from_token(&token))
        .then(
            lambda_parameter_clause()
                .then_ignore(token_arrow())
                .then(lambda_body_parser(statement)),
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

fn lambda_body_parser(
    statement: impl ChumskyParser<Token, Statement, Error = Simple<Token>> + Clone + 'static,
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    statement
        .repeated()
        .at_least(1)
        .map(|mut statements| {
            if statements.len() == 1 {
                match statements.pop().unwrap() {
                    Statement::Expression { expr, .. } => expr,
                    statement => {
                        let span = statement_span(&statement);
                        Expression::Block {
                            statements: vec![statement],
                            span,
                        }
                    }
                }
            } else {
                let span = statements
                    .first()
                    .zip(statements.last())
                    .map(|(first, last)| {
                        let start = statement_span(first);
                        let end = statement_span(last);
                        merge_spans(&start, &end)
                    })
                    .unwrap_or_else(Span::dummy);

                Expression::Block { statements, span }
            }
        })
        .boxed()
}

pub(super) fn array_literal_parser(
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
        .then(token_any_comma().or_not())
        .try_map(|((first, rest), trailing_separator), span| {
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

            if let Some(trailing) = trailing_separator {
                match trailing.token_type {
                    TokenType::Comma => saw_comma = true,
                    TokenType::LayoutComma => saw_layout = true,
                    _ => {}
                }
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

pub(super) fn string_interpolation_parser(
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

pub(super) fn literal_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    filter_map(|span, token: Token| match &token.token_type {
        TokenType::String(_)
        | TokenType::Number(_)
        | TokenType::Character(_)
        | TokenType::Boolean(_)
        | TokenType::Null
        | TokenType::RegexLiteral(_) => Ok(token),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
    .map(build_literal_expression)
}

pub(super) fn this_expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    keyword("this").map(|token| Expression::This(span_from_token(&token)))
}

pub(super) fn super_expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    keyword("super").map(|token| Expression::Super(span_from_token(&token)))
}

pub(super) fn identifier_expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    identifier_with_span().map(|(name, span)| Expression::Identifier(name, span))
}

fn build_literal_expression(token: Token) -> Expression {
    let span = span_from_token(&token);
    match &token.token_type {
        TokenType::String(value) => build_string_literal(&token, value.clone(), span),
        TokenType::Number(value) => {
            let rendered = rebuild_number_literal(&token, value);
            Expression::Literal(Literal::Number(rendered), span)
        }
        TokenType::Character(value) => Expression::Literal(Literal::Character(*value), span),
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

fn rebuild_number_literal(token: &Token, normalized: &str) -> String {
    let mut rendered = normalized.to_string();
    if let Some(suffix) = number_literal_suffix(token) {
        rendered.push(suffix);
    }
    rendered
}

fn number_literal_suffix(token: &Token) -> Option<char> {
    token.metadata.iter().find_map(|metadata| match metadata {
        TokenMetadata::NumberLiteral(info) => info.suffix,
        _ => None,
    })
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
