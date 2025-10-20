use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::json::{
    JsonComment, JsonCommentKind, JsonEntry, JsonLiteral, JsonValue, NumberGrouping,
};
use jv_ast::{Expression, SequenceDelimiter, Span};
use jv_lexer::{
    JsonCommentTriviaKind, JsonConfidence, NumberGroupingKind, Token, TokenMetadata, TokenTrivia,
    TokenType,
};

use super::support::{
    merge_spans, span_from_token, token_colon, token_comma, token_left_brace, token_left_bracket,
    token_right_brace, token_right_bracket,
};

pub(crate) fn json_expression_parser(
) -> impl ChumskyParser<Token, Expression, Error = Simple<Token>> + Clone {
    let value = json_value_parser();
    let object = root_json_object(value.clone());
    let array = root_json_array(value);

    choice((object, array)).map(Expression::JsonLiteral)
}

fn root_json_object<P>(
    value: P,
) -> impl ChumskyParser<Token, JsonLiteral, Error = Simple<Token>> + Clone
where
    P: ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone + 'static,
{
    object_components(confident_left_brace(), value)
        .map(|(left, entries, right)| build_json_literal_from_object(left, entries, right))
}

fn root_json_array<P>(
    value: P,
) -> impl ChumskyParser<Token, JsonLiteral, Error = Simple<Token>> + Clone
where
    P: ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone + 'static,
{
    array_components(confident_left_bracket(), value).map(|(left, elements, right)| {
        let left_span = span_from_token(&left);
        let right_span = span_from_token(&right);
        let span = merge_spans(&left_span, &right_span);
        JsonLiteral {
            value: JsonValue::Array {
                elements,
                delimiter: SequenceDelimiter::Comma,
                span: span.clone(),
            },
            leading_comments: collect_json_comments(&left.leading_trivia),
            trailing_comments: collect_json_comments(&right.leading_trivia),
            span,
            inferred_schema: None,
        }
    })
}

fn build_json_literal_from_object(
    left: Token,
    entries: Vec<JsonEntry>,
    right: Token,
) -> JsonLiteral {
    let left_span = span_from_token(&left);
    let right_span = span_from_token(&right);
    let span = merge_spans(&left_span, &right_span);

    JsonLiteral {
        value: JsonValue::Object {
            entries,
            span: span.clone(),
        },
        leading_comments: collect_json_comments(&left.leading_trivia),
        trailing_comments: collect_json_comments(&right.leading_trivia),
        span,
        inferred_schema: None,
    }
}

fn json_value_parser() -> impl ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone {
    recursive(|value| {
        let object = json_object_value(value.clone());
        let array = json_array_value(value.clone());
        let string = json_string_value();
        let number = json_number_value();
        let boolean = json_boolean_value();
        let null = json_null_value();

        choice((object, array, string, number, boolean, null))
    })
}

fn json_object_value<P>(
    value: P,
) -> impl ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone
where
    P: ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone + 'static,
{
    object_components(token_left_brace(), value)
        .map(|(left, entries, right)| build_json_object_value(left, entries, right))
}

fn json_array_value<P>(
    value: P,
) -> impl ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone
where
    P: ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone + 'static,
{
    array_components(token_left_bracket(), value).map(|(left, elements, right)| {
        let left_span = span_from_token(&left);
        let right_span = span_from_token(&right);
        let span = merge_spans(&left_span, &right_span);
        JsonValue::Array {
            elements,
            delimiter: SequenceDelimiter::Comma,
            span,
        }
    })
}

fn object_components<P, S>(
    start: S,
    value: P,
) -> impl ChumskyParser<Token, (Token, Vec<JsonEntry>, Token), Error = Simple<Token>> + Clone
where
    S: ChumskyParser<Token, Token, Error = Simple<Token>> + Clone + 'static,
    P: ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone + 'static,
{
    start
        .then(
            json_entry_parser(value.clone())
                .separated_by(token_comma())
                .allow_trailing(),
        )
        .then(token_right_brace())
        .map(|((left, entries), right)| (left, entries, right))
}

fn array_components<P, S>(
    start: S,
    value: P,
) -> impl ChumskyParser<Token, (Token, Vec<JsonValue>, Token), Error = Simple<Token>> + Clone
where
    S: ChumskyParser<Token, Token, Error = Simple<Token>> + Clone + 'static,
    P: ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone + 'static,
{
    start
        .then(value.separated_by(token_comma()).allow_trailing())
        .then(token_right_bracket())
        .map(|((left, elements), right)| (left, elements, right))
}

fn json_entry_parser<P>(
    value: P,
) -> impl ChumskyParser<Token, JsonEntry, Error = Simple<Token>> + Clone
where
    P: ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone + 'static,
{
    json_key_parser()
        .then_ignore(expect_colon())
        .then(value)
        .map(|((key, key_token), value)| {
            let key_span = span_from_token(&key_token);
            let value_span = json_value_span(&value);
            let span = merge_spans(&key_span, &value_span);
            let comments = collect_json_comments(&key_token.leading_trivia);

            JsonEntry {
                key,
                comments,
                value,
                span,
            }
        })
}

fn json_key_parser() -> impl ChumskyParser<Token, (String, Token), Error = Simple<Token>> + Clone {
    choice((string_key_parser(), identifier_key_parser()))
}

fn string_key_parser() -> impl ChumskyParser<Token, (String, Token), Error = Simple<Token>> + Clone
{
    filter(|token: &Token| matches!(token.token_type, TokenType::String(_))).map(|token| {
        let key = match &token.token_type {
            TokenType::String(value) => value.clone(),
            _ => unreachable!(),
        };
        (key, token)
    })
}

fn identifier_key_parser(
) -> impl ChumskyParser<Token, (String, Token), Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Identifier(_))).map(|token| {
        let key = match &token.token_type {
            TokenType::Identifier(value) => value.clone(),
            _ => unreachable!(),
        };
        (key, token)
    })
}

fn expect_colon() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    token_colon()
}

fn json_string_value() -> impl ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::String(_))).map(|token| {
        let value = match &token.token_type {
            TokenType::String(content) => content.clone(),
            _ => unreachable!(),
        };
        let span = span_from_token(&token);
        JsonValue::String { value, span }
    })
}

fn json_number_value() -> impl ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Number(_))).map(|token| {
        let literal = match &token.token_type {
            TokenType::Number(value) => value.clone(),
            _ => unreachable!(),
        };
        let grouping = number_grouping_from_token(&token);
        let span = span_from_token(&token);
        JsonValue::Number {
            literal,
            grouping,
            span,
        }
    })
}

fn json_boolean_value() -> impl ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Boolean(_))).map(|token| {
        let value = match &token.token_type {
            TokenType::Boolean(flag) => *flag,
            _ => unreachable!(),
        };
        let span = span_from_token(&token);
        JsonValue::Boolean { value, span }
    })
}

fn json_null_value() -> impl ChumskyParser<Token, JsonValue, Error = Simple<Token>> + Clone {
    filter(|token: &Token| matches!(token.token_type, TokenType::Null)).map(|token| {
        let span = span_from_token(&token);
        JsonValue::Null { span }
    })
}

fn build_json_object_value(left: Token, entries: Vec<JsonEntry>, right: Token) -> JsonValue {
    let left_span = span_from_token(&left);
    let right_span = span_from_token(&right);
    let span = merge_spans(&left_span, &right_span);
    JsonValue::Object { entries, span }
}

fn json_value_span(value: &JsonValue) -> Span {
    match value {
        JsonValue::Object { span, .. }
        | JsonValue::Array { span, .. }
        | JsonValue::String { span, .. }
        | JsonValue::Number { span, .. }
        | JsonValue::Boolean { span, .. }
        | JsonValue::Null { span } => span.clone(),
    }
}

fn collect_json_comments(trivia: &TokenTrivia) -> Vec<JsonComment> {
    trivia
        .json_comments
        .iter()
        .map(|comment| JsonComment {
            kind: match comment.kind {
                JsonCommentTriviaKind::Line => JsonCommentKind::Line,
                JsonCommentTriviaKind::Block => JsonCommentKind::Block,
            },
            text: comment.text.clone(),
            span: comment_span(comment),
        })
        .collect()
}

fn comment_span(comment: &jv_lexer::JsonCommentTrivia) -> Span {
    let width = comment.text.chars().count().max(1);
    Span::new(
        comment.line,
        comment.column,
        comment.line,
        comment.column + width,
    )
}

fn number_grouping_from_token(token: &Token) -> NumberGrouping {
    token
        .metadata
        .iter()
        .find_map(|metadata| match metadata {
            TokenMetadata::NumberLiteral(info) => Some(match info.grouping {
                NumberGroupingKind::None => NumberGrouping::None,
                NumberGroupingKind::Comma => NumberGrouping::Comma,
                NumberGroupingKind::Underscore => NumberGrouping::Underscore,
                NumberGroupingKind::Mixed => NumberGrouping::Mixed,
            }),
            _ => None,
        })
        .unwrap_or_default()
}

fn has_high_json_confidence(token: &Token) -> bool {
    token.metadata.iter().any(|metadata| match metadata {
        TokenMetadata::PotentialJsonStart { confidence } => {
            matches!(confidence, JsonConfidence::High)
        }
        _ => false,
    })
}

fn confident_left_brace() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| {
        matches!(token.token_type, TokenType::LeftBrace) && has_high_json_confidence(token)
    })
}

fn confident_left_bracket() -> impl ChumskyParser<Token, Token, Error = Simple<Token>> + Clone {
    filter(|token: &Token| {
        matches!(token.token_type, TokenType::LeftBracket) && has_high_json_confidence(token)
    })
}
