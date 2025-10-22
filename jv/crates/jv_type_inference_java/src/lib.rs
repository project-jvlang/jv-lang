//! Type lowering utilities shared between Rowan and Chumsky parsing routes.
//!
//! このクレートは jv の型推論パイプラインに向けた共通の型ローワリング
//! エントリーポイントを提供する。Rowan ベースのローワラーはトークン列から、
//! Chumsky ベースのローワラーは既存の `TypeAnnotation` から同じインターフェースで
//! 型情報を取得できる。

use chumsky::error::{Simple, SimpleReason};
use chumsky::Parser as ChumskyParser;
use jv_ast::{Span, TypeAnnotation};
use jv_lexer::{Token, TokenType};
use jv_parser_syntax_support::support::{
    parsers::type_annotation as type_annotation_parser,
    spans::{merge_spans, span_from_token},
};
use std::borrow::Cow;
use thiserror::Error;

/// 型注釈ローワリングの入力ソース。
#[derive(Debug, Clone)]
pub enum TypeAnnotationSource<'a> {
    /// Rowan 構文木から取得したトークン列。
    Tokens(&'a [Token]),
    /// すでに解析済みの `TypeAnnotation`。
    Parsed(&'a TypeAnnotation),
}

/// 型ローワリング結果。
#[derive(Debug, Clone)]
pub struct LoweredTypeAnnotation {
    annotation: TypeAnnotation,
    span: Option<Span>,
}

impl LoweredTypeAnnotation {
    /// 解析済みの `TypeAnnotation` を返す。
    pub fn annotation(&self) -> &TypeAnnotation {
        &self.annotation
    }

    /// 解析済みの `TypeAnnotation` を消費して取得する。
    pub fn into_annotation(self) -> TypeAnnotation {
        self.annotation
    }

    /// ローワリング対象のソーススパン（存在する場合）。
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

/// 型ローワリング時に発生するエラー。
#[derive(Debug, Clone, Error, PartialEq)]
#[error("{message}")]
pub struct TypeLoweringError {
    kind: TypeLoweringErrorKind,
    message: Cow<'static, str>,
    span: Option<Span>,
}

impl TypeLoweringError {
    /// エラー種別を返す。
    pub fn kind(&self) -> TypeLoweringErrorKind {
        self.kind
    }

    /// エラー本文を返す。
    pub fn message(&self) -> &str {
        &self.message
    }

    /// エラー位置のスパン（存在する場合）。
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    fn new(
        kind: TypeLoweringErrorKind,
        message: impl Into<Cow<'static, str>>,
        span: Option<Span>,
    ) -> Self {
        Self {
            kind,
            message: message.into(),
            span,
        }
    }
}

/// 型ローワリングにおけるエラーの分類。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeLoweringErrorKind {
    /// トークン列が空で型を特定できない。
    Empty,
    /// 型注釈として解析できなかった。
    Parse,
}

/// 型注釈をローワリングする。Rowan/Chumsky 双方で利用可能な共通エントリ。
pub fn lower_type_annotation(
    source: TypeAnnotationSource<'_>,
) -> Result<LoweredTypeAnnotation, TypeLoweringError> {
    match source {
        TypeAnnotationSource::Parsed(annotation) => Ok(LoweredTypeAnnotation {
            annotation: annotation.clone(),
            span: None,
        }),
        TypeAnnotationSource::Tokens(tokens) => lower_from_tokens(tokens),
    }
}

/// Rowan ローワラー向け: トークン列から型注釈を解析する。
pub fn lower_type_annotation_from_tokens(
    tokens: &[Token],
) -> Result<LoweredTypeAnnotation, TypeLoweringError> {
    lower_from_tokens(tokens)
}

/// Chumsky 経路向け: 既存 `TypeAnnotation` をそのまま包む。
pub fn lower_type_annotation_from_parsed(annotation: &TypeAnnotation) -> LoweredTypeAnnotation {
    LoweredTypeAnnotation {
        annotation: annotation.clone(),
        span: None,
    }
}

fn lower_from_tokens(tokens: &[Token]) -> Result<LoweredTypeAnnotation, TypeLoweringError> {
    let filtered: Vec<Token> = tokens
        .iter()
        .filter(|token| !is_trivia_token(token))
        .cloned()
        .collect();

    if filtered.is_empty() {
        return Err(TypeLoweringError::new(
            TypeLoweringErrorKind::Empty,
            "型注釈に解釈可能なトークンが存在しません",
            None,
        ));
    }

    let parser = type_annotation_parser();
    match parser.parse(filtered.clone()) {
        Ok(annotation) => {
            let span = tokens_span(&filtered);
            Ok(LoweredTypeAnnotation { annotation, span })
        }
        Err(errors) => {
            let span = errors
                .first()
                .and_then(|err| error_span(err, &filtered))
                .or_else(|| tokens_span(&filtered));
            let message = format_parse_errors(&errors);
            Err(TypeLoweringError::new(
                TypeLoweringErrorKind::Parse,
                message,
                span,
            ))
        }
    }
}

fn is_trivia_token(token: &Token) -> bool {
    matches!(
        token.token_type,
        TokenType::Whitespace(_)
            | TokenType::Newline
            | TokenType::LineComment(_)
            | TokenType::BlockComment(_)
            | TokenType::JavaDocComment(_)
            | TokenType::LayoutComma
            | TokenType::Eof
    )
}

fn tokens_span(tokens: &[Token]) -> Option<Span> {
    let first = tokens.first()?;
    let last = tokens.last().unwrap_or(first);
    let start = span_from_token(first);
    let end = span_from_token(last);
    Some(merge_spans(&start, &end))
}

fn format_parse_errors(errors: &[Simple<Token>]) -> Cow<'static, str> {
    if errors.is_empty() {
        return Cow::Borrowed("型注釈の解析に失敗しました");
    }

    let mut messages = Vec::with_capacity(errors.len());
    for error in errors {
        messages.push(format_single_error(error));
    }
    Cow::Owned(messages.join("; "))
}

fn format_single_error(error: &Simple<Token>) -> String {
    match error.reason() {
        SimpleReason::Custom(message) => message.clone(),
        SimpleReason::Unexpected => match error.found() {
            Some(token) => format!("想定外のトークン `{}`", token.lexeme.as_str()),
            None => "入力の終端で型注釈が未完です".to_string(),
        },
        SimpleReason::Unclosed { span: _, delimiter } => {
            format!("`{}` が閉じられていません", delimiter.lexeme.as_str())
        }
    }
}

fn error_span(error: &Simple<Token>, tokens: &[Token]) -> Option<Span> {
    if tokens.is_empty() {
        return None;
    }
    let range = error.span();
    let tokens_len = tokens.len();
    let start_index = range.start.min(tokens_len.saturating_sub(1));
    let raw_end = if range.end == range.start {
        range.start
    } else {
        range.end.saturating_sub(1)
    };
    let end_index = raw_end.min(tokens_len.saturating_sub(1)).max(start_index);
    let start_token = tokens
        .get(start_index)
        .unwrap_or_else(|| tokens.last().unwrap());
    let end_token = tokens
        .get(end_index)
        .unwrap_or_else(|| tokens.last().unwrap());
    Some(merge_spans(
        &span_from_token(start_token),
        &span_from_token(end_token),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token(token_type: TokenType, lexeme: &str) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            line: 0,
            column: 0,
            leading_trivia: Default::default(),
            diagnostic: None,
            metadata: Vec::new(),
        }
    }

    #[test]
    fn lower_simple_identifier() {
        let tokens = vec![token(TokenType::Identifier("Int".into()), "Int")];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower");
        assert_eq!(
            lowered.annotation(),
            &TypeAnnotation::Simple("Int".to_string())
        );
    }

    #[test]
    fn lower_nullable_type() {
        let tokens = vec![
            token(TokenType::Identifier("String".into()), "String"),
            token(TokenType::Question, "?"),
        ];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower");
        match lowered.annotation() {
            TypeAnnotation::Nullable(inner) => match inner.as_ref() {
                TypeAnnotation::Simple(name) => assert_eq!(name, "String"),
                other => panic!("unexpected inner annotation: {:?}", other),
            },
            other => panic!("unexpected annotation: {:?}", other),
        }
    }

    #[test]
    fn lower_reports_parse_error() {
        let tokens = vec![token(TokenType::Question, "?")];
        let error = lower_type_annotation_from_tokens(&tokens).expect_err("should error");
        assert_eq!(error.kind(), TypeLoweringErrorKind::Parse);
    }

    #[test]
    fn lower_from_parsed_clone() {
        let annotation = TypeAnnotation::Simple("Int".into());
        let lowered = lower_type_annotation_from_parsed(&annotation);
        assert_eq!(lowered.annotation(), &annotation);
    }
}
