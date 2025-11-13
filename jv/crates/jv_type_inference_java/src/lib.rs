//! Type lowering utilities shared between Rowan and Chumsky parsing routes.
//!
//! このクレートは jv の型推論パイプラインに向けた共通の型ローワリング
//! エントリーポイントを提供する。Rowan ベースのローワラーはトークン列から、
//! Chumsky ベースのローワラーは既存の `TypeAnnotation` から同じインターフェースで
//! 型情報を取得できる。

use jv_ast::{types::UnitSymbol, Span, TypeAnnotation};
use jv_lexer::{Token, TokenType};
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

    if let Some(lowered) = try_lower_tuple_annotation(tokens, &filtered) {
        return Ok(lowered);
    }

    match TypeAnnotationParser::new(&filtered).parse() {
        Ok(annotation) => {
            let span = tokens_span(&filtered);
            Ok(LoweredTypeAnnotation { annotation, span })
        }
        Err(error) => {
            let ParseError { message, span } = error;
            let diagnostic_span = span.or_else(|| tokens_span(&filtered));
            Err(TypeLoweringError::new(
                TypeLoweringErrorKind::Parse,
                message,
                diagnostic_span,
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
            | TokenType::FieldNameLabel(_)
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

fn try_lower_tuple_annotation(
    tokens: &[Token],
    filtered: &[Token],
) -> Option<LoweredTypeAnnotation> {
    let (mut base_annotation, base_span, tuple_end_index) =
        detect_tuple_annotation(tokens, filtered)?;
    let span = base_span.or_else(|| tokens_span(filtered));
    // Apply array and nullable modifiers by re-parsing the trimmed suffix portion.
    let raw_text = tokens_to_text(tokens);
    let trimmed = raw_text.trim().to_string();

    // Remove parentheses part to inspect suffixes. `detect_tuple_annotation` guarantees
    // the tuple text is at the beginning of the trimmed string.
    let mut suffix = trimmed[tuple_end_index..].trim_start().to_string();

    while suffix.starts_with("[]") {
        base_annotation = TypeAnnotation::Array(Box::new(base_annotation));
        suffix = suffix[2..].trim_start().to_string();
    }

    if suffix.starts_with('?') {
        base_annotation = TypeAnnotation::Nullable(Box::new(base_annotation));
        suffix = suffix[1..].trim_start().to_string();
    }

    if !suffix.is_empty() {
        return None;
    }

    Some(LoweredTypeAnnotation {
        annotation: base_annotation,
        span,
    })
}

fn detect_tuple_annotation(
    tokens: &[Token],
    filtered: &[Token],
) -> Option<(TypeAnnotation, Option<Span>, usize)> {
    if filtered.first()?.token_type != TokenType::LeftParen {
        return None;
    }

    let joined = tokens_to_text(tokens);
    let trimmed = joined.trim();

    let (tuple_text, tuple_end_index) = extract_tuple_text(trimmed)?;
    let elements = match tuple_elements(tuple_text) {
        Ok(elements) if elements.len() >= 2 => elements,
        _ => return None,
    };
    let normalized_text = format!("({})", elements.join(" "));

    // Build the base annotation using the normalized tuple text.
    let annotation = TypeAnnotation::Simple(normalized_text);
    let span = tokens_span(filtered);

    // Ensure that there are no unexpected characters immediately following the tuple
    // opening sequence such as an arrow that would indicate a function type.
    let suffix = trimmed[tuple_end_index..].trim_start();
    if suffix.starts_with("->") {
        return None;
    }

    Some((annotation, span, tuple_end_index))
}

fn tokens_to_text(tokens: &[Token]) -> String {
    let mut text = String::new();
    for token in tokens {
        if matches!(token.token_type, TokenType::Eof) {
            continue;
        }
        let trivia = &token.leading_trivia;
        for _ in 0..trivia.newlines {
            text.push('\n');
        }
        for _ in 0..trivia.spaces {
            text.push(' ');
        }
        text.push_str(token.lexeme.as_str());
    }
    text
}

fn extract_tuple_text(text: &str) -> Option<(&str, usize)> {
    let trimmed = text.trim_start();
    if !trimmed.starts_with('(') {
        return None;
    }

    let mut depth = 0usize;
    for (idx, ch) in trimmed.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 {
                    let end_index = idx + 1;
                    let tuple_text = &trimmed[..end_index];
                    return Some((tuple_text, end_index));
                }
            }
            _ => {}
        }
    }
    None
}

fn tuple_elements(value: &str) -> Result<Vec<String>, ()> {
    if value.len() < 2 {
        return Err(());
    }
    let inner = &value[1..value.len() - 1];
    split_tuple_elements(inner)
}

fn split_tuple_elements(value: &str) -> Result<Vec<String>, ()> {
    let mut elements = Vec::new();
    let mut current = String::new();
    let mut paren_depth = 0usize;
    let mut angle_depth = 0usize;
    let mut square_depth = 0usize;

    let mut chars = value.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '(' => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' => {
                if paren_depth == 0 {
                    return Err(());
                }
                paren_depth -= 1;
                current.push(ch);
            }
            '<' => {
                angle_depth += 1;
                current.push(ch);
            }
            '>' => {
                if angle_depth == 0 {
                    return Err(());
                }
                angle_depth -= 1;
                current.push(ch);
            }
            '[' => {
                square_depth += 1;
                current.push(ch);
            }
            ']' => {
                if square_depth == 0 {
                    return Err(());
                }
                square_depth -= 1;
                current.push(ch);
            }
            ',' => {
                if paren_depth == 0 && angle_depth == 0 && square_depth == 0 {
                    flush_segment(&mut current, &mut elements);
                } else {
                    current.push(ch);
                }
            }
            ch if ch.is_whitespace() => {
                if paren_depth == 0 && angle_depth == 0 && square_depth == 0 {
                    flush_segment(&mut current, &mut elements);
                } else {
                    current.push(ch);
                }
            }
            _ => current.push(ch),
        }
    }

    if paren_depth != 0 || angle_depth != 0 || square_depth != 0 {
        return Err(());
    }

    flush_segment(&mut current, &mut elements);

    if elements.is_empty() {
        return Err(());
    }

    Ok(elements)
}

fn flush_segment(buffer: &mut String, elements: &mut Vec<String>) {
    let trimmed = buffer.trim();
    if !trimmed.is_empty() {
        elements.push(trimmed.to_string());
    }
    buffer.clear();
}

#[derive(Debug)]
struct ParseError {
    message: Cow<'static, str>,
    span: Option<Span>,
}

impl ParseError {
    fn new(message: impl Into<Cow<'static, str>>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

struct TypeAnnotationParser<'a> {
    tokens: &'a [Token],
    position: usize,
}

impl<'a> TypeAnnotationParser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    fn parse(mut self) -> Result<TypeAnnotation, ParseError> {
        let ty = self.parse_type_annotation()?;
        if let Some(token) = self.peek() {
            return Err(ParseError::new(
                format!("型注釈の末尾に余分なトークン `{}` があります", token.lexeme),
                Some(span_from_token(token)),
            ));
        }
        Ok(ty)
    }

    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation, ParseError> {
        let mut ty = if self.peek_is(|kind| matches!(kind, TokenType::LeftParen)) {
            self.parse_function_type()?
        } else {
            self.parse_named_type()?
        };

        if let Some((unit_symbol, implicit)) = self.try_parse_unit_suffix()? {
            ty = TypeAnnotation::Unit {
                base: Box::new(ty),
                unit: unit_symbol,
                implicit,
            };
        }

        while self
            .consume_if(|kind| matches!(kind, TokenType::LeftBracket))
            .is_some()
        {
            self.expect(|kind| matches!(kind, TokenType::RightBracket), "`]`")?;
            ty = TypeAnnotation::Array(Box::new(ty));
        }

        if self
            .consume_if(|kind| matches!(kind, TokenType::Question))
            .is_some()
        {
            ty = TypeAnnotation::Nullable(Box::new(ty));
        }

        Ok(ty)
    }

    fn parse_function_type(&mut self) -> Result<TypeAnnotation, ParseError> {
        self.expect(|kind| matches!(kind, TokenType::LeftParen), "`(`")?;
        let mut params = Vec::new();
        if !self.peek_is(|kind| matches!(kind, TokenType::RightParen)) {
            loop {
                params.push(self.parse_type_annotation()?);
                if self
                    .consume_if(|kind| matches!(kind, TokenType::Comma))
                    .is_some()
                {
                    continue;
                }
                break;
            }
        }
        self.expect(|kind| matches!(kind, TokenType::RightParen), "`)`")?;
        self.expect(|kind| matches!(kind, TokenType::Arrow), "`->`")?;
        let return_type = self.parse_type_annotation()?;
        Ok(TypeAnnotation::Function {
            params,
            return_type: Box::new(return_type),
        })
    }

    fn parse_named_type(&mut self) -> Result<TypeAnnotation, ParseError> {
        let (name, _span) = self.parse_qualified_name()?;

        if self
            .consume_if(|kind| matches!(kind, TokenType::Less))
            .is_some()
        {
            let args = self.parse_type_arguments()?;
            Ok(TypeAnnotation::Generic {
                name,
                type_args: args,
            })
        } else {
            Ok(TypeAnnotation::Simple(name))
        }
    }

    fn try_parse_unit_suffix(&mut self) -> Result<Option<(UnitSymbol, bool)>, ParseError> {
        if self.peek_is(|kind| matches!(kind, TokenType::At)) {
            self.advance();
            if self.peek_is(|kind| matches!(kind, TokenType::LeftBracket))
                && self.peek_n_is(1, |kind| matches!(kind, TokenType::RightBracket))
            {
                return Err(ParseError::new(
                    "単位注釈に識別子が指定されていません",
                    self.peek().map(span_from_token),
                ));
            }

            let symbol = if self.peek_is(|kind| {
                matches!(kind, TokenType::Identifier(_)) || matches!(kind, TokenType::Invalid(_))
            }) {
                self.parse_identifier_symbol()?
            } else if self.peek_is(|kind| matches!(kind, TokenType::LeftBracket)) {
                self.parse_bracket_symbol()?
            } else {
                return Err(ParseError::new(
                    "単位注釈の形式が正しくありません",
                    self.peek().map(span_from_token),
                ));
            };

            return Ok(Some((symbol, false)));
        }

        if self.peek_is(|kind| matches!(kind, TokenType::Identifier(_))) {
            let symbol = self.parse_identifier_symbol()?;
            return Ok(Some((symbol, true)));
        }

        if self.peek_is(|kind| matches!(kind, TokenType::LeftBracket))
            && !self.peek_n_is(1, |kind| matches!(kind, TokenType::RightBracket))
        {
            let symbol = self.parse_bracket_symbol()?;
            return Ok(Some((symbol, true)));
        }

        Ok(None)
    }

    fn parse_identifier_symbol(&mut self) -> Result<UnitSymbol, ParseError> {
        let (mut name, mut span) = if self.peek_is(|kind| matches!(kind, TokenType::Identifier(_)))
        {
            let ident = self.expect_identifier()?;
            let text = identifier_text(ident).map(str::to_string).ok_or_else(|| {
                ParseError::new(
                    "単位注釈の識別子が取得できませんでした",
                    Some(span_from_token(ident)),
                )
            })?;
            (text, span_from_token(ident))
        } else if self.peek_is(|kind| matches!(kind, TokenType::Invalid(_))) {
            let invalid = self.advance().expect("invalid token should be present");
            (invalid.lexeme.clone(), span_from_token(invalid))
        } else {
            return Err(ParseError::new(
                "単位注釈の識別子が取得できませんでした",
                self.peek().map(span_from_token),
            ));
        };
        let mut has_default_marker = false;

        if self.peek_is(|kind| matches!(kind, TokenType::Not)) {
            let bang = self.advance().unwrap();
            name.push_str(bang.lexeme.as_str());
            let bang_span = span_from_token(bang);
            span = merge_spans(&span, &bang_span);
            has_default_marker = true;
        }

        Ok(UnitSymbol {
            name,
            is_bracketed: false,
            has_default_marker,
            span,
        })
    }

    fn parse_bracket_symbol(&mut self) -> Result<UnitSymbol, ParseError> {
        let open = self.expect(|kind| matches!(kind, TokenType::LeftBracket), "`[`")?;
        let mut text = open.lexeme.clone();
        let mut span = span_from_token(open);
        let mut has_default_marker = false;

        loop {
            let token = self.advance().ok_or_else(|| {
                ParseError::new("単位注釈の括弧が閉じていません", Some(span.clone()))
            })?;
            text.push_str(token.lexeme.as_str());
            let token_span = span_from_token(token);
            span = merge_spans(&span, &token_span);
            match token.token_type {
                TokenType::RightBracket => break,
                TokenType::Not => has_default_marker = true,
                _ => {}
            }
        }

        Ok(UnitSymbol {
            name: text,
            is_bracketed: true,
            has_default_marker,
            span,
        })
    }

    fn parse_type_arguments(&mut self) -> Result<Vec<TypeAnnotation>, ParseError> {
        let mut args = Vec::new();
        if self
            .consume_if(|kind| matches!(kind, TokenType::Greater))
            .is_some()
        {
            return Ok(args);
        }
        loop {
            args.push(self.parse_type_annotation()?);
            if self
                .consume_if(|kind| matches!(kind, TokenType::Comma))
                .is_some()
            {
                continue;
            }
            self.expect(|kind| matches!(kind, TokenType::Greater), "`>`")?;
            break;
        }
        Ok(args)
    }

    fn parse_qualified_name(&mut self) -> Result<(String, Span), ParseError> {
        let first = self.expect_identifier()?;
        let mut name = identifier_text(first).map(str::to_string).ok_or_else(|| {
            ParseError::new("識別子を解析できませんでした", Some(span_from_token(first)))
        })?;
        let mut span = span_from_token(first);

        while self
            .consume_if(|kind| matches!(kind, TokenType::Dot))
            .is_some()
        {
            let next = self.expect_identifier()?;
            let next_name = identifier_text(next).ok_or_else(|| {
                ParseError::new("識別子を解析できませんでした", Some(span_from_token(next)))
            })?;
            name.push('.');
            name.push_str(next_name);
            let next_span = span_from_token(next);
            span = merge_spans(&span, &next_span);
        }

        Ok((name, span))
    }

    fn expect_identifier(&mut self) -> Result<&'a Token, ParseError> {
        self.expect(|kind| matches!(kind, TokenType::Identifier(_)), "識別子")
    }

    fn expect<F>(
        &mut self,
        predicate: F,
        description: &'static str,
    ) -> Result<&'a Token, ParseError>
    where
        F: Fn(&TokenType) -> bool,
    {
        match self.peek() {
            Some(token) if predicate(&token.token_type) => {
                self.advance();
                Ok(token)
            }
            Some(token) => Err(ParseError::new(
                format!(
                    "{description} を期待しましたが `{}` が見つかりました",
                    token.lexeme
                ),
                Some(span_from_token(token)),
            )),
            None => Err(ParseError::new(
                format!("{description} を期待しましたが、入力の終端に達しました"),
                None,
            )),
        }
    }

    fn consume_if<F>(&mut self, predicate: F) -> Option<&'a Token>
    where
        F: Fn(&TokenType) -> bool,
    {
        if let Some(token) = self.peek() {
            if predicate(&token.token_type) {
                self.position += 1;
                return Some(token);
            }
        }
        None
    }

    fn peek_is<F>(&self, predicate: F) -> bool
    where
        F: Fn(&TokenType) -> bool,
    {
        self.peek()
            .map(|token| predicate(&token.token_type))
            .unwrap_or(false)
    }

    fn peek_n_is<F>(&self, offset: usize, predicate: F) -> bool
    where
        F: Fn(&TokenType) -> bool,
    {
        self.tokens
            .get(self.position + offset)
            .map(|token| predicate(&token.token_type))
            .unwrap_or(false)
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.position)
    }

    fn advance(&mut self) -> Option<&'a Token> {
        let token = self.tokens.get(self.position)?;
        self.position += 1;
        Some(token)
    }
}

fn identifier_text(token: &Token) -> Option<&str> {
    match &token.token_type {
        TokenType::Identifier(name) => Some(name.as_str()),
        _ => None,
    }
}

fn span_from_token(token: &Token) -> Span {
    Span::from_token_lexeme(token.line, token.column, &token.lexeme)
}

fn merge_spans(start: &Span, end: &Span) -> Span {
    start.merge(end)
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

    #[test]
    fn lower_function_type() {
        let tokens = vec![
            token(TokenType::LeftParen, "("),
            token(TokenType::Identifier("Int".into()), "Int"),
            token(TokenType::RightParen, ")"),
            token(TokenType::Arrow, "->"),
            token(TokenType::Identifier("String".into()), "String"),
        ];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower");
        match lowered.annotation() {
            TypeAnnotation::Function {
                params,
                return_type,
            } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], TypeAnnotation::Simple("Int".into()));
                assert_eq!(
                    return_type.as_ref(),
                    &TypeAnnotation::Simple("String".into())
                );
            }
            other => panic!("unexpected annotation: {:?}", other),
        }
    }

    #[test]
    fn lower_generic_type_arguments() {
        let tokens = vec![
            token(TokenType::Identifier("Result".into()), "Result"),
            token(TokenType::Less, "<"),
            token(TokenType::Identifier("String".into()), "String"),
            token(TokenType::Comma, ","),
            token(TokenType::Identifier("Error".into()), "Error"),
            token(TokenType::Greater, ">"),
        ];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower");
        match lowered.annotation() {
            TypeAnnotation::Generic { name, type_args } => {
                assert_eq!(name, "Result");
                assert_eq!(
                    type_args,
                    &[
                        TypeAnnotation::Simple("String".into()),
                        TypeAnnotation::Simple("Error".into())
                    ]
                );
            }
            other => panic!("unexpected annotation: {:?}", other),
        }
    }

    #[test]
    fn lower_unit_type_annotation_with_at() {
        let tokens = vec![
            token(TokenType::Identifier("Double".into()), "Double"),
            token(TokenType::At, "@"),
            token(TokenType::Identifier("km".into()), "km"),
        ];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower");
        match lowered.annotation() {
            TypeAnnotation::Unit {
                ref base,
                unit,
                implicit,
            } => {
                assert!(!implicit);
                assert!(matches!(base.as_ref(), TypeAnnotation::Simple(name) if name == "Double"));
                assert_eq!(unit.name, "km");
                assert!(!unit.is_bracketed);
                assert!(!unit.has_default_marker);
            }
            other => panic!("unexpected annotation: {:?}", other),
        }
    }

    #[test]
    fn lower_tuple_type_annotation() {
        let tokens = vec![
            token(TokenType::LeftParen, "("),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::Identifier("Int".into()), "Int"),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::Identifier("String".into()), "String"),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::RightParen, ")"),
        ];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower tuple");
        assert_eq!(
            lowered.annotation(),
            &TypeAnnotation::Simple("(Int String)".into())
        );
    }

    #[test]
    fn lower_tuple_type_annotation_with_newlines() {
        let tokens = vec![
            token(TokenType::LeftParen, "("),
            token(TokenType::Newline, "\n"),
            token(TokenType::Identifier("Int".into()), "Int"),
            token(TokenType::Newline, "\n"),
            token(TokenType::Identifier("String".into()), "String"),
            token(TokenType::Newline, "\n"),
            token(TokenType::RightParen, ")"),
        ];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower tuple");
        assert_eq!(
            lowered.annotation(),
            &TypeAnnotation::Simple("(Int String)".into())
        );
    }

    #[test]
    fn lower_nullable_tuple_type_annotation() {
        let tokens = vec![
            token(TokenType::LeftParen, "("),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::Identifier("Int".into()), "Int"),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::Identifier("String".into()), "String"),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::RightParen, ")"),
            token(TokenType::Question, "?"),
        ];
        let lowered =
            lower_type_annotation_from_tokens(&tokens).expect("should lower nullable tuple");
        match lowered.annotation() {
            TypeAnnotation::Nullable(inner) => match inner.as_ref() {
                TypeAnnotation::Simple(text) => assert_eq!(text, "(Int String)"),
                other => panic!("unexpected inner annotation: {:?}", other),
            },
            other => panic!("unexpected annotation: {:?}", other),
        }
    }

    #[test]
    fn lower_unit_type_annotation_with_bracket() {
        let tokens = vec![
            token(TokenType::Identifier("Double".into()), "Double"),
            token(TokenType::LeftBracket, "["),
            token(TokenType::Identifier("℃".into()), "℃"),
            token(TokenType::RightBracket, "]"),
        ];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower");
        match lowered.annotation() {
            TypeAnnotation::Unit {
                ref base,
                unit,
                implicit,
            } => {
                assert!(implicit);
                assert!(matches!(base.as_ref(), TypeAnnotation::Simple(name) if name == "Double"));
                assert_eq!(unit.name, "[℃]");
                assert!(unit.is_bracketed);
            }
            other => panic!("unexpected annotation: {:?}", other),
        }
    }

    #[test]
    fn lower_array_tuple_type_annotation() {
        let tokens = vec![
            token(TokenType::LeftParen, "("),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::Identifier("Int".into()), "Int"),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::Identifier("String".into()), "String"),
            token(TokenType::Whitespace(" ".into()), " "),
            token(TokenType::RightParen, ")"),
            token(TokenType::LeftBracket, "["),
            token(TokenType::RightBracket, "]"),
        ];
        let lowered = lower_type_annotation_from_tokens(&tokens).expect("should lower tuple array");
        match lowered.annotation() {
            TypeAnnotation::Array(inner) => match inner.as_ref() {
                TypeAnnotation::Simple(text) => assert_eq!(text, "(Int String)"),
                other => panic!("unexpected inner annotation: {:?}", other),
            },
            other => panic!("unexpected annotation: {:?}", other),
        }
    }

    #[test]
    fn lower_tuple_type_annotation_from_lexer() {
        use jv_lexer::Lexer;
        let mut lexer = Lexer::new("(Int String)".to_string());
        let mut tokens = lexer.tokenize().expect("lexing should succeed");
        if let Some(last) = tokens.last() {
            if matches!(last.token_type, TokenType::Eof) {
                tokens.pop();
            }
        }
        let lowered =
            lower_type_annotation_from_tokens(&tokens).expect("should lower tuple annotation");
        assert_eq!(
            lowered.annotation(),
            &TypeAnnotation::Simple("(Int String)".into())
        );
    }
}
