use std::char;

use unicode_normalization::UnicodeNormalization;

use crate::{
    pipeline::{
        context::LexerContext,
        pipeline::NormalizerStage,
        types::{NormalizedToken, PreMetadata, RawToken, RawTokenKind, Span},
    },
    LayoutCommaMetadata, LayoutSequenceKind, LexError, NumberGroupingKind, NumberLiteralMetadata,
    StringDelimiterKind, StringInterpolationSegment, StringLiteralMetadata, TokenDiagnostic,
    TokenMetadata,
};

use super::json_utils::{detect_array_confidence, detect_object_confidence};

fn normalize_decimal_number(
    lexeme: &str,
    line: usize,
    column: usize,
    saw_comma: &mut bool,
    saw_underscore: &mut bool,
) -> Result<String, LexError> {
    let mut normalized = String::with_capacity(lexeme.len());
    let mut chars = lexeme.chars().peekable();
    let mut seen_digit = false;
    let mut seen_dot = false;
    let mut seen_exp = false;
    let mut exponent_digits = 0usize;

    while let Some(ch) = chars.next() {
        match ch {
            '_' => {
                *saw_underscore = true;
            }
            ',' => {
                *saw_comma = true;
            }
            '.' => {
                if seen_dot || seen_exp {
                    return Err(LexError::UnexpectedChar('.', line, column));
                }
                seen_dot = true;
                normalized.push('.');
            }
            'e' | 'E' => {
                if seen_exp {
                    return Err(LexError::UnexpectedChar('e', line, column));
                }
                seen_exp = true;
                exponent_digits = 0;
                normalized.push('e');
                if let Some(&next) = chars.peek() {
                    if next == '+' || next == '-' {
                        normalized.push(next);
                        chars.next();
                    }
                }
            }
            _ => {
                normalized.push(ch);
                seen_digit = true;
                if seen_exp {
                    exponent_digits = exponent_digits.saturating_add(1);
                }
            }
        }
    }

    if !seen_digit {
        return Err(LexError::UnexpectedChar(
            lexeme.chars().next().unwrap_or('\0'),
            line,
            column,
        ));
    }

    if seen_exp && exponent_digits == 0 {
        return Err(LexError::UnexpectedChar('e', line, column));
    }

    Ok(normalized)
}

fn detect_layout_metadata(source: &str, span: &Span) -> Option<LayoutCommaMetadata> {
    let (_prev_idx, prev_char) = prev_non_whitespace(source, span.byte_range.start)?;
    if matches!(prev_char, ',') {
        return None;
    }

    let (_, next_char) = next_non_whitespace(source, span.byte_range.end)?;
    if matches!(next_char, ',' | ']' | ')') {
        return None;
    }

    let sequence_kind = enclosing_sequence_kind(source, span.byte_range.start)?;
    match sequence_kind {
        LayoutSequenceKind::Array | LayoutSequenceKind::Call => Some(LayoutCommaMetadata {
            sequence: sequence_kind,
            explicit_separator: None,
        }),
    }
}

fn prev_non_whitespace(source: &str, mut idx: usize) -> Option<(usize, char)> {
    while idx > 0 {
        let ch = source[..idx].chars().rev().next()?;
        idx -= ch.len_utf8();
        if !ch.is_whitespace() {
            return Some((idx, ch));
        }
    }
    None
}

fn next_non_whitespace(source: &str, mut idx: usize) -> Option<(usize, char)> {
    let len = source.len();
    while idx < len {
        let ch = source[idx..].chars().next()?;
        if !ch.is_whitespace() {
            return Some((idx, ch));
        }
        idx += ch.len_utf8();
    }
    None
}

fn enclosing_sequence_kind(source: &str, pos: usize) -> Option<LayoutSequenceKind> {
    let mut stack: Vec<char> = Vec::new();
    let mut idx = 0;
    while idx < pos {
        let ch = source[idx..].chars().next().unwrap();
        let ch_len = ch.len_utf8();
        match ch {
            '[' | '(' => stack.push(ch),
            ']' => {
                if matches!(stack.last(), Some('[')) {
                    stack.pop();
                }
            }
            ')' => {
                if matches!(stack.last(), Some('(')) {
                    stack.pop();
                }
            }
            '"' => {
                idx += ch_len;
                while idx < pos {
                    let next = source[idx..].chars().next().unwrap();
                    let len = next.len_utf8();
                    if next == '\\' {
                        idx += len;
                        if idx < pos {
                            let escaped = source[idx..].chars().next().unwrap();
                            idx += escaped.len_utf8();
                        }
                        continue;
                    }
                    idx += len;
                    if next == '"' {
                        break;
                    }
                }
                continue;
            }
            _ => {}
        }
        idx += ch_len;
    }

    match stack.iter().rev().find(|&&c| c == '[' || c == '(') {
        Some('[') => Some(LayoutSequenceKind::Array),
        Some('(') => Some(LayoutSequenceKind::Call),
        _ => None,
    }
}

/// RawToken から文字列正規化・数値整形を行い、NormalizedToken を生成するステージ。
#[derive(Debug, Default)]
pub struct Normalizer {
    _private: (),
}

impl Normalizer {
    pub fn new() -> Self {
        Self { _private: () }
    }

    fn finalize_token<'source>(
        &self,
        token: RawToken<'source>,
        mut metadata: PreMetadata,
        normalized_text: String,
    ) -> NormalizedToken<'source> {
        if let Some(carry) = token.carry_over.clone() {
            if !matches!(
                token.kind,
                RawTokenKind::CommentCandidate | RawTokenKind::Eof
            ) {
                metadata
                    .provisional_metadata
                    .push(TokenMetadata::CommentCarryOver(carry));
            }
        }

        NormalizedToken::new(token, normalized_text, metadata)
    }

    fn normalize_number<'source>(
        &mut self,
        token: RawToken<'source>,
    ) -> Result<NormalizedToken<'source>, LexError> {
        let lexeme = token.text;
        let mut metadata = PreMetadata::default();
        let mut saw_comma = false;
        let mut saw_underscore = false;

        let normalized = if Self::has_radix_prefix(lexeme) {
            let (prefix, rest) = lexeme.split_at(2);
            let mut normalized = String::with_capacity(rest.len() + 2);
            normalized.push_str(prefix);
            for ch in rest.chars() {
                match ch {
                    '_' => {
                        saw_underscore = true;
                    }
                    ',' => {
                        saw_comma = true;
                    }
                    _ => normalized.push(ch),
                }
            }
            normalized
        } else {
            normalize_decimal_number(
                lexeme,
                token.span.end.line,
                token.span.end.column,
                &mut saw_comma,
                &mut saw_underscore,
            )?
        };

        let grouping = match (saw_comma, saw_underscore) {
            (true, true) => NumberGroupingKind::Mixed,
            (true, false) => NumberGroupingKind::Comma,
            (false, true) => NumberGroupingKind::Underscore,
            (false, false) => NumberGroupingKind::None,
        };

        metadata
            .provisional_metadata
            .push(TokenMetadata::NumberLiteral(NumberLiteralMetadata {
                grouping,
                original_lexeme: lexeme.to_string(),
            }));

        Ok(self.finalize_token(token, metadata, normalized))
    }

    fn has_radix_prefix(lexeme: &str) -> bool {
        let lowered = lexeme.to_ascii_lowercase();
        lowered.starts_with("0x") || lowered.starts_with("0b") || lowered.starts_with("0o")
    }

    fn normalize_string<'source>(
        &mut self,
        token: RawToken<'source>,
    ) -> Result<NormalizedToken<'source>, LexError> {
        let lexeme = token.text;
        let (delimiter_kind, opening_len) = self.detect_delimiter(lexeme);
        let closing_len = opening_len;

        if lexeme.len() < opening_len + closing_len {
            return Err(LexError::UnterminatedString(
                token.span.start.line,
                token.span.start.column,
            ));
        }

        let inner = &lexeme[opening_len..lexeme.len() - closing_len];
        let unescaped = if matches!(delimiter_kind, StringDelimiterKind::BacktickBlock) {
            inner.to_string()
        } else {
            self.unescape(inner, token.span.start)?
        };

        let normalized = unescaped.nfc().collect::<String>();

        let mut metadata = PreMetadata::default();
        let mut string_metadata = StringLiteralMetadata::from_kind(delimiter_kind);
        string_metadata.normalize_indentation =
            matches!(delimiter_kind, StringDelimiterKind::TripleQuote);
        metadata
            .provisional_metadata
            .push(TokenMetadata::StringLiteral(string_metadata));

        if let Some(segments) = self.collect_interpolation_segments(inner, &token.span)? {
            metadata
                .provisional_metadata
                .push(TokenMetadata::StringInterpolation { segments });
        }

        Ok(self.finalize_token(token, metadata, normalized))
    }

    fn collect_interpolation_segments(
        &self,
        inner: &str,
        span: &Span,
    ) -> Result<Option<Vec<StringInterpolationSegment>>, LexError> {
        if !inner.contains("${") {
            return Ok(None);
        }

        let mut segments = Vec::new();
        let mut index = 0usize;
        let bytes = inner.as_bytes();
        let mut literal_start = 0usize;
        let mut has_interpolation = false;

        while index < inner.len() {
            if bytes[index] == b'\\' {
                // Skip escaped sequence. We defer actual unescape to dedicated helper.
                index += 1;
                if index < inner.len() {
                    index += inner[index..]
                        .chars()
                        .next()
                        .map(|c| c.len_utf8())
                        .unwrap_or(1);
                }
                continue;
            }

            if bytes[index] == b'$' && index + 1 < inner.len() && bytes[index + 1] == b'{' {
                let literal_slice = &inner[literal_start..index];
                if let Some(literal) =
                    self.unescape_literal_segment(literal_slice, span.start.clone())?
                {
                    segments.push(StringInterpolationSegment::Literal(literal));
                }

                index += 2; // consume "${"
                let expr_start = index;
                let mut depth = 1usize;
                let mut expr_end = expr_start;
                while index < inner.len() {
                    let ch = inner[index..].chars().next().unwrap();
                    index += ch.len_utf8();
                    match ch {
                        '{' => depth += 1,
                        '}' => {
                            depth -= 1;
                            if depth == 0 {
                                expr_end = index - ch.len_utf8();
                                break;
                            }
                        }
                        _ => {}
                    }
                }

                if depth != 0 {
                    return Err(LexError::UnterminatedString(
                        span.start.line,
                        span.start.column,
                    ));
                }

                let expr_raw = inner[expr_start..expr_end].trim().to_string();
                segments.push(StringInterpolationSegment::Expression(expr_raw));
                has_interpolation = true;

                literal_start = index;
                continue;
            }

            index += inner[index..]
                .chars()
                .next()
                .map(|c| c.len_utf8())
                .unwrap_or(1);
        }

        let trailing_slice = &inner[literal_start..];
        if let Some(literal) = self.unescape_literal_segment(trailing_slice, span.start.clone())? {
            segments.push(StringInterpolationSegment::Literal(literal));
        }

        if has_interpolation {
            Ok(Some(segments))
        } else {
            Ok(None)
        }
    }

    fn unescape_literal_segment(
        &self,
        segment: &str,
        start: crate::pipeline::types::ScannerPosition,
    ) -> Result<Option<String>, LexError> {
        let unescaped = self.unescape(segment, start)?;
        Ok(Some(unescaped))
    }

    fn detect_delimiter(&self, lexeme: &str) -> (StringDelimiterKind, usize) {
        if lexeme.starts_with("```") {
            (StringDelimiterKind::BacktickBlock, 3)
        } else if lexeme.starts_with("\"\"\"") {
            (StringDelimiterKind::TripleQuote, 3)
        } else if lexeme.starts_with("\"") {
            (StringDelimiterKind::DoubleQuote, 1)
        } else if lexeme.starts_with("'") {
            (StringDelimiterKind::DoubleQuote, 1)
        } else {
            (StringDelimiterKind::DoubleQuote, 0)
        }
    }

    fn unescape(
        &self,
        input: &str,
        start: crate::pipeline::types::ScannerPosition,
    ) -> Result<String, LexError> {
        let mut output = String::new();
        let mut chars = input.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch != '\\' {
                output.push(ch);
                continue;
            }

            let Some(next) = chars.next() else {
                return Err(LexError::UnterminatedString(start.line, start.column));
            };

            match next {
                '\\' => output.push('\\'),
                '"' => output.push('"'),
                '\'' => output.push('\''),
                'n' => output.push('\n'),
                'r' => output.push('\r'),
                't' => output.push('\t'),
                '0' => output.push('\0'),
                'u' => {
                    if chars.next_if_eq(&'{').is_none() {
                        return Err(LexError::UnexpectedChar('u', start.line, start.column));
                    }

                    let mut hex = String::new();
                    while let Some(&digit) = chars.peek() {
                        if digit == '}' {
                            chars.next();
                            break;
                        }
                        hex.push(digit);
                        chars.next();
                    }

                    if hex.is_empty() {
                        return Err(LexError::UnexpectedChar('u', start.line, start.column));
                    }

                    let code_point = u32::from_str_radix(&hex, 16)
                        .map_err(|_| LexError::UnexpectedChar('u', start.line, start.column))?;
                    let Some(resolved) = char::from_u32(code_point) else {
                        return Err(LexError::UnexpectedChar('u', start.line, start.column));
                    };
                    output.push(resolved);
                }
                other => output.push(other),
            }
        }

        Ok(output)
    }

    fn normalize_symbol<'source>(
        &mut self,
        token: RawToken<'source>,
        ctx: &LexerContext<'source>,
    ) -> Result<NormalizedToken<'source>, LexError> {
        let mut metadata = PreMetadata::default();
        match token.text {
            "{" => {
                if let Some(confidence) =
                    detect_object_confidence(ctx.source, token.span.byte_range.end)
                {
                    metadata
                        .provisional_metadata
                        .push(TokenMetadata::PotentialJsonStart { confidence });
                }
            }
            "[" => {
                if let Some(confidence) =
                    detect_array_confidence(ctx.source, token.span.byte_range.end)
                {
                    metadata
                        .provisional_metadata
                        .push(TokenMetadata::PotentialJsonStart { confidence });
                }
            }
            _ => {}
        }

        let normalized_text = token.text.to_string();
        Ok(self.finalize_token(token, metadata, normalized_text))
    }

    fn normalize_trivia<'source>(
        &mut self,
        token: RawToken<'source>,
        ctx: &LexerContext<'source>,
    ) -> Result<NormalizedToken<'source>, LexError> {
        let mut metadata = PreMetadata::default();

        if let Some(layout_meta) = detect_layout_metadata(ctx.source, &token.span) {
            metadata
                .provisional_metadata
                .push(TokenMetadata::LayoutComma(layout_meta));
        }

        let normalized_text = token.text.to_string();
        Ok(self.finalize_token(token, metadata, normalized_text))
    }

    fn normalize_comment<'source>(
        &mut self,
        token: RawToken<'source>,
    ) -> Result<NormalizedToken<'source>, LexError> {
        let raw_text = token.text;
        let normalized_text = if raw_text.starts_with("/**") {
            raw_text.trim_start_matches('/').to_string()
        } else if raw_text.starts_with("//*") && raw_text.ends_with("*//") {
            let mut converted = String::from("/*");
            converted.push_str(&raw_text[3..]);
            converted
        } else {
            raw_text.to_string()
        };
        Ok(self.finalize_token(token, PreMetadata::default(), normalized_text))
    }
}

impl NormalizerStage for Normalizer {
    fn normalize<'source>(
        &mut self,
        token: RawToken<'source>,
        ctx: &mut LexerContext<'source>,
    ) -> Result<NormalizedToken<'source>, LexError> {
        match token.kind {
            RawTokenKind::NumberCandidate => self.normalize_number(token),
            RawTokenKind::Whitespace | RawTokenKind::CommentCandidate => {
                if matches!(token.kind, RawTokenKind::Whitespace) {
                    self.normalize_trivia(token, ctx)
                } else {
                    self.normalize_comment(token)
                }
            }
            RawTokenKind::Eof => {
                Ok(self.finalize_token(token, PreMetadata::default(), String::new()))
            }
            RawTokenKind::Identifier => {
                let normalized_text = token.text.to_string();
                Ok(self.finalize_token(token, PreMetadata::default(), normalized_text))
            }
            RawTokenKind::Symbol => {
                if token.text.starts_with('"') || token.text.starts_with("```") {
                    self.normalize_string(token)
                } else {
                    self.normalize_symbol(token, ctx)
                }
            }
        }
    }
}

/// Normalizer が生成する仮メタデータを扱いやすくするヘルパー。
impl PreMetadata {
    pub fn with_metadata(metadata: TokenMetadata) -> Self {
        let mut pre = Self::default();
        pre.provisional_metadata.push(metadata);
        pre
    }

    pub fn with_diagnostic(diagnostic: TokenDiagnostic) -> Self {
        let mut pre = Self::default();
        pre.provisional_diagnostics.push(diagnostic);
        pre
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipeline::types::{RawToken, ScannerPosition, Span};
    use crate::{
        CommentCarryOverMetadata, JsonCommentTrivia, JsonCommentTriviaKind, SourceCommentKind,
        SourceCommentTrivia,
    };

    fn make_span(len: usize) -> Span {
        make_span_with_offset(0, len, 1, 1)
    }

    fn make_span_with_offset(start: usize, len: usize, line: usize, column: usize) -> Span {
        let start_pos = ScannerPosition {
            byte_offset: start,
            line,
            column,
        };
        let end_pos = ScannerPosition {
            byte_offset: start + len,
            line,
            column: column + len,
        };
        Span::new(start..start + len, start_pos, end_pos)
    }

    fn make_raw_token(kind: RawTokenKind, text: &str) -> RawToken<'static> {
        let leaked: &'static str = Box::leak(text.to_string().into_boxed_str());
        RawToken {
            kind,
            text: leaked,
            span: make_span(leaked.len()),
            trivia: None,
            carry_over: None,
        }
    }

    #[test]
    fn normalize_string_unescapes_and_normalizes() {
        let mut normalizer = Normalizer::new();
        let raw = make_raw_token(RawTokenKind::Symbol, "\"Cafe\\u{0301}\"");
        let mut ctx = LexerContext::new("\"Cafe\\u{0301}\"");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("string normalization");

        assert_eq!(normalized.normalized_text, "Café");
        assert!(normalized
            .metadata
            .provisional_metadata
            .iter()
            .any(|meta| matches!(meta, TokenMetadata::StringLiteral(_))));
        assert!(normalized.metadata.provisional_diagnostics.is_empty());

        let string_meta = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::StringLiteral(info) => Some(info),
                _ => None,
            })
            .expect("string metadata");
        assert!(string_meta.allows_interpolation);
    }

    #[test]
    fn normalize_string_records_interpolation_segments() {
        let mut normalizer = Normalizer::new();
        let raw = make_raw_token(RawTokenKind::Symbol, "\"Hello ${name}!\"");
        let mut ctx = LexerContext::new("\"Hello ${name}!\"");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("string normalization");

        let segments = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::StringInterpolation { segments } => Some(segments.clone()),
                _ => None,
            })
            .expect("expected interpolation metadata");

        assert_eq!(
            segments,
            vec![
                StringInterpolationSegment::Literal("Hello ".to_string()),
                StringInterpolationSegment::Expression("name".to_string()),
                StringInterpolationSegment::Literal("!".to_string()),
            ]
        );
    }

    #[test]
    fn normalize_number_collapses_grouping_metadata() {
        let mut normalizer = Normalizer::new();
        let raw = make_raw_token(RawTokenKind::NumberCandidate, "1_234,567");
        let mut ctx = LexerContext::new("1_234,567");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("number normalization");

        assert_eq!(normalized.normalized_text, "1234567");
        assert!(normalized
            .metadata
            .provisional_metadata
            .iter()
            .any(|meta| matches!(
                meta,
                TokenMetadata::NumberLiteral(info)
                if matches!(info.grouping, NumberGroupingKind::Mixed)
            )));
    }

    #[test]
    fn normalize_left_brace_attaches_json_metadata() {
        let mut normalizer = Normalizer::new();
        let raw = make_raw_token(RawTokenKind::Symbol, "{");
        let mut ctx = LexerContext::new("{\"key\": 1}");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("brace normalization");

        let confidence = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
                _ => None,
            })
            .expect("json metadata");

        assert!(matches!(confidence, crate::JsonConfidence::High));
    }

    #[test]
    fn normalize_left_bracket_attaches_json_metadata() {
        let mut normalizer = Normalizer::new();
        let raw = make_raw_token(RawTokenKind::Symbol, "[");
        let mut ctx = LexerContext::new("[1, 2, 3]");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("bracket normalization");

        let confidence = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
                _ => None,
            })
            .expect("json metadata");

        assert!(matches!(
            confidence,
            crate::JsonConfidence::Medium | crate::JsonConfidence::High
        ));
    }

    #[test]
    fn normalize_whitespace_tokens_preserve_text_without_diagnostics() {
        let mut normalizer = Normalizer::new();
        let raw = make_raw_token(RawTokenKind::Whitespace, "  \n\t");
        let mut ctx = LexerContext::new("  \n\tval");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("whitespace normalization");

        assert_eq!(normalized.normalized_text, "  \n\t");
        assert!(normalized.metadata.provisional_metadata.is_empty());
        assert!(normalized.metadata.provisional_diagnostics.is_empty());
    }

    #[test]
    fn normalize_decimal_with_exponent_and_grouping() {
        let mut normalizer = Normalizer::new();
        let raw = make_raw_token(RawTokenKind::NumberCandidate, "1,234.5e-6");
        let mut ctx = LexerContext::new("1,234.5e-6");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("decimal normalization");

        assert_eq!(normalized.normalized_text, "1234.5e-6");
        let metadata = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::NumberLiteral(info) => Some(info),
                _ => None,
            })
            .expect("number metadata");
        assert_eq!(metadata.original_lexeme, "1,234.5e-6");
        assert!(matches!(metadata.grouping, NumberGroupingKind::Comma));
    }

    #[test]
    fn normalize_hex_number_strips_underscores() {
        let mut normalizer = Normalizer::new();
        let raw = make_raw_token(RawTokenKind::NumberCandidate, "0xAB_CD");
        let mut ctx = LexerContext::new("0xAB_CD");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("hex normalization");

        assert_eq!(normalized.normalized_text, "0xABCD");
        let metadata = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::NumberLiteral(info) => Some(info),
                _ => None,
            })
            .expect("number metadata");
        assert_eq!(metadata.original_lexeme, "0xAB_CD");
        assert!(matches!(metadata.grouping, NumberGroupingKind::Underscore));
    }

    #[test]
    fn normalize_whitespace_between_array_elements_sets_layout_metadata() {
        let mut normalizer = Normalizer::new();
        let raw = RawToken {
            kind: RawTokenKind::Whitespace,
            text: " ",
            span: make_span_with_offset(2, 1, 1, 3),
            trivia: None,
            carry_over: None,
        };
        let mut ctx = LexerContext::new("[1 2]");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("layout whitespace normalization");

        let layout_meta = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::LayoutComma(info) => Some(info),
                _ => None,
            })
            .expect("layout metadata");
        assert!(matches!(layout_meta.sequence, LayoutSequenceKind::Array));
    }

    #[test]
    fn normalize_whitespace_between_call_arguments_sets_layout_metadata() {
        let mut normalizer = Normalizer::new();
        let raw = RawToken {
            kind: RawTokenKind::Whitespace,
            text: " ",
            span: make_span_with_offset(5, 1, 1, 6),
            trivia: None,
            carry_over: None,
        };
        let mut ctx = LexerContext::new("plot(1 2)");

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("call whitespace normalization");

        let layout_meta = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::LayoutComma(info) => Some(info),
                _ => None,
            })
            .expect("layout metadata");
        assert!(matches!(layout_meta.sequence, LayoutSequenceKind::Call));
    }

    #[test]
    fn normalize_identifier_appends_comment_carry_metadata() {
        let mut normalizer = Normalizer::new();
        let mut ctx = LexerContext::new("val");

        let mut raw = make_raw_token(RawTokenKind::Identifier, "val");
        let mut carry = CommentCarryOverMetadata::default();
        carry.passthrough.push(SourceCommentTrivia {
            kind: SourceCommentKind::Line,
            text: "// keep".to_string(),
            line: 1,
            column: 1,
        });
        carry.json.push(JsonCommentTrivia {
            kind: JsonCommentTriviaKind::Line,
            text: "keep".to_string(),
            line: 1,
            column: 1,
        });
        raw.carry_over = Some(carry.clone());

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("identifier normalization with carry");

        let carry_meta = normalized
            .metadata
            .provisional_metadata
            .iter()
            .find_map(|meta| match meta {
                TokenMetadata::CommentCarryOver(info) => Some(info.clone()),
                _ => None,
            })
            .expect("comment carry metadata");

        assert_eq!(carry_meta.passthrough, carry.passthrough);
        assert_eq!(carry_meta.json, carry.json);
        assert!(normalized.raw.carry_over.is_some());
    }

    #[test]
    fn normalize_comment_keeps_carry_metadata_on_raw_without_injecting_pre_metadata() {
        let mut normalizer = Normalizer::new();
        let mut ctx = LexerContext::new("// keep");

        let mut raw = make_raw_token(RawTokenKind::CommentCandidate, "// keep");
        let mut carry = CommentCarryOverMetadata::default();
        carry.passthrough.push(SourceCommentTrivia {
            kind: SourceCommentKind::Line,
            text: "// keep".to_string(),
            line: 1,
            column: 1,
        });
        raw.carry_over = Some(carry.clone());

        let normalized = normalizer
            .normalize(raw, &mut ctx)
            .expect("comment normalization");

        assert!(normalized
            .metadata
            .provisional_metadata
            .iter()
            .all(|meta| !matches!(meta, TokenMetadata::CommentCarryOver(_))));
        assert_eq!(normalized.raw.carry_over, Some(carry));
    }
}
