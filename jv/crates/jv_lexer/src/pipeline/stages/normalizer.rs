use std::char;

use unicode_normalization::UnicodeNormalization;

use crate::{
    pipeline::{
        context::LexerContext,
        pipeline::NormalizerStage,
        types::{NormalizedToken, PreMetadata, RawToken, RawTokenKind},
    },
    LexError, NumberGroupingKind, NumberLiteralMetadata, StringDelimiterKind,
    StringLiteralMetadata, TokenDiagnostic, TokenMetadata,
};

/// RawToken から文字列正規化・数値整形を行い、NormalizedToken を生成するステージ。
#[derive(Debug, Default)]
pub struct Normalizer {
    _private: (),
}

impl Normalizer {
    pub fn new() -> Self {
        Self { _private: () }
    }

    fn normalize_number<'source>(
        &mut self,
        token: RawToken<'source>,
    ) -> Result<NormalizedToken<'source>, LexError> {
        let lexeme = token.text;
        let mut metadata = PreMetadata::default();

        if Self::has_radix_prefix(lexeme) {
            return Ok(NormalizedToken::new(token, lexeme.to_string(), metadata));
        }

        let mut normalized = String::with_capacity(lexeme.len());
        let mut saw_comma = false;
        let mut saw_underscore = false;
        let mut prev_is_separator = false;

        for ch in lexeme.chars() {
            match ch {
                ',' => {
                    saw_comma = true;
                    prev_is_separator = true;
                }
                '_' => {
                    saw_underscore = true;
                    prev_is_separator = true;
                }
                _ => {
                    normalized.push(ch);
                    prev_is_separator = false;
                }
            }
        }

        if prev_is_separator {
            return Err(LexError::UnexpectedChar(
                lexeme.chars().last().unwrap_or_default(),
                token.span.end.line,
                token.span.end.column,
            ));
        }

        if saw_comma || saw_underscore {
            let grouping = match (saw_comma, saw_underscore) {
                (true, true) => NumberGroupingKind::Mixed,
                (true, false) => NumberGroupingKind::Comma,
                (false, true) => NumberGroupingKind::Underscore,
                (false, false) => NumberGroupingKind::None,
            };

            if grouping != NumberGroupingKind::None {
                metadata
                    .provisional_metadata
                    .push(TokenMetadata::NumberLiteral(NumberLiteralMetadata {
                        grouping,
                        original_lexeme: lexeme.to_string(),
                    }));
            }
        }

        Ok(NormalizedToken::new(token, normalized, metadata))
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
        let string_metadata = StringLiteralMetadata {
            delimiter: delimiter_kind,
            allows_interpolation: true,
            normalize_indentation: matches!(delimiter_kind, StringDelimiterKind::TripleQuote),
        };
        metadata
            .provisional_metadata
            .push(TokenMetadata::StringLiteral(string_metadata));

        Ok(NormalizedToken::new(token, normalized, metadata))
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
}

impl NormalizerStage for Normalizer {
    fn normalize<'source>(
        &mut self,
        token: RawToken<'source>,
        _ctx: &mut LexerContext<'source>,
    ) -> Result<NormalizedToken<'source>, LexError> {
        match token.kind {
            RawTokenKind::NumberCandidate => self.normalize_number(token),
            RawTokenKind::Whitespace | RawTokenKind::CommentCandidate => {
                let normalized_text = token.text.to_string();
                Ok(NormalizedToken::new(
                    token,
                    normalized_text,
                    PreMetadata::default(),
                ))
            }
            RawTokenKind::Eof => Ok(NormalizedToken::new(
                token,
                String::new(),
                PreMetadata::default(),
            )),
            RawTokenKind::Identifier => {
                let normalized_text = token.text.to_string();
                Ok(NormalizedToken::new(
                    token,
                    normalized_text,
                    PreMetadata::default(),
                ))
            }
            RawTokenKind::Symbol => {
                if token.text.starts_with('"') || token.text.starts_with("```") {
                    self.normalize_string(token)
                } else {
                    let normalized_text = token.text.to_string();
                    Ok(NormalizedToken::new(
                        token,
                        normalized_text,
                        PreMetadata::default(),
                    ))
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
