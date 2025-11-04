use std::num::IntErrorKind;

use crate::{
    InvalidImplicitParamReason, LexError, TokenDiagnostic, TokenMetadata, TokenType,
    UnderscoreInfoMetadata,
    pipeline::{
        context::LexerContext,
        types::{NormalizedToken, RawTokenKind},
    },
};

use super::{ClassificationModule, ClassificationState};

pub struct UnderscoreModule;

impl UnderscoreModule {
    pub fn new() -> Self {
        Self
    }

    fn classify_candidate<'a>(text: &'a str) -> UnderscoreCandidate<'a> {
        if text == "_" {
            UnderscoreCandidate::Wildcard
        } else if let Some(rest) = text.strip_prefix('_') {
            if !rest.is_empty() && rest.chars().all(|ch| ch.is_ascii_digit()) {
                UnderscoreCandidate::Implicit(rest)
            } else {
                UnderscoreCandidate::None
            }
        } else {
            UnderscoreCandidate::None
        }
    }

    fn is_non_code_region(state: &ClassificationState<'_>) -> bool {
        state.metadata_contains(|meta| {
            matches!(
                meta,
                TokenMetadata::StringLiteral(_)
                    | TokenMetadata::StringInterpolation { .. }
                    | TokenMetadata::RegexLiteral { .. }
            )
        })
    }

    fn push_metadata(
        state: &mut ClassificationState<'_>,
        token: &NormalizedToken<'_>,
        is_implicit: bool,
        number: Option<u32>,
        in_non_code_region: bool,
    ) {
        let span = token.raw.span.start;
        let raw = token.raw.text.to_string();
        let length = token.raw.text.chars().count();
        state
            .metadata_mut()
            .push(TokenMetadata::UnderscoreInfo(UnderscoreInfoMetadata {
                raw,
                is_implicit,
                number,
                line: span.line,
                column: span.column,
                length,
                in_non_code_region,
            }));
    }

    fn emit_invalid(
        state: &mut ClassificationState<'_>,
        token: &NormalizedToken<'_>,
        reason: InvalidImplicitParamReason,
        suggested: Option<String>,
    ) {
        Self::push_metadata(state, token, true, None, false);
        state.set_token_type(TokenType::Invalid(token.raw.text.to_string()));
        state
            .diagnostics()
            .push(TokenDiagnostic::InvalidImplicitParam { reason, suggested });
    }
}

impl ClassificationModule for UnderscoreModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        _ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        if state.token_type().is_some() {
            return Ok(());
        }

        if !matches!(token.raw.kind, RawTokenKind::Identifier) {
            return Ok(());
        }

        match Self::classify_candidate(&token.normalized_text) {
            UnderscoreCandidate::None => Ok(()),
            UnderscoreCandidate::Wildcard => {
                if Self::is_non_code_region(state) {
                    Self::push_metadata(state, token, false, None, true);
                    return Ok(());
                }

                Self::push_metadata(state, token, false, None, false);
                state.set_token_type(TokenType::Underscore);
                Ok(())
            }
            UnderscoreCandidate::Implicit(digits) => {
                if Self::is_non_code_region(state) {
                    Self::push_metadata(state, token, true, None, true);
                    return Ok(());
                }

                if digits.starts_with('0') {
                    Self::emit_invalid(
                        state,
                        token,
                        InvalidImplicitParamReason::LeadingZero,
                        Some("_1".to_string()),
                    );
                    return Ok(());
                }

                match digits.parse::<u32>() {
                    Ok(value) => {
                        Self::push_metadata(state, token, true, Some(value), false);
                        state.set_token_type(TokenType::ImplicitParam(value));
                    }
                    Err(err) => {
                        if matches!(err.kind(), IntErrorKind::PosOverflow) {
                            Self::emit_invalid(
                                state,
                                token,
                                InvalidImplicitParamReason::Overflow,
                                None,
                            );
                        } else {
                            Self::emit_invalid(
                                state,
                                token,
                                InvalidImplicitParamReason::NonDigit,
                                None,
                            );
                        }
                    }
                }
                Ok(())
            }
        }
    }
}

enum UnderscoreCandidate<'a> {
    None,
    Wildcard,
    Implicit(&'a str),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        StringDelimiterKind, StringLiteralMetadata,
        pipeline::types::{NormalizedToken, PreMetadata, RawToken, Span},
    };

    fn make_raw_token(text: &str, line: usize, column: usize) -> RawToken<'static> {
        use crate::pipeline::types::{RawTokenKind, ScannerPosition};

        let leaked: &'static str = Box::leak(text.to_string().into_boxed_str());
        let start = ScannerPosition {
            byte_offset: 0,
            line,
            column,
        };
        let mut end = start;
        end.advance(leaked.len(), 0, leaked.len());
        RawToken {
            kind: RawTokenKind::Identifier,
            text: leaked,
            span: Span::new(0..leaked.len(), start, end),
            trivia: None,
            carry_over: None,
        }
    }

    fn classify(text: &str) -> (Option<TokenType>, Vec<TokenMetadata>, Vec<TokenDiagnostic>) {
        let raw = make_raw_token(text, 3, 5);
        let token = NormalizedToken::new(raw, text.to_string(), PreMetadata::default());
        let ctx = LexerContext::new(text);
        let mut state = ClassificationState::new(&token);
        let mut module = UnderscoreModule::new();

        module.apply(&token, &ctx, &mut state).unwrap();

        let (token_type, metadata, diagnostics, _) = state.into_parts();
        (token_type, metadata, diagnostics)
    }

    #[test]
    fn classifies_wildcard_underscore() {
        let (token_type, metadata, diagnostics) = classify("_");
        assert!(diagnostics.is_empty());
        assert!(matches!(token_type, Some(TokenType::Underscore)));
        assert!(metadata.iter().any(|meta| matches!(
            meta,
            TokenMetadata::UnderscoreInfo(info)
            if !info.is_implicit && info.number.is_none() && info.line == 3 && info.column == 5 && !info.in_non_code_region
        )));
    }

    #[test]
    fn classifies_implicit_parameter() {
        let (token_type, metadata, diagnostics) = classify("_42");
        assert!(diagnostics.is_empty());
        assert!(matches!(token_type, Some(TokenType::ImplicitParam(42))));
        assert!(metadata.iter().any(|meta| matches!(
            meta,
            TokenMetadata::UnderscoreInfo(info)
            if info.is_implicit && info.number == Some(42)
        )));
    }

    #[test]
    fn classifies_minimum_valid_implicit_parameter() {
        let (token_type, metadata, diagnostics) = classify("_1");
        assert!(diagnostics.is_empty());
        assert!(matches!(token_type, Some(TokenType::ImplicitParam(1))));
        assert!(metadata.iter().any(|meta| matches!(
            meta,
            TokenMetadata::UnderscoreInfo(info)
            if info.is_implicit && info.number == Some(1)
        )));
    }

    #[test]
    fn classifies_three_digit_implicit_parameter() {
        let (token_type, metadata, diagnostics) = classify("_999");
        assert!(diagnostics.is_empty());
        assert!(matches!(token_type, Some(TokenType::ImplicitParam(999))));
        assert!(metadata.iter().any(|meta| matches!(
            meta,
            TokenMetadata::UnderscoreInfo(info)
            if info.is_implicit && info.number == Some(999)
        )));
    }

    #[test]
    fn rejects_leading_zero() {
        let (token_type, metadata, diagnostics) = classify("_0");
        assert!(matches!(token_type, Some(TokenType::Invalid(value)) if value == "_0"));
        assert!(metadata.iter().any(|meta| matches!(
            meta,
            TokenMetadata::UnderscoreInfo(info)
            if info.is_implicit && info.number.is_none()
        )));
        assert_eq!(diagnostics.len(), 1);
        match &diagnostics[0] {
            TokenDiagnostic::InvalidImplicitParam { reason, suggested } => {
                assert_eq!(*reason, InvalidImplicitParamReason::LeadingZero);
                assert_eq!(suggested.as_deref(), Some("_1"));
            }
            other => panic!("unexpected diagnostic: {other:?}"),
        }
    }

    #[test]
    fn rejects_overflow() {
        let (token_type, _metadata, diagnostics) = classify("_4294967296");
        assert!(matches!(token_type, Some(TokenType::Invalid(value)) if value == "_4294967296"));
        assert!(matches!(
            diagnostics.as_slice(),
            [TokenDiagnostic::InvalidImplicitParam {
                reason: InvalidImplicitParamReason::Overflow,
                suggested: None
            }]
        ));
    }

    #[test]
    fn ignores_mixed_alphanumeric_pattern() {
        let (token_type, metadata, diagnostics) = classify("_1value");
        assert!(token_type.is_none(), "識別子として扱われるべきケース");
        assert!(
            metadata
                .iter()
                .all(|meta| !matches!(meta, TokenMetadata::UnderscoreInfo(_)))
        );
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn ignores_identifier_like_patterns() {
        let (token_type, metadata, diagnostics) = classify("_value");
        assert!(token_type.is_none());
        assert!(
            metadata
                .iter()
                .all(|meta| !matches!(meta, TokenMetadata::UnderscoreInfo(_)))
        );
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn skips_non_code_regions() {
        let raw = make_raw_token("_", 2, 8);
        let mut pre = PreMetadata::default();
        pre.provisional_metadata
            .push(TokenMetadata::StringLiteral(StringLiteralMetadata {
                delimiter: StringDelimiterKind::DoubleQuote,
                allows_interpolation: true,
                normalize_indentation: false,
            }));
        let token = NormalizedToken::new(raw, "_".to_string(), pre);
        let ctx = LexerContext::new("_");
        let mut state = ClassificationState::new(&token);
        let mut module = UnderscoreModule::new();

        module.apply(&token, &ctx, &mut state).unwrap();

        let (token_type, metadata, diagnostics, _) = state.into_parts();
        assert!(token_type.is_none());
        assert!(diagnostics.is_empty());
        assert!(metadata.iter().any(|meta| matches!(
            meta,
            TokenMetadata::UnderscoreInfo(info)
            if info.in_non_code_region
        )));
    }
}
