use std::ops::Range;

use crate::{TokenDiagnostic, TokenMetadata, TokenTrivia, TokenType};

/// ソース全体に対する位置情報を表す。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ScannerPosition {
    pub byte_offset: usize,
    pub line: usize,
    pub column: usize,
}

impl ScannerPosition {
    pub fn advance(&mut self, delta_bytes: usize, delta_line: usize, delta_column: usize) {
        self.byte_offset = self.byte_offset.saturating_add(delta_bytes);
        self.line = self.line.saturating_add(delta_line);
        if delta_line > 0 {
            self.column = delta_column;
        } else {
            self.column = self.column.saturating_add(delta_column);
        }
    }
}

/// バイトオフセットと行・列位置の範囲を保持する。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub byte_range: Range<usize>,
    pub start: ScannerPosition,
    pub end: ScannerPosition,
}

impl Span {
    pub fn new(byte_range: Range<usize>, start: ScannerPosition, end: ScannerPosition) -> Self {
        Self {
            byte_range,
            start,
            end,
        }
    }

    pub fn empty(at: ScannerPosition) -> Self {
        Self::new(at.byte_offset..at.byte_offset, at, at)
    }
}

/// CharScannerが抽出したトークン種別の候補。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RawTokenKind {
    Identifier,
    NumberCandidate,
    Symbol,
    Whitespace,
    CommentCandidate,
    Eof,
}

/// CharScannerからNormalizerへ受け渡す中間トークン。
#[derive(Debug, Clone)]
pub struct RawToken<'source> {
    pub kind: RawTokenKind,
    pub text: &'source str,
    pub span: Span,
    pub trivia: Option<TokenTrivia>,
}

impl<'source> RawToken<'source> {
    pub fn is_eof(&self) -> bool {
        matches!(self.kind, RawTokenKind::Eof)
    }
}

/// Normalizerが組み立てた前処理メタデータ。
#[derive(Debug, Clone, Default)]
pub struct PreMetadata {
    pub provisional_metadata: Vec<TokenMetadata>,
    pub provisional_diagnostics: Vec<TokenDiagnostic>,
}

/// NormalizerからClassifierへ渡す正規化済みトークン。
#[derive(Debug, Clone)]
pub struct NormalizedToken<'source> {
    pub raw: RawToken<'source>,
    pub normalized_text: String,
    pub metadata: PreMetadata,
}

impl<'source> NormalizedToken<'source> {
    pub fn new(raw: RawToken<'source>, normalized_text: String, metadata: PreMetadata) -> Self {
        Self {
            raw,
            normalized_text,
            metadata,
        }
    }
}

/// ClassifierがEmitterへ渡す確定済みトークン。
#[derive(Debug, Clone)]
pub struct ClassifiedToken<'source> {
    pub normalized: NormalizedToken<'source>,
    pub token_type: TokenType,
    pub diagnostics: Vec<TokenDiagnostic>,
    pub metadata: Vec<TokenMetadata>,
}

impl<'source> ClassifiedToken<'source> {
    pub fn new(
        normalized: NormalizedToken<'source>,
        token_type: TokenType,
        diagnostics: Vec<TokenDiagnostic>,
        metadata: Vec<TokenMetadata>,
    ) -> Self {
        Self {
            normalized,
            token_type,
            diagnostics,
            metadata,
        }
    }
}
