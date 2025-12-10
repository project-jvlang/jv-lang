use crate::lexer::Span as TokenSpan;
use crate::parser::OwnedToken;
use jv_ast::{Span, Statement};

/// ローワリング診断の深刻度。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoweringDiagnosticSeverity {
    /// エラー。
    Error,
    /// 警告。
    Warning,
}

/// ローワリング診断情報。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoweringDiagnostic {
    /// メッセージ本文。
    pub message: String,
    /// 深刻度。
    pub severity: LoweringDiagnosticSeverity,
    /// 対象スパン（取得できない場合は None）。
    pub span: Option<Span>,
}

impl LoweringDiagnostic {
    pub fn error(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            severity: LoweringDiagnosticSeverity::Error,
            span,
        }
    }

    pub fn warning(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            severity: LoweringDiagnosticSeverity::Warning,
            span,
        }
    }
}

/// トークンオフセットから行・桁情報へ写像するストア。
#[derive(Debug, Clone)]
pub struct TokenStore {
    line_starts: Vec<usize>,
}

impl TokenStore {
    pub fn new(source: &str) -> Self {
        let mut line_starts = Vec::new();
        line_starts.push(0);
        for (idx, ch) in source.char_indices() {
            if ch == '\n' {
                line_starts.push(idx + 1);
            }
        }
        Self { line_starts }
    }

    fn offset_to_line_col(&self, offset: usize) -> (usize, usize) {
        // line_starts は昇順なので二分探索を使用する。
        let idx = self.line_starts.partition_point(|&start| start <= offset);
        let line_idx = idx.saturating_sub(1);
        let line_start = *self.line_starts.get(line_idx).unwrap_or(&0);
        let column = offset.saturating_sub(line_start);
        (line_idx + 1, column + 1)
    }

    /// トークンスパンを AST スパンへ変換する。
    pub fn span_for(&self, span: TokenSpan) -> Span {
        let (start_line, start_column) = self.offset_to_line_col(span.start as usize);
        let (end_line, end_column) = self.offset_to_line_col(span.end as usize);
        Span {
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }
}

/// ローワリング時に共有するコンテキスト。
#[derive(Debug)]
pub struct LoweringContext<'src> {
    source: &'src str,
    pub tokens: &'src [OwnedToken],
    store: TokenStore,
    diagnostics: Vec<LoweringDiagnostic>,
}

impl<'src> LoweringContext<'src> {
    pub fn new(source: &'src str, tokens: &'src [OwnedToken]) -> Self {
        Self {
            source,
            tokens,
            store: TokenStore::new(source),
            diagnostics: Vec::new(),
        }
    }

    /// トークンに対応する AST スパンを返す。
    pub fn span_for_token(&self, token: &OwnedToken) -> Span {
        self.store.span_for(token.span)
    }

    /// トークン範囲から AST スパンを生成する。
    pub fn span_for_range(&self, start: &OwnedToken, end: &OwnedToken) -> Span {
        let start_span = start.span;
        let end_span = end.span;
        self.store.span_for(TokenSpan {
            start: start_span.start,
            end: end_span.end,
        })
    }

    /// バイト範囲から AST スパンを取得する。
    pub fn span_from_raw(&self, span: TokenSpan) -> Span {
        self.store.span_for(span)
    }

    /// 診断を追加する。
    pub fn push_diagnostic(&mut self, diagnostic: LoweringDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// 収集済み診断を返す。
    pub fn into_diagnostics(self) -> Vec<LoweringDiagnostic> {
        self.diagnostics
    }

    /// 元ソースを返す（スパン計算用）。
    pub fn source(&self) -> &'src str {
        self.source
    }

    /// ソースマップ用にトークンスパン一覧を AST スパンへ変換する。
    pub fn token_spans(&self) -> Vec<Span> {
        self.tokens
            .iter()
            .map(|tok| self.span_for_token(tok))
            .collect()
    }
}

/// ローワリング結果。
#[derive(Debug, Clone, PartialEq)]
pub struct LoweringResult {
    /// 生成されたステートメント列。
    pub statements: Vec<Statement>,
    /// 生成された診断一覧。
    pub diagnostics: Vec<LoweringDiagnostic>,
    /// トークンに対応するスパン一覧。
    pub token_spans: Vec<Span>,
}
