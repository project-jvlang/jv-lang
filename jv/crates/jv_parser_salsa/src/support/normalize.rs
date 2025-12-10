//! 診断やスパンを正規化して比較するためのユーティリティ。

use jv_ast::Span;
use jv_parser_frontend::{Diagnostic, DiagnosticSeverity};

/// 診断のソートキー。安定した順序で比較できる。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticSortKey {
    pub source: String,
    pub line: u32,
    pub column: u32,
    pub code: String,
    pub severity: DiagnosticSeverity,
}

impl Ord for DiagnosticSortKey {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (
            &self.source,
            self.line,
            self.column,
            self.code.as_str(),
            severity_rank(self.severity),
        )
            .cmp(&(
                &other.source,
                other.line,
                other.column,
                other.code.as_str(),
                severity_rank(other.severity),
            ))
    }
}

impl PartialOrd for DiagnosticSortKey {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl DiagnosticSortKey {
    /// 診断からソートキーを構築する。
    pub fn from_diagnostic(diagnostic: &Diagnostic) -> Self {
        let (line, column) = diagnostic
            .span()
            .map(|span| (span.start_line as u32, span.start_column as u32))
            .unwrap_or((0, 0));
        Self {
            source: diagnostic.source().name().to_string(),
            line,
            column,
            code: diagnostic.code().unwrap_or("").to_string(),
            severity: diagnostic.severity(),
        }
    }
}

/// 診断リストを正規化し、安定ソートした結果を返す。
pub fn normalize_diagnostics(diagnostics: &[Diagnostic]) -> Vec<(DiagnosticSortKey, String)> {
    let mut items: Vec<_> = diagnostics
        .iter()
        .map(|d| {
            (
                DiagnosticSortKey::from_diagnostic(d),
                d.message().to_string(),
            )
        })
        .collect();
    items.sort_by(|(a_key, _), (b_key, _)| a_key.cmp(b_key));
    items
}

/// スパンが ±1 カラム以内の差分かどうかを判定する。
pub fn spans_within_tolerance(left: &Span, right: &Span) -> bool {
    left.start_line == right.start_line
        && left.end_line == right.end_line
        && left.start_column.abs_diff(right.start_column) <= 1
        && left.end_column.abs_diff(right.end_column) <= 1
}

fn severity_rank(severity: DiagnosticSeverity) -> u8 {
    match severity {
        DiagnosticSeverity::Error => 0,
        DiagnosticSeverity::Warning => 1,
        DiagnosticSeverity::Information => 2,
    }
}
