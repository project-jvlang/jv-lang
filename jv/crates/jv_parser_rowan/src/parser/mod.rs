//! Rowan ベースのイベント駆動パーサ。

use jv_lexer::Token;

use crate::syntax::SyntaxKind;

mod context;
mod strategies;

pub(crate) use context::ParserContext;

/// 構文解析イベント。
#[derive(Debug, Clone)]
pub enum ParseEvent {
    /// 新しいノードを開始する。
    StartNode {
        /// 開始するノード種別。
        kind: SyntaxKind,
    },
    /// 現在のノードを終了する。
    FinishNode,
    /// トークンを追加する。
    Token {
        /// トークンに対応する種別。
        kind: SyntaxKind,
        /// トークン配列内のインデックス。
        token_index: usize,
    },
    /// パーサ内部で発生したエラー。
    Error {
        /// 診断メッセージ。
        message: String,
        /// 対象範囲。
        span: TokenSpan,
    },
}

/// 診断の深刻度。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticSeverity {
    /// エラー。
    Error,
    /// 警告。
    Warning,
}

/// パーサ診断情報。
#[derive(Debug, Clone)]
pub struct ParserDiagnostic {
    /// メッセージ本文。
    pub message: String,
    /// 深刻度。
    pub severity: DiagnosticSeverity,
    /// 対象トークン範囲。
    pub span: TokenSpan,
}

impl ParserDiagnostic {
    /// 新しい診断を生成する。
    pub fn new(message: impl Into<String>, severity: DiagnosticSeverity, span: TokenSpan) -> Self {
        Self {
            message: message.into(),
            severity,
            span,
        }
    }
}

/// トークン範囲（開始≦idx＜終了）。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenSpan {
    /// 開始トークンインデックス。
    pub start: usize,
    /// 終了トークンインデックス。
    pub end: usize,
}

impl TokenSpan {
    /// 範囲を作成する。
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

/// 構文解析結果。
#[derive(Debug, Clone)]
pub struct ParseOutput {
    /// 発行されたイベント列。
    pub events: Vec<ParseEvent>,
    /// 解析中に発生した診断。
    pub diagnostics: Vec<ParserDiagnostic>,
    /// エラー回復が発生したか。
    pub recovered: bool,
}

impl ParseOutput {
    fn new(events: Vec<ParseEvent>, diagnostics: Vec<ParserDiagnostic>, recovered: bool) -> Self {
        Self {
            events,
            diagnostics,
            recovered,
        }
    }
}

/// トークン列からイベント列を生成する。
pub fn parse(tokens: &[Token]) -> ParseOutput {
    ParserContext::new(tokens).parse()
}
