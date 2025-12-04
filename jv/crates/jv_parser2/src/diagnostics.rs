//! パーサー診断の軽量表現。

use crate::span::Span;

/// 診断の種別。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    Error,
    Warning,
    Info,
}

impl DiagnosticKind {
    pub const fn is_error(self) -> bool {
        matches!(self, DiagnosticKind::Error)
    }
}

/// 構造化診断。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub message: String,
    pub span: Span,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            kind: DiagnosticKind::Error,
            message: message.into(),
            span,
        }
    }

    pub fn with_kind(mut self, kind: DiagnosticKind) -> Self {
        self.kind = kind;
        self
    }
}
