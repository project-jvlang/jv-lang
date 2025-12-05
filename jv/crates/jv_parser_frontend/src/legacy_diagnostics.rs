// Legacy diagnostic types for backwards compatibility
// These types were previously defined in jv_parser_preprocess and jv_parser_semantics

use jv_ast::Span;

/// Preprocess ステージが発行する診断情報（未整形）。
#[derive(Debug, Clone, PartialEq)]
pub struct PreprocessDiagnostic {
    stage: &'static str,
    message: String,
    span: Option<Span>,
}

impl PreprocessDiagnostic {
    /// 診断インスタンスを生成するヘルパ。
    pub fn new(stage: &'static str, message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            stage,
            message: message.into(),
            span,
        }
    }

    /// 診断を発行したステージ名を返す。
    pub fn stage(&self) -> &'static str {
        self.stage
    }

    /// メッセージ本文を返す。
    pub fn message(&self) -> &str {
        &self.message
    }

    /// 診断に紐づくスパンを返す。
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

/// Semantics ステージが発行する診断情報。
#[derive(Debug, Clone)]
pub struct SemanticsDiagnostic {
    stage: &'static str,
    message: String,
    span: Option<Span>,
}

impl SemanticsDiagnostic {
    pub fn new(stage: &'static str, message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            stage,
            message: message.into(),
            span,
        }
    }

    pub fn stage(&self) -> &'static str {
        self.stage
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}
