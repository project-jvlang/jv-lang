//! 正規表現 `is` 演算子の型解析結果を共有する補助データ構造を定義する。
//!
//! 型推論フェーズで収集したガード戦略や警告を保持し、後続の null 安全解析や
//! IR ローワリングで再利用できるようにする。

use jv_ast::{RegexGuardStrategy, Span};

/// `is /pattern/` 判定に関する型解析結果。
#[derive(Debug, Clone, PartialEq)]
pub struct RegexMatchTyping {
    /// 判定全体のソーススパン。
    pub span: Span,
    /// 左辺に適用すべきガード戦略。
    pub guard_strategy: RegexGuardStrategy,
    /// 付随する警告メッセージ。
    pub warnings: Vec<RegexMatchWarning>,
}

impl RegexMatchTyping {
    pub fn new(span: Span, guard_strategy: RegexGuardStrategy) -> Self {
        Self {
            span,
            guard_strategy,
            warnings: Vec::new(),
        }
    }

    pub fn with_warning(mut self, warning: RegexMatchWarning) -> Self {
        self.warnings.push(warning);
        self
    }

    pub fn push_warning(&mut self, warning: RegexMatchWarning) {
        self.warnings.push(warning);
    }
}

/// 型解析時に記録される個別警告。
#[derive(Debug, Clone, PartialEq)]
pub struct RegexMatchWarning {
    pub code: String,
    pub message: String,
}

impl RegexMatchWarning {
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
        }
    }
}
