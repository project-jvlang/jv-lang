//! 正規表現に関する型解析メタデータを管理するモジュール群。
//!
//! `PatternTypeBinder` がパターン型推論の期待値と結果を橋渡しし、
//! 既存の `RegexMatchTyping` / `RegexCommandTyping` とあわせて null
//! 安全解析や IR ローワリングで共有できる情報を提供する。

pub mod pattern_type;

use crate::inference::types::TypeKind;
use jv_ast::{RegexCommandMode, RegexGuardStrategy, Span};

pub use jv_ast::PatternOrigin;
pub use pattern_type::{PatternTypeBinder, PatternTypeBinding, TypeExpectation};

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

/// 正規表現コマンド式に関する型解析結果。
#[derive(Debug, Clone, PartialEq)]
pub struct RegexCommandTyping {
    /// 式全体を示すスパン。
    pub span: Span,
    /// 推論済みモード。
    pub mode: RegexCommandMode,
    /// 推論された主題の型。
    pub subject_type: TypeKind,
    /// 戻り値型。
    pub return_type: TypeKind,
    /// Optional 主題向けのガード戦略。
    pub guard_strategy: RegexGuardStrategy,
    /// Stream をリスト化すべきかどうかのヒント。
    pub requires_stream_materialization: bool,
    /// 付随する診断一覧。
    pub diagnostics: Vec<RegexCommandIssue>,
}

impl RegexCommandTyping {
    /// 空の診断リストで `RegexCommandTyping` を構築する。
    pub fn new(
        span: Span,
        mode: RegexCommandMode,
        subject_type: TypeKind,
        return_type: TypeKind,
        guard_strategy: RegexGuardStrategy,
    ) -> Self {
        Self {
            span,
            mode,
            subject_type,
            return_type,
            guard_strategy,
            requires_stream_materialization: false,
            diagnostics: Vec::new(),
        }
    }

    /// 診断を追加する。
    pub fn push_diagnostic(&mut self, diagnostic: RegexCommandIssue) {
        self.diagnostics.push(diagnostic);
    }
}

/// `RegexCommand` に付随する診断情報。
#[derive(Debug, Clone, PartialEq)]
pub struct RegexCommandIssue {
    pub code: String,
    pub message: String,
}

impl RegexCommandIssue {
    /// コードとメッセージから診断を生成する。
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
        }
    }
}

impl RegexMatchWarning {
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
        }
    }
}
