//! DSLプラグイン向けの安定API。
//!
//! TokenPlugin/BlockPlugin/GlobalOperatorPluginの3層で拡張ポイントを提供する。

mod block_plugin;
mod global_operator_plugin;
mod token_plugin;

pub use block_plugin::{BlockPlugin, DslBlock};
pub use global_operator_plugin::{Associativity, GlobalOperator, GlobalOperatorPlugin};
pub use token_plugin::TokenPlugin;

use serde::{Deserialize, Serialize};

/// DSLキーワードに割り当てるトークンID型（TokenKind相当）。
pub type DslTokenKind = u8;

/// DSLトークンの軽量表現（lexemeは任意で保持）。
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DslToken {
    pub kind: DslTokenKind,
    pub lexeme: Option<String>,
}

impl DslToken {
    pub fn new(kind: DslTokenKind, lexeme: impl Into<Option<String>>) -> Self {
        Self {
            kind,
            lexeme: lexeme.into(),
        }
    }
}

/// DSLパース/トークン処理時のエラー。
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DslError {
    pub message: String,
    pub span: Option<(usize, usize)>,
}

impl DslError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
        }
    }

    pub fn with_span(message: impl Into<String>, span: (usize, usize)) -> Self {
        Self {
            message: message.into(),
            span: Some(span),
        }
    }
}

/// DSLプラグインの基本トレイト。
pub trait DslPlugin: Send + Sync {
    /// DSL名（例: "log"）
    fn name(&self) -> &'static str;

    /// DSLが登録するキーワード一覧。
    fn registered_keywords(&self) -> &'static [&'static str];

    /// このプラグインを有効化するfeature flag名（例: "dsl-log"）。
    fn feature_flag(&self) -> Option<&'static str> {
        None
    }
}
