use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{DslError, DslPlugin};

/// DSLブロックの解析結果を表す汎用コンテナ。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DslBlock {
    pub keyword: &'static str,
    pub payload: Value,
    pub scope_transparent: bool,
    pub max_nesting: usize,
}

impl DslBlock {
    pub fn new(
        keyword: &'static str,
        payload: Value,
        scope_transparent: bool,
        max_nesting: usize,
    ) -> Self {
        Self {
            keyword,
            payload,
            scope_transparent,
            max_nesting,
        }
    }
}

/// ブロックDSL用の拡張ポイント。
pub trait BlockPlugin: DslPlugin {
    /// ブロックのメインキーワード（例: "LOG"）。
    fn keyword(&self) -> &'static str;

    /// ブロックがスコープ透過かどうか。デフォルトは透過。
    fn is_scope_transparent(&self) -> bool {
        true
    }

    /// 許容される最大ネスト数。デフォルトは1。
    fn max_nesting(&self) -> usize {
        1
    }

    /// ブロック本体を解析し、AST相当のペイロードを返す。
    fn parse_block_body(&self, body: &str) -> Result<DslBlock, DslError>;
}
