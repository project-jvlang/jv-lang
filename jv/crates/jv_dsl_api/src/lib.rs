//! DSLプラグイン向けの基本API（プレースホルダ）。
//!
//! Phase1ではインタフェースのみ定義し、実装はPhase2以降で拡張する。

/// DSLプラグインの基本トレイト。
pub trait DslPlugin: Send + Sync {
    /// DSL名（例: "log"）
    fn name(&self) -> &'static str;

    /// DSLが登録するキーワード一覧。
    fn registered_keywords(&self) -> &'static [&'static str];
}

/// トークンレベルの拡張ポイント。
pub trait TokenPlugin: DslPlugin {
    fn keyword_to_token(&self, _keyword: &str) -> Option<u8> {
        let _ = _keyword;
        None
    }
}

/// ブロックDSL用の拡張ポイント。
pub trait BlockPlugin: DslPlugin {
    fn parse_block_body(&self, _source: &str) -> Result<(), String> {
        let _ = _source;
        Ok(())
    }
}

/// グローバル演算子DSL用の拡張ポイント。
pub trait GlobalOperatorPlugin: DslPlugin {
    fn register_operators(&self) -> &'static [&'static str] {
        &[]
    }
}
