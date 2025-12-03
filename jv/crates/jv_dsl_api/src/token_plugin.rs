use crate::{DslPlugin, DslToken, DslTokenKind};

/// トークンレベルの拡張ポイント。
///
/// レキサーがDSLキーワードを見つけた際に、どのトークンIDを割り当てるかを制御する。
/// 返却値は`u8`（TokenKind相当）で、200未満に収まる必要がある。
pub trait TokenPlugin: DslPlugin {
    /// キーワードに対応するトークンIDを返す。`None`の場合はレジストリが採番する。
    fn keyword_to_token(&self, keyword: &str) -> Option<DslTokenKind> {
        let _ = keyword;
        None
    }

    /// 追加のトークン処理を行いたい場合に利用するフック。
    /// デフォルトでは何もしない。
    fn apply(&self, _token: &mut DslToken) -> Result<(), String> {
        Ok(())
    }
}
