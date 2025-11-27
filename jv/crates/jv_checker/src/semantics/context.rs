use super::units::UnitRegistry;
use std::sync::Arc;

/// 解析フェーズ間で共有される意味解析コンテキスト。
///
/// 現時点では単位レジストリのみを保持し、将来的なフェーズ拡張で
/// 追加メタデータを集約できるようにする。
#[derive(Debug, Default)]
pub struct SemanticContext {
    unit_registry: Option<Arc<UnitRegistry>>,
}

impl SemanticContext {
    /// 新しい空のコンテキストを生成する。
    pub fn new() -> Self {
        Self::default()
    }

    /// 単位レジストリをアタッチする。既存のレジストリは置き換えられる。
    pub fn attach_unit_registry(&mut self, registry: Arc<UnitRegistry>) {
        self.unit_registry = Some(registry);
    }

    /// 現在アタッチされている単位レジストリを参照する。
    pub fn unit_registry(&self) -> Option<&Arc<UnitRegistry>> {
        self.unit_registry.as_ref()
    }

    /// 単位レジストリが設定済みか判定する。
    pub fn has_unit_registry(&self) -> bool {
        self.unit_registry.is_some()
    }

    /// 単位レジストリを取り外し、所有権を呼び出し側へ移す。
    pub fn release_unit_registry(&mut self) -> Option<Arc<UnitRegistry>> {
        self.unit_registry.take()
    }
}
