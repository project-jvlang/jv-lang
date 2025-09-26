//! 型推論モジュール内で共有される小規模ユーティリティ群。
//!
//! 現段階ではID発行のみを提供し、後続タスクで補助関数を拡張できるようにする。

use crate::inference::types::TypeId;

/// 型変数IDを連番で払い出すジェネレータ。
#[derive(Debug, Default)]
pub struct TypeIdGenerator {
    next: u32,
}

impl TypeIdGenerator {
    /// 新しいジェネレータを作成する。
    pub fn new() -> Self {
        Self::default()
    }

    /// 次の `TypeId` を払い出す。
    pub fn allocate(&mut self) -> TypeId {
        let current = self.next;
        self.next = self.next.saturating_add(1);
        TypeId::new(current)
    }

    /// 現在のカウンタを取得する。
    pub fn counter(&self) -> u32 {
        self.next
    }
}
