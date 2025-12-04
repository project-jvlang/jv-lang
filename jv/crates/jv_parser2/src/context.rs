//! 文法コンテキストを表すビットフラグ。

use bitflags::bitflags;

bitflags! {
    /// jvパーサー内で利用するコンテキストフラグ。
    ///
    /// すべて `u32` 内に収まるよう定義し、軽量に合成できるようにする。
    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
    pub struct JvContext: u32 {
        const IN = 1 << 0;
        const YIELD = 1 << 1;
        const AWAIT = 1 << 2;
        const RETURN = 1 << 3;
        const WHEN_BLOCK = 1 << 4;
        const UNIT_TYPE = 1 << 5;
        const DATA_CLASS = 1 << 6;
        const PATTERN = 1 << 7;
        const ARROW_SAFE = 1 << 8;
        const DSL_BLOCK = 1 << 9;
        const DSL_LOG = 1 << 10;
        const DSL_IO = 1 << 11;
        const DSL_LOCK = 1 << 12;
        const DSL_CRON = 1 << 13;
        const DSL_ASSERT = 1 << 14;
        const SCOPE_TRANSPARENT = 1 << 15;
    }
}

impl JvContext {
    /// コンテキストを追加した新しいフラグを返す。
    pub fn with(self, other: JvContext) -> Self {
        self | other
    }

    /// 指定したフラグがすべて含まれているか判定する。
    pub fn allows(self, other: JvContext) -> bool {
        self.contains(other)
    }
}
