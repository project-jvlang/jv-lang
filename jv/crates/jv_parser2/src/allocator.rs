//! bumpaloを用いたアリーナアロケータの薄いラッパー。
//!
//! - `Arena` はスレッド安全に共有可能（Mutex保護）。
//! - `ArenaGuard` 経由でスコープ付きに確保する。
//! - `reset` / `clear` でアリーナを再利用できる。

use std::sync::{Arc, Mutex, MutexGuard};

use bumpalo::Bump;

/// スレッド安全に共有できるアリーナ。
#[derive(Clone, Default)]
pub struct Arena {
    inner: Arc<Mutex<Bump>>,
}

impl Arena {
    /// 新しいアリーナを作成する。
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(Bump::new())),
        }
    }

    /// 事前確保容量付きでアリーナを作成する。
    pub fn with_capacity(bytes: usize) -> Self {
        Self {
            inner: Arc::new(Mutex::new(Bump::with_capacity(bytes))),
        }
    }

    /// アリーナをロックし、確保操作用のガードを取得する。
    pub fn guard(&self) -> ArenaGuard<'_> {
        ArenaGuard {
            guard: self.inner.lock().expect("arena mutex poisoned"),
        }
    }

    /// アリーナを空にして、確保済みメモリを再利用可能にする。
    pub fn reset(&self) {
        let mut bump = self.inner.lock().expect("arena mutex poisoned");
        bump.reset();
    }

    /// `reset` の別名。
    pub fn clear(&self) {
        self.reset();
    }

    /// 現在の確保バイト数を返す（目安）。
    pub fn allocated_bytes(&self) -> usize {
        let bump = self.inner.lock().expect("arena mutex poisoned");
        bump.allocated_bytes()
    }
}

/// アリーナへのスコープ付きアクセス。
pub struct ArenaGuard<'a> {
    guard: MutexGuard<'a, Bump>,
}

impl<'a> ArenaGuard<'a> {
    /// 値を確保し、アリーナ上の参照を返す。
    pub fn alloc<T>(&'a self, value: T) -> &'a mut T {
        self.guard.alloc(value)
    }

    /// スライスをコピーして確保する。
    pub fn alloc_slice_clone<T: Clone>(&'a self, values: &[T]) -> &'a mut [T] {
        self.guard.alloc_slice_clone(values)
    }

    /// アロケータ本体への参照（コレクション確保用）。
    pub fn bump(&'a self) -> &'a Bump {
        &self.guard
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arena_reuses_memory_after_reset() {
        let arena = Arena::new();

        {
            let guard = arena.guard();
            let values = guard.alloc_slice_clone(&[1u32, 2, 3]);
            assert_eq!(values, &[1, 2, 3]);
        }

        let before = arena.allocated_bytes();
        arena.reset();
        let after_reset = arena.allocated_bytes();
        assert!(after_reset <= before, "reset後の確保量は元より増えないこと");

        {
            let guard = arena.guard();
            let value = guard.alloc("jv");
            assert_eq!(value, &"jv");
        }
    }
}
