//! 型推論中の一時的なコンテキスト（期待型など）を管理する簡易セッション。

use crate::types::TypeKind;
use jv_build::metadata::SymbolIndex;
use std::collections::VecDeque;
use std::sync::Arc;

/// 推論ステップ間で共有する軽量なコンテキスト。
#[derive(Debug, Default)]
pub struct InferenceSession {
    expected_stack: VecDeque<Option<TypeKind>>,
    symbol_index: Option<Arc<SymbolIndex>>,
}

impl InferenceSession {
    /// 新しいセッションを生成する。
    pub fn new() -> Self {
        Self::default()
    }

    /// シンボルインデックスを指定して新しいセッションを構築する。
    pub fn with_symbol_index(symbol_index: Option<Arc<SymbolIndex>>) -> Self {
        Self {
            expected_stack: VecDeque::new(),
            symbol_index,
        }
    }

    /// セッションにシンボルインデックスを設定する。
    pub fn set_symbol_index(&mut self, symbol_index: Option<Arc<SymbolIndex>>) {
        self.symbol_index = symbol_index;
    }

    /// 利用可能なシンボルインデックスを参照で取得する。
    pub fn symbol_index(&self) -> Option<&SymbolIndex> {
        self.symbol_index.as_deref()
    }

    /// 期待型をスタックへ追加する。`None` は未指定を表す。
    pub fn push_expected(&mut self, ty: Option<TypeKind>) {
        self.expected_stack.push_back(ty);
    }

    /// 直近の期待型情報を破棄する。
    pub fn pop_expected(&mut self) {
        self.expected_stack.pop_back();
    }

    /// 現在有効な期待型を参照で取得する。
    pub fn expected_type(&self) -> Option<&TypeKind> {
        self.expected_stack
            .iter()
            .rev()
            .find_map(|candidate| candidate.as_ref())
    }

    /// 期待型スタックをすべてクリアする。
    pub fn clear_expected(&mut self) {
        self.expected_stack.clear();
    }
}
