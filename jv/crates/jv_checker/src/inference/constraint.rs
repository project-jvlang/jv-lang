//! 型制約とそのコレクション操作を定義する。
//!
//! 制約生成や単一化ソルバから利用されるキュー構造をここで提供し、
//! 後続タスクで詳細ロジックを追加しやすい形にしておく。

use crate::inference::types::{TypeId, TypeKind};
use std::collections::VecDeque;

/// 制約の種類を表す列挙体。
#[derive(Debug, Clone, PartialEq)]
pub enum ConstraintKind {
    /// 2つの型が等価であるべきことを示す。
    Equal(TypeKind, TypeKind),
    /// 型変数に対して特定の型を割り当てる命令。
    Assign(TypeId, TypeKind),
    /// まだ詳細が固まっていないプレースホルダ制約。
    Placeholder(&'static str),
}

/// 単体の制約を保持する構造体。
#[derive(Debug, Clone, PartialEq)]
pub struct Constraint {
    pub kind: ConstraintKind,
    pub note: Option<String>,
}

impl Constraint {
    /// 制約を生成するユーティリティコンストラクタ。
    pub fn new(kind: ConstraintKind) -> Self {
        Self { kind, note: None }
    }

    /// 補足情報を追加するチェイン可能なメソッド。
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }
}

/// 制約をFIFO順で蓄積・取り出しするキュー。
#[derive(Debug, Default)]
pub struct ConstraintSet {
    queue: VecDeque<Constraint>,
}

impl ConstraintSet {
    /// 空の制約集合を生成する。
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    /// 制約を末尾に追加する。
    pub fn push(&mut self, constraint: Constraint) {
        self.queue.push_back(constraint);
    }

    /// 次の制約を取り出す。
    pub fn pop(&mut self) -> Option<Constraint> {
        self.queue.pop_front()
    }

    /// 残っている制約の有無を確認する。
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    /// 保持している制約数を取得する。
    pub fn len(&self) -> usize {
        self.queue.len()
    }
}
