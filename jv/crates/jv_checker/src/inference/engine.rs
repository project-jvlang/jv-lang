//! 型推論処理のオーケストレーションを担当するエンジン定義。
//!
//! ここでは推論実行のための公開APIだけを先行して準備し、後続タスクで実装される
//! 制約生成や単一化ロジックと接続できるようにする。

use crate::inference::constraint::ConstraintSet;
use crate::inference::types::{TypeBinding, TypeKind};
use jv_ast::Program;

/// 型推論処理で発生し得るエラーを表す。
#[derive(thiserror::Error, Debug)]
pub enum InferenceError {
    #[error("type inference is not yet implemented")]
    NotImplemented,
}

/// 型推論関数の共通戻り値エイリアス。
pub type InferenceResult<T> = Result<T, InferenceError>;

/// 推論ワークフローの状態を保持し、エントリーポイントを提供する。
#[derive(Debug, Default)]
pub struct InferenceEngine {
    constraints: ConstraintSet,
    bindings: Vec<TypeBinding>,
    result_type: Option<TypeKind>,
}

impl InferenceEngine {
    /// 空の推論コンテキストを生成する。
    pub fn new() -> Self {
        Self {
            constraints: ConstraintSet::new(),
            bindings: Vec::new(),
            result_type: None,
        }
    }

    /// AST 全体に対する推論を開始するエントリーポイント。
    pub fn infer_program(&mut self, _program: &Program) -> InferenceResult<()> {
        // 実装は後続タスクで追加される。現段階では未実装エラーを返しておく。
        Err(InferenceError::NotImplemented)
    }

    /// 現在保持している型束縛を取得する。
    pub fn bindings(&self) -> &[TypeBinding] {
        &self.bindings
    }

    /// 推論結果として得られたトップレベルの型を返す。
    pub fn result_type(&self) -> Option<&TypeKind> {
        self.result_type.as_ref()
    }
}
