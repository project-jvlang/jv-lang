use serde::{Deserialize, Serialize};

use crate::DslPlugin;

/// 演算子の結合規則。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Associativity {
    Left,
    Right,
}

/// DSLが追加するグローバル演算子の定義。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct GlobalOperator {
    pub symbol: &'static str,
    pub precedence: u8,
    pub associativity: Associativity,
}

/// グローバル演算子DSL用の拡張ポイント。
pub trait GlobalOperatorPlugin: DslPlugin {
    fn register_operators(&self) -> &'static [GlobalOperator] {
        &[]
    }
}
