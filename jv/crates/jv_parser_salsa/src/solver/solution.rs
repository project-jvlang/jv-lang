use crate::constraints::{Constraint, TypeVarId};
use jv_ast::types::TypeAnnotation;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// 型解決結果を表す。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct TypeSolution {
    pub substitutions: HashMap<TypeVarId, TypeAnnotation>,
    #[serde(default)]
    pub constraints: Vec<Constraint>,
    #[serde(default)]
    pub diagnostics: Vec<String>,
    /// 入力ソースの簡易フィンガープリント。キャッシュ境界の検証に用いる。
    #[serde(default)]
    pub fingerprint: u64,
}

impl Eq for TypeSolution {}
