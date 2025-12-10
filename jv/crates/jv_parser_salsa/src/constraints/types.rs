use jv_ast::types::TypeAnnotation;
use serde::{Deserialize, Serialize};
use std::hash::{Hash, Hasher};

/// 型変数識別子。
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub struct TypeVarId(pub u32);

impl Hash for TypeVarId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

/// 型注釈 or 型変数を示す参照。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeRef {
    Annotation(TypeAnnotation),
    TypeVar(TypeVarId),
}

impl Eq for TypeRef {}

/// 制約の種類。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Constraint {
    /// 型同士が等価であることを要求する。
    Equal(TypeRef, TypeRef),
    /// サブタイプ関係を要求する。
    Subtype { sub: TypeRef, sup: TypeRef },
    /// 特定フィールドを持つことを要求する。
    HasField {
        target: TypeRef,
        field: String,
        field_type: TypeRef,
    },
    /// 呼び出し可能性の制約。
    Callable {
        function: TypeRef,
        args: Vec<TypeRef>,
        ret: TypeRef,
    },
}

impl Eq for Constraint {}
