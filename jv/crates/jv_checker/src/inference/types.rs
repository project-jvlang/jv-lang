//! 型推論で扱う型表現・型変数・束縛を定義する。

use std::collections::HashSet;
use std::fmt;

/// 推論で利用する型変数ID。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

impl TypeId {
    /// 新しい型IDを生成する。
    pub fn new(raw: u32) -> Self {
        Self(raw)
    }

    /// 内部の連番値を取得する。
    pub fn to_raw(self) -> u32 {
        self.0
    }
}

impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.0)
    }
}

/// 型推論で用いる主な型表現。
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Primitive(&'static str),
    Reference(String),
    Optional(Box<TypeKind>),
    Variable(TypeId),
    Function(Vec<TypeKind>, Box<TypeKind>),
    Unknown,
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::Unknown
    }
}

impl TypeKind {
    /// ヘルパー: 関数型を生成する。
    pub fn function(params: Vec<TypeKind>, return_type: TypeKind) -> Self {
        TypeKind::Function(params, Box::new(return_type))
    }

    /// 型表現に未決定な `Unknown` が含まれているか判定する。
    pub fn contains_unknown(&self) -> bool {
        match self {
            TypeKind::Unknown => true,
            TypeKind::Optional(inner) => inner.contains_unknown(),
            TypeKind::Function(params, ret) => {
                params.iter().any(TypeKind::contains_unknown) || ret.contains_unknown()
            }
            TypeKind::Primitive(_) | TypeKind::Reference(_) | TypeKind::Variable(_) => false,
        }
    }

    /// 自由型変数を収集し、ソート済みベクタで返す。
    pub fn free_type_vars(&self) -> Vec<TypeId> {
        let mut vars = HashSet::new();
        self.collect_free_type_vars_into(&mut vars);
        let mut vars: Vec<_> = vars.into_iter().collect();
        vars.sort_by_key(|id| id.to_raw());
        vars
    }

    pub(crate) fn collect_free_type_vars_into(&self, acc: &mut HashSet<TypeId>) {
        match self {
            TypeKind::Primitive(_) | TypeKind::Reference(_) | TypeKind::Unknown => {}
            TypeKind::Variable(id) => {
                acc.insert(*id);
            }
            TypeKind::Optional(inner) => inner.collect_free_type_vars_into(acc),
            TypeKind::Function(params, ret) => {
                for param in params {
                    param.collect_free_type_vars_into(acc);
                }
                ret.collect_free_type_vars_into(acc);
            }
        }
    }
}

/// 型変数に関する現在の状態を示す。
#[derive(Debug, Clone, PartialEq)]
pub enum TypeVariableKind {
    Unbound,
    Bound(TypeKind),
}

impl Default for TypeVariableKind {
    fn default() -> Self {
        TypeVariableKind::Unbound
    }
}

/// 型変数を表し、推論過程での束縛状態を保持する。
#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariable {
    pub id: TypeId,
    pub name: Option<String>,
    pub kind: TypeVariableKind,
}

impl TypeVariable {
    /// 指定したIDと任意の名前で型変数を生成する。
    pub fn new(id: TypeId, name: Option<String>) -> Self {
        Self {
            id,
            name,
            kind: TypeVariableKind::default(),
        }
    }
}

/// 型変数と具体的な型の束縛を表す。
#[derive(Debug, Clone, PartialEq)]
pub struct TypeBinding {
    pub variable: TypeVariable,
    pub ty: TypeKind,
}

impl TypeBinding {
    /// 型変数とその型のペアを生成する。
    pub fn new(variable: TypeVariable, ty: TypeKind) -> Self {
        Self { variable, ty }
    }
}
