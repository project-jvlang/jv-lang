//! 型推論で扱う型表現・型変数・束縛を定義する。

use std::collections::HashSet;
use std::fmt;
use thiserror::Error;

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

/// Java プリミティブ型を表現する列挙体。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Boolean,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Char,
}

impl PrimitiveType {
    /// Java のプリミティブ型表記（lower snake case）を返す。
    pub const fn java_name(self) -> &'static str {
        match self {
            PrimitiveType::Boolean => "boolean",
            PrimitiveType::Byte => "byte",
            PrimitiveType::Short => "short",
            PrimitiveType::Int => "int",
            PrimitiveType::Long => "long",
            PrimitiveType::Float => "float",
            PrimitiveType::Double => "double",
            PrimitiveType::Char => "char",
        }
    }

    /// jv 言語でのプリミティブ型名称（先頭大文字）を返す。
    pub const fn jv_name(self) -> &'static str {
        match self {
            PrimitiveType::Boolean => "Boolean",
            PrimitiveType::Byte => "Byte",
            PrimitiveType::Short => "Short",
            PrimitiveType::Int => "Int",
            PrimitiveType::Long => "Long",
            PrimitiveType::Float => "Float",
            PrimitiveType::Double => "Double",
            PrimitiveType::Char => "Char",
        }
    }

    /// 対応する boxed 型の完全修飾クラス名を返す。
    pub const fn boxed_fqcn(self) -> &'static str {
        match self {
            PrimitiveType::Boolean => "java.lang.Boolean",
            PrimitiveType::Byte => "java.lang.Byte",
            PrimitiveType::Short => "java.lang.Short",
            PrimitiveType::Int => "java.lang.Integer",
            PrimitiveType::Long => "java.lang.Long",
            PrimitiveType::Float => "java.lang.Float",
            PrimitiveType::Double => "java.lang.Double",
            PrimitiveType::Char => "java.lang.Character",
        }
    }

    /// Java 言語仕様における widening conversion の対象を返す。
    pub fn widening_targets(self) -> &'static [PrimitiveType] {
        const NONE: &[PrimitiveType] = &[];
        const BYTE_TARGETS: &[PrimitiveType] = &[
            PrimitiveType::Short,
            PrimitiveType::Int,
            PrimitiveType::Long,
            PrimitiveType::Float,
            PrimitiveType::Double,
        ];
        const SHORT_TARGETS: &[PrimitiveType] = &[
            PrimitiveType::Int,
            PrimitiveType::Long,
            PrimitiveType::Float,
            PrimitiveType::Double,
        ];
        const CHAR_TARGETS: &[PrimitiveType] = &[
            PrimitiveType::Int,
            PrimitiveType::Long,
            PrimitiveType::Float,
            PrimitiveType::Double,
        ];
        const INT_TARGETS: &[PrimitiveType] = &[
            PrimitiveType::Long,
            PrimitiveType::Float,
            PrimitiveType::Double,
        ];
        const LONG_TARGETS: &[PrimitiveType] = &[PrimitiveType::Float, PrimitiveType::Double];
        const FLOAT_TARGETS: &[PrimitiveType] = &[PrimitiveType::Double];

        match self {
            PrimitiveType::Boolean => NONE,
            PrimitiveType::Byte => BYTE_TARGETS,
            PrimitiveType::Short => SHORT_TARGETS,
            PrimitiveType::Char => CHAR_TARGETS,
            PrimitiveType::Int => INT_TARGETS,
            PrimitiveType::Long => LONG_TARGETS,
            PrimitiveType::Float => FLOAT_TARGETS,
            PrimitiveType::Double => NONE,
        }
    }

    /// Java のプリミティブ型表記から `PrimitiveType` を生成する。
    pub fn from_java_name(name: &str) -> Option<Self> {
        match name {
            "boolean" => Some(PrimitiveType::Boolean),
            "byte" => Some(PrimitiveType::Byte),
            "short" => Some(PrimitiveType::Short),
            "int" => Some(PrimitiveType::Int),
            "long" => Some(PrimitiveType::Long),
            "float" => Some(PrimitiveType::Float),
            "double" => Some(PrimitiveType::Double),
            "char" => Some(PrimitiveType::Char),
            _ => None,
        }
    }
}

/// 型推論で用いる主な型表現。
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    Boxed(PrimitiveType),
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

    /// プリミティブ型を生成する。
    pub fn primitive(primitive: PrimitiveType) -> Self {
        TypeKind::Primitive(primitive)
    }

    /// ボックス化されたプリミティブ型を生成する。
    pub fn boxed(primitive: PrimitiveType) -> Self {
        TypeKind::Boxed(primitive)
    }

    /// 参照型を生成する。
    pub fn reference(name: impl Into<String>) -> Self {
        TypeKind::Reference(name.into())
    }

    /// Optional 型を生成する。
    pub fn optional(inner: TypeKind) -> Self {
        TypeKind::Optional(Box::new(inner))
    }

    /// プリミティブ型の nullable 版を生成する（ボックス型を Optional で包む）。
    pub fn optional_primitive(primitive: PrimitiveType) -> Self {
        TypeKind::Optional(Box::new(TypeKind::Boxed(primitive)))
    }

    /// プリミティブ型かどうかを判定する。
    pub fn is_primitive(&self) -> bool {
        matches!(self, TypeKind::Primitive(_))
    }

    /// ボックスプリミティブ型かどうかを判定する。
    pub fn is_boxed(&self) -> bool {
        matches!(self, TypeKind::Boxed(_))
    }

    /// null 許容かどうかを判定する。
    pub fn is_nullable(&self) -> bool {
        !matches!(self, TypeKind::Primitive(_))
    }

    /// 型表現に未決定な `Unknown` が含まれているか判定する。
    pub fn contains_unknown(&self) -> bool {
        match self {
            TypeKind::Unknown => true,
            TypeKind::Optional(inner) => inner.contains_unknown(),
            TypeKind::Function(params, ret) => {
                params.iter().any(TypeKind::contains_unknown) || ret.contains_unknown()
            }
            TypeKind::Primitive(_)
            | TypeKind::Boxed(_)
            | TypeKind::Reference(_)
            | TypeKind::Variable(_) => false,
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
            TypeKind::Primitive(_)
            | TypeKind::Boxed(_)
            | TypeKind::Reference(_)
            | TypeKind::Unknown => {}
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

/// 型関連のエラーを表現する。
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum TypeError {
    #[error("unknown primitive type `{identifier}`")]
    UnknownPrimitive { identifier: String },
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
