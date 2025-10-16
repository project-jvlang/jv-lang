//! Java 固有のプリミティブ型情報とボクシング/ヌル許容ポリシー。

use serde::{Deserialize, Serialize};
use std::borrow::Cow;

/// Java プリミティブ型の列挙。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JavaPrimitive {
    Boolean,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Char,
}

impl JavaPrimitive {
    /// Java 言語仕様に基づくプリミティブ型名（lower snake case）。
    pub const fn java_name(self) -> &'static str {
        match self {
            JavaPrimitive::Boolean => "boolean",
            JavaPrimitive::Byte => "byte",
            JavaPrimitive::Short => "short",
            JavaPrimitive::Int => "int",
            JavaPrimitive::Long => "long",
            JavaPrimitive::Float => "float",
            JavaPrimitive::Double => "double",
            JavaPrimitive::Char => "char",
        }
    }

    /// jv 言語での慣用名（先頭大文字）。
    pub const fn jv_name(self) -> &'static str {
        match self {
            JavaPrimitive::Boolean => "Boolean",
            JavaPrimitive::Byte => "Byte",
            JavaPrimitive::Short => "Short",
            JavaPrimitive::Int => "Int",
            JavaPrimitive::Long => "Long",
            JavaPrimitive::Float => "Float",
            JavaPrimitive::Double => "Double",
            JavaPrimitive::Char => "Char",
        }
    }

    /// 対応する boxed 型の完全修飾クラス名。
    pub const fn boxed_fqcn(self) -> &'static str {
        match self {
            JavaPrimitive::Boolean => "java.lang.Boolean",
            JavaPrimitive::Byte => "java.lang.Byte",
            JavaPrimitive::Short => "java.lang.Short",
            JavaPrimitive::Int => "java.lang.Integer",
            JavaPrimitive::Long => "java.lang.Long",
            JavaPrimitive::Float => "java.lang.Float",
            JavaPrimitive::Double => "java.lang.Double",
            JavaPrimitive::Char => "java.lang.Character",
        }
    }

    /// Widening conversion の対象となるプリミティブ型の一覧。
    pub fn widening_targets(self) -> &'static [JavaPrimitive] {
        const NONE: &[JavaPrimitive] = &[];
        const BYTE_TARGETS: &[JavaPrimitive] = &[
            JavaPrimitive::Short,
            JavaPrimitive::Int,
            JavaPrimitive::Long,
            JavaPrimitive::Float,
            JavaPrimitive::Double,
        ];
        const SHORT_TARGETS: &[JavaPrimitive] = &[
            JavaPrimitive::Int,
            JavaPrimitive::Long,
            JavaPrimitive::Float,
            JavaPrimitive::Double,
        ];
        const CHAR_TARGETS: &[JavaPrimitive] = &[
            JavaPrimitive::Int,
            JavaPrimitive::Long,
            JavaPrimitive::Float,
            JavaPrimitive::Double,
        ];
        const INT_TARGETS: &[JavaPrimitive] = &[
            JavaPrimitive::Long,
            JavaPrimitive::Float,
            JavaPrimitive::Double,
        ];
        const LONG_TARGETS: &[JavaPrimitive] = &[JavaPrimitive::Float, JavaPrimitive::Double];
        const FLOAT_TARGETS: &[JavaPrimitive] = &[JavaPrimitive::Double];

        match self {
            JavaPrimitive::Boolean => NONE,
            JavaPrimitive::Byte => BYTE_TARGETS,
            JavaPrimitive::Short => SHORT_TARGETS,
            JavaPrimitive::Char => CHAR_TARGETS,
            JavaPrimitive::Int => INT_TARGETS,
            JavaPrimitive::Long => LONG_TARGETS,
            JavaPrimitive::Float => FLOAT_TARGETS,
            JavaPrimitive::Double => NONE,
        }
    }

    /// Java のプリミティブ型名称から列挙値を生成する。
    pub fn from_java_name(name: &str) -> Option<Self> {
        match name {
            "boolean" => Some(JavaPrimitive::Boolean),
            "byte" => Some(JavaPrimitive::Byte),
            "short" => Some(JavaPrimitive::Short),
            "int" => Some(JavaPrimitive::Int),
            "long" => Some(JavaPrimitive::Long),
            "float" => Some(JavaPrimitive::Float),
            "double" => Some(JavaPrimitive::Double),
            "char" => Some(JavaPrimitive::Char),
            _ => None,
        }
    }

    /// jv で利用される識別子やFQCNから列挙値を解決する。
    pub fn from_identifier(identifier: &str) -> Option<Self> {
        let trimmed = identifier.trim();
        if trimmed.is_empty() {
            return None;
        }

        let lower = trimmed.to_ascii_lowercase();

        match lower.as_str() {
            "bool" | "boolean" => Some(JavaPrimitive::Boolean),
            "byte" => Some(JavaPrimitive::Byte),
            "short" | "i16" => Some(JavaPrimitive::Short),
            "int" | "i" | "i32" => Some(JavaPrimitive::Int),
            "long" | "i64" | "l" => Some(JavaPrimitive::Long),
            "float" | "f" | "f32" => Some(JavaPrimitive::Float),
            "double" | "f64" => Some(JavaPrimitive::Double),
            "char" | "c" => Some(JavaPrimitive::Char),
            _ => JavaBoxingTable::primitive_from_boxed(trimmed)
                .or_else(|| Self::from_java_name(lower.as_str())),
        }
    }

    /// 利便性: 列挙値から `Cow<'static, str>` の Java 型表現を生成する。
    pub fn java_name_cow(self) -> Cow<'static, str> {
        Cow::Borrowed(self.java_name())
    }
}

/// Boxing / unboxing 判定を提供するテーブル。
#[derive(Debug, Default)]
pub struct JavaBoxingTable;

impl JavaBoxingTable {
    /// プリミティブに対応する boxed クラス名（FQCN）を返す。
    pub const fn boxed_fqcn(primitive: JavaPrimitive) -> &'static str {
        primitive.boxed_fqcn()
    }

    /// プリミティブに対応する boxed クラス名（単純名）を返す。
    pub const fn boxed_simple_name(primitive: JavaPrimitive) -> &'static str {
        match primitive {
            JavaPrimitive::Boolean => "Boolean",
            JavaPrimitive::Byte => "Byte",
            JavaPrimitive::Short => "Short",
            JavaPrimitive::Int => "Integer",
            JavaPrimitive::Long => "Long",
            JavaPrimitive::Float => "Float",
            JavaPrimitive::Double => "Double",
            JavaPrimitive::Char => "Character",
        }
    }

    /// Boxed 型の識別子（単純名/FQCN）からプリミティブ型を判定する。
    pub fn primitive_from_boxed(identifier: &str) -> Option<JavaPrimitive> {
        match identifier {
            "Integer" | "java.lang.Integer" => Some(JavaPrimitive::Int),
            "Long" | "java.lang.Long" => Some(JavaPrimitive::Long),
            "Short" | "java.lang.Short" => Some(JavaPrimitive::Short),
            "Byte" | "java.lang.Byte" => Some(JavaPrimitive::Byte),
            "Float" | "java.lang.Float" => Some(JavaPrimitive::Float),
            "Double" | "java.lang.Double" => Some(JavaPrimitive::Double),
            "Boolean" | "java.lang.Boolean" => Some(JavaPrimitive::Boolean),
            "Char" | "Character" | "java.lang.Character" => Some(JavaPrimitive::Char),
            _ => None,
        }
    }

    /// Boxed 型かどうかを判定する。
    pub fn is_boxed_identifier(identifier: &str) -> bool {
        Self::primitive_from_boxed(identifier).is_some()
    }
}

/// Java の null 許容ポリシー。
#[derive(Debug, Default)]
pub struct JavaNullabilityPolicy;

impl JavaNullabilityPolicy {
    /// プリミティブ型が `null` を許容するかどうか。
    pub const fn primitives_allow_null() -> bool {
        false
    }

    /// Boxed プリミティブ型が `null` を許容するかどうか。
    pub const fn boxed_allow_null() -> bool {
        true
    }

    /// 可 null 文脈でプリミティブを扱う際に boxing が必要かどうか。
    pub const fn requires_boxing_for_nullable(_primitive: JavaPrimitive) -> bool {
        true
    }
}
