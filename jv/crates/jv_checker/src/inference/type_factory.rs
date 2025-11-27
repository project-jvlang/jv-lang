//! ユーティリティ: 識別子やFQCNから `TypeKind` を生成するヘルパ。

use crate::inference::types::{PrimitiveType, TypeError, TypeKind};
use crate::java::{JavaBoxingTable, JavaPrimitive};

/// 型生成に関する補助関数群。
#[derive(Debug, Default)]
pub struct TypeFactory;

impl TypeFactory {
    /// 既知のプリミティブ識別子から `PrimitiveType` を生成する。
    pub fn primitive_from_identifier(identifier: &str) -> Result<PrimitiveType, TypeError> {
        let normalized = identifier.trim();
        JavaPrimitive::from_identifier(normalized).ok_or_else(|| TypeError::UnknownPrimitive {
            identifier: identifier.to_string(),
        })
    }

    /// Java boxed 型の FQCN もしくは短縮名からプリミティブ種別を検出する。
    pub fn boxed_primitive(identifier: &str) -> Option<PrimitiveType> {
        JavaBoxingTable::primitive_from_boxed(identifier.trim())
    }

    /// 型注釈などの識別子から `TypeKind` を生成する。未知プリミティブはエラーとする。
    pub fn from_annotation(identifier: &str) -> Result<TypeKind, TypeError> {
        let normalized = identifier.trim();
        let normalized_lower = normalized.to_ascii_lowercase();

        if matches!(
            normalized_lower.as_str(),
            "bigdecimal" | "decimal" | "java.math.bigdecimal" | "math.bigdecimal"
        ) {
            return Ok(TypeKind::reference("java.math.BigDecimal"));
        }

        if let Some(primitive) = JavaPrimitive::from_identifier(normalized) {
            return Ok(TypeKind::primitive(primitive));
        }
        if let Some(primitive) = Self::boxed_primitive(normalized) {
            return Ok(TypeKind::boxed(primitive));
        }

        match normalized {
            "String" => Ok(TypeKind::reference("java.lang.String")),
            "Iterable" | "java.lang.Iterable" => Ok(TypeKind::reference("java.lang.Iterable")),
            "Iterator" | "java.util.Iterator" => Ok(TypeKind::reference("java.util.Iterator")),
            "Stream" | "java.util.stream.Stream" => {
                Ok(TypeKind::reference("java.util.stream.Stream"))
            }
            "List" | "java.util.List" => Ok(TypeKind::reference("java.util.List")),
            "Map" | "java.util.Map" => Ok(TypeKind::reference("java.util.Map")),
            other => Ok(TypeKind::reference(other.to_string())),
        }
    }

    /// Java プリミティブ名から `TypeKind::Primitive` を生成する。
    pub fn primitive_from_java_name(name: &str) -> Result<TypeKind, TypeError> {
        JavaPrimitive::from_java_name(name)
            .map(TypeKind::primitive)
            .ok_or_else(|| TypeError::UnknownPrimitive {
                identifier: name.to_string(),
            })
    }

    /// ボックス型 FQCN から `TypeKind::Boxed` を生成する。
    pub fn boxed_from_fqcn(name: &str) -> Option<TypeKind> {
        Self::boxed_primitive(name).map(TypeKind::boxed)
    }
}
