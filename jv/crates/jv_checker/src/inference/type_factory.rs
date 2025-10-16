//! ユーティリティ: 識別子やFQCNから `TypeKind` を生成するヘルパ。

use crate::inference::types::{PrimitiveType, TypeError, TypeKind};

/// 型生成に関する補助関数群。
#[derive(Debug, Default)]
pub struct TypeFactory;

impl TypeFactory {
    /// 既知のプリミティブ識別子から `PrimitiveType` を生成する。
    pub fn primitive_from_identifier(identifier: &str) -> Result<PrimitiveType, TypeError> {
        let normalized = identifier.trim();
        Self::primitive_alias(normalized)
            .or_else(|| PrimitiveType::from_java_name(normalized.to_ascii_lowercase().as_str()))
            .ok_or_else(|| TypeError::UnknownPrimitive {
                identifier: identifier.to_string(),
            })
    }

    /// Java boxed 型の FQCN もしくは短縮名からプリミティブ種別を検出する。
    pub fn boxed_primitive(identifier: &str) -> Option<PrimitiveType> {
        match identifier {
            "Integer" | "java.lang.Integer" => Some(PrimitiveType::Int),
            "Long" | "java.lang.Long" => Some(PrimitiveType::Long),
            "Short" | "java.lang.Short" => Some(PrimitiveType::Short),
            "Byte" | "java.lang.Byte" => Some(PrimitiveType::Byte),
            "Float" | "java.lang.Float" => Some(PrimitiveType::Float),
            "Double" | "java.lang.Double" => Some(PrimitiveType::Double),
            "Boolean" | "java.lang.Boolean" => Some(PrimitiveType::Boolean),
            "Char" | "Character" | "java.lang.Character" => Some(PrimitiveType::Char),
            _ => None,
        }
    }

    /// 型注釈などの識別子から `TypeKind` を生成する。未知プリミティブはエラーとする。
    pub fn from_annotation(identifier: &str) -> Result<TypeKind, TypeError> {
        if let Some(primitive) = Self::primitive_alias(identifier) {
            return Ok(TypeKind::primitive(primitive));
        }
        if let Some(primitive) = Self::boxed_primitive(identifier) {
            return Ok(TypeKind::boxed(primitive));
        }

        match identifier {
            "String" => Ok(TypeKind::reference("java.lang.String")),
            "SequenceCore" => Ok(TypeKind::reference("jv.collections.SequenceCore")),
            "jv.collections.SequenceCore" => Ok(TypeKind::reference("jv.collections.SequenceCore")),
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
        PrimitiveType::from_java_name(name)
            .map(TypeKind::primitive)
            .ok_or_else(|| TypeError::UnknownPrimitive {
                identifier: name.to_string(),
            })
    }

    /// ボックス型 FQCN から `TypeKind::Boxed` を生成する。
    pub fn boxed_from_fqcn(name: &str) -> Option<TypeKind> {
        Self::boxed_primitive(name).map(TypeKind::boxed)
    }

    fn primitive_alias(identifier: &str) -> Option<PrimitiveType> {
        match identifier {
            "bool" | "Bool" | "boolean" | "Boolean" => Some(PrimitiveType::Boolean),
            "byte" | "Byte" => Some(PrimitiveType::Byte),
            "short" | "Short" => Some(PrimitiveType::Short),
            "int" | "Int" => Some(PrimitiveType::Int),
            "long" | "Long" => Some(PrimitiveType::Long),
            "float" | "Float" => Some(PrimitiveType::Float),
            "double" | "Double" => Some(PrimitiveType::Double),
            "char" | "Char" => Some(PrimitiveType::Char),
            _ => None,
        }
    }
}
