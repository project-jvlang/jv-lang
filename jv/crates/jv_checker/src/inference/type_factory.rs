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

        if let Some(primitive) = Self::boxed_primitive(normalized) {
            return Ok(TypeKind::boxed(primitive));
        }
        if let Some(primitive) = JavaPrimitive::from_identifier(normalized) {
            return Ok(TypeKind::primitive(primitive));
        }

        match normalized_lower.as_str() {
            "string" => return Ok(TypeKind::reference("java.lang.String")),
            "iterable" | "java.lang.iterable" => {
                return Ok(TypeKind::reference("java.lang.Iterable"));
            }
            "iterator" | "java.util.iterator" => {
                return Ok(TypeKind::reference("java.util.Iterator"));
            }
            "stream" | "java.util.stream.stream" => {
                return Ok(TypeKind::reference("java.util.stream.Stream"));
            }
            "collection" | "java.util.collection" => {
                return Ok(TypeKind::reference("java.util.Collection"));
            }
            "list" | "java.util.list" => return Ok(TypeKind::reference("java.util.List")),
            "set" | "java.util.set" => return Ok(TypeKind::reference("java.util.Set")),
            "map" | "java.util.map" => return Ok(TypeKind::reference("java.util.Map")),
            "queue" | "java.util.queue" => return Ok(TypeKind::reference("java.util.Queue")),
            "deque" | "java.util.deque" => return Ok(TypeKind::reference("java.util.Deque")),
            "navigableset" | "java.util.navigableset" => {
                return Ok(TypeKind::reference("java.util.NavigableSet"));
            }
            "sortedset" | "java.util.sortedset" => {
                return Ok(TypeKind::reference("java.util.SortedSet"));
            }
            "navigablemap" | "java.util.navigablemap" => {
                return Ok(TypeKind::reference("java.util.NavigableMap"));
            }
            "sortedmap" | "java.util.sortedmap" => {
                return Ok(TypeKind::reference("java.util.SortedMap"));
            }
            "concurrentmap" | "java.util.concurrentmap" => {
                return Ok(TypeKind::reference("java.util.concurrent.ConcurrentMap"));
            }
            "arraylist" | "java.util.arraylist" => {
                return Ok(TypeKind::reference("java.util.ArrayList"));
            }
            "linkedlist" | "java.util.linkedlist" => {
                return Ok(TypeKind::reference("java.util.LinkedList"));
            }
            "hashmap" | "java.util.hashmap" => {
                return Ok(TypeKind::reference("java.util.HashMap"));
            }
            "linkedhashmap" | "java.util.linkedhashmap" => {
                return Ok(TypeKind::reference("java.util.LinkedHashMap"));
            }
            "treemap" | "java.util.treemap" => {
                return Ok(TypeKind::reference("java.util.TreeMap"));
            }
            "hashset" | "java.util.hashset" => {
                return Ok(TypeKind::reference("java.util.HashSet"));
            }
            "linkedhashset" | "java.util.linkedhashset" => {
                return Ok(TypeKind::reference("java.util.LinkedHashSet"));
            }
            "treeset" | "java.util.treeset" => {
                return Ok(TypeKind::reference("java.util.TreeSet"));
            }
            "arraydeque" | "java.util.arraydeque" => {
                return Ok(TypeKind::reference("java.util.ArrayDeque"));
            }
            "concurrenthashmap" | "java.util.concurrenthashmap" => {
                return Ok(TypeKind::reference(
                    "java.util.concurrent.ConcurrentHashMap",
                ));
            }
            _ => {}
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
            "ArrayList" | "java.util.ArrayList" => Ok(TypeKind::reference("java.util.ArrayList")),
            "LinkedList" | "java.util.LinkedList" => {
                Ok(TypeKind::reference("java.util.LinkedList"))
            }
            "HashMap" | "java.util.HashMap" => Ok(TypeKind::reference("java.util.HashMap")),
            "LinkedHashMap" | "java.util.LinkedHashMap" => {
                Ok(TypeKind::reference("java.util.LinkedHashMap"))
            }
            "TreeMap" | "java.util.TreeMap" => Ok(TypeKind::reference("java.util.TreeMap")),
            "HashSet" | "java.util.HashSet" => Ok(TypeKind::reference("java.util.HashSet")),
            "LinkedHashSet" | "java.util.LinkedHashSet" => {
                Ok(TypeKind::reference("java.util.LinkedHashSet"))
            }
            "TreeSet" | "java.util.TreeSet" => Ok(TypeKind::reference("java.util.TreeSet")),
            "ArrayDeque" | "java.util.ArrayDeque" => {
                Ok(TypeKind::reference("java.util.ArrayDeque"))
            }
            "ConcurrentHashMap" | "java.util.concurrent.ConcurrentHashMap" => Ok(
                TypeKind::reference("java.util.concurrent.ConcurrentHashMap"),
            ),
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
