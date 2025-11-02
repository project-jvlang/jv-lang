//! Doublebrace 初期化向けに暗黙レシーバーのメンバー有無を検証するヘルパ。
//!
//! `SymbolIndex` に記録されたメタデータを参照し、指定された型が
//! 指定メンバー（メソッド／フィールド）を公開しているかを判定する。

use super::primitive::JavaBoxingTable;
use crate::inference::types::TypeKind;
use jv_build::metadata::{SymbolIndex, TypeEntry};
use std::sync::Arc;

/// Doublebrace 暗黙レシーバーのメンバー解決を補助するリゾルバ。
#[derive(Debug, Clone)]
pub struct MemberResolver {
    symbol_index: Option<Arc<SymbolIndex>>,
}

impl MemberResolver {
    /// SymbolIndex への参照を保持した新しいリゾルバを構築する。
    pub fn new(symbol_index: Option<Arc<SymbolIndex>>) -> Self {
        Self { symbol_index }
    }

    /// レシーバー型が指定メンバーを持たない場合、そのメンバー名を返す。
    pub fn missing_members(&self, receiver: &TypeKind, members: &[String]) -> Vec<String> {
        let Some(entry) = self.type_entry(receiver) else {
            return Vec::new();
        };

        members
            .iter()
            .filter(|name| !Self::has_member(entry, name))
            .cloned()
            .collect()
    }

    /// レシーバー型が公開しているメンバー名を取得する（診断候補用、重複除去済み）。
    pub fn member_candidates(&self, receiver: &TypeKind, limit: usize) -> Vec<String> {
        let Some(entry) = self.type_entry(receiver) else {
            return Vec::new();
        };

        let mut names: Vec<String> = entry.instance_fields.iter().cloned().collect();
        names.extend(entry.instance_methods.keys().cloned());
        if names.len() > 1 {
            names.sort();
            names.dedup();
        }
        if names.len() > limit {
            names.truncate(limit);
        }
        names
    }

    fn type_entry(&self, receiver: &TypeKind) -> Option<&TypeEntry> {
        let index = self.symbol_index.as_ref()?;
        let fqcn = Self::receiver_fqcn(receiver)?;
        index.lookup_type(&fqcn)
    }

    fn receiver_fqcn(ty: &TypeKind) -> Option<String> {
        match ty {
            TypeKind::Primitive(primitive) => {
                Some(JavaBoxingTable::boxed_fqcn(*primitive).to_string())
            }
            TypeKind::Boxed(primitive) => Some(primitive.boxed_fqcn().to_string()),
            TypeKind::Reference(name) => Some(name.clone()),
            TypeKind::Optional(inner) => Self::receiver_fqcn(inner),
            _ => None,
        }
    }

    fn has_member(entry: &TypeEntry, name: &str) -> bool {
        entry.has_field(name) || entry.has_instance_method(name)
    }
}
