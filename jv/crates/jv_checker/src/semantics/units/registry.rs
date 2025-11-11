use super::{BaseTypeCapability, UnitCategorySpec, UnitConversionKind, UnitSymbolRaw};
use crate::inference::types::TypeKind;
use jv_ast::{Expression, Span, UnitRelation};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

pub type UnitCategoryId = u32;
pub type UnitId = u32;
pub type UnitEdgeId = u32;

/// 単位カテゴリの登録情報。
#[derive(Debug, Clone)]
pub struct UnitCategoryEntry {
    pub id: UnitCategoryId,
    pub name: String,
    pub spec: UnitCategorySpec,
    pub base_type: TypeKind,
    pub base_capability: BaseTypeCapability,
    pub default_unit: Option<UnitId>,
}

/// 単位シンボルの登録情報。
#[derive(Debug, Clone)]
pub struct UnitEntry {
    pub id: UnitId,
    pub category_id: UnitCategoryId,
    pub symbol: UnitSymbolRaw,
    pub is_default: bool,
}

/// 単位間の依存エッジ。
#[derive(Debug, Clone)]
pub struct UnitEdge {
    pub id: UnitEdgeId,
    pub from: UnitId,
    pub to: UnitId,
    pub relation: UnitRelation,
    pub rate: Option<Decimal>,
    pub reverse_mode: ReverseMode,
    pub conversion_ref: Option<UnitConversionRef>,
    pub span: Span,
}

/// 変換ラムダIR本体。
#[derive(Debug, Clone)]
pub struct UnitConversionBody {
    pub edge: UnitEdgeId,
    pub ir: Arc<ConversionLambdaIr>,
    pub original_ast: Option<UnitConversionAstRef>,
}

/// 解析済み変換ラムダのIR。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConversionLambdaIr {
    pub parameter_name: String,
    pub parameter_type: TypeKind,
    pub return_type: TypeKind,
    pub lambda: Expression,
    pub span: Span,
}

impl ConversionLambdaIr {
    pub fn body_expression(&self) -> Option<&Expression> {
        match &self.lambda {
            Expression::Lambda { body, .. } => Some(body),
            _ => None,
        }
    }
}

/// 元となったAST参照。
#[derive(Debug, Clone)]
pub struct UnitConversionAstRef {
    pub kind: UnitConversionKind,
    pub span: Span,
}

/// 逆変換の状態。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReverseMode {
    Provided,
    Auto { scale: Decimal, offset: Decimal },
    Unavailable,
}

impl Default for ReverseMode {
    fn default() -> Self {
        ReverseMode::Unavailable
    }
}

/// 変換IR配列上の参照。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct UnitConversionRef(pub u32);

/// `UnitRegistry` のルックアップに利用するキー。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct UnitLookupKey {
    pub(crate) category_id: UnitCategoryId,
    pub(crate) name: String,
    pub(crate) is_bracketed: bool,
}

impl UnitLookupKey {
    pub(crate) fn new(
        category_id: UnitCategoryId,
        name: impl Into<String>,
        is_bracketed: bool,
    ) -> Self {
        Self {
            category_id,
            name: name.into(),
            is_bracketed,
        }
    }
}

/// 単位カテゴリ/単位/依存エッジを集約する不変データ構造。
#[derive(Debug, Clone)]
pub struct UnitRegistry {
    categories: Vec<UnitCategoryEntry>,
    units: Vec<UnitEntry>,
    edges: Vec<UnitEdge>,
    conversions: Vec<UnitConversionBody>,
    lookup: HashMap<UnitLookupKey, UnitId>,
    category_lookup: HashMap<String, UnitCategoryId>,
}

impl UnitRegistry {
    pub(crate) fn new(
        categories: Vec<UnitCategoryEntry>,
        units: Vec<UnitEntry>,
        edges: Vec<UnitEdge>,
        conversions: Vec<UnitConversionBody>,
        lookup: HashMap<UnitLookupKey, UnitId>,
        category_lookup: HashMap<String, UnitCategoryId>,
    ) -> Self {
        Self {
            categories,
            units,
            edges,
            conversions,
            lookup,
            category_lookup,
        }
    }

    /// 登録済みカテゴリを列挙する。
    pub fn categories(&self) -> &[UnitCategoryEntry] {
        &self.categories
    }

    /// 登録済み単位を列挙する。
    pub fn units(&self) -> &[UnitEntry] {
        &self.units
    }

    /// 登録済みエッジを列挙する。
    pub fn edges(&self) -> &[UnitEdge] {
        &self.edges
    }

    /// 変換IRの一覧を取得する（タスク4以降で利用）。
    pub fn conversions(&self) -> &[UnitConversionBody] {
        &self.conversions
    }

    /// カテゴリ名と単位記号から `UnitId` を解決する。
    pub fn resolve(&self, category: &str, symbol_text: &str) -> Option<UnitId> {
        let category_id = *self.category_lookup.get(category)?;
        let dummy_span = Span::dummy();
        let symbol = UnitSymbolRaw::from_identifier(symbol_text, &dummy_span);
        self.lookup_in_category(category_id, &symbol.name, Some(symbol.is_bracketed))
    }

    pub(crate) fn lookup_in_category(
        &self,
        category_id: UnitCategoryId,
        name: &str,
        is_bracketed: Option<bool>,
    ) -> Option<UnitId> {
        match is_bracketed {
            Some(flag) => self
                .lookup
                .get(&UnitLookupKey::new(category_id, name, flag))
                .copied(),
            None => self
                .lookup
                .get(&UnitLookupKey::new(category_id, name, false))
                .copied()
                .or_else(|| {
                    self.lookup
                        .get(&UnitLookupKey::new(category_id, name, true))
                        .copied()
                }),
        }
    }

    /// `UnitRegistrySummary` に変換する。
    pub fn to_summary(&self) -> UnitRegistrySummary {
        UnitRegistrySummary {
            categories: self
                .categories
                .iter()
                .map(|entry| UnitCategorySummary {
                    id: entry.id,
                    name: entry.name.clone(),
                    default_unit: entry.default_unit,
                    base_type: entry.base_type.clone(),
                    base_capability: entry.base_capability,
                })
                .collect(),
            units: self
                .units
                .iter()
                .map(|entry| UnitSummary {
                    id: entry.id,
                    category_id: entry.category_id,
                    symbol: entry.symbol.clone(),
                    is_default: entry.is_default,
                })
                .collect(),
            edges: self
                .edges
                .iter()
                .map(|edge| UnitEdgeSummary {
                    id: edge.id,
                    from: edge.from,
                    to: edge.to,
                    relation: edge.relation,
                    rate: edge.rate,
                    reverse_mode: edge.reverse_mode,
                    conversion_ref: edge.conversion_ref,
                })
                .collect(),
            conversions: self
                .conversions
                .iter()
                .enumerate()
                .map(|(index, body)| UnitConversionSummary {
                    edge: body.edge,
                    index: index as u32,
                })
                .collect(),
        }
    }
}

/// IR へ搬送するためのサマリ構造。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitRegistrySummary {
    pub categories: Vec<UnitCategorySummary>,
    pub units: Vec<UnitSummary>,
    pub edges: Vec<UnitEdgeSummary>,
    pub conversions: Vec<UnitConversionSummary>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitCategorySummary {
    pub id: UnitCategoryId,
    pub name: String,
    pub default_unit: Option<UnitId>,
    pub base_type: TypeKind,
    pub base_capability: BaseTypeCapability,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitSummary {
    pub id: UnitId,
    pub category_id: UnitCategoryId,
    pub symbol: UnitSymbolRaw,
    pub is_default: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitEdgeSummary {
    pub id: UnitEdgeId,
    pub from: UnitId,
    pub to: UnitId,
    pub relation: UnitRelation,
    pub rate: Option<Decimal>,
    pub reverse_mode: ReverseMode,
    pub conversion_ref: Option<UnitConversionRef>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitConversionSummary {
    pub edge: UnitEdgeId,
    pub index: u32,
}
