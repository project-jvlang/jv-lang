use jv_ast::UnitRelation;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// Representation of a normalized unit symbol for summary purposes.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitSymbolSummary {
    pub name: String,
    pub is_bracketed: bool,
    pub has_default_marker: bool,
}

/// Metadata for a registered unit category.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitCategorySummary {
    pub id: u32,
    pub name: String,
    pub default_unit: Option<u32>,
    pub base_type: String,
    pub base_capability: String,
}

/// Metadata for an individual unit.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitSummary {
    pub id: u32,
    pub category_id: u32,
    pub symbol: UnitSymbolSummary,
    pub is_default: bool,
}

/// Reference into the conversion metadata list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct UnitConversionRef(pub u32);

/// Reverse conversion information attached to each edge.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ReverseMode {
    Provided,
    Auto { scale: Decimal, offset: Decimal },
    Unavailable,
}

/// Summary of a dependency edge between units.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitEdgeSummary {
    pub id: u32,
    pub from: u32,
    pub to: u32,
    pub relation: UnitRelation,
    pub rate: Option<Decimal>,
    pub reverse_mode: ReverseMode,
    pub conversion_ref: Option<UnitConversionRef>,
}

/// Summary entry that links an edge to a conversion IR slot.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitConversionSummary {
    pub edge: u32,
    pub index: u32,
}

/// Serializable snapshot of the entire unit registry.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitRegistrySummary {
    pub categories: Vec<UnitCategorySummary>,
    pub units: Vec<UnitSummary>,
    pub edges: Vec<UnitEdgeSummary>,
    pub conversions: Vec<UnitConversionSummary>,
}
