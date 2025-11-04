use jv_ast::{
    Expression, Span, Statement, TypeAnnotation, UnitConversionBlock, UnitConversionKind,
    UnitDependency, UnitRelation, UnitSymbol, UnitTypeDefinition, UnitTypeMember,
};
use serde::{Deserialize, Serialize};

pub mod collector;

pub use collector::UnitCatalogCollector;

/// ASTから抽出した単位定義の生データ。
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct RawUnitCatalog {
    pub definitions: Vec<UnitDefinitionRaw>,
}

impl RawUnitCatalog {
    pub fn new(definitions: Vec<UnitDefinitionRaw>) -> Self {
        Self { definitions }
    }

    pub fn is_empty(&self) -> bool {
        self.definitions.is_empty()
    }
}

/// 単位定義1件を表すデータ構造。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitDefinitionRaw {
    pub span: Span,
    pub category: String,
    pub base_type: TypeAnnotation,
    pub symbol: UnitSymbolRaw,
    pub members: Vec<UnitMemberRaw>,
}

impl From<&UnitTypeDefinition> for UnitDefinitionRaw {
    fn from(definition: &UnitTypeDefinition) -> Self {
        Self {
            span: definition.span.clone(),
            category: definition.category.clone(),
            base_type: definition.base_type.clone(),
            symbol: UnitSymbolRaw::from_symbol(&definition.name),
            members: definition
                .members
                .iter()
                .map(UnitMemberRaw::from_ast_member)
                .collect(),
        }
    }
}

/// 単位記号メタデータ。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitSymbolRaw {
    pub name: String,
    pub is_bracketed: bool,
    pub has_default_marker: bool,
    pub span: Span,
}

impl UnitSymbolRaw {
    pub fn from_symbol(symbol: &UnitSymbol) -> Self {
        Self {
            name: symbol.name.clone(),
            is_bracketed: symbol.is_bracketed,
            has_default_marker: symbol.has_default_marker,
            span: symbol.span.clone(),
        }
    }

    pub fn from_identifier(name: &str, span: &Span) -> Self {
        let mut normalized = name.trim().to_string();
        let mut has_default_marker = false;
        if normalized.ends_with('!') {
            has_default_marker = true;
            normalized.pop();
        }

        let mut is_bracketed = false;
        if normalized.starts_with('[') && normalized.ends_with(']') && normalized.len() >= 2 {
            is_bracketed = true;
            normalized = normalized[1..normalized.len() - 1].to_string();
        }

        Self {
            name: normalized,
            is_bracketed,
            has_default_marker,
            span: span.clone(),
        }
    }
}

/// 単位ブロック内に出現するメンバー情報。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnitMemberRaw {
    SymbolDecl(UnitSymbolRaw),
    Dependency(UnitDependencyRaw),
    Conversion(UnitConversionRaw),
    ConversionRate(UnitRateRaw),
    NestedStatement(Box<Statement>),
}

impl UnitMemberRaw {
    fn from_ast_member(member: &UnitTypeMember) -> Self {
        match member {
            UnitTypeMember::Dependency(dependency) => match dependency.relation {
                UnitRelation::DefinitionAssign
                    if dependency.value.is_none() && dependency.target.is_none() =>
                {
                    UnitMemberRaw::SymbolDecl(UnitSymbolRaw::from_identifier(
                        &dependency.name,
                        &dependency.span,
                    ))
                }
                _ => UnitMemberRaw::Dependency(UnitDependencyRaw::from(dependency)),
            },
            UnitTypeMember::Conversion(block) => {
                UnitMemberRaw::Conversion(UnitConversionRaw::from(block))
            }
            UnitTypeMember::NestedStatement(statement) => {
                UnitMemberRaw::NestedStatement(statement.clone())
            }
        }
    }
}

/// 単位依存関係。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitDependencyRaw {
    pub symbol: UnitSymbolRaw,
    pub relation: UnitRelation,
    pub value: Option<Expression>,
    pub target: Option<String>,
    pub span: Span,
}

impl From<&UnitDependency> for UnitDependencyRaw {
    fn from(dependency: &UnitDependency) -> Self {
        Self {
            symbol: UnitSymbolRaw::from_identifier(&dependency.name, &dependency.span),
            relation: dependency.relation,
            value: dependency.value.clone(),
            target: dependency.target.clone(),
            span: dependency.span.clone(),
        }
    }
}

/// 変換ブロック情報。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitConversionRaw {
    pub kind: UnitConversionKind,
    pub body: Vec<Statement>,
    pub span: Span,
}

impl From<&UnitConversionBlock> for UnitConversionRaw {
    fn from(block: &UnitConversionBlock) -> Self {
        Self {
            kind: block.kind,
            body: block.body.clone(),
            span: block.span.clone(),
        }
    }
}

/// `@ConversionRate` 情報（将来拡張用）。
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnitRateRaw {
    pub expression: Expression,
    pub span: Span,
}
