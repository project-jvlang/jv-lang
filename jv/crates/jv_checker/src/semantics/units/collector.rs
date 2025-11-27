use super::{RawUnitCatalog, UnitDefinitionRaw};
use jv_ast::{Program, Statement};

/// ASTから単位定義を抽出するコレクタ。
#[derive(Debug, Default)]
pub struct UnitCatalogCollector;

impl UnitCatalogCollector {
    /// プログラム全体から単位定義を収集する。
    pub fn collect(program: &Program) -> RawUnitCatalog {
        let definitions = program
            .statements
            .iter()
            .filter_map(|statement| match statement {
                Statement::UnitTypeDefinition(definition) => {
                    Some(UnitDefinitionRaw::from(definition))
                }
                _ => None,
            })
            .collect();

        RawUnitCatalog::new(definitions)
    }

    /// 既に抽出済みの定義列からカタログを構築する補助関数。
    pub fn from_definitions(
        definitions: impl IntoIterator<Item = UnitDefinitionRaw>,
    ) -> RawUnitCatalog {
        RawUnitCatalog::new(definitions.into_iter().collect())
    }
}
