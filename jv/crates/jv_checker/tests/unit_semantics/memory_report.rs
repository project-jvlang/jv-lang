use std::fs;
use std::mem;
use std::path::PathBuf;

use toml;

use jv_checker::semantics::units::{
    RawUnitCatalog, UnitCategoryEntry, UnitConversionBody, UnitDependencyGraphBuilder, UnitEntry,
    UnitEdge, UnitRegistry, UnitSchemaValidator,
};

fn catalog_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/unit_semantics/data/catalog_200.toml")
}

fn load_catalog() -> RawUnitCatalog {
    let contents =
        fs::read_to_string(catalog_path()).expect("catalog fixture must be readable");
    toml::from_str(&contents).expect("fixture must parse into RawUnitCatalog")
}

#[test]
#[ignore]
fn memory_report() {
    let catalog = load_catalog();
    let mut diagnostics = Vec::new();
    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(
        diagnostics.is_empty(),
        "catalog validation should be clean: {diagnostics:?}"
    );
    let registry = UnitDependencyGraphBuilder::build(validated, &mut diagnostics)
        .expect("registry should build");
    assert!(diagnostics.is_empty());

    let registry = registry.as_ref();
    let total_bytes = mem::size_of::<UnitRegistry>()
        + registry.categories().len() * mem::size_of::<UnitCategoryEntry>()
        + registry.units().len() * mem::size_of::<UnitEntry>()
        + registry.edges().len() * mem::size_of::<UnitEdge>()
        + registry.conversions().len() * mem::size_of::<UnitConversionBody>();
    let per_unit_bytes = total_bytes as f64 / registry.units().len() as f64;
    println!(
        "estimated per-unit bytes: {:.2} (total entries: {}; total estimated bytes: {})",
        per_unit_bytes,
        registry.units().len(),
        total_bytes
    );
    assert!(
        per_unit_bytes < 1_000.0,
        "per-unit memory {} exceeds threshold",
        per_unit_bytes
    );
}
