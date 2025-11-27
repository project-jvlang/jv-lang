use std::fs;
use std::path::PathBuf;
use std::time::Instant;

use toml;

use jv_checker::semantics::units::{
    RawUnitCatalog, UnitDependencyGraphBuilder, UnitSchemaValidator,
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
fn catalog_perf() {
    let catalog = load_catalog();
    let mut diagnostics = Vec::new();
    let start = Instant::now();
    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(
        diagnostics.is_empty(),
        "catalog validation produced diagnostics: {diagnostics:?}"
    );
    let registry =
        UnitDependencyGraphBuilder::build(validated, &mut diagnostics).expect("registry builds");
    let elapsed = start.elapsed();
    eprintln!(
        "unit registry build with {} entries took {:?}",
        registry.units().len(),
        elapsed
    );
    assert!(elapsed.as_millis() < 1_000, "registry build is too slow: {:?}", elapsed);
}
