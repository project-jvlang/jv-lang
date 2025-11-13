use crate::CheckError;
use crate::semantics::units::{
    UnitCatalogCollector, UnitDependencyGraphBuilder, UnitRegistry, UnitSchemaValidator,
};
use jv_ast::Program;
use std::sync::Arc;

/// Result of running the unit registry construction pipeline.
#[derive(Debug)]
pub struct RegistryBuildResult {
    pub registry: Option<Arc<UnitRegistry>>,
    pub diagnostics: Vec<CheckError>,
    pub had_error: bool,
}

/// Collects, validates, and builds the unit registry from the provided program.
pub fn build_registry(program: &Program) -> RegistryBuildResult {
    let mut diagnostics = Vec::new();
    let catalog = UnitCatalogCollector::collect(program);
    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);

    if validated.had_error {
        return RegistryBuildResult {
            registry: None,
            diagnostics,
            had_error: true,
        };
    }

    let registry = UnitDependencyGraphBuilder::build(validated, &mut diagnostics);
    let had_error = registry.is_none();
    RegistryBuildResult {
        registry,
        diagnostics,
        had_error,
    }
}
