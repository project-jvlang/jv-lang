use jv_ast::{Expression, Span, TypeAnnotation, UnitRelation};
use jv_checker::semantics::units::{
    UnitDependencyGraphBuilder, UnitSchemaValidator, RawUnitCatalog, UnitDefinitionRaw,
    UnitDependencyRaw, UnitMemberRaw, UnitSymbolRaw,
};

fn span() -> Span {
    Span::new(1, 0, 1, 1)
}

fn symbol(name: &str) -> UnitSymbolRaw {
    UnitSymbolRaw {
        name: name.to_string(),
        is_bracketed: false,
        has_default_marker: false,
        span: span(),
    }
}

fn default_symbol(name: &str) -> UnitSymbolRaw {
    UnitSymbolRaw {
        has_default_marker: true,
        ..symbol(name)
    }
}

fn definition(
    category: &str,
    base_type: TypeAnnotation,
    symbol: UnitSymbolRaw,
    members: Vec<UnitMemberRaw>,
) -> UnitDefinitionRaw {
    UnitDefinitionRaw {
        span: span(),
        category: category.to_string(),
        base_type,
        symbol,
        members,
    }
}

fn dependency_assign(symbol: &str, reference: &str) -> UnitMemberRaw {
    UnitMemberRaw::Dependency(UnitDependencyRaw {
        symbol: symbol(symbol),
        relation: UnitRelation::DefinitionAssign,
        value: Some(Expression::Identifier(reference.to_string(), span())),
        target: None,
        span: span(),
    })
}

fn dependency_arrow(symbol: &str, target: &str) -> UnitMemberRaw {
    UnitMemberRaw::Dependency(UnitDependencyRaw {
        symbol: symbol(symbol),
        relation: UnitRelation::ConversionArrow,
        value: None,
        target: Some(target.to_string()),
        span: span(),
    })
}

#[test]
fn builds_registry_and_summary() {
    let usd_definition = definition(
        "Currency",
        TypeAnnotation::Simple("Decimal".to_string()),
        default_symbol("USD"),
        vec![
            UnitMemberRaw::SymbolDecl(symbol("EUR")),
            UnitMemberRaw::SymbolDecl(symbol("JPY")),
            dependency_assign("EUR", "USD"),
            dependency_assign("JPY", "EUR"),
        ],
    );

    let catalog = RawUnitCatalog::new(vec![usd_definition]);
    let mut diagnostics = Vec::new();
    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(diagnostics.is_empty(), "schema diagnostics: {diagnostics:?}");

    let registry = UnitDependencyGraphBuilder::build(validated, &mut diagnostics)
        .expect("registry should build without errors");
    assert!(diagnostics.is_empty(), "graph diagnostics: {diagnostics:?}");

    assert_eq!(registry.units().len(), 3);
    assert_eq!(registry.edges().len(), 2);

    let euro_id = registry.resolve("Currency", "EUR").expect("EUR registered");
    let usd_id = registry.resolve("Currency", "USD").expect("USD registered");
    let jpy_id = registry.resolve("Currency", "JPY").expect("JPY registered");

    let mut euro_edges = registry
        .edges()
        .iter()
        .filter(|edge| edge.from == euro_id)
        .collect::<Vec<_>>();
    assert_eq!(euro_edges.len(), 1);
    assert_eq!(euro_edges[0].to, usd_id);

    let summary = registry.to_summary();
    assert_eq!(summary.categories.len(), 1);
    assert_eq!(summary.units.len(), 3);
    assert_eq!(summary.edges.len(), 2);
    assert_eq!(summary.categories[0].default_unit, Some(usd_id));
    assert!(summary.edges.iter().all(|edge| edge.rate.is_none()));
    assert!(summary.conversions.is_empty());

    let jpy_edge = registry
        .edges()
        .iter()
        .find(|edge| edge.from == jpy_id)
        .expect("JPY edge exists");
    assert_eq!(jpy_edge.to, euro_id);
}

#[test]
fn reports_unknown_dependency() {
    let usd_definition = definition(
        "Currency",
        TypeAnnotation::Simple("Decimal".to_string()),
        default_symbol("USD"),
        vec![dependency_arrow("USD", "AUD")],
    );

    let catalog = RawUnitCatalog::new(vec![usd_definition]);
    let mut diagnostics = Vec::new();
    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(diagnostics.is_empty(), "schema diagnostics: {diagnostics:?}");

    let registry = UnitDependencyGraphBuilder::build(validated, &mut diagnostics);
    assert!(registry.is_none(), "registry should fail to build");
    assert!(
        diagnostics.iter().any(|error| matches!(
            error,
            jv_checker::CheckError::ValidationError { message, .. }
                if message.contains(\"JV_UNIT_SEM_020\")
        )),
        "expected JV_UNIT_SEM_020 diagnostics, got {diagnostics:?}"
    );
}

#[test]
fn detects_cycles() {
    let definition = definition(
        "Currency",
        TypeAnnotation::Simple("Decimal".to_string()),
        default_symbol("USD"),
        vec![
            UnitMemberRaw::SymbolDecl(symbol("EUR")),
            dependency_assign("EUR", "USD"),
            dependency_assign("USD", "EUR"),
        ],
    );

    let catalog = RawUnitCatalog::new(vec![definition]);
    let mut diagnostics = Vec::new();
    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(diagnostics.is_empty(), "schema diagnostics: {diagnostics:?}");

    let registry = UnitDependencyGraphBuilder::build(validated, &mut diagnostics);
    assert!(registry.is_none(), "registry should fail with cycle");
    assert!(
        diagnostics.iter().any(|error| matches!(
            error,
            jv_checker::CheckError::ValidationError { message, .. }
                if message.contains(\"JV_UNIT_SEM_030\")
        )),
        "expected JV_UNIT_SEM_030 diagnostics, got {diagnostics:?}"
    );
}
