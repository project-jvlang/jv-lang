use jv_ast::{Span, TypeAnnotation};
use jv_checker::semantics::units::{
    BaseTypeCapability, NumericCapability, RawUnitCatalog, UnitDefinitionRaw, UnitMemberRaw,
    UnitSymbolRaw, UnitSchemaValidator,
};

fn span() -> Span {
    Span::new(1, 0, 1, 1)
}

fn default_symbol(name: &str) -> UnitSymbolRaw {
    UnitSymbolRaw {
        name: name.to_string(),
        is_bracketed: false,
        has_default_marker: true,
        span: span(),
    }
}

fn plain_symbol(name: &str) -> UnitSymbolRaw {
    UnitSymbolRaw {
        name: name.to_string(),
        is_bracketed: false,
        has_default_marker: false,
        span: span(),
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

#[test]
fn validates_currency_decimal_base_type() {
    let definition = definition(
        "Currency",
        TypeAnnotation::Simple("Decimal".to_string()),
        default_symbol("USD"),
        vec![],
    );
    let catalog = RawUnitCatalog::new(vec![definition]);
    let mut diagnostics = Vec::new();

    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);

    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:?}");
    assert_eq!(validated.definitions.len(), 1);
    let entry = &validated.definitions[0];
    assert_eq!(entry.base_capability, BaseTypeCapability::Numeric(NumericCapability::Decimal));
    assert!(validated.defaults.contains_key("Currency"));
}

#[test]
fn reports_unknown_category() {
    let definition = definition(
        "Distance",
        TypeAnnotation::Simple("Int".to_string()),
        default_symbol("M"),
        vec![],
    );
    let catalog = RawUnitCatalog::new(vec![definition]);
    let mut diagnostics = Vec::new();

    UnitSchemaValidator::validate(catalog, &mut diagnostics);

    assert!(
        diagnostics
            .iter()
            .any(|error| matches!(error,
                jv_checker::CheckError::ValidationError { message, .. }
                if message.contains("JV_UNIT_SEM_001")
            )),
        "expected JV_UNIT_SEM_001, got {diagnostics:?}",
    );
}

#[test]
fn custom_category_emits_warning() {
    let definition = definition(
        "Custom",
        TypeAnnotation::Simple("String".to_string()),
        default_symbol("CODE"),
        vec![],
    );
    let catalog = RawUnitCatalog::new(vec![definition]);
    let mut diagnostics = Vec::new();

    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);

    assert_eq!(validated.definitions.len(), 1);
    assert!(
        diagnostics
            .iter()
            .any(|error| matches!(error,
                jv_checker::CheckError::ValidationError { message, .. }
                if message.contains("JV_UNIT_SEM_005")
            )),
        "expected JV_UNIT_SEM_005 warning, got {diagnostics:?}",
    );
}

#[test]
fn rejects_incompatible_base_type() {
    let definition = definition(
        "Currency",
        TypeAnnotation::Simple("Boolean".to_string()),
        default_symbol("FLAG"),
        vec![],
    );
    let catalog = RawUnitCatalog::new(vec![definition]);
    let mut diagnostics = Vec::new();

    UnitSchemaValidator::validate(catalog, &mut diagnostics);

    assert!(
        diagnostics
            .iter()
            .any(|error| matches!(error,
                jv_checker::CheckError::ValidationError { message, .. }
                if message.contains("JV_UNIT_SEM_002")
            )),
        "expected JV_UNIT_SEM_002, got {diagnostics:?}",
    );
}

#[test]
fn detects_duplicate_default_markers() {
    let members = vec![UnitMemberRaw::SymbolDecl(default_symbol("EUR"))];
    let definition = definition(
        "Currency",
        TypeAnnotation::Simple("Int".to_string()),
        default_symbol("USD"),
        members,
    );
    let catalog = RawUnitCatalog::new(vec![definition]);
    let mut diagnostics = Vec::new();

    UnitSchemaValidator::validate(catalog, &mut diagnostics);

    assert!(
        diagnostics
            .iter()
            .any(|error| matches!(error,
                jv_checker::CheckError::ValidationError { message, .. }
                if message.contains("JV_UNIT_SEM_010")
            )),
        "expected JV_UNIT_SEM_010, got {diagnostics:?}",
    );
}

#[test]
fn warns_when_default_marker_is_missing() {
    let definition = definition(
        "Encoding",
        TypeAnnotation::Simple("String".to_string()),
        plain_symbol("UTF8"),
        vec![],
    );
    let catalog = RawUnitCatalog::new(vec![definition]);
    let mut diagnostics = Vec::new();

    UnitSchemaValidator::validate(catalog, &mut diagnostics);

    assert!(
        diagnostics
            .iter()
            .any(|error| matches!(error,
                jv_checker::CheckError::ValidationError { message, .. }
                if message.contains("JV_UNIT_SEM_011")
            )),
        "expected JV_UNIT_SEM_011, got {diagnostics:?}",
    );
}
