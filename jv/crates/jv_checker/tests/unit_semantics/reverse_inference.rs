use jv_ast::{
    BinaryOp, Expression, Literal, Parameter, ParameterModifiers, Span, Statement, TypeAnnotation,
    UnitConversionKind, UnitRelation,
};
use jv_checker::semantics::units::{
    RawUnitCatalog, ReverseMode, UnitConversionRaw, UnitDefinitionRaw, UnitDependencyGraphBuilder,
    UnitDependencyRaw, UnitMemberRaw, UnitSchemaValidator, UnitSymbolRaw,
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

fn definition(members: Vec<UnitMemberRaw>) -> UnitDefinitionRaw {
    UnitDefinitionRaw {
        span: span(),
        category: "Currency".to_string(),
        base_type: TypeAnnotation::Simple("Decimal".to_string()),
        symbol: default_symbol("USD"),
        members,
    }
}

fn dependency_arrow(from: &str, to: &str) -> UnitMemberRaw {
    UnitMemberRaw::Dependency(UnitDependencyRaw {
        symbol: symbol(from),
        relation: UnitRelation::ConversionArrow,
        value: None,
        target: Some(to.to_string()),
        span: span(),
    })
}

fn lambda(body: Expression) -> Expression {
    Expression::Lambda {
        parameters: vec![Parameter {
            name: "value".to_string(),
            type_annotation: Some(TypeAnnotation::Simple("Decimal".to_string())),
            default_value: None,
            modifiers: ParameterModifiers::default(),
            span: span(),
        }],
        body: Box::new(body),
        span: span(),
    }
}

fn conversion_block(kind: UnitConversionKind, expr: Expression) -> UnitMemberRaw {
    UnitMemberRaw::Conversion(UnitConversionRaw {
        kind,
        body: vec![Statement::Expression { expr, span: span() }],
        span: span(),
    })
}

#[test]
fn warns_when_auto_reverse_is_impossible() {
    let body = Expression::Binary {
        left: Box::new(Expression::Identifier("value".to_string(), span())),
        op: BinaryOp::Multiply,
        right: Box::new(Expression::Identifier("rate".to_string(), span())),
        span: span(),
    };
    let members = vec![
        UnitMemberRaw::SymbolDecl(symbol("EUR")),
        dependency_arrow("USD", "EUR"),
        conversion_block(UnitConversionKind::Conversion, lambda(body)),
    ];
    let catalog = RawUnitCatalog::new(vec![definition(members)]);
    let mut diagnostics = Vec::new();

    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(
        diagnostics.is_empty(),
        "schema diagnostics: {diagnostics:?}"
    );

    let registry =
        UnitDependencyGraphBuilder::build(validated, &mut diagnostics).expect("build succeeds");
    assert!(
        diagnostics.iter().any(|error| matches!(
            error,
            jv_checker::CheckError::ValidationError { message, .. }
            if message.contains("JV_UNIT_SEM_050")
        )),
        "expected JV_UNIT_SEM_050 warning, got {diagnostics:?}"
    );

    let edge = &registry.edges()[0];
    assert!(matches!(edge.reverse_mode, ReverseMode::Unavailable));
}

#[test]
fn reverse_block_marks_provided() {
    let forward_body = Expression::Binary {
        left: Box::new(Expression::Identifier("value".to_string(), span())),
        op: BinaryOp::Add,
        right: Box::new(Expression::Literal(
            Literal::Number("10".to_string()),
            span(),
        )),
        span: span(),
    };
    let reverse_body = Expression::Binary {
        left: Box::new(Expression::Identifier("value".to_string(), span())),
        op: BinaryOp::Subtract,
        right: Box::new(Expression::Literal(
            Literal::Number("10".to_string()),
            span(),
        )),
        span: span(),
    };
    let members = vec![
        UnitMemberRaw::SymbolDecl(symbol("EUR")),
        dependency_arrow("USD", "EUR"),
        conversion_block(UnitConversionKind::Conversion, lambda(forward_body)),
        conversion_block(UnitConversionKind::ReverseConversion, lambda(reverse_body)),
    ];
    let catalog = RawUnitCatalog::new(vec![definition(members)]);
    let mut diagnostics = Vec::new();

    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(
        diagnostics.is_empty(),
        "schema diagnostics: {diagnostics:?}"
    );

    let registry =
        UnitDependencyGraphBuilder::build(validated, &mut diagnostics).expect("build succeeds");
    assert!(
        diagnostics.iter().all(|error| !matches!(error,
            jv_checker::CheckError::ValidationError { message, .. }
            if message.contains("JV_UNIT_SEM_050")
        )),
        "unexpected JV_UNIT_SEM_050 warnings: {diagnostics:?}"
    );

    let edge = &registry.edges()[0];
    assert!(matches!(edge.reverse_mode, ReverseMode::Provided));
}
