use jv_ast::{
    BinaryOp, Expression, Literal, Parameter, ParameterModifiers, Span, Statement, TypeAnnotation,
    UnitConversionKind, UnitRelation,
};
use jv_checker::semantics::units::{
    RawUnitCatalog, ReverseMode, UnitConversionRaw, UnitDefinitionRaw, UnitDependencyGraphBuilder,
    UnitDependencyRaw, UnitMemberRaw, UnitRateRaw, UnitSchemaValidator, UnitSymbolRaw,
};
use rust_decimal::Decimal;
use std::str::FromStr;

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

fn conversion_block(lambda: Expression, kind: UnitConversionKind) -> UnitMemberRaw {
    let statement = Statement::Expression {
        expr: lambda,
        span: span(),
    };
    UnitMemberRaw::Conversion(UnitConversionRaw {
        kind,
        body: vec![statement],
        span: span(),
    })
}

fn conversion_rate(expr: Expression) -> UnitMemberRaw {
    UnitMemberRaw::ConversionRate(UnitRateRaw {
        expression: expr,
        span: span(),
    })
}

fn lambda_with_param(param_type: TypeAnnotation, body: Expression) -> Expression {
    Expression::Lambda {
        parameters: vec![Parameter {
            name: "value".to_string(),
            type_annotation: Some(param_type),
            default_value: None,
            modifiers: ParameterModifiers::default(),
            span: span(),
        }],
        body: Box::new(body),
        span: span(),
    }
}

fn multiply_lambda(factor: &str) -> Expression {
    let body = Expression::Binary {
        left: Box::new(Expression::Identifier("value".to_string(), span())),
        op: BinaryOp::Multiply,
        right: Box::new(Expression::Literal(
            Literal::Number(factor.to_string()),
            span(),
        )),
        span: span(),
    };
    lambda_with_param(TypeAnnotation::Simple("Decimal".to_string()), body)
}

fn literal_number(value: &str) -> Expression {
    Expression::Literal(Literal::Number(value.to_string()), span())
}

#[test]
fn attaches_conversion_ir_and_rate() {
    let members = vec![
        UnitMemberRaw::SymbolDecl(symbol("EUR")),
        dependency_arrow("USD", "EUR"),
        conversion_block(multiply_lambda("1.25"), UnitConversionKind::Conversion),
        conversion_rate(literal_number("1.25")),
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
    assert!(diagnostics.is_empty(), "graph diagnostics: {diagnostics:?}");

    assert_eq!(registry.edges().len(), 1);
    let edge = &registry.edges()[0];
    assert_eq!(
        edge.rate,
        Some(Decimal::from_str("1.25").expect("decimal literal"))
    );
    assert!(matches!(
        edge.reverse_mode,
        ReverseMode::Auto { scale, offset }
        if scale == Decimal::from_str("0.8").unwrap() && offset.is_zero()
    ));

    let conversion_ref = edge.conversion_ref.expect("conversion ref");
    let body = &registry.conversions()[conversion_ref.0 as usize];
    assert_eq!(body.edge, edge.id);
}

#[test]
fn reports_signature_mismatch() {
    let body = Expression::Identifier("value".to_string(), span());
    let lambda = lambda_with_param(TypeAnnotation::Simple("String".to_string()), body);
    let members = vec![
        UnitMemberRaw::SymbolDecl(symbol("EUR")),
        dependency_arrow("USD", "EUR"),
        conversion_block(lambda, UnitConversionKind::Conversion),
    ];
    let catalog = RawUnitCatalog::new(vec![definition(members)]);
    let mut diagnostics = Vec::new();

    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(
        diagnostics.is_empty(),
        "schema diagnostics: {diagnostics:?}"
    );

    let registry = UnitDependencyGraphBuilder::build(validated, &mut diagnostics);
    assert!(registry.is_none(), "conversion mismatch should fail");
    assert!(
        diagnostics.iter().any(|error| matches!(
            error,
            jv_checker::CheckError::ValidationError { message, .. }
            if message.contains("JV_UNIT_SEM_040")
        )),
        "expected JV_UNIT_SEM_040, got {diagnostics:?}"
    );
}

#[test]
fn rejects_non_numeric_conversion_rate() {
    let members = vec![
        UnitMemberRaw::SymbolDecl(symbol("EUR")),
        dependency_arrow("USD", "EUR"),
        conversion_block(multiply_lambda("1.0"), UnitConversionKind::Conversion),
        conversion_rate(Expression::Identifier("RATE".to_string(), span())),
    ];
    let catalog = RawUnitCatalog::new(vec![definition(members)]);
    let mut diagnostics = Vec::new();

    let validated = UnitSchemaValidator::validate(catalog, &mut diagnostics);
    assert!(
        diagnostics.is_empty(),
        "schema diagnostics: {diagnostics:?}"
    );

    let registry = UnitDependencyGraphBuilder::build(validated, &mut diagnostics);
    assert!(registry.is_none(), "invalid rate should fail");
    assert!(
        diagnostics.iter().any(|error| matches!(
            error,
            jv_checker::CheckError::ValidationError { message, .. }
            if message.contains("JV_UNIT_SEM_041")
        )),
        "expected JV_UNIT_SEM_041, got {diagnostics:?}"
    );
}
