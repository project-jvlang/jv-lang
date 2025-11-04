use jv_checker::semantics::units::{UnitCatalogCollector, UnitMemberRaw};
use jv_ast::{
    comments::{CommentKind, CommentStatement, CommentVisibility},
    statement::{
        UnitConversionBlock, UnitConversionKind, UnitDependency, UnitRelation, UnitTypeDefinition,
        UnitTypeMember,
    },
    Expression, Literal, Program, Span, Statement, TypeAnnotation, UnitSymbol,
};

#[test]
fn collects_unit_definitions_into_raw_catalog() {
    let span = Span::new(1, 0, 1, 10);
    let base_symbol = UnitSymbol {
        name: "USD".to_string(),
        is_bracketed: false,
        has_default_marker: true,
        span: span.clone(),
    };

    let symbol_decl = UnitDependency {
        name: "[EUR]!".to_string(),
        relation: UnitRelation::DefinitionAssign,
        value: None,
        target: None,
        span: span.clone(),
    };

    let milli_relation = UnitDependency {
        name: "JPY".to_string(),
        relation: UnitRelation::DefinitionAssign,
        value: Some(Expression::Literal(
            Literal::Number("110.0".to_string()),
            span.clone(),
        )),
        target: None,
        span: span.clone(),
    };

    let conversion = UnitConversionBlock {
        kind: UnitConversionKind::Conversion,
        body: vec![Statement::Expression {
            expr: Expression::Identifier("value".to_string(), span.clone()),
            span: span.clone(),
        }],
        span: span.clone(),
    };

    let nested_comment = Statement::Comment(CommentStatement {
        kind: CommentKind::Line,
        visibility: CommentVisibility::JvOnly,
        text: "nested".to_string(),
        span: span.clone(),
    });

    let primary_definition = UnitTypeDefinition {
        category: "Currency".to_string(),
        base_type: TypeAnnotation::Simple("Decimal".to_string()),
        name: base_symbol,
        members: vec![
            UnitTypeMember::Dependency(symbol_decl),
            UnitTypeMember::Dependency(milli_relation),
            UnitTypeMember::Conversion(conversion),
            UnitTypeMember::NestedStatement(Box::new(nested_comment.clone())),
        ],
        span: span.clone(),
    };

    let secondary_definition = UnitTypeDefinition {
        category: "Encoding".to_string(),
        base_type: TypeAnnotation::Simple("ByteArray".to_string()),
        name: UnitSymbol {
            name: "UTF8".to_string(),
            is_bracketed: false,
            has_default_marker: false,
            span: span.clone(),
        },
        members: Vec::new(),
        span: span.clone(),
    };

    let program = Program {
        package: None,
        imports: vec![],
        statements: vec![
            Statement::UnitTypeDefinition(primary_definition),
            Statement::UnitTypeDefinition(secondary_definition),
        ],
        span: span.clone(),
    };

    let catalog = UnitCatalogCollector::collect(&program);
    assert_eq!(catalog.definitions.len(), 2);

    let currency = &catalog.definitions[0];
    assert_eq!(currency.category, "Currency");
    assert!(currency.symbol.has_default_marker);
    assert_eq!(currency.members.len(), 4);

    match &currency.members[0] {
        UnitMemberRaw::SymbolDecl(symbol) => {
            assert!(symbol.is_bracketed);
            assert!(symbol.has_default_marker);
            assert_eq!(symbol.name, "EUR");
        }
        other => panic!("expected symbol declaration, got {:?}", other),
    }

    match &currency.members[1] {
        UnitMemberRaw::Dependency(dependency) => {
            assert_eq!(dependency.symbol.name, "JPY");
            assert!(dependency.value.is_some());
        }
        other => panic!("expected dependency, got {:?}", other),
    }

    match &currency.members[2] {
        UnitMemberRaw::Conversion(block) => {
            assert_eq!(block.body.len(), 1);
        }
        other => panic!("expected conversion block, got {:?}", other),
    }

    match &currency.members[3] {
        UnitMemberRaw::NestedStatement(statement) => {
            if let Statement::Comment(comment) = &**statement {
                assert_eq!(comment.text, "nested");
            } else {
                panic!("unexpected nested statement kind");
            }
        }
        other => panic!("expected nested statement, got {:?}", other),
    }
}
