use jv_ast::{Literal, Span};
use jv_codegen_java::generate_java_source;
use jv_ir::{
    ConversionKind, ConversionMetadata, ConversionMetadataEntry, IrExpression, IrProgram,
    IrStatement, JavaType, NullableGuard, NullableGuardReason,
};

fn span(line: usize) -> Span {
    Span::new(line, 0, line, 10)
}

#[test]
fn boxing_conversion_wraps_literal_expression() {
    let expr_span = span(1);
    let expression = IrExpression::Literal(Literal::Number("1".to_string()), expr_span.clone());
    let statement = IrStatement::Expression {
        expr: expression,
        span: expr_span.clone(),
    };

    let mut program = IrProgram {
        package: None,
        imports: Vec::new(),
        type_declarations: vec![statement],
        generic_metadata: Default::default(),
        conversion_metadata: vec![ConversionMetadataEntry {
            span: expr_span.clone(),
            metadata: ConversionMetadata::new(ConversionKind::Boxing, "int", "java.lang.Integer"),
        }],
        span: expr_span,
    };

    let source = generate_java_source(&program).expect("java source generation");
    assert!(source.contains("Integer.valueOf(1);") || source.contains("Integer.valueOf(1)"));
}

#[test]
fn unboxing_conversion_injects_nullable_guard() {
    let expr_span = span(2);
    let expression = IrExpression::Identifier {
        name: "boxed".to_string(),
        java_type: JavaType::Reference {
            name: "java.lang.Integer".to_string(),
            generic_args: Vec::new(),
        },
        span: expr_span.clone(),
    };
    let statement = IrStatement::Expression {
        expr: expression,
        span: expr_span.clone(),
    };

    let metadata = ConversionMetadata::new(ConversionKind::Unboxing, "java.lang.Integer", "int")
        .with_nullable_guard(NullableGuard::new(NullableGuardReason::Unboxing));

    let program = IrProgram {
        package: None,
        imports: Vec::new(),
        type_declarations: vec![statement],
        generic_metadata: Default::default(),
        conversion_metadata: vec![ConversionMetadataEntry {
            span: expr_span.clone(),
            metadata,
        }],
        span: expr_span,
    };

    let source = generate_java_source(&program).expect("java source generation");
    assert!(source.contains("Objects.requireNonNull(boxed).intValue();"));
    assert!(source.contains("import java.util.Objects;"));
}
