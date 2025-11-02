use jv_ast::{Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_ir::{
    ConversionHelper, ConversionKind, ConversionMetadata, ConversionMetadataEntry, IrExpression,
    IrProgram, IrStatement, JavaType, NullableGuard, NullableGuardReason,
};
use jv_mapper::SourceMap;

fn span(line: usize) -> Span {
    Span::new(line, 0, line, 10)
}

fn generate_source_and_map(program: &IrProgram) -> (String, SourceMap) {
    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(program)
        .expect("java code generation");
    let config = JavaCodeGenConfig::default();
    let source = unit.to_source(&config);
    let source_map = generator
        .build_conversion_source_map(&source, "test.jv", "Test.java")
        .expect("source map generation");
    (source, source_map)
}

#[test]
fn boxing_conversion_wraps_literal_expression() {
    let expr_span = span(1);
    let expression = IrExpression::Literal(Literal::Number("1".to_string()), expr_span.clone());
    let statement = IrStatement::Expression {
        expr: expression,
        span: expr_span.clone(),
    };

    let program = IrProgram {
        package: None,
        imports: Vec::new(),
        type_declarations: vec![statement],
        generic_metadata: Default::default(),
        conversion_metadata: vec![ConversionMetadataEntry {
            span: expr_span.clone(),
            metadata: ConversionMetadata::new(ConversionKind::Boxing, "int", "java.lang.Integer"),
        }],
        logging: Default::default(),
        span: expr_span.clone(),
    };

    let (source, source_map) = generate_source_and_map(&program);
    assert!(source.contains("Integer.valueOf(1);") || source.contains("Integer.valueOf(1)"));

    assert_eq!(source_map.entries.len(), 1);
    let entry = &source_map.entries[0];
    assert_eq!(entry.ir_span, expr_span);
    let conversions = entry
        .conversions
        .as_ref()
        .expect("conversion metadata associated with source map entry");
    assert_eq!(conversions.len(), 1);
    let metadata = &conversions[0];
    assert_eq!(metadata.kind, ConversionKind::Boxing);
    assert_eq!(metadata.from_type, "int");
    assert_eq!(metadata.to_type, "java.lang.Integer");
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
        logging: Default::default(),
        span: expr_span,
    };

    let (source, source_map) = generate_source_and_map(&program);
    assert!(
        source.contains("Objects.requireNonNull(boxed)") && source.contains(".intValue()"),
        "Expected nullable guard with unboxing, got: {}",
        source
    );
    assert!(source.contains("import java.util.Objects;"));

    assert_eq!(source_map.entries.len(), 1);
    let entry = &source_map.entries[0];
    let conversions = entry
        .conversions
        .as_ref()
        .expect("conversion metadata associated with source map entry");
    assert_eq!(conversions.len(), 1);
    let metadata = &conversions[0];
    assert_eq!(metadata.kind, ConversionKind::Unboxing);
    assert_eq!(metadata.from_type, "java.lang.Integer");
    assert_eq!(metadata.to_type, "int");
    assert!(metadata.nullable_guard.is_some());
}

#[test]
fn method_invocation_conversion_wraps_collection_transform() {
    let expr_span = span(3);
    let expression = IrExpression::Identifier {
        name: "values".to_string(),
        java_type: JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: Vec::new(),
        },
        span: expr_span.clone(),
    };
    let statement = IrStatement::Expression {
        expr: expression,
        span: expr_span.clone(),
    };

    let metadata = ConversionMetadata::new(
        ConversionKind::MethodInvocation,
        "java.util.List",
        "java.util.stream.Stream",
    )
    .with_helper(ConversionHelper {
        owner: "java.util.List".to_string(),
        method: "stream".to_string(),
        is_static: false,
    });

    let program = IrProgram {
        package: None,
        imports: Vec::new(),
        type_declarations: vec![statement],
        generic_metadata: Default::default(),
        conversion_metadata: vec![ConversionMetadataEntry {
            span: expr_span.clone(),
            metadata,
        }],
        logging: Default::default(),
        span: expr_span.clone(),
    };

    let (source, source_map) = generate_source_and_map(&program);
    assert!(
        source.contains("(values).stream();"),
        "collection transform should invoke stream() helper"
    );

    assert_eq!(source_map.entries.len(), 1);
    let entry = &source_map.entries[0];
    let conversions = entry
        .conversions
        .as_ref()
        .expect("conversion metadata should be recorded in source map");
    assert_eq!(conversions.len(), 1);
    let metadata = &conversions[0];
    assert_eq!(metadata.kind, ConversionKind::MethodInvocation);
    let helper = metadata
        .helper
        .as_ref()
        .expect("helper metadata should be preserved");
    assert_eq!(helper.method, "stream");
    assert!(!helper.is_static);
}
