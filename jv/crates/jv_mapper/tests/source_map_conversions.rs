use jv_ast::Span;
use jv_ir::{ConversionKind as IrConversionKind, ConversionMetadata};
use jv_mapper::{JavaPosition, JavaSpan, Mapper, MappingCategory, MappingEntry, SourceMap};

fn span() -> Span {
    Span::new(1, 0, 1, 5)
}

fn java_span() -> JavaSpan {
    JavaSpan::new(JavaPosition::new(2, 4), JavaPosition::new(2, 12)).unwrap()
}

#[test]
fn mapper_merges_conversion_metadata_and_returns_mappings() {
    let mut base = SourceMap::new("example.jv", "Example.java");
    base.entries.push(MappingEntry {
        ir_span: span(),
        java_span: java_span(),
        category: MappingCategory::Expression,
        ir_node: Some("IrExpression::Literal".to_string()),
        conversions: None,
    });

    let metadata = ConversionMetadata::new(IrConversionKind::Boxing, "int", "java.lang.Integer");

    let mut conversion_map = SourceMap::new("example.jv", "Example.java");
    conversion_map.entries.push(MappingEntry {
        ir_span: span(),
        java_span: java_span(),
        category: MappingCategory::Expression,
        ir_node: Some("IrExpression::Literal".to_string()),
        conversions: Some(vec![metadata.clone()]),
    });

    let mappings = Mapper::apply_conversions(&mut base, &conversion_map);
    assert_eq!(mappings.len(), 1);

    let entry = base.entries.first().expect("entry should exist");
    let conversions = entry
        .conversions
        .as_ref()
        .expect("conversion metadata should be attached");
    assert_eq!(conversions.len(), 1);
    assert_eq!(conversions[0], metadata);

    let mapping = &mappings[0];
    assert_eq!(mapping.source_file, "example.jv");
    assert_eq!(mapping.generated_file, "Example.java");
    assert_eq!(mapping.metadata, metadata);
}

#[test]
fn legacy_source_map_is_upgraded_to_v2() {
    let legacy = r#"
    {
        "version": 1,
        "source_file": "legacy.jv",
        "generated_file": "Legacy.java",
        "entries": [
            {
                "ir_span": {
                    "start_line": 1,
                    "start_column": 0,
                    "end_line": 1,
                    "end_column": 4
                },
                "java_span": {
                    "start": {"line": 1, "column": 0},
                    "end": {"line": 1, "column": 4}
                },
                "category": "expression"
            }
        ]
    }
    "#;

    let map = SourceMap::from_json(legacy).expect("legacy map should parse");
    assert_eq!(map.version, SourceMap::VERSION);
    assert!(map.entries[0].conversions.is_none());
    assert_eq!(map.source_file, "legacy.jv");
    assert_eq!(map.generated_file, "Legacy.java");
}
