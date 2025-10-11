use std::collections::BTreeMap;

use jv_ast::types::Kind;
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{IrGenericMetadata, IrProgram, IrStatement, IrTypeLevelValue, IrTypeParameter};

fn build_program() -> IrProgram {
    let span = jv_ast::Span::dummy();

    let mut type_param = IrTypeParameter::new("T", span.clone());
    type_param.kind = Some(Kind::Star);

    let class = IrStatement::ClassDeclaration {
        name: "Reflective".to_string(),
        type_parameters: vec![type_param],
        superclass: None,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers: Default::default(),
        span: span.clone(),
    };

    let mut metadata_entry = IrGenericMetadata::default();
    metadata_entry
        .type_parameter_kinds
        .insert("T".to_string(), Kind::Star);
    metadata_entry
        .const_parameter_values
        .insert("SIZE".to_string(), IrTypeLevelValue::Int(32));
    metadata_entry.type_level_bindings.insert(
        "dimension".to_string(),
        IrTypeLevelValue::String("3D".to_string()),
    );

    let mut metadata = BTreeMap::new();
    metadata.insert("fixtures::Reflective".to_string(), metadata_entry);

    IrProgram {
        package: Some("fixtures".to_string()),
        imports: Vec::new(),
        type_declarations: vec![class],
        generic_metadata: metadata,
        span,
    }
}

#[test]
fn reflective_metadata_matches_golden_java25() {
    let program = build_program();
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("java generation succeeds");

    let actual = unit
        .type_declarations
        .get(0)
        .expect("generated class present")
        .trim();
    let expected = include_str!("golden/expected/advanced_generics_metadata__java25.java").trim();

    assert_eq!(actual, expected);
}
