use jv_ast::Span;
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{IrModifiers, IrProgram, IrStatement, JavaType};

fn dummy_span() -> Span {
    Span::dummy()
}

#[test]
fn wildcard_arguments_are_rendered() {
    let mut generator = JavaCodeGenerator::new();
    let list_type = JavaType::Reference {
        name: "java.util.List".to_string(),
        generic_args: vec![JavaType::wildcard_extends(JavaType::Reference {
            name: "Number".to_string(),
            generic_args: Vec::new(),
        })],
    };

    let rendered = generator
        .generate_type(&list_type)
        .expect("type rendering succeeds");

    assert_eq!(rendered, "java.util.List<? extends Number>");
}

#[test]
fn sealed_classes_emit_target_specific_metadata() {
    let span = dummy_span();
    let class = IrStatement::ClassDeclaration {
        name: "Demo".to_string(),
        type_parameters: Vec::new(),
        superclass: None,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers: IrModifiers {
            is_sealed: true,
            permitted_types: vec!["demo.Foo".to_string()],
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo".to_string()),
        imports: Vec::new(),
        type_declarations: vec![class],
        span,
    };

    let mut java21_generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let unit21 = java21_generator
        .generate_compilation_unit(&program)
        .expect("java21 unit");
    let class_source21 = &unit21.type_declarations[0];
    assert!(
        class_source21.contains("// Fallback: sealed hierarchy permits [demo.Foo]"),
        "expected fallback comment for Java 21 output: {}",
        class_source21
    );

    let mut java25_generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let unit25 = java25_generator
        .generate_compilation_unit(&program)
        .expect("java25 unit");
    let class_source25 = &unit25.type_declarations[0];
    assert!(
        class_source25.contains("permits demo.Foo"),
        "expected permits clause for Java 25 output: {}",
        class_source25
    );
}
