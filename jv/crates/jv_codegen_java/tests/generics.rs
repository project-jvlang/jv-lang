use jv_ast::{
    types::{QualifiedName, RawTypeContinuation, RawTypeDirective},
    Span,
};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{IrModifiers, IrProgram, IrStatement, IrTypeParameter, IrVariance, JavaType};

fn dummy_span() -> Span {
    Span::dummy()
}

#[test]
fn wildcard_arguments_are_rendered() {
    let generator = JavaCodeGenerator::new();
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

#[test]
fn class_signature_includes_generic_bounds() {
    let span = dummy_span();
    let class = IrStatement::ClassDeclaration {
        name: "Demo".to_string(),
        type_parameters: vec![IrTypeParameter {
            name: "T".to_string(),
            bounds: vec![JavaType::Reference {
                name: "Comparable".to_string(),
                generic_args: Vec::new(),
            }],
            variance: IrVariance::Invariant,
            permits: Vec::new(),
            span: span.clone(),
        }],
        superclass: None,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo".to_string()),
        imports: Vec::new(),
        type_declarations: vec![class],
        span,
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("class generation");
    let class_source = &unit.type_declarations[0];

    assert!(
        class_source.contains("class Demo<T extends Comparable>"),
        "expected generic bounds in class signature: {}",
        class_source
    );
}

#[test]
fn covariant_type_arguments_render_wildcards() {
    let span = dummy_span();
    let class = IrStatement::ClassDeclaration {
        name: "Box".to_string(),
        type_parameters: vec![IrTypeParameter {
            name: "T".to_string(),
            bounds: Vec::new(),
            variance: IrVariance::Covariant,
            permits: Vec::new(),
            span: span.clone(),
        }],
        superclass: None,
        interfaces: Vec::new(),
        fields: vec![IrStatement::FieldDeclaration {
            name: "items".to_string(),
            java_type: JavaType::Reference {
                name: "java.util.List".to_string(),
                generic_args: vec![JavaType::Reference {
                    name: "T".to_string(),
                    generic_args: Vec::new(),
                }],
            },
            initializer: None,
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo".to_string()),
        imports: Vec::new(),
        type_declarations: vec![class],
        span,
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("class generation");
    let class_source = &unit.type_declarations[0];

    assert!(
        class_source.contains("java.util.List<? extends T> items;"),
        "expected covariant wildcard: {}",
        class_source
    );
}

#[test]
fn contravariant_type_arguments_render_wildcards() {
    let span = dummy_span();
    let class = IrStatement::ClassDeclaration {
        name: "Sink".to_string(),
        type_parameters: vec![IrTypeParameter {
            name: "U".to_string(),
            bounds: Vec::new(),
            variance: IrVariance::Contravariant,
            permits: Vec::new(),
            span: span.clone(),
        }],
        superclass: None,
        interfaces: Vec::new(),
        fields: vec![IrStatement::FieldDeclaration {
            name: "consumer".to_string(),
            java_type: JavaType::Reference {
                name: "java.util.function.Consumer".to_string(),
                generic_args: vec![JavaType::Reference {
                    name: "U".to_string(),
                    generic_args: Vec::new(),
                }],
            },
            initializer: None,
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("demo".to_string()),
        imports: Vec::new(),
        type_declarations: vec![class],
        span,
    };

    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("class generation");
    let class_source = &unit.type_declarations[0];

    assert!(
        class_source.contains("java.util.function.Consumer<? super U> consumer;"),
        "expected contravariant wildcard: {}",
        class_source
    );
}

#[test]
fn raw_type_comment_generation_matches_directive() {
    let generator = JavaCodeGenerator::new();
    let span = dummy_span();
    let directive = RawTypeDirective {
        owner: QualifiedName::new(
            vec!["java".into(), "util".into(), "List".into()],
            span.clone(),
        ),
        span,
        mode: RawTypeContinuation::AllowWithComment,
    };

    let comment = generator.generate_raw_type_comment(&directive);
    assert_eq!(comment, "// jv:raw-allow java.util.List");
}
