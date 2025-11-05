use jv_ast::{AnnotationName, Literal, Span, types::Kind};
use jv_codegen_java::JavaCodeGenerator;
use jv_ir::{
    IrAnnotation, IrAnnotationArgument, IrAnnotationValue, IrGenericMetadata, IrModifiers,
    IrProgram, IrStatement, IrTypeParameter, IrVariance, IrVisibility,
};

fn dummy_span() -> Span {
    Span::dummy()
}

fn audited_program() -> IrProgram {
    let span = dummy_span();

    let author_annotation = IrAnnotation {
        name: AnnotationName::new(
            vec![
                "com".to_string(),
                "example".to_string(),
                "meta".to_string(),
                "Author".to_string(),
            ],
            span.clone(),
        ),
        arguments: vec![IrAnnotationArgument::Named {
            name: "name".to_string(),
            value: IrAnnotationValue::Literal(Literal::String("team".to_string())),
        }],
        span: span.clone(),
    };

    let audited_annotation = IrAnnotation {
        name: AnnotationName::new(
            vec![
                "com".to_string(),
                "example".to_string(),
                "security".to_string(),
                "Audited".to_string(),
            ],
            span.clone(),
        ),
        arguments: vec![
            IrAnnotationArgument::Positional(IrAnnotationValue::Nested(author_annotation.clone())),
            IrAnnotationArgument::Named {
                name: "level".to_string(),
                value: IrAnnotationValue::EnumConstant {
                    type_name: "AuditLevel".to_string(),
                    constant: "HIGH".to_string(),
                },
            },
            IrAnnotationArgument::Named {
                name: "tags".to_string(),
                value: IrAnnotationValue::Array(vec![
                    IrAnnotationValue::Literal(Literal::String("service".to_string())),
                    IrAnnotationValue::Literal(Literal::String("core".to_string())),
                ]),
            },
        ],
        span: span.clone(),
    };

    let modifiers = IrModifiers {
        visibility: IrVisibility::Public,
        annotations: vec![audited_annotation],
        ..IrModifiers::default()
    };

    let class = IrStatement::ClassDeclaration {
        name: "AuditedService".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: Vec::new(),
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers,
        span,
    };

    IrProgram {
        package: Some("com.example.app".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    }
}

#[test]
fn annotations_render_with_arguments_and_generate_imports() {
    let program = audited_program();
    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("compilation unit");

    assert!(
        unit.imports
            .contains(&"com.example.security.Audited".to_string())
    );
    assert!(
        unit.imports
            .contains(&"com.example.meta.Author".to_string())
    );

    let class_source = &unit.type_declarations[0];
    assert!(class_source.contains("@Audited"));
    assert!(class_source.contains("@Author(name = \"team\")"));
    assert!(class_source.contains("level = AuditLevel.HIGH"));
    assert!(class_source.contains("tags = {\"service\", \"core\"}"));
}

#[test]
fn annotations_render_before_generic_metadata_comment() {
    let mut program = audited_program();

    let param_span = Span::dummy();
    let type_param = IrTypeParameter {
        name: "T".to_string(),
        bounds: Vec::new(),
        variance: IrVariance::Invariant,
        permits: Vec::new(),
        kind: None,
        span: param_span,
    };

    let declaration = program
        .type_declarations
        .get_mut(0)
        .expect("expected audited service declaration");
    let IrStatement::ClassDeclaration {
        type_parameters, ..
    } = declaration
    else {
        panic!("expected class declaration for audited service");
    };
    type_parameters.push(type_param);

    let mut metadata_entry = IrGenericMetadata::default();
    metadata_entry
        .type_parameter_kinds
        .insert("T".to_string(), Kind::Star);
    program.generic_metadata.insert(
        "com::example::app::AuditedService".to_string(),
        metadata_entry,
    );

    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("compilation unit");
    let class_source = &unit.type_declarations[0];

    let annotation_index = class_source
        .find("@Audited")
        .expect("annotation should be present");
    let metadata_index = class_source
        .find("JV Generic Metadata")
        .expect("metadata comment should be present");
    assert!(
        annotation_index < metadata_index,
        "annotations should precede metadata comment: {}",
        class_source
    );
    assert!(class_source.contains("class AuditedService<T /* kind: * */>"));
    assert!(class_source.contains("type parameter T kind = *"));
}

#[test]
fn field_annotations_render_and_imports() {
    let span = dummy_span();

    // @org.jetbrains.annotations.Nullable
    let nullable = IrAnnotation {
        name: AnnotationName::new(
            vec![
                "org".to_string(),
                "jetbrains".to_string(),
                "annotations".to_string(),
                "Nullable".to_string(),
            ],
            span.clone(),
        ),
        arguments: vec![],
        span: span.clone(),
    };

    let field = IrStatement::FieldDeclaration {
        name: "desc".to_string(),
        java_type: jv_ir::JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        },
        initializer: None,
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            annotations: vec![nullable],
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Note".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![field],
        methods: vec![],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("com.example.annots".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("compilation unit");

    assert!(
        unit.imports
            .contains(&"org.jetbrains.annotations.Nullable".to_string()),
        "should register import for qualified field annotation"
    );
    let class_source = &unit.type_declarations[0];
    assert!(
        class_source.contains("@Nullable"),
        "annotation line should be present"
    );
}

#[test]
fn parameter_annotations_render_in_signature_and_imports() {
    let span = dummy_span();

    let nullable = IrAnnotation {
        name: AnnotationName::new(
            vec![
                "org".to_string(),
                "jetbrains".to_string(),
                "annotations".to_string(),
                "Nullable".to_string(),
            ],
            span.clone(),
        ),
        arguments: vec![],
        span: span.clone(),
    };

    let param = jv_ir::IrParameter {
        name: "input".to_string(),
        java_type: jv_ir::JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        },
        modifiers: IrModifiers {
            annotations: vec![nullable],
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let method = IrStatement::MethodDeclaration {
        name: "echo".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![param],
        primitive_return: None,
        return_type: jv_ir::JavaType::Reference {
            name: "String".to_string(),
            generic_args: vec![],
        },
        body: Some(jv_ir::IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(jv_ir::IrExpression::Identifier {
                    name: "input".to_string(),
                    java_type: jv_ir::JavaType::Reference {
                        name: "String".to_string(),
                        generic_args: vec![],
                    },
                    span: span.clone(),
                }),
                span: span.clone(),
            }],
            java_type: jv_ir::JavaType::Reference {
                name: "String".to_string(),
                generic_args: vec![],
            },
            span: span.clone(),
        }),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: span.clone(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Echo".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    let program = IrProgram {
        package: Some("com.example.annots".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("compilation unit");

    assert!(
        unit.imports
            .contains(&"org.jetbrains.annotations.Nullable".to_string()),
        "should register import for qualified parameter annotation"
    );
    let class_source = &unit.type_declarations[0];
    assert!(
        class_source.contains("echo(@Nullable String input)"),
        "method signature should contain inline parameter annotation"
    );
}
