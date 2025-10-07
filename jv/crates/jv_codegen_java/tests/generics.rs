use jv_ast::{
    types::{QualifiedName, RawTypeContinuation, RawTypeDirective},
    Span,
};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{
    IrCommentKind, IrExpression, IrModifiers, IrProgram, IrStatement, IrTypeParameter, IrVariance,
    JavaType,
};

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

#[test]
fn raw_default_comment_on_variable_inserts_guard() {
    let span = dummy_span();
    let base = IrStatement::VariableDeclaration {
        name: "list".to_string(),
        java_type: JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: Vec::new(),
        },
        initializer: Some(IrExpression::Literal(Literal::Null, span.clone())),
        is_final: false,
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let commented = IrStatement::Commented {
        statement: Box::new(base),
        comment: "// jv:raw-default java.util.List".to_string(),
        kind: IrCommentKind::Line,
        comment_span: span.clone(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_statement(&commented)
        .expect("guarded statement");

    assert_eq!(
        rendered,
        "java.util.List list = Objects.requireNonNull(null, \"JV: raw type guard for java.util.List\"); // jv:raw-default java.util.List"
    );
}

#[test]
fn raw_default_comment_on_field_adds_import_and_guard() {
    let span = dummy_span();
    let field = IrStatement::FieldDeclaration {
        name: "items".to_string(),
        java_type: JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: Vec::new(),
        },
        initializer: Some(IrExpression::Literal(Literal::Null, span.clone())),
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let commented_field = IrStatement::Commented {
        statement: Box::new(field),
        comment: "// jv:raw-default java.util.List".to_string(),
        kind: IrCommentKind::Line,
        comment_span: span.clone(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Demo".to_string(),
        type_parameters: Vec::new(),
        superclass: None,
        interfaces: Vec::new(),
        fields: vec![commented_field],
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

    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("compilation unit");

    assert!(
        unit.imports
            .contains(&"import java.util.Objects;".to_string()),
        "expected Objects import but found {:?}",
        unit.imports
    );

    let declaration = &unit.type_declarations[0];
    assert!(
        declaration
            .contains("Objects.requireNonNull(null, \"JV: raw type guard for java.util.List\")"),
        "expected guard in field declaration: {}",
        declaration
    );
}

#[test]
fn raw_allow_comment_on_field_keeps_imports_clean() {
    let span = dummy_span();
    let field = IrStatement::FieldDeclaration {
        name: "items".to_string(),
        java_type: JavaType::Reference {
            name: "java.util.List".to_string(),
            generic_args: Vec::new(),
        },
        initializer: None,
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let commented_field = IrStatement::Commented {
        statement: Box::new(field),
        comment: "// jv:raw-allow demo.Widget".to_string(),
        kind: IrCommentKind::Line,
        comment_span: span.clone(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Demo".to_string(),
        type_parameters: Vec::new(),
        superclass: None,
        interfaces: Vec::new(),
        fields: vec![commented_field],
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

    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("compilation unit");

    assert!(
        unit.imports
            .iter()
            .all(|import| !import.contains("java.util.Objects")),
        "unexpected Objects import for raw-allow comment: {:?}",
        unit.imports
    );

    let declaration = &unit.type_declarations[0];
    assert!(
        declaration.contains("java.util.List items; // jv:raw-allow demo.Widget"),
        "expected field to remain unchanged aside from comment: {}",
        declaration
    );
}

#[test]
fn raw_allow_comment_keeps_statement_unchanged() {
    let span = dummy_span();
    let base = IrStatement::Return {
        value: Some(IrExpression::Identifier {
            name: "value".to_string(),
            java_type: JavaType::Reference {
                name: "Object".to_string(),
                generic_args: Vec::new(),
            },
            span: span.clone(),
        }),
        span: span.clone(),
    };

    let commented = IrStatement::Commented {
        statement: Box::new(base),
        comment: "// jv:raw-allow demo.Value".to_string(),
        kind: IrCommentKind::Line,
        comment_span: span.clone(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_statement(&commented)
        .expect("commented statement");

    assert_eq!(rendered, "return value; // jv:raw-allow demo.Value");
}

#[test]
fn raw_default_comment_on_return_inserts_guard() {
    let span = dummy_span();
    let base = IrStatement::Return {
        value: Some(IrExpression::Identifier {
            name: "result".to_string(),
            java_type: JavaType::Reference {
                name: "Object".to_string(),
                generic_args: Vec::new(),
            },
            span: span.clone(),
        }),
        span: span.clone(),
    };

    let commented = IrStatement::Commented {
        statement: Box::new(base),
        comment: "// jv:raw-default demo.Result".to_string(),
        kind: IrCommentKind::Line,
        comment_span: span.clone(),
    };

    let mut generator = JavaCodeGenerator::new();
    let rendered = generator
        .generate_statement(&commented)
        .expect("guarded return");

    assert_eq!(
        rendered,
        "return Objects.requireNonNull(result, \"JV: raw type guard for demo.Result\"); // jv:raw-default demo.Result"
    );
}
