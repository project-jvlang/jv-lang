use super::*;
use super::{dummy_span, reference_type};
use insta::assert_snapshot;

fn sealed_program() -> IrProgram {
    let sealed_modifiers = IrModifiers {
        visibility: IrVisibility::Public,
        is_sealed: true,
        permitted_types: vec!["Success".to_string(), "Failure".to_string()],
        ..IrModifiers::default()
    };

    let result_class = IrStatement::ClassDeclaration {
        name: "Result".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![],
        nested_classes: vec![],
        modifiers: sealed_modifiers,
        span: dummy_span(),
    };

    let success_class = IrStatement::ClassDeclaration {
        name: "Success".to_string(),
        type_parameters: vec![],
        superclass: Some(reference_type("Result")),
        interfaces: vec![],
        fields: vec![],
        methods: vec![],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let failure_class = IrStatement::ClassDeclaration {
        name: "Failure".to_string(),
        type_parameters: vec![],
        superclass: Some(reference_type("Result")),
        interfaces: vec![],
        fields: vec![],
        methods: vec![],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    IrProgram {
        package: Some("com.example".to_string()),
        imports: vec![],
        type_declarations: vec![result_class, success_class, failure_class],
        span: dummy_span(),
    }
}

#[test]
fn sealed_type_renders_with_permits_for_java25() {
    let program = sealed_program();
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("code generation should succeed for Java 25");
    let source = unit.to_source(&JavaCodeGenConfig::for_target(JavaTarget::Java25));

    assert_snapshot!("sealed_java25", source);
}

#[test]
fn sealed_type_falls_back_to_final_for_java21() {
    let program = sealed_program();
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java21));
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("code generation should succeed for Java 21");
    let source = unit.to_source(&JavaCodeGenConfig::for_target(JavaTarget::Java21));

    assert_snapshot!("sealed_java21", source);
}
