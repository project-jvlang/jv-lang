use super::*;
use jv_pm::JavaTarget;

fn render_java(program: IrProgram) -> String {
    let config = JavaCodeGenConfig::for_target(JavaTarget::Java25);
    let mut generator = JavaCodeGenerator::with_config(config.clone());
    let unit = generator
        .generate_compilation_unit(&program)
        .expect("code generation should succeed");
    unit.to_source(&config)
}

#[test]
fn implicit_val_fields_render_with_final_modifier() {
    let mut immutable_mods = IrModifiers::default();
    immutable_mods.visibility = IrVisibility::Private;
    immutable_mods.is_final = true;

    let immutable_field = IrStatement::FieldDeclaration {
        name: "IMMUTABLE".to_string(),
        java_type: int_type(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("1".to_string()),
            dummy_span(),
        )),
        modifiers: immutable_mods,
        span: dummy_span(),
    };

    let mut mutable_mods = IrModifiers::default();
    mutable_mods.visibility = IrVisibility::Private;

    let mutable_field = IrStatement::FieldDeclaration {
        name: "mutableValue".to_string(),
        java_type: int_type(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("2".to_string()),
            dummy_span(),
        )),
        modifiers: mutable_mods,
        span: dummy_span(),
    };

    let mut class_modifiers = IrModifiers::default();
    class_modifiers.visibility = IrVisibility::Public;

    let holder = IrStatement::ClassDeclaration {
        name: "Holder".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![immutable_field, mutable_field],
        methods: vec![],
        nested_classes: vec![],
        modifiers: class_modifiers,
        span: dummy_span(),
    };

    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![holder],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: Default::default(),
        span: dummy_span(),
    };

    let java = render_java(program);
    assert!(
        java.contains("private final int IMMUTABLE = 1;"),
        "expected IMMUTABLE field to be final: {java}"
    );
    assert!(
        java.contains("private int mutableValue = 2;"),
        "expected mutableValue field without final: {java}"
    );
    assert!(
        !java.contains("final int mutableValue = 2;"),
        "mutableValue should remain mutable: {java}"
    );
}

#[test]
fn implicit_vals_in_function_scope_render_as_final_locals() {
    let mut method_statements = Vec::new();

    method_statements.push(IrStatement::VariableDeclaration {
        name: "implicitLocal".to_string(),
        java_type: int_type(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("1".to_string()),
            dummy_span(),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    });

    method_statements.push(IrStatement::VariableDeclaration {
        name: "typedName".to_string(),
        java_type: string_type(),
        initializer: Some(IrExpression::Literal(
            Literal::String("hello".to_string()),
            dummy_span(),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    });

    method_statements.push(IrStatement::VariableDeclaration {
        name: "mutableLocal".to_string(),
        java_type: int_type(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("2".to_string()),
            dummy_span(),
        )),
        is_final: false,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    });

    let mut inner_block_statements = Vec::new();
    inner_block_statements.push(IrStatement::VariableDeclaration {
        name: "innerValue".to_string(),
        java_type: int_type(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("42".to_string()),
            dummy_span(),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    });
    inner_block_statements.push(IrStatement::VariableDeclaration {
        name: "innerMutable".to_string(),
        java_type: int_type(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("24".to_string()),
            dummy_span(),
        )),
        is_final: false,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    });

    method_statements.push(IrStatement::Block {
        statements: inner_block_statements,
        span: dummy_span(),
    });

    let method_body = IrExpression::Block {
        statements: method_statements,
        java_type: JavaType::Void,
        span: dummy_span(),
    };

    let mut method_modifiers = IrModifiers::default();
    method_modifiers.visibility = IrVisibility::Public;

    let method = IrStatement::MethodDeclaration {
        name: "demo".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![],
        primitive_return: None,
        return_type: JavaType::Void,
        body: Some(method_body),
        modifiers: method_modifiers,
        throws: vec![],
        assertion_patterns: Vec::new(),
        span: dummy_span(),
    };

    let mut class_modifiers = IrModifiers::default();
    class_modifiers.visibility = IrVisibility::Public;

    let container = IrStatement::ClassDeclaration {
        name: "Locals".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![method],
        nested_classes: vec![],
        modifiers: class_modifiers,
        span: dummy_span(),
    };

    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![container],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: Default::default(),
        span: dummy_span(),
    };

    let java = render_java(program);
    assert!(
        java.contains("final int implicitLocal = 1;"),
        "implicit val should be final: {java}"
    );
    assert!(
        java.contains("final String typedName = \"hello\";"),
        "typed implicit val should be final with correct type: {java}"
    );
    assert!(
        java.contains("int mutableLocal = 2;"),
        "mutable var should remain without final: {java}"
    );
    assert!(
        !java.contains("final int mutableLocal = 2;"),
        "mutableLocal must not be final: {java}"
    );
    assert!(
        java.contains("final int innerValue = 42;"),
        "inner implicit val should be final: {java}"
    );
    assert!(
        java.contains("int innerMutable = 24;"),
        "inner mutable var should omit final: {java}"
    );
}

#[test]
fn implicit_vals_across_multiple_methods_remain_final() {
    let mut first_body = Vec::new();
    first_body.push(IrStatement::VariableDeclaration {
        name: "first".to_string(),
        java_type: int_type(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("7".to_string()),
            dummy_span(),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    });

    let first_method = IrStatement::MethodDeclaration {
        name: "firstMethod".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![],
        primitive_return: None,
        return_type: JavaType::Void,
        body: Some(IrExpression::Block {
            statements: first_body,
            java_type: JavaType::Void,
            span: dummy_span(),
        }),
        modifiers: {
            let mut mods = IrModifiers::default();
            mods.visibility = IrVisibility::Public;
            mods
        },
        throws: vec![],
        assertion_patterns: Vec::new(),
        span: dummy_span(),
    };

    let mut second_statements = Vec::new();
    second_statements.push(IrStatement::VariableDeclaration {
        name: "second".to_string(),
        java_type: string_type(),
        initializer: Some(IrExpression::Literal(
            Literal::String("value".to_string()),
            dummy_span(),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: dummy_span(),
    });

    let second_method = IrStatement::MethodDeclaration {
        name: "secondMethod".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![],
        primitive_return: None,
        return_type: JavaType::Void,
        body: Some(IrExpression::Block {
            statements: second_statements,
            java_type: JavaType::Void,
            span: dummy_span(),
        }),
        modifiers: {
            let mut mods = IrModifiers::default();
            mods.visibility = IrVisibility::Public;
            mods
        },
        throws: vec![],
        assertion_patterns: Vec::new(),
        span: dummy_span(),
    };

    let mut class_modifiers = IrModifiers::default();
    class_modifiers.visibility = IrVisibility::Public;

    let program = IrProgram {
        package: None,
        imports: vec![],
        type_declarations: vec![IrStatement::ClassDeclaration {
            name: "Multi".to_string(),
            type_parameters: vec![],
            superclass: None,
            interfaces: vec![],
            fields: vec![],
            methods: vec![first_method, second_method],
            nested_classes: vec![],
            modifiers: class_modifiers,
            span: dummy_span(),
        }],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: Default::default(),
        span: dummy_span(),
    };

    let java = render_java(program);
    assert!(
        java.contains("final int first = 7;"),
        "first method val should be final: {java}"
    );
    assert!(
        java.contains("final String second = \"value\";"),
        "second method val should be final: {java}"
    );
}
