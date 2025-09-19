use super::*;
use jv_ast::{Literal, Span};
use jv_ir::{
    IrExpression, IrModifiers, IrParameter, IrProgram, IrStatement, IrVisibility, JavaType,
    MethodOverload,
};

fn dummy_span() -> Span {
    Span::dummy()
}

fn string_type() -> JavaType {
    JavaType::Reference {
        name: "String".to_string(),
        generic_args: vec![],
    }
}

fn int_type() -> JavaType {
    JavaType::Primitive("int".to_string())
}

fn simple_method() -> IrStatement {
    let return_expression = IrExpression::FieldAccess {
        receiver: Box::new(IrExpression::This {
            java_type: JavaType::Reference {
                name: "Sample".to_string(),
                generic_args: vec![],
            },
            span: dummy_span(),
        }),
        field_name: "greeting".to_string(),
        java_type: string_type(),
        span: dummy_span(),
    };
    let return_stmt = IrStatement::Return {
        value: Some(return_expression),
        span: dummy_span(),
    };
    let body = IrExpression::Block {
        statements: vec![return_stmt],
        java_type: string_type(),
        span: dummy_span(),
    };
    IrStatement::MethodDeclaration {
        name: "sayHello".to_string(),
        parameters: vec![],
        return_type: string_type(),
        body: Some(body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    }
}

fn simple_class() -> IrStatement {
    let field = IrStatement::FieldDeclaration {
        name: "greeting".to_string(),
        java_type: string_type(),
        initializer: Some(IrExpression::Literal(
            Literal::String("Hello".to_string()),
            dummy_span(),
        )),
        modifiers: IrModifiers {
            visibility: IrVisibility::Private,
            is_final: true,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    IrStatement::ClassDeclaration {
        name: "Sample".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![field],
        methods: vec![simple_method()],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    }
}

#[test]
fn class_generation_renders_members() {
    let mut generator = JavaCodeGenerator::new();
    let class_code = generator
        .generate_class(&simple_class())
        .expect("class generation should succeed");

    assert!(class_code.contains("public class Sample"));
    assert!(class_code.contains("private final String greeting = \"Hello\";"));
    assert!(class_code.contains("public String sayHello"));
    assert!(class_code.contains("return this.greeting"));
}

#[test]
fn expression_generation_handles_binary_arithmetic() {
    let mut generator = JavaCodeGenerator::new();
    let expression = IrExpression::Binary {
        left: Box::new(IrExpression::Identifier {
            name: "a".to_string(),
            java_type: int_type(),
            span: dummy_span(),
        }),
        op: jv_ast::BinaryOp::Add,
        right: Box::new(IrExpression::Identifier {
            name: "b".to_string(),
            java_type: int_type(),
            span: dummy_span(),
        }),
        java_type: int_type(),
        span: dummy_span(),
    };

    let generated = generator
        .generate_expression(&expression)
        .expect("expression generation should succeed");

    assert_eq!(generated, "a + b");
}

#[test]
fn compilation_unit_collects_type_declarations() {
    let program = IrProgram {
        package: Some("com.example".to_string()),
        imports: vec![],
        type_declarations: vec![simple_class()],
        span: dummy_span(),
    };

    let unit = generate_java_code(&program).expect("compilation unit generation");
    let source = unit.to_source(&JavaCodeGenConfig::default());

    assert!(source.contains("package com.example;"));
    assert!(source.contains("public class Sample"));
    assert!(source.contains("sayHello"));
}

#[test]
fn method_overload_generation_wraps_body() {
    let mut generator = JavaCodeGenerator::new();
    let overload = MethodOverload {
        name: "sum".to_string(),
        parameters: vec![IrParameter {
            name: "a".to_string(),
            java_type: int_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        return_type: int_type(),
        body: IrExpression::Identifier {
            name: "a".to_string(),
            java_type: int_type(),
            span: dummy_span(),
        },
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    let overloads = generator
        .generate_method_overloads(&[overload])
        .expect("overload generation should succeed");

    assert_eq!(overloads.len(), 1);
    assert!(overloads[0].contains("public int sum"));
    assert!(overloads[0].contains("return a;"));
}
