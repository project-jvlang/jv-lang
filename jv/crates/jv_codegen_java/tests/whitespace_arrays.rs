use jv_ast::{BinaryOp, Literal, SequenceDelimiter, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{IrExpression, JavaType};

fn dummy_span() -> Span {
    Span::dummy()
}

fn int_type() -> JavaType {
    JavaType::Primitive("int".to_string())
}

fn identifier(name: &str) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: int_type(),
        span: dummy_span(),
    }
}

fn number_literal(value: &str) -> IrExpression {
    IrExpression::Literal(Literal::Number(value.to_string()), dummy_span())
}

fn addition_expr(right_value: &str) -> IrExpression {
    IrExpression::Binary {
        left: Box::new(identifier("i")),
        op: BinaryOp::Add,
        right: Box::new(number_literal(right_value)),
        java_type: int_type(),
        span: dummy_span(),
    }
}

fn whitespace_array_expression() -> IrExpression {
    IrExpression::ArrayCreation {
        element_type: int_type(),
        dimensions: vec![],
        initializer: Some(vec![
            identifier("i"),
            addition_expr("1"),
            addition_expr("2"),
            addition_expr("3"),
            addition_expr("4"),
        ]),
        delimiter: SequenceDelimiter::Whitespace,
        span: dummy_span(),
    }
}

#[test]
fn whitespace_array_with_binary_expressions_uses_list_of() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));

    let expression = whitespace_array_expression();

    let rendered = generator
        .generate_expression(&expression)
        .expect("whitespace array generation should succeed");

    assert_eq!(rendered, "List.of(i, i + 1, i + 2, i + 3, i + 4)");
}

#[test]
fn whitespace_array_inside_lambda_uses_list_of() {
    let mut generator =
        JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(JavaTarget::Java25));

    let expression = IrExpression::Lambda {
        functional_interface: "java.util.function.IntFunction".to_string(),
        param_names: vec!["i".to_string()],
        param_types: vec![int_type()],
        body: Box::new(whitespace_array_expression()),
        java_type: JavaType::object(),
        span: dummy_span(),
    };

    let rendered = generator
        .generate_expression(&expression)
        .expect("lambda with whitespace array should render");

    assert_eq!(
        rendered,
        "(i) -> List.of(i, i + 1, i + 2, i + 3, i + 4)"
    );
}
