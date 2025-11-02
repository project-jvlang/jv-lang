use jv_ast::{CallArgumentMetadata, CallArgumentStyle, Literal, Span};
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator, JavaTarget};
use jv_ir::{
    DoublebraceBaseStrategy, DoublebraceCopySourceStrategy, IrDoublebraceCopyPlan,
    IrDoublebraceFieldUpdate, IrDoublebraceMethodInvocation, IrDoublebraceMutatePlan,
    IrDoublebraceMutation, IrDoublebracePlan, IrExpression, JavaType,
};

fn span() -> Span {
    Span::dummy()
}

fn java_array_list() -> JavaType {
    JavaType::Reference {
        name: "java.util.ArrayList".to_string(),
        generic_args: Vec::new(),
    }
}

fn java_array_list_of_string() -> JavaType {
    JavaType::Reference {
        name: "java.util.ArrayList".to_string(),
        generic_args: vec![JavaType::Reference {
            name: "java.lang.String".to_string(),
            generic_args: Vec::new(),
        }],
    }
}

fn java_user_type() -> JavaType {
    JavaType::Reference {
        name: "com.example.ImmutableUser".to_string(),
        generic_args: Vec::new(),
    }
}

fn mutate_expression() -> IrExpression {
    let span = span();
    let base = IrExpression::Identifier {
        name: "list".to_string(),
        java_type: java_array_list(),
        span: span.clone(),
    };

    let mutation = IrDoublebraceMutation::MethodCall(IrDoublebraceMethodInvocation {
        name: "add".to_string(),
        arguments: vec![IrExpression::Literal(
            Literal::Number("1".to_string()),
            span.clone(),
        )],
        argument_style: CallArgumentStyle::Comma,
        metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        span: span.clone(),
    });

    IrExpression::DoublebraceInit {
        base: Some(Box::new(base)),
        receiver_type: java_array_list(),
        plan: IrDoublebracePlan::Mutate(IrDoublebraceMutatePlan {
            base: DoublebraceBaseStrategy::ExistingInstance,
            steps: vec![mutation],
        }),
        java_type: java_array_list(),
        span,
    }
}

fn copy_expression() -> IrExpression {
    copy_expression_with_updates(vec![make_name_update()])
}

fn make_name_update() -> IrDoublebraceFieldUpdate {
    let span = span();
    IrDoublebraceFieldUpdate {
        name: "name".to_string(),
        value: IrExpression::Literal(Literal::String("Alice".to_string()), span.clone()),
        span,
    }
}

fn copy_expression_with_updates(updates: Vec<IrDoublebraceFieldUpdate>) -> IrExpression {
    let span = span();
    let base = IrExpression::Identifier {
        name: "user".to_string(),
        java_type: java_user_type(),
        span: span.clone(),
    };

    IrExpression::DoublebraceInit {
        base: Some(Box::new(base)),
        receiver_type: java_user_type(),
        plan: IrDoublebracePlan::Copy(IrDoublebraceCopyPlan {
            source: DoublebraceCopySourceStrategy::ExistingInstance,
            updates,
        }),
        java_type: java_user_type(),
        span,
    }
}

fn render(expr: &IrExpression, target: JavaTarget) -> String {
    let mut generator = JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(target));
    generator
        .generate_expression(expr)
        .expect("Doublebrace のコード生成に失敗しました")
}

#[test]
fn doublebrace_mutate_generates_block_expression() {
    let expr = mutate_expression();
    let expected = "((java.util.function.Supplier<java.util.ArrayList>) () -> {\n    java.util.ArrayList __db_base0 = list;\n    __db_base0.add(1);\n    \n    return __db_base0;\n}).get()";

    assert_eq!(render(&expr, JavaTarget::Java25), expected);
    assert_eq!(render(&expr, JavaTarget::Java21), expected);
}

#[test]
fn doublebrace_copy_generates_method_chain() {
    let expr = copy_expression();
    let expected = "((java.util.function.Supplier<com.example.ImmutableUser>) () -> {\n    com.example.ImmutableUser __db_base0 = user;\n    com.example.ImmutableUser __db_copy1 = __db_base0;\n    __db_copy1 = __db_copy1.withName(\"Alice\");\n    \n    return __db_copy1;\n}).get()";

    assert_eq!(render(&expr, JavaTarget::Java25), expected);
    assert_eq!(render(&expr, JavaTarget::Java21), expected);
}

#[test]
fn doublebrace_copy_generates_chain_for_multiple_updates() {
    let span = span();
    let mut updates = Vec::new();
    updates.push(make_name_update());
    updates.push(IrDoublebraceFieldUpdate {
        name: "age".to_string(),
        value: IrExpression::Literal(Literal::Number("30".to_string()), span.clone()),
        span: span.clone(),
    });

    let expr = copy_expression_with_updates(updates);
    let expected = "((java.util.function.Supplier<com.example.ImmutableUser>) () -> {\n    com.example.ImmutableUser __db_base0 = user;\n    com.example.ImmutableUser __db_copy1 = __db_base0;\n    __db_copy1 = __db_copy1.withName(\"Alice\");\n    __db_copy1 = __db_copy1.withAge(30);\n    \n    return __db_copy1;\n}).get()";

    assert_eq!(render(&expr, JavaTarget::Java25), expected);
    assert_eq!(render(&expr, JavaTarget::Java21), expected);
}

#[test]
fn doublebrace_mutate_preserves_generic_receiver_in_java() {
    let span = span();
    let java_type = java_array_list_of_string();
    let base = IrExpression::Identifier {
        name: "list".to_string(),
        java_type: java_type.clone(),
        span: span.clone(),
    };

    let mutation = IrDoublebraceMutation::MethodCall(IrDoublebraceMethodInvocation {
        name: "add".to_string(),
        arguments: vec![IrExpression::Literal(
            Literal::String("herb".to_string()),
            span.clone(),
        )],
        argument_style: CallArgumentStyle::Comma,
        metadata: CallArgumentMetadata::with_style(CallArgumentStyle::Comma),
        span: span.clone(),
    });

    let expr = IrExpression::DoublebraceInit {
        base: Some(Box::new(base)),
        receiver_type: java_type.clone(),
        plan: IrDoublebracePlan::Mutate(IrDoublebraceMutatePlan {
            base: DoublebraceBaseStrategy::ExistingInstance,
            steps: vec![mutation],
        }),
        java_type: java_type,
        span,
    };

    let expected = "((java.util.function.Supplier<java.util.ArrayList<java.lang.String>>) () -> {\n    java.util.ArrayList<java.lang.String> __db_base0 = list;\n    __db_base0.add(\"herb\");\n    \n    return __db_base0;\n}).get()";

    assert_eq!(render(&expr, JavaTarget::Java25), expected);
    assert_eq!(render(&expr, JavaTarget::Java21), expected);
}
