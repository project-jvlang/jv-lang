//! タプルレコード生成とタプルリテラル/分割代入展開のGoldenテスト。

use jv_ast::{Literal, Span, expression::TupleFieldMeta};
use jv_codegen_java::{JavaCodeGenConfig, JavaTarget, generate_java_source_with_config};
use jv_ir::{
    IrExpression, IrModifiers, IrProgram, IrStatement, JavaType, TupleRecordPlan,
    TupleRecordStrategy, TupleUsageContext, TupleUsageKind,
};

fn tuple_program() -> IrProgram {
    let program_span = Span::new(1, 0, 1, 0);
    let tuple_span = Span::new(4, 8, 4, 28);
    let assign_left_span = Span::new(6, 4, 6, 24);
    let assign_right_span = Span::new(7, 4, 7, 26);

    let int_type = JavaType::Primitive("int".to_string());
    let record_type = JavaType::Reference {
        name: "Divmod_Result".to_string(),
        generic_args: vec![],
    };

    let left_decl = IrStatement::VariableDeclaration {
        name: "left".to_string(),
        java_type: int_type.clone(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("9".to_string()),
            Span::new(2, 15, 2, 16),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: Span::new(2, 4, 2, 20),
    };

    let right_decl = IrStatement::VariableDeclaration {
        name: "right".to_string(),
        java_type: int_type.clone(),
        initializer: Some(IrExpression::Literal(
            Literal::Number("4".to_string()),
            Span::new(3, 16, 3, 17),
        )),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: Span::new(3, 4, 3, 21),
    };

    let tuple_literal = IrExpression::TupleLiteral {
        elements: vec![
            IrExpression::Identifier {
                name: "left".to_string(),
                java_type: int_type.clone(),
                span: tuple_span.clone(),
            },
            IrExpression::Identifier {
                name: "right".to_string(),
                java_type: int_type.clone(),
                span: tuple_span.clone(),
            },
        ],
        java_type: record_type.clone(),
        span: tuple_span.clone(),
    };

    let tuple_binding = IrStatement::VariableDeclaration {
        name: "result".to_string(),
        java_type: record_type.clone(),
        initializer: Some(tuple_literal),
        is_final: true,
        modifiers: IrModifiers::default(),
        span: tuple_span.clone(),
    };

    let quotient_decl = IrStatement::VariableDeclaration {
        name: "quotient".to_string(),
        java_type: int_type.clone(),
        initializer: None,
        is_final: false,
        modifiers: IrModifiers::default(),
        span: Span::new(5, 4, 5, 17),
    };

    let remainder_decl = IrStatement::VariableDeclaration {
        name: "remainder".to_string(),
        java_type: int_type.clone(),
        initializer: None,
        is_final: false,
        modifiers: IrModifiers::default(),
        span: Span::new(5, 20, 5, 36),
    };

    let quotient_assign = IrStatement::Expression {
        expr: IrExpression::Assignment {
            target: Box::new(IrExpression::Identifier {
                name: "quotient".to_string(),
                java_type: int_type.clone(),
                span: assign_left_span.clone(),
            }),
            value: Box::new(IrExpression::FieldAccess {
                receiver: Box::new(IrExpression::Identifier {
                    name: "result".to_string(),
                    java_type: record_type.clone(),
                    span: assign_left_span.clone(),
                }),
                field_name: "quotient".to_string(),
                java_type: int_type.clone(),
                span: assign_left_span.clone(),
                is_record_component: true,
            }),
            java_type: int_type.clone(),
            span: assign_left_span.clone(),
        },
        span: assign_left_span.clone(),
    };

    let remainder_assign = IrStatement::Expression {
        expr: IrExpression::Assignment {
            target: Box::new(IrExpression::Identifier {
                name: "remainder".to_string(),
                java_type: int_type.clone(),
                span: assign_right_span.clone(),
            }),
            value: Box::new(IrExpression::FieldAccess {
                receiver: Box::new(IrExpression::Identifier {
                    name: "result".to_string(),
                    java_type: record_type.clone(),
                    span: assign_right_span.clone(),
                }),
                field_name: "remainder".to_string(),
                java_type: int_type.clone(),
                span: assign_right_span.clone(),
                is_record_component: true,
            }),
            java_type: int_type,
            span: assign_right_span.clone(),
        },
        span: assign_right_span,
    };

    let tuple_plan = TupleRecordPlan {
        arity: 2,
        strategy: TupleRecordStrategy::Specific,
        specific_name: Some("Divmod_Result".to_string()),
        generic_name: "Tuple2_Int_Int".to_string(),
        fields: vec![
            TupleFieldMeta {
                primary_label: Some("quotient".to_string()),
                secondary_labels: Vec::new(),
                identifier_hint: Some("quotient".to_string()),
                fallback_index: 2,
                span: tuple_span.clone(),
            },
            TupleFieldMeta {
                primary_label: Some("remainder".to_string()),
                secondary_labels: Vec::new(),
                identifier_hint: Some("remainder".to_string()),
                fallback_index: 1,
                span: tuple_span.clone(),
            },
        ],
        type_hints: vec!["Int".to_string(), "Int".to_string()],
        usage_sites: vec![TupleUsageContext {
            kind: TupleUsageKind::BindingInitializer,
            owner: Some("result".to_string()),
            span: tuple_span,
        }],
    };

    IrProgram {
        package: Some("tuples.codegen".to_string()),
        imports: Vec::new(),
        type_declarations: vec![
            left_decl,
            right_decl,
            tuple_binding,
            quotient_decl,
            remainder_decl,
            quotient_assign,
            remainder_assign,
        ],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        tuple_record_plans: vec![tuple_plan],
        span: program_span,
    }
}

fn render_java(target: JavaTarget) -> String {
    let program = tuple_program();
    let config = JavaCodeGenConfig::for_target(target);
    generate_java_source_with_config(&program, &config)
        .expect("Javaコード生成が成功すること")
        .trim()
        .to_string()
}

#[test]
fn tuple_codegen_java25_golden() {
    let actual = render_java(JavaTarget::Java25);
    let expected =
        include_str!("golden/expected/tuple_codegen_record_literal_destructure__java25.java")
            .trim();
    assert_eq!(
        actual, expected,
        "Java25向けタプルコード生成結果がGoldenと一致しない: {actual}"
    );
}

#[test]
fn tuple_codegen_java21_golden() {
    let actual = render_java(JavaTarget::Java21);
    let expected =
        include_str!("golden/expected/tuple_codegen_record_literal_destructure__java21.java")
            .trim();
    assert_eq!(
        actual, expected,
        "Java21向けタプルコード生成結果がGoldenと一致しない: {actual}"
    );
}
