use super::*;
use insta::assert_snapshot;
use jv_ir::{
    IrCaseLabel, IrDeconstructionComponent, IrDeconstructionPattern, IrExpression, IrModifiers,
    IrParameter, IrProgram, IrRecordComponent, IrStatement, IrVisibility, JavaType,
};
use jv_pm::JavaTarget;

fn render_program(program: &IrProgram, target: JavaTarget) -> String {
    let mut generator = JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(target));
    let unit = generator
        .generate_compilation_unit(program)
        .expect("code generation");
    unit.to_source(&JavaCodeGenConfig::for_target(target))
}

fn snapshot_program(name: &str, program: IrProgram) {
    let java25 = render_program(&program, JavaTarget::Java25);
    let java21 = render_program(&program, JavaTarget::Java21);
    let combined = format!(
        "// ---- Java 25 ----\n{}\n\n// ---- Java 21 ----\n{}\n",
        java25.trim_end(),
        java21.trim_end()
    );
    assert_snapshot!(name, combined);
}

fn snapshot_java21_fallback(name: &str, summary: &str, program: IrProgram) {
    let java21 = render_program(&program, JavaTarget::Java21);
    let content = format!("{summary}\n{}\n", java21.trim_end());
    assert_snapshot!(name, content);
}

fn example1_program() -> IrProgram {
    let object_type = reference_type("Object");
    let string = string_type();
    let int = int_type();

    let discriminant = Box::new(ir_identifier("x", &object_type));

    let string_case = switch_case(
        vec![IrCaseLabel::TypePattern {
            type_name: "String".to_string(),
            variable: "value".to_string(),
            deconstruction: None,
        }],
        None,
        IrExpression::MethodCall {
            receiver: Some(Box::new(ir_identifier("value", &string))),
            method_name: "length".to_string(),
            java_name: None,
            resolved_target: None,
            args: vec![],
            argument_style: CallArgumentStyle::Comma,
            java_type: int.clone(),
            span: dummy_span(),
        },
    );

    let int_case = switch_case(
        vec![IrCaseLabel::TypePattern {
            type_name: "Integer".to_string(),
            variable: "value".to_string(),
            deconstruction: None,
        }],
        None,
        IrExpression::Binary {
            left: Box::new(ir_identifier("value", &int)),
            op: BinaryOp::Multiply,
            right: Box::new(IrExpression::Literal(
                Literal::Number("2".to_string()),
                dummy_span(),
            )),
            java_type: int.clone(),
            span: dummy_span(),
        },
    );

    let default_case = switch_case(
        vec![IrCaseLabel::Default],
        None,
        IrExpression::Literal(Literal::Number("0".to_string()), dummy_span()),
    );

    let switch_expr = IrExpression::Switch {
        discriminant,
        cases: vec![string_case, int_case, default_case],
        java_type: int.clone(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=3 guards=0 default=true exhaustive=false".to_string(),
        ),
        span: dummy_span(),
    };

    let method_body = IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(switch_expr),
            span: dummy_span(),
        }],
        java_type: int,
        span: dummy_span(),
    };

    let evaluate_method = IrStatement::MethodDeclaration {
        name: "evaluate".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "x".to_string(),
            java_type: object_type,
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        primitive_return: None,
        return_type: int_type(),
        body: Some(method_body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Example1".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![evaluate_method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    IrProgram {
        package: Some("patterns".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: Default::default(),
        span: dummy_span(),
    }
}

fn example2_program() -> IrProgram {
    let int = int_type();

    fn range_case(lower: &str, upper: &str, inclusive: bool, label: &str) -> IrSwitchCase {
        let guard = if inclusive {
            BinaryOp::LessEqual
        } else {
            BinaryOp::Less
        };

        let lower_check = IrExpression::Binary {
            left: Box::new(ir_identifier("score", &int_type())),
            op: BinaryOp::GreaterEqual,
            right: Box::new(IrExpression::Literal(
                Literal::Number(lower.to_string()),
                dummy_span(),
            )),
            java_type: JavaType::boolean(),
            span: dummy_span(),
        };

        let upper_check = IrExpression::Binary {
            left: Box::new(ir_identifier("score", &int_type())),
            op: guard,
            right: Box::new(IrExpression::Literal(
                Literal::Number(upper.to_string()),
                dummy_span(),
            )),
            java_type: JavaType::boolean(),
            span: dummy_span(),
        };

        let condition = IrExpression::Binary {
            left: Box::new(lower_check),
            op: BinaryOp::And,
            right: Box::new(upper_check),
            java_type: JavaType::boolean(),
            span: dummy_span(),
        };

        switch_case(
            vec![IrCaseLabel::Range {
                type_name: "int".to_string(),
                variable: "it0".to_string(),
                lower: Box::new(IrExpression::Literal(
                    Literal::Number(lower.to_string()),
                    dummy_span(),
                )),
                upper: Box::new(IrExpression::Literal(
                    Literal::Number(upper.to_string()),
                    dummy_span(),
                )),
                inclusive_end: inclusive,
            }],
            Some(condition),
            IrExpression::Literal(Literal::String(label.to_string()), dummy_span()),
        )
    }

    let discriminant = Box::new(ir_identifier("score", &int));

    let cases = vec![
        range_case("0", "60", false, "Fail"),
        range_case("60", "80", false, "Pass"),
        range_case("80", "100", true, "Excellent"),
        switch_case(
            vec![IrCaseLabel::Default],
            None,
            IrExpression::Literal(Literal::String("Invalid".to_string()), dummy_span()),
        ),
    ];

    let switch_expr = IrExpression::Switch {
        discriminant,
        cases,
        java_type: string_type(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=4 guards=3 default=true exhaustive=false".to_string(),
        ),
        span: dummy_span(),
    };

    let method_body = IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(switch_expr),
            span: dummy_span(),
        }],
        java_type: string_type(),
        span: dummy_span(),
    };

    let method = IrStatement::MethodDeclaration {
        name: "categorize".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "score".to_string(),
            java_type: int_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        primitive_return: None,
        return_type: string_type(),
        body: Some(method_body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Example2".to_string(),
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
        span: dummy_span(),
    };

    IrProgram {
        package: Some("patterns".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: Default::default(),
        span: dummy_span(),
    }
}

fn example5_program() -> IrProgram {
    let int = int_type();
    let inner_type = reference_type("Inner");
    let outer_type = reference_type("Outer");

    let outer_record = IrStatement::RecordDeclaration {
        name: "Outer".to_string(),
        type_parameters: vec![],
        components: vec![
            record_component("inner", inner_type.clone()),
            record_component("count", int.clone()),
        ],
        interfaces: vec![],
        methods: vec![],
        modifiers: public_modifiers(),
        span: dummy_span(),
    };

    let inner_record = IrStatement::RecordDeclaration {
        name: "Inner".to_string(),
        type_parameters: vec![],
        components: vec![
            record_component("x", int.clone()),
            record_component("y", int.clone()),
        ],
        interfaces: vec![],
        methods: vec![],
        modifiers: public_modifiers(),
        span: dummy_span(),
    };

    let nested_pattern = IrDeconstructionPattern {
        components: vec![
            IrDeconstructionComponent::Type {
                type_name: "Inner".to_string(),
                pattern: Some(Box::new(IrDeconstructionPattern {
                    components: vec![
                        IrDeconstructionComponent::Binding {
                            name: "x".to_string(),
                        },
                        IrDeconstructionComponent::Binding {
                            name: "y".to_string(),
                        },
                    ],
                })),
            },
            IrDeconstructionComponent::Binding {
                name: "count".to_string(),
            },
        ],
    };

    let sum_xy = IrExpression::Binary {
        left: Box::new(ir_identifier("x", &int)),
        op: BinaryOp::Add,
        right: Box::new(ir_identifier("y", &int)),
        java_type: int.clone(),
        span: dummy_span(),
    };

    let total_body = IrExpression::Binary {
        left: Box::new(sum_xy),
        op: BinaryOp::Add,
        right: Box::new(ir_identifier("count", &int)),
        java_type: int.clone(),
        span: dummy_span(),
    };

    let match_case = switch_case(
        vec![IrCaseLabel::TypePattern {
            type_name: "Outer".to_string(),
            variable: "outer".to_string(),
            deconstruction: Some(nested_pattern),
        }],
        None,
        total_body,
    );

    let default_case = switch_case(vec![IrCaseLabel::Default], None, number_literal("0"));

    let switch_expr = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("value", &outer_type)),
        cases: vec![match_case, default_case],
        java_type: int.clone(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=2 guards=0 default=true exhaustive=true".to_string(),
        ),
        span: dummy_span(),
    };

    let return_stmt = IrStatement::Return {
        value: Some(switch_expr),
        span: dummy_span(),
    };

    let total_method = IrStatement::MethodDeclaration {
        name: "total".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "value".to_string(),
            java_type: outer_type,
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        primitive_return: None,
        return_type: int.clone(),
        body: Some(IrExpression::Block {
            statements: vec![return_stmt],
            java_type: int.clone(),
            span: dummy_span(),
        }),
        modifiers: method_modifiers(),
        throws: vec![],
        span: dummy_span(),
    };

    let example_class = IrStatement::ClassDeclaration {
        name: "Example5".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![total_method],
        nested_classes: vec![],
        modifiers: public_modifiers(),
        span: dummy_span(),
    };

    IrProgram {
        package: Some("patterns".to_string()),
        imports: vec![],
        type_declarations: vec![inner_record, outer_record, example_class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        logging: Default::default(),
        span: dummy_span(),
    }
}

fn method_modifiers() -> IrModifiers {
    IrModifiers {
        visibility: IrVisibility::Public,
        is_static: true,
        ..IrModifiers::default()
    }
}

fn public_modifiers() -> IrModifiers {
    IrModifiers {
        visibility: IrVisibility::Public,
        ..IrModifiers::default()
    }
}

fn record_component(name: &str, java_type: JavaType) -> IrRecordComponent {
    IrRecordComponent {
        name: name.to_string(),
        java_type,
        span: dummy_span(),
    }
}

fn number_literal(value: &str) -> IrExpression {
    IrExpression::Literal(Literal::Number(value.to_string()), dummy_span())
}

#[test]
fn pattern_example1_type_patterns() {
    snapshot_program("pattern_example1_type_patterns", example1_program());
}

#[test]
fn pattern_example2_range_patterns() {
    snapshot_program("pattern_example2_range_patterns", example2_program());
}

#[test]
fn pattern_example5_destructuring_patterns() {
    snapshot_program(
        "pattern_example5_destructuring_patterns",
        example5_program(),
    );
}

#[test]
fn pattern_example1_java21_fallback_instanceof() {
    snapshot_java21_fallback(
        "pattern_example1_java21_fallback_instanceof",
        "// Java 21 fallback expands type patterns into instanceof chains guarded by __matched",
        example1_program(),
    );
}

#[test]
fn pattern_example2_java21_fallback_ranges() {
    snapshot_java21_fallback(
        "pattern_example2_java21_fallback_ranges",
        "// Java 21 fallback lowers range patterns into sequential guard checks with __matched",
        example2_program(),
    );
}

#[test]
fn pattern_example5_java21_fallback_depth2() {
    snapshot_java21_fallback(
        "pattern_example5_java21_fallback_depth2",
        "// Java 21 fallback handles nested destructuring via instanceof loop and do-while guard",
        example5_program(),
    );
}
