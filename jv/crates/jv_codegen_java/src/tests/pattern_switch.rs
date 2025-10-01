use super::*;
use insta::assert_snapshot;
use jv_ir::{IrExpression, IrProgram, IrStatement, JavaType};
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

fn example1_program() -> IrProgram {
    let object_type = reference_type("Object");
    let string = string_type();
    let int = int_type();

    let discriminant = Box::new(ir_identifier("x", &object_type));

    let string_case = switch_case(
        vec![IrCaseLabel::TypePattern {
            type_name: "String".to_string(),
            variable: "value".to_string(),
        }],
        None,
        IrExpression::MethodCall {
            receiver: Some(Box::new(ir_identifier("value", &string))),
            method_name: "length".to_string(),
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
        parameters: vec![IrParameter {
            name: "x".to_string(),
            java_type: object_type,
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
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
        parameters: vec![IrParameter {
            name: "score".to_string(),
            java_type: int_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
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
        span: dummy_span(),
    }
}

#[test]
fn pattern_example1_type_patterns() {
    snapshot_program("pattern_example1_type_patterns", example1_program());
}

#[test]
fn pattern_example2_range_patterns() {
    snapshot_program("pattern_example2_range_patterns", example2_program());
}
