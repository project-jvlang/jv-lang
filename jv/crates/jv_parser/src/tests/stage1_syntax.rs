use super::support::{first_statement, parse_program};
use jv_ast::{
    Argument, CallArgumentStyle, Expression, Literal, Pattern, SequenceDelimiter, Statement,
};

#[test]
fn call_expression_preserves_whitespace_argument_style() {
    let program = parse_program("val result = sum(1 2 3)");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call {
                args,
                argument_metadata,
                ..
            } => {
                assert_eq!(args.len(), 3, "expected three positional arguments");
                for argument in args {
                    match argument {
                        Argument::Positional(_) => {}
                        other => panic!("expected positional argument, found {:?}", other),
                    }
                }
                assert_eq!(
                    argument_metadata.style,
                    CallArgumentStyle::Whitespace,
                    "Stage0 should promote whitespace-separated arguments into layout metadata"
                );
                assert!(
                    !argument_metadata.used_commas,
                    "metadata should record that commas were not used"
                );
            }
            other => panic!("expected call expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn when_expression_keeps_branch_order_and_else_arm() {
    let source = r#"
        val description = when (value) {
            1 -> "one"
            2 -> "two"
            else -> "other"
        }
    "#;
    let program = parse_program(source);
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::When { arms, else_arm, .. } => {
                assert_eq!(arms.len(), 2, "expected two explicit arms");
                let patterns: Vec<String> = arms
                    .iter()
                    .map(|arm| match &arm.pattern {
                        Pattern::Literal(Literal::Number(value), _) => value.clone(),
                        other => panic!("unexpected pattern {:?}", other),
                    })
                    .collect();
                assert_eq!(
                    patterns,
                    vec!["1".to_string(), "2".to_string()],
                    "arm order should be preserved"
                );
                let else_branch = else_arm
                    .as_deref()
                    .expect("else branch should be materialised");
                match else_branch {
                    Expression::Literal(Literal::String(value), _) => {
                        assert_eq!(value, "other", "else branch should match literal payload");
                    }
                    other => panic!("unexpected else arm {:?}", other),
                }
            }
            other => panic!("expected when expression, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn array_literal_records_whitespace_delimiter() {
    let program = parse_program("val numbers = [1 2 3]");
    let statement = first_statement(&program);

    match statement {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Array {
                elements,
                delimiter,
                ..
            } => {
                assert_eq!(elements.len(), 3);
                assert_eq!(
                    *delimiter,
                    SequenceDelimiter::Whitespace,
                    "layout-aware array literals should record whitespace delimiter metadata"
                );
            }
            other => panic!("expected array literal, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}
