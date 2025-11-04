use jv_ast::annotation::{AnnotationArgument, AnnotationValue};
use jv_ast::expression::Expression;
use jv_ast::statement::{Statement, TestDataset, TestDeclaration};
use jv_ast::{BindingPatternKind, Literal, Span};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

fn parse_tests(source: &str) -> Vec<TestDeclaration> {
    let pipeline = RowanPipeline::default();
    let program = pipeline
        .parse(source)
        .expect("test fixture source should parse")
        .into_program();

    program
        .statements
        .into_iter()
        .filter_map(|statement| match statement {
            Statement::TestDeclaration(declaration) => Some(declaration),
            _ => None,
        })
        .collect()
}

#[test]
fn parses_basic_test_declaration() {
    let tests = parse_tests(
        r#"
        test "adds numbers" {
            val result = 1 + 1
            result == 2
        }
    "#,
    );

    assert_eq!(tests.len(), 1, "expected a single TestDeclaration");
    let declaration = &tests[0];
    assert_eq!(declaration.display_name, "adds numbers");
    assert!(
        declaration.dataset.is_none(),
        "simple test should not infer a dataset"
    );
    assert!(
        declaration.parameters.is_empty(),
        "non-parameterized tests should not declare parameters"
    );
    assert!(
        matches!(declaration.body, Expression::Block { .. }),
        "test body should remain a block expression"
    );
    assert_ne!(
        declaration.span,
        Span::dummy(),
        "spans should reflect the original source location"
    );
}

#[test]
fn parses_inline_dataset_with_parameters() {
    let tests = parse_tests(
        r#"
        test "dataset addition" [
            ["carry" 11 17 28]
            ["negative" -5 3 -2]
        ] (label: String, lhs: Int, rhs: Int, expected: Int) {
            val sum = lhs + rhs
            sum == expected
        }
    "#,
    );

    assert_eq!(tests.len(), 1, "expected a single TestDeclaration");
    let declaration = &tests[0];
    let dataset = declaration
        .dataset
        .as_ref()
        .expect("inline dataset should be captured");
    match dataset {
        TestDataset::InlineArray { rows, .. } => {
            assert_eq!(rows.len(), 2, "two dataset rows should be parsed");
            assert_eq!(
                rows[0].values.len(),
                4,
                "each inline dataset row keeps all values"
            );
        }
        other => panic!("expected inline dataset, got {other:?}"),
    }
    assert_eq!(
        declaration.parameters.len(),
        4,
        "parameter list must mirror dataset column count"
    );
    for parameter in &declaration.parameters {
        assert!(
            matches!(parameter.pattern, BindingPatternKind::Identifier { .. }),
            "lowering relies on identifier parameters, got {:?}",
            parameter.pattern
        );
    }
}

#[test]
fn parses_sample_based_dataset_metadata() {
    let tests = parse_tests(
        r#"
        test "external cases" [@Sample("cases.json", mode = SampleMode.Load, region = "ap-northeast-1")] (row) {
            row != null
        }
    "#,
    );

    assert_eq!(tests.len(), 1, "expected a single TestDeclaration");
    let declaration = &tests[0];
    let dataset = declaration
        .dataset
        .as_ref()
        .expect("sample dataset should be present");
    let metadata = match dataset {
        TestDataset::Sample(metadata) => metadata,
        other => panic!("expected @Sample dataset, got {other:?}"),
    };
    assert_eq!(metadata.source, "cases.json");
    assert!(
        !metadata.arguments.is_empty(),
        "@Sample arguments should be preserved"
    );
    let mut seen_mode = false;
    for argument in &metadata.arguments {
        match argument {
            AnnotationArgument::Named { name, value, .. } if name == "mode" => {
                seen_mode = true;
                match value {
                    AnnotationValue::EnumConstant { constant, .. } => {
                        assert_eq!(constant, "Load");
                    }
                    other => panic!("expected enum constant for mode, got {other:?}"),
                }
            }
            AnnotationArgument::Named { name, value, .. } if name == "region" => match value {
                AnnotationValue::Literal(Literal::String(region)) => {
                    assert_eq!(region, "ap-northeast-1");
                }
                other => panic!("expected string literal for region, got {other:?}"),
            },
            _ => {}
        }
    }
    assert!(seen_mode, "mode argument should be parsed and retained");
}
