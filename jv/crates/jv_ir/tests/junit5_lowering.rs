use jv_ir::error::{TestLoweringDiagnostic, TransformError};
use jv_ir::types::{IrAnnotation, IrModifiers, IrStatement, JavaType};
use jv_ir::{AssertionPattern, TransformContext};
use jv_ir::{IrProgram, transform::transform_program_with_context};
use jv_parser_frontend::{Parser2Pipeline, ParserPipeline};

fn lower_program(source: &str) -> Result<IrProgram, TransformError> {
    let program = Parser2Pipeline::default()
        .parse(source)
        .expect("test fixtures should parse")
        .into_program();
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context)
}

fn extract_class<'a>(program: &'a IrProgram) -> &'a IrStatement {
    program
        .type_declarations
        .first()
        .expect("expected at least one type declaration")
}

fn collect_methods<'a>(class: &'a IrStatement) -> Vec<&'a IrStatement> {
    match class {
        IrStatement::ClassDeclaration { methods, .. } => methods.iter().collect(),
        other => panic!("expected class declaration, got {other:?}"),
    }
}

fn has_annotation(modifiers: &IrModifiers, simple: &str) -> bool {
    modifiers
        .annotations
        .iter()
        .any(|annotation: &IrAnnotation| annotation.name.simple_name() == simple)
}

#[test]
fn lowers_basic_test_into_suite_with_assertions() {
    let source = r#"
        test "basic equality" {
            val result = 40 + 2
            result == 42
        }
    "#;

    let program = lower_program(source).expect("lowering should succeed");
    let class = extract_class(&program);
    let methods = collect_methods(class);
    assert_eq!(methods.len(), 1, "simple test should emit a single method");

    let IrStatement::MethodDeclaration {
        modifiers,
        assertion_patterns,
        ..
    } = methods[0]
    else {
        panic!("expected method declaration");
    };

    assert!(
        has_annotation(modifiers, "Test"),
        "JUnit @Test annotation should be attached"
    );
    assert!(
        has_annotation(modifiers, "DisplayName"),
        "DisplayName annotation should preserve the original label"
    );
    assert_eq!(
        assertion_patterns.len(),
        1,
        "arithmetic equality should register an assertion pattern"
    );
    assert!(
        matches!(assertion_patterns[0], AssertionPattern::Equals { .. }),
        "equality comparisons should map to AssertionPattern::Equals"
    );
}

#[test]
fn lowers_parameterized_test_with_dataset_provider() {
    let source = r#"
        test "addition dataset" [
            ["carry" 11 17 28]
            ["negative" -5 3 -2]
        ] (label: String, lhs: Int, rhs: Int, expected: Int) {
            val sum = lhs + rhs
            sum == expected
        }
    "#;

    let program = lower_program(source).expect("lowering should succeed");
    let class = extract_class(&program);
    let methods = collect_methods(class);
    assert_eq!(
        methods.len(),
        2,
        "parameterized tests should emit the test body and its provider"
    );

    let (test_method, provider_method) =
        if let IrStatement::MethodDeclaration { modifiers, .. } = methods[0] {
            if has_annotation(modifiers, "ParameterizedTest") {
                (methods[0], methods[1])
            } else {
                (methods[1], methods[0])
            }
        } else {
            (methods[1], methods[0])
        };

    if let IrStatement::MethodDeclaration {
        modifiers,
        assertion_patterns,
        ..
    } = test_method
    {
        assert!(has_annotation(modifiers, "ParameterizedTest"));
        assert!(has_annotation(modifiers, "MethodSource"));
        assert!(
            matches!(
                assertion_patterns.as_slice(),
                [AssertionPattern::Equals { .. }]
            ),
            "dataset-driven test should classify assertions"
        );
    } else {
        panic!("expected test method declaration");
    }

    if let IrStatement::MethodDeclaration {
        modifiers,
        return_type,
        ..
    } = provider_method
    {
        assert!(
            modifiers.is_static,
            "dataset provider should be emitted as a static helper"
        );
        if let JavaType::Reference { name, .. } = return_type {
            assert_eq!(
                name, "java.util.stream.Stream",
                "provider should return a Stream<Arguments>"
            );
        } else {
            panic!("provider return type should be a Stream, got {return_type:?}");
        }
    } else {
        panic!("expected provider method declaration");
    }
}

#[test]
fn reports_column_mismatches_as_jv5301() {
    let source = r#"
        test "broken dataset" [
            [1]
            [2]
        ] (lhs: Int, rhs: Int) {
            (lhs + rhs) == 0
        }
    "#;

    let error = lower_program(source).expect_err("mismatched dataset should fail");
    match error {
        TransformError::TestLoweringError { code, details, .. } => {
            assert_eq!(code, "JV5301");
            match details.expect("dataset mismatch should include structured details") {
                TestLoweringDiagnostic::DatasetColumnMismatch {
                    parameter_count,
                    column_count,
                } => {
                    assert_eq!(parameter_count, 2);
                    assert_eq!(column_count, 1);
                }
                other => panic!("unexpected diagnostic payload: {other:?}"),
            }
        }
        other => panic!("expected JV5301 lowering error, got {other:?}"),
    }
}

#[test]
fn reports_assertion_misuse_as_jv5305() {
    let source = r#"
        test "missing assertion" {
            val computed = 10 + 5
            computed
        }
    "#;

    let error = lower_program(source).expect_err("non-boolean expression should fail");
    match error {
        TransformError::TestLoweringError { code, .. } => {
            assert_eq!(code, "JV5305");
        }
        other => panic!("expected JV5305 assertion rewrite error, got {other:?}"),
    }
}

#[test]
fn reports_sample_driven_tests_as_unimplemented() {
    let source = r#"
        test "external sample" [@Sample("cases.json", mode = SampleMode.Load)] (row) {
            row != null
        }
    "#;

    let error = lower_program(source).expect_err("@Sample dataset lowering is not implemented yet");
    match error {
        TransformError::TestLoweringError { code, .. } => {
            assert_eq!(code, "JV5306");
        }
        other => panic!("expected JV5306 sample error, got {other:?}"),
    }
}
