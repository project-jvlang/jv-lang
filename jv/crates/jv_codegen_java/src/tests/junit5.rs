use super::{JavaCodeGenConfig, JavaCodeGenerator, parse_program};

fn java_source_for(source: &str) -> String {
    let ir_program = parse_program(source);
    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&ir_program)
        .expect("code generation should succeed for fixtures");
    unit.to_source(&JavaCodeGenConfig::default())
}

#[test]
fn parameterized_test_emits_matching_provider() {
    let source = r#"
        test "dataset addition" [
            ["carry" 11 17 28]
            ["negative" -5 3 -2]
        ] (label: String, lhs: Int, rhs: Int, expected: Int) {
            val sum = lhs + rhs
            sum == expected
        }
    "#;

    let java_source = java_source_for(source);
    let method_source_line = java_source
        .lines()
        .find(|line| line.contains("@MethodSource"))
        .expect("MethodSource annotation should be rendered");
    let provider_name = method_source_line
        .split('"')
        .nth(1)
        .expect("annotation literal should include provider name");

    assert!(
        java_source.contains("@ParameterizedTest"),
        "ParameterizedTest annotation should be emitted:\n{java_source}"
    );
    assert!(
        java_source.contains("@MethodSource"),
        "MethodSource annotation should reference provider:\n{java_source}"
    );
    assert!(
        java_source.contains("Stream.of"),
        "dataset provider should materialize Stream.of(...):\n{java_source}"
    );
    assert!(
        java_source.contains("Arguments.of(\"carry\", 11, 17, 28)"),
        "dataset rows should lower into Arguments.of entries:\n{java_source}"
    );
    let provider_signature = format!(
        "static java.util.stream.Stream<org.junit.jupiter.params.provider.Arguments> {}()",
        provider_name
    );
    assert!(
        java_source.contains(&provider_signature),
        "provider method '{provider_name}' should declare a Stream<Arguments> signature:\n{java_source}"
    );
}

#[test]
fn suite_with_mixed_tests_emits_single_class() {
    let source = r#"
        test "simple equality" {
            val answer = 40 + 2
            answer == 42
        }

        test "labeled cases" [
            ["carry" 11 17 28]
        ] (label: String, lhs: Int, rhs: Int, expected: Int) {
            val sum = lhs + rhs
            sum == expected
        }
    "#;

    let java_source = java_source_for(source);
    assert!(
        java_source.matches("@Test").count() == 1,
        "inline test should include exactly one @Test annotation:\n{java_source}"
    );
    assert!(
        java_source.matches("@ParameterizedTest").count() == 1,
        "dataset test should include a single @ParameterizedTest annotation:\n{java_source}"
    );
    assert!(
        java_source.contains("@DisplayName(\"simple equality\")"),
        "display names should be preserved for basic tests:\n{java_source}"
    );
    assert!(
        java_source.contains("@DisplayName(\"labeled cases\")"),
        "display names should be preserved for parameterized tests:\n{java_source}"
    );
    assert!(
        java_source.contains("Assertions.assertEquals"),
        "rewritten equality assertions should call Assertions.assertEquals:\n{java_source}"
    );
}
