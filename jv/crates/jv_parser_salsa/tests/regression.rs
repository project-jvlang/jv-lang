use jv_parser_salsa::pipeline::{ParseOptions, SalsaPipeline};

#[test]
fn parses_simple_program_without_errors() {
    let source = "package regression\nval x = 1\nval y = x + 2\n";
    let pipeline = SalsaPipeline::new_without_jdk();
    let output = pipeline
        .execute_with_options(
            source,
            ParseOptions {
                generate_cst: true,
                generate_trivia_map: true,
                trim_trivia_and_metadata: false,
            },
        )
        .expect("pipeline should succeed");
    assert!(
        output.artifacts.diagnostics.final_diagnostics().is_empty(),
        "expected no diagnostics"
    );
}
