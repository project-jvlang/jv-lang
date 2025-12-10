use super::lower_source;
use crate::pipeline::{ParseOptions, SalsaPipeline};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

#[test]
fn salsa_and_rowan_produce_compatible_log_block_programs() {
    let source = r#"LOG { "compat" }"#;
    let salsa = SalsaPipeline::new();
    let rowan = RowanPipeline::new();

    let salsa_program = salsa
        .execute_with_options(source, ParseOptions::default())
        .expect("salsa pipeline succeeds")
        .artifacts
        .program;
    let rowan_program = rowan
        .execute(source)
        .expect("rowan pipeline succeeds")
        .program;

    assert_eq!(
        salsa_program.statements.len(),
        rowan_program.statements.len(),
        "statement count should match"
    );
}

#[test]
fn salsa_lowers_use_block_compatibly() {
    let lowered = lower_source("use res { }");
    assert!(
        lowered
            .statements
            .iter()
            .any(|stmt| matches!(stmt, jv_ast::statement::Statement::ResourceManagement(_))),
        "expected at least one resource management statement"
    );
}
