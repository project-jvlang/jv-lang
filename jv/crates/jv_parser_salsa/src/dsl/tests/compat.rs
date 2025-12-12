use super::lower_source;
use crate::pipeline::{ParseOptions, SalsaPipeline};

#[test]
fn salsa_parses_log_block_program() {
    let source = r#"LOG { "compat" }"#;
    let salsa = SalsaPipeline::new_without_jdk();

    let salsa_program = salsa
        .execute_with_options(source, ParseOptions::default())
        .expect("salsa pipeline succeeds")
        .artifacts
        .program;

    assert_eq!(salsa_program.statements.len(), 1, "one statement emitted");
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
