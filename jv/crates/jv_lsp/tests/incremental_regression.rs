use std::fs;
use std::path::PathBuf;

use jv_lsp::ParserAdapter;
use jv_parser_frontend::Parser2Pipeline;
use serde_json::to_string;

fn fixture(name: &str) -> String {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/incremental")
        .join(name);
    fs::read_to_string(path).expect("fixture should exist")
}

fn assert_incremental_matches(uri: &str, base: &str, edited: &str) {
    let mut adapter = ParserAdapter::new();
    let full = Parser2Pipeline::default()
        .parse(edited)
        .expect("full parse should succeed");
    let incremental = adapter
        .parse_incremental(uri, base, edited)
        .expect("incremental parse should succeed");

    let full_program = full.into_program();
    let incremental_program = incremental.into_program();

    let full_json = to_string(&full_program).expect("serialize full program");
    let incremental_json = to_string(&incremental_program).expect("serialize incremental program");

    assert_eq!(
        full_json, incremental_json,
        "incremental parse should match full parse"
    );
}

#[test]
fn incremental_regression_small_edit() {
    let base = fixture("small_file_edit.jv");
    let edited = base.replace("value050", "value050 + 1");
    assert_incremental_matches("file:///small", &base, &edited);
}

#[test]
fn incremental_regression_medium_edit() {
    let base = fixture("medium_file_edit.jv");
    let edited = base.replace("metric250", "metric250 + metric249");
    assert_incremental_matches("file:///medium", &base, &edited);
}

#[test]
fn incremental_regression_block_insertion() {
    let base = fixture("block_insertion.jv");
    let edited = base.replace("LOG {", "LOG {\n        DEBUG { \"inserting\" }");
    assert_incremental_matches("file:///block", &base, &edited);
}
