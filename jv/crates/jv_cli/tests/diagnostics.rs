use std::fs;
use std::process::Command;

use tempfile::tempdir;

#[test]
fn check_command_reports_raw_type_warning() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(Into::into) else {
        eprintln!("Skipping raw type diagnostic test: CLI binary unavailable");
        return;
    };

    let dir = tempdir().expect("create temp dir");
    let source_path = dir.path().join("raw_comment.jv");
    let source = r#"
fun main(): Unit {
    val xs = [] // jv:raw-default demo.Value
}
"#;
    fs::write(&source_path, source.trim_start()).expect("write source file");

    let output = Command::new(cli_path)
        .arg("check")
        .arg(&source_path)
        .output()
        .expect("invoke jv check");

    assert!(
        output.status.success(),
        "jv check should succeed even with warnings"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("JV3202"),
        "expected JV3202 diagnostic in output, got:\n{}",
        stdout
    );
}
