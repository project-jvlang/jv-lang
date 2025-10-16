use std::fs;
use std::path::PathBuf;
use std::process::Command;

use tempfile::tempdir;

#[test]
fn check_command_reports_conversion_telemetry() {
    let Some(cli_path): Option<PathBuf> = std::env::var_os("CARGO_BIN_EXE_jv").map(Into::into)
    else {
        eprintln!("Skipping telemetry output test: CLI binary unavailable");
        return;
    };

    let dir = tempdir().expect("create temp dir");
    let source_path = dir.path().join("conversion_helpers.jv");
    let source = r#"
fun convert(value: Int): String {
    return value
}

fun main(): Unit {
    val label: String = convert(42)
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
        "jv check should succeed with implicit conversions: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Conversion telemetry:"),
        "expected telemetry summary in output, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("string: 1"),
        "expected string conversion count in output, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("Helper recommendations:"),
        "expected helper recommendations header, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("java.lang.Integer#toString was applied implicitly"),
        "expected helper recommendation message, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("Implicit conversion diagnostics:"),
        "expected implicit conversion diagnostics section, got:\n{}",
        stdout
    );
    assert!(
        stdout.contains("JV_TYPE_003"),
        "expected conversion diagnostic code JV_TYPE_003, got:\n{}",
        stdout
    );
}
