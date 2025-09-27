use std::fs;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use serde_json::Value;
use tempfile::tempdir;

const EMPTY_IR_JSON: &str = r#"
{
  "package": null,
  "imports": [],
  "type_declarations": [],
  "span": {
    "start_line": 0,
    "start_column": 0,
    "end_line": 0,
    "end_column": 0
  }
}
"#;

#[test]
fn debug_ir_emits_expected_json_artifact() {
    let Some(cli_path) = find_cli_binary() else {
        eprintln!("Skipping debug_ir test: CLI binary path not set");
        return;
    };

    let (_tmp, ir_path) = write_ir_fixture();

    let output = Command::new(&cli_path)
        .arg("debug")
        .arg("--stage")
        .arg("ir")
        .arg("--emit")
        .arg("ast")
        .arg("--input")
        .arg(&ir_path)
        .arg("--output")
        .arg("-")
        .arg("--format")
        .arg("json")
        .arg("--no-stats")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("failed to run CLI debug command");

    assert!(
        output.status.success(),
        "CLI exited with status {:?}: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8(output.stdout).expect("stdout must be UTF-8");
    let mut artifact: Value = serde_json::from_str(&stdout).expect("stdout should be JSON");
    normalize_elapsed(&mut artifact);

    let sanitized = serde_json::to_string_pretty(&artifact).expect("serialize sanitized JSON");
    let expected = include_str!("fixtures/debug_ir_expected.json").trim();

    assert_eq!(sanitized, expected, "sanitized JSON artifact mismatch");
    assert!(
        output.stderr.iter().all(u8::is_ascii_whitespace),
        "expected no stderr when --no-stats is set, got: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn debug_ir_prints_summary_footer() {
    let Some(cli_path) = find_cli_binary() else {
        eprintln!("Skipping debug_ir summary test: CLI binary path not set");
        return;
    };

    let (_tmp, ir_path) = write_ir_fixture();

    let output = Command::new(&cli_path)
        .arg("debug")
        .arg("--stage")
        .arg("ir")
        .arg("--emit")
        .arg("ast")
        .arg("--input")
        .arg(&ir_path)
        .arg("--output")
        .arg("-")
        .arg("--format")
        .arg("json")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("failed to run CLI debug command");

    assert!(
        output.status.success(),
        "CLI exited with status {:?}: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = String::from_utf8(output.stderr).expect("stderr must be UTF-8");
    let summary_lines: Vec<_> = stderr.lines().collect();

    assert!(summary_lines
        .iter()
        .any(|line| line.trim() == "=== 再構築サマリー ==="));
    assert!(summary_lines
        .iter()
        .any(|line| line.contains("合計ノード数: ")));
    assert!(summary_lines
        .iter()
        .any(|line| line.contains("復元ノード数: ")));
    assert!(summary_lines
        .iter()
        .any(|line| line.contains("プレースホルダー数: 0")));
    assert!(summary_lines
        .iter()
        .any(|line| line.starts_with("警告件数: 0")));
    assert!(summary_lines
        .iter()
        .any(|line| line.trim_start().starts_with("警告内訳")));
    assert!(summary_lines
        .iter()
        .any(|line| line.starts_with("経過時間: ")));

    // Also ensure stdout remains valid JSON even when stats footer is enabled.
    let stdout = String::from_utf8(output.stdout).expect("stdout must be UTF-8");
    let mut artifact: Value = serde_json::from_str(&stdout).expect("stdout should be JSON");
    normalize_elapsed(&mut artifact);
    let sanitized = serde_json::to_string_pretty(&artifact).expect("serialize sanitized JSON");
    let expected = include_str!("fixtures/debug_ir_expected.json").trim();
    assert_eq!(
        sanitized, expected,
        "JSON artifact should match golden snapshot"
    );
}

fn find_cli_binary() -> Option<PathBuf> {
    std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from)
}

fn write_ir_fixture() -> (tempfile::TempDir, PathBuf) {
    let tmp = tempdir().expect("create temp directory for IR fixture");
    let ir_path = tmp.path().join("fixture.ir.json");

    fs::write(&ir_path, EMPTY_IR_JSON).expect("write IR fixture to temp file");

    (tmp, ir_path)
}

fn normalize_elapsed(root: &mut Value) {
    if let Some(stats) = root.get_mut("stats") {
        if let Some(elapsed) = stats.get_mut("elapsed_millis") {
            *elapsed = Value::from(0);
        }
    }
}
