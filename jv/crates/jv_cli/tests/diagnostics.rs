use std::fs;
use std::path::PathBuf;
use std::process::Command;

use tempfile::tempdir;

#[test]
fn check_command_reports_raw_type_warning() {
    let Some(cli_path): Option<PathBuf> = std::env::var_os("CARGO_BIN_EXE_jv").map(Into::into)
    else {
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

#[test]
fn check_command_reports_raw_allow_info() {
    let Some(cli_path): Option<PathBuf> = std::env::var_os("CARGO_BIN_EXE_jv").map(Into::into)
    else {
        eprintln!("Skipping raw allow diagnostic test: CLI binary unavailable");
        return;
    };

    let dir = tempdir().expect("create temp dir");
    let source_path = dir.path().join("raw_allow.jv");
    let source = r#"
fun main(): Unit {
    val xs = [] // jv:raw-allow demo.Value
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
        "jv check should succeed even with raw-allow comments"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("JV3203"),
        "expected JV3203 diagnostic in output, got:\n{}",
        stdout
    );
}

#[test]
fn check_command_reports_null_safety_conflict_for_when_block() {
    let Some(cli_path): Option<PathBuf> = std::env::var_os("CARGO_BIN_EXE_jv").map(Into::into)
    else {
        eprintln!("Skipping null safety diagnostic test: CLI binary unavailable");
        return;
    };

    let dir = tempdir().expect("create temp dir");
    let source_path = dir.path().join("when_block_null_conflict.jv");
    let source = r#"
fun main(): Unit {
    val token: String = "value"

    val label = when (token) {
        null -> {
            "missing"
        }
        else -> {
            val trimmed = token.trim()
            trimmed
        }
    }

    val length = label.length
    println(length)
}
"#;
    fs::write(&source_path, source.trim_start()).expect("write source file");

    let output = Command::new(cli_path)
        .arg("check")
        .arg(&source_path)
        .output()
        .expect("invoke jv check");

    assert!(
        !output.status.success(),
        "jv check should fail when null safety detects unreachable null branch"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}{}", stdout, stderr);

    assert!(
        combined.contains("JV3108"),
        "expected JV3108 diagnostic in CLI output, got:\n{}",
        combined
    );
    assert!(
        combined.contains("token"),
        "expected diagnostic to reference `token`, got:\n{}",
        combined
    );
}

#[test]
fn check_command_reports_label_error() {
    let Some(cli_path): Option<PathBuf> = std::env::var_os("CARGO_BIN_EXE_jv").map(Into::into)
    else {
        eprintln!("Skipping label diagnostic test: CLI binary unavailable");
        return;
    };

    let dir = tempdir().expect("create temp dir");
    let source_path = dir.path().join("label_error.jv");
    let source = r#"
fun main(): Unit {
    val values = [1 2 3]
    #outer for value in values {
        break #missing
    }
}
"#;
    fs::write(&source_path, source.trim_start()).expect("write label error source");

    let output = Command::new(cli_path)
        .arg("check")
        .arg(&source_path)
        .output()
        .expect("invoke jv check for label error");

    assert!(
        !output.status.success(),
        "未定義ラベルを含む場合、jv check は失敗すべきです"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}{}", stdout, stderr);

    assert!(
        combined.contains("E-LABEL-UNDEFINED"),
        "CLI出力に E-LABEL-UNDEFINED が含まれるべきです:\n{}",
        combined
    );
}
