use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use jv_cli::pipeline::compute_script_main_class;
use tempfile::tempdir;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../..")
        .canonicalize()
        .expect("ワークスペースルート")
}

fn sample_source_path() -> PathBuf {
    workspace_root().join("samples/regex/concise_command.jv")
}

fn has_javac() -> bool {
    Command::new("javac")
        .arg("-version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
}

fn has_java_runtime() -> bool {
    Command::new("java")
        .arg("-version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|status| status.success())
        .unwrap_or(false)
}

#[test]
fn concise_regex_sample_runs_end_to_end() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("jv バイナリが見つからないためテストをスキップします");
        return;
    };

    if !has_javac() || !has_java_runtime() {
        eprintln!("javac もしくは java が利用できないためテストをスキップします");
        return;
    }

    let sample = sample_source_path();
    assert!(
        sample.exists(),
        "サンプルソース {} が存在しません",
        sample.display()
    );

    let workspace = tempdir().expect("一時ディレクトリ作成");
    let main_path = workspace.path().join("main.jv");
    fs::copy(&sample, &main_path).expect("サンプルソースをコピー");
    let output_dir = workspace.path().join("out");
    let main_class = compute_script_main_class("", &main_path);

    let status = Command::new(&cli_path)
        .arg("build")
        .arg(&main_path)
        .arg("-o")
        .arg(&output_dir)
        .status()
        .expect("jv build 実行");
    assert!(status.success(), "jv build が失敗しました: {:?}", status);

    let class_file = output_dir.join(format!("{}.class", main_class));
    assert!(
        class_file.exists(),
        "{} が生成されていません",
        class_file.display()
    );

    let run_output = Command::new("java")
        .arg("-cp")
        .arg(&output_dir)
        .arg(&main_class)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("java 実行");
    assert!(
        run_output.status.success(),
        "Java 実行が失敗しました: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let stdout = String::from_utf8(run_output.stdout).expect("標準出力はUTF-8");
    let expected = "\
replaceAll結果: alice@example.dev,bob@example.dev,carol@example.dev
先頭置換: ALICE@example.com,bob@example.org,carol@example.net
全体マッチ: true
split結果:
  alice@example.com
  bob@example.org
  carol@example.net
iterate結果:
  alice -> example.com
  bob -> example.org
  carol -> example.net";

    assert_eq!(
        stdout.trim(),
        expected.trim(),
        "サンプル実行結果が想定と一致しません"
    );
}
