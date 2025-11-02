use std::fs;
use std::path::PathBuf;
use std::process::Command;

use tempfile::tempdir;

#[test]
fn doublebrace_control_flow_error_is_reported() {
    // Doublebrace ブロックで return を使用した場合に CLI が診断を出力することを確認する。
    let Some(cli_path): Option<PathBuf> = std::env::var_os("CARGO_BIN_EXE_jv").map(Into::into)
    else {
        eprintln!("jv バイナリが存在しないためテストをスキップします");
        return;
    };

    let dir = tempdir().expect("一時ディレクトリを作成する");
    let source_path = dir.path().join("doublebrace_return.jv");
    let source = r#"
import java.util.ArrayList

fun main(): Unit {
    val list = ArrayList() {{
        add(1)
        return
    }}
}
"#;
    fs::write(&source_path, source.trim_start())
        .expect("Doublebrace 用のサンプルソースを書き込む");

    let output = Command::new(cli_path)
        .arg("check")
        .arg(&source_path)
        .output()
        .expect("jv check を実行する");

    assert!(
        !output.status.success(),
        "制御フロー違反を含むため失敗ステータスになる想定"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}\n{}", stdout, stderr);

    assert!(
        combined.contains("E-DBLOCK-RETURN"),
        "Doublebrace の return 診断コードが出力されること: {}",
        combined
    );
    assert!(
        combined.contains("Doublebrace 初期化ブロックでは `return` 文を使用できません。"),
        "日本語メッセージが含まれること: {}",
        combined
    );
}
