use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::tempdir;

use jv_cli::pipeline::project::{
    layout::ProjectLayout, locator::ProjectLocator, manifest::ManifestLoader, output::OutputManager,
};
use jv_cli::pipeline::{BuildOptionsFactory, CliOverrides, compile};
use jv_pm::JavaTarget;

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
    fs::write(&source_path, source.trim_start()).expect("Doublebrace 用のサンプルソースを書き込む");

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

#[test]
fn doublebrace_cli_check_handles_example() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let example_path = manifest_dir
        .join("../../examples/doublebrace-init-block/src/main.jv")
        .canonicalize()
        .expect("Doublebrace のサンプルを解決する");
    let example = example_path
        .to_str()
        .expect("Doublebrace サンプルのパスは UTF-8 で表現できる必要がある");

    jv_cli::commands::check::run(example)
        .expect("バンドルされている Doublebrace サンプルで jv check が成功すること");
}

#[test]
fn doublebrace_pipeline_recovers_when_type_check_is_skipped() {
    let fixture = tempdir().expect("Doublebrace 用の一時ディレクトリを作成する");
    let root = fixture.path();
    prepare_manifest(root);

    let src_dir = root.join("src");
    fs::create_dir_all(&src_dir).expect("ソースディレクトリを作成する");
    let entrypoint = src_dir.join("main.jv");
    fs::write(
        &entrypoint,
        doublebrace_sample_with_type_error().trim_start(),
    )
    .expect("型エラーを含む Doublebrace サンプルコードを書き込む");

    let project_root = ProjectLocator::new()
        .locate(root)
        .expect("プロジェクトルートを解決する");
    let manifest_path = project_root.manifest_path().to_path_buf();
    let settings = ManifestLoader::load(&manifest_path).expect("manifest を読み込む");
    let layout =
        ProjectLayout::from_settings(&project_root, &settings).expect("レイアウトを構築する");

    let mut overrides = CliOverrides::default();
    overrides.output = Some(project_root.join("dist"));
    overrides.java_only = true;
    overrides.check = false;
    overrides.target = Some(JavaTarget::Java25);

    let plan = BuildOptionsFactory::compose(project_root.clone(), settings, layout, overrides)
        .expect("ビルドプランを組み立てる");

    let mut prepared = OutputManager::prepare(plan).expect("出力ディレクトリを準備する");
    let artifacts =
        compile(prepared.plan()).expect("型エラーを含んでも Doublebrace プランが補完される");
    prepared.mark_success();

    assert!(
        artifacts
            .warnings
            .iter()
            .any(|warning| warning.contains("Type checking skipped")),
        "型チェックを省略した警告が発生すること: {:?}",
        artifacts.warnings
    );
    assert!(
        artifacts
            .warnings
            .iter()
            .all(|warning| !warning.contains("Doublebrace 初期化式のプラン情報が見つかりません")),
        "フォールバックにより Doublebrace プラン不足が解消されていること: {:?}",
        artifacts.warnings
    );
}

fn prepare_manifest(root: &Path) {
    let manifest = r#"[package]
name = "doublebrace-cli"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]

[project.output]
directory = "dist"
clean = true
"#;
    fs::write(root.join("jv.toml"), manifest.trim_start()).expect("manifest を書き込む");
}

fn doublebrace_sample_with_type_error() -> &'static str {
    r#"
import java.util.ArrayList

fun main(): Unit {
    val menu = ArrayList<String>() {{
        add("季節のスープ")
    }}

    val mismatch: Int = "not a number"
}
"#
}
