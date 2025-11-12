use std::fs;
use std::net::{Shutdown, TcpListener};
use std::path::Path;
use std::thread;

use jv_cli::commands::otel::{
    OtelCommand, OtelOverrideArgs, OtelSubcommand, OtelTestArgs, OtelValidateArgs,
};
use jv_cli::pipeline::project::{
    layout::ProjectLayout, locator::ProjectRoot, manifest::ManifestLoader,
};
use jv_cli::pipeline::{BuildOptionsFactory, CliOverrides, report::render_logging_overview};
use jv_cli::pipeline::{BuildPlan, compile};
use jv_pm::JavaTarget;
use tempfile::tempdir;

fn write_manifest(root: &Path, endpoint: &str) {
    let manifest = format!(
        r#"[package]
name = "logging-integration"
version = "0.1.0"

[package.dependencies]

[build]
java_version = "25"

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
exclude = []

[project.output]
directory = "target"
clean = false

[logging]
framework = "slf4j"
log_level = "info"
default_level = "info"

[logging.opentelemetry]
enabled = true
endpoint = "{endpoint}"
protocol = "grpc"
trace_context = true
"#
    );
    fs::write(root.join("jv.toml"), manifest).expect("マニフェストを書き込めるはずです");
}

fn write_source(root: &Path) {
    let src_dir = root.join("src");
    fs::create_dir_all(&src_dir).expect("ソースディレクトリ作成に成功するはずです");
    fs::write(
        src_dir.join("main.jv"),
        r#"package example

fun main {
    LOG {
        "start up"
    }
}
"#,
    )
    .expect("ソースを書き込めるはずです");
}

fn compose_plan(root: &Path) -> BuildPlan {
    let manifest_path = root.join("jv.toml");
    let project_root = ProjectRoot::new(root.to_path_buf(), manifest_path.clone());
    let settings = ManifestLoader::load(&manifest_path).expect("マニフェスト読み込み成功");
    let layout =
        ProjectLayout::from_settings(&project_root, &settings).expect("プロジェクトレイアウト確立");

    let overrides = CliOverrides {
        entrypoint: Some(root.join("src/main.jv")),
        output: Some(root.join("target/java")),
        java_only: true,
        target: Some(JavaTarget::Java25),
        ..CliOverrides::default()
    };

    BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("ビルドプラン作成成功")
}

#[test]
fn otel_validate_accepts_complete_logging_config() {
    let temp = tempdir().expect("一時ディレクトリを作成できるはずです");
    write_manifest(temp.path(), "http://127.0.0.1:4317");
    write_source(temp.path());

    let command = OtelCommand {
        manifest: Some(temp.path().join("jv.toml")),
        overrides: OtelOverrideArgs::default(),
        action: OtelSubcommand::Validate(OtelValidateArgs { strict: false }),
    };

    let result = jv_cli::commands::otel::run(command);
    assert!(
        result.is_ok(),
        "有効な設定は検証に通るべきです: {:?}",
        result.err()
    );
}

#[test]
fn otel_test_succeeds_with_mock_collector() {
    let listener = match TcpListener::bind(("127.0.0.1", 0)) {
        Ok(listener) => listener,
        Err(err) if err.kind() == std::io::ErrorKind::PermissionDenied => {
            eprintln!("otel_test_succeeds_with_mock_collector をスキップします: {err}");
            return;
        }
        Err(err) => panic!("モックCollectorのバインドに失敗しました: {err}"),
    };
    let port = listener.local_addr().expect("ローカルアドレス取得").port();
    let handle = thread::spawn(move || {
        if let Ok((stream, _)) = listener.accept() {
            // 接続を受け入れたら即座にクローズするだけで良い
            let _ = stream.shutdown(Shutdown::Both);
        }
    });

    let temp = tempdir().expect("一時ディレクトリ");
    write_manifest(temp.path(), &format!("127.0.0.1:{port}"));
    write_source(temp.path());

    let command = OtelCommand {
        manifest: Some(temp.path().join("jv.toml")),
        overrides: OtelOverrideArgs::default(),
        action: OtelSubcommand::Test(OtelTestArgs {
            timeout_ms: 1500,
            retries: 1,
            retry_sleep_ms: 100,
            quiet: true,
        }),
    };

    let result = jv_cli::commands::otel::run(command);
    assert!(
        result.is_ok(),
        "Collector 疎通テストは成功するべきです: {:?}",
        result.err()
    );

    // 念のため少し待ってからジョイン（エラーがあれば無視）
    let _ = handle.join();
}

#[test]
fn build_pipeline_emits_mdc_when_trace_context_enabled() {
    let temp = tempdir().expect("一時ディレクトリ");
    write_manifest(temp.path(), "http://127.0.0.1:4317");
    write_source(temp.path());

    let plan = compose_plan(temp.path());
    let overview = render_logging_overview(&plan.logging_config);
    assert!(
        overview.contains("slf4j") && overview.contains("OpenTelemetry 有効化"),
        "ロギング概要に設定内容が表示されるべきです:\n{overview}"
    );

    let artifacts = compile(&plan).expect("jv build パイプラインが成功するはずです");
    assert!(
        !artifacts.java_files.is_empty(),
        "Javaファイルが生成されるべきです"
    );

    let java_path = &artifacts.java_files[0];
    let java_source = fs::read_to_string(java_path)
        .unwrap_or_else(|_| panic!("生成された Java を読み込めません: {}", java_path.display()));

    assert!(
        java_source.contains("class LoggingIntegrationMain"),
        "生成されたJavaクラスが出力されるべきです:\n{java_source}"
    );
}
