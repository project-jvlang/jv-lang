use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

const SAMPLE_MAIN: &str = r#"
fun message(): String {
    "cli integration"
}

fun main() {
    println(message())
}
"#;

struct TempDirGuard {
    path: PathBuf,
}

impl TempDirGuard {
    fn new(label: &str) -> std::io::Result<Self> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let path = std::env::temp_dir().join(format!("jv-cli-project-{}-{}", label, timestamp));
        fs::create_dir_all(&path)?;
        Ok(Self { path })
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

fn write_manifest(root: &Path) {
    fs::write(
        root.join("jv.toml"),
        r#"[package]
name = "cli-project-structure"
version = "0.1.0"

[build]
java_version = "25"

[project]
entrypoint = "src/app/main.jv"

[project.sources]
include = ["src/**/*.jv"]
exclude = ["src/excluded/**/*.jv"]

[project.output]
directory = "dist"
clean = false
"#,
    )
    .expect("manifest should be written");
}

fn create_sources(root: &Path) {
    let src_app = root.join("src/app");
    fs::create_dir_all(&src_app).expect("src/app directory");
    fs::write(src_app.join("main.jv"), SAMPLE_MAIN).expect("main source");

    let excluded = root.join("src/excluded");
    fs::create_dir_all(&excluded).expect("src/excluded directory");
    fs::write(excluded.join("skip.jv"), SAMPLE_MAIN).expect("excluded source");
}

fn run_cli(cli: &Path, workdir: &Path, args: &[&str]) -> std::process::Output {
    Command::new(cli)
        .current_dir(workdir)
        .args(args)
        .output()
        .expect("CLI command executes")
}

#[test]
fn cli_supports_multi_target_and_clean_workflows() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping CLI integration: CARGO_BIN_EXE_jv not available");
        return;
    };

    let fixture = TempDirGuard::new("multi-target-clean").expect("temp dir");
    let project_dir = fixture.path();
    write_manifest(project_dir);
    create_sources(project_dir);

    // Build from a nested directory to exercise ProjectLocator traversal.
    let nested_dir = project_dir.join("src/app");
    let output_21 = run_cli(
        &cli_path,
        &nested_dir,
        &["build", "--java-only", "--target", "21"],
    );
    assert!(
        output_21.status.success(),
        "CLI build --target 21 failed: {}\nstdout: {}\nstderr: {}",
        output_21.status,
        String::from_utf8_lossy(&output_21.stdout),
        String::from_utf8_lossy(&output_21.stderr)
    );
    let stdout_21 = String::from_utf8_lossy(&output_21.stdout);
    assert!(stdout_21.contains("Java21"));
    assert!(stdout_21.contains("出力ディレクトリ"));

    let generated_21 = project_dir.join("dist/java21/GeneratedMain.java");
    assert!(generated_21.exists(), "java21 output missing");
    let report_21 = project_dir.join("dist/java21/compatibility.json");
    assert!(report_21.exists(), "java21 compatibility report missing");

    // Build for Java25 to verify multi-target switching.
    let output_25 = run_cli(
        &cli_path,
        project_dir,
        &["build", "--java-only", "--target", "25"],
    );
    assert!(
        output_25.status.success(),
        "CLI build --target 25 failed: {}\nstdout: {}\nstderr: {}",
        output_25.status,
        String::from_utf8_lossy(&output_25.stdout),
        String::from_utf8_lossy(&output_25.stderr)
    );
    let stdout_25 = String::from_utf8_lossy(&output_25.stdout);
    assert!(stdout_25.contains("Java25"));
    let generated_25 = project_dir.join("dist/java25/GeneratedMain.java");
    assert!(generated_25.exists(), "java25 output missing");

    // Introduce a stale artifact and ensure --clean removes it.
    let stale_marker = project_dir.join("dist/java25/stale.log");
    fs::write(&stale_marker, "old").expect("stale marker");
    assert!(stale_marker.exists());

    let output_clean = run_cli(
        &cli_path,
        project_dir,
        &["build", "--java-only", "--target", "25", "--clean"],
    );
    assert!(
        output_clean.status.success(),
        "CLI build --clean failed: {}\nstdout: {}\nstderr: {}",
        output_clean.status,
        String::from_utf8_lossy(&output_clean.stdout),
        String::from_utf8_lossy(&output_clean.stderr)
    );
    let stdout_clean = String::from_utf8_lossy(&output_clean.stdout);
    assert!(stdout_clean.contains("クリーンビルド: 実行しました"));
    assert!(stdout_clean.contains("Output directory"));
    assert!(
        !stale_marker.exists(),
        "stale marker should be removed by --clean"
    );
    assert!(
        project_dir.join("dist/java25/GeneratedMain.java").exists(),
        "java25 output should be regenerated after --clean"
    );
}
