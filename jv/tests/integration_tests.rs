use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

use jv_cli::pipeline::{compile, BuildOptions};

struct TempDirGuard {
    path: PathBuf,
}

impl TempDirGuard {
    fn new(prefix: &str) -> std::io::Result<Self> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let path = std::env::temp_dir().join(format!("jv-integration-{}-{}", prefix, timestamp));
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

fn workspace_file(relative: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join(relative)
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

#[test]
fn cli_build_generates_java_sources() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping CLI binary integration test: CARGO_BIN_EXE_jv not set");
        return;
    };

    let temp_dir = TempDirGuard::new("cli-build").expect("Failed to create temp dir");
    let input = workspace_file("test_simple.jv");

    let status = Command::new(&cli_path)
        .arg("build")
        .arg(&input)
        .arg("--java-only")
        .arg("-o")
        .arg(temp_dir.path())
        .status()
        .expect("Failed to execute CLI");

    assert!(status.success(), "CLI build failed with status: {}", status);

    let java_files: Vec<_> = fs::read_dir(temp_dir.path())
        .expect("Failed to read output directory")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("java"))
        .collect();

    assert!(
        !java_files.is_empty(),
        "Expected at least one generated Java file"
    );

    let java_source = fs::read_to_string(&java_files[0]).expect("Failed to read Java output");
    assert!(java_source.contains("Generated"));
    assert!(java_source.contains("message"));
}

#[test]
fn pipeline_compile_produces_artifacts() {
    let temp_dir = TempDirGuard::new("pipeline").expect("Failed to create temp dir");
    let input = workspace_file("test_simple.jv");

    let mut options = BuildOptions::new(input.as_path(), temp_dir.path());
    options.java_only = true;
    options.format = true;

    let artifacts = compile(&options).expect("Pipeline compilation failed");

    assert!(
        !artifacts.java_files.is_empty(),
        "Expected generated Java files"
    );
    assert!(artifacts.class_files.is_empty());

    for file in &artifacts.java_files {
        assert!(file.exists(), "Java file missing: {}", file.display());
    }
}

#[test]
fn pipeline_runs_javac_when_available() {
    if !has_javac() {
        eprintln!("Skipping javac integration test: javac not available");
        return;
    }

    let temp_dir = TempDirGuard::new("javac").expect("Failed to create temp dir");
    let input = workspace_file("test_simple.jv");

    let mut options = BuildOptions::new(input.as_path(), temp_dir.path());
    options.java_only = false;

    let artifacts = compile(&options).expect("Pipeline compilation with javac failed");

    assert!(artifacts.javac_version.is_some());
    assert!(
        !artifacts.class_files.is_empty(),
        "Expected compiled class files"
    );

    for file in &artifacts.class_files {
        assert!(file.exists(), "Class file missing: {}", file.display());
    }
}
