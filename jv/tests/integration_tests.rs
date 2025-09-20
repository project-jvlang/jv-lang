use std::fs;
use std::io::Write;
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

#[test]
fn cli_all_subcommands_smoke_test() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping CLI integration test: CARGO_BIN_EXE_jv not set");
        return;
    };

    let temp_dir = TempDirGuard::new("cli-all").expect("Failed to create temp dir");
    let project_dir = temp_dir.path().join("cli-demo");

    let version_output = Command::new(&cli_path)
        .arg("version")
        .output()
        .expect("Failed to run jv version");
    assert!(version_output.status.success());
    let version_stdout = String::from_utf8_lossy(&version_output.stdout);
    assert!(
        version_stdout.contains("jv "),
        "Expected version banner, got: {}",
        version_stdout
    );

    let init_status = Command::new(&cli_path)
        .arg("init")
        .arg(&project_dir)
        .status()
        .expect("Failed to run jv init");
    assert!(init_status.success());
    assert!(project_dir.join("jv.toml").exists());
    assert!(project_dir.join("src/main.jv").exists());

    let main_path = project_dir.join("src/main.jv");
    let main_source = r#"fun main() {
    val message = "Hello from CLI"
    println(message)
}
"#;
    fs::write(&main_path, main_source).expect("Failed to write main.jv");

    let fmt_status = Command::new(&cli_path)
        .arg("fmt")
        .arg(&main_path)
        .status()
        .expect("Failed to run jv fmt");
    assert!(fmt_status.success());
    let formatted = fs::read_to_string(&main_path).expect("Failed to read formatted file");
    assert!(formatted.contains("Hello from CLI"));

    let check_status = Command::new(&cli_path)
        .arg("check")
        .arg(&main_path)
        .status()
        .expect("Failed to run jv check");
    assert!(check_status.success());

    let output_dir = project_dir.join("out");
    let build_output = Command::new(&cli_path)
        .arg("build")
        .arg(&main_path)
        .arg("-o")
        .arg(&output_dir)
        .arg("--java-only")
        .arg("--check")
        .arg("--format")
        .output()
        .expect("Failed to run jv build");
    assert!(build_output.status.success());
    let generated_java: Vec<_> = fs::read_dir(&output_dir)
        .expect("Failed to read build output")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("java"))
        .collect();
    assert!(
        !generated_java.is_empty(),
        "Expected generated Java files, build stdout: {}",
        String::from_utf8_lossy(&build_output.stdout)
    );

    if has_javac() && has_java_runtime() {
        let run_output = Command::new(&cli_path)
            .arg("run")
            .arg(&main_path)
            .output()
            .expect("Failed to run jv run");
        assert!(run_output.status.success());
    } else {
        eprintln!("Skipping jv run command: java runtime or javac missing");
    }

    let mut repl_child = Command::new(&cli_path)
        .arg("repl")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to start jv repl");

    {
        let mut stdin = repl_child.stdin.take().expect("Failed to access repl stdin");
        stdin
            .write_all(b":help\n:q\n")
            .expect("Failed to send commands to repl");
    }

    let repl_output = repl_child
        .wait_with_output()
        .expect("Failed to wait for repl");
    assert!(repl_output.status.success());
    let repl_stdout = String::from_utf8_lossy(&repl_output.stdout);
    assert!(repl_stdout.contains("jv REPL"));
    assert!(repl_stdout.contains("Commands:"));
    assert!(repl_stdout.contains("Bye"));
}
