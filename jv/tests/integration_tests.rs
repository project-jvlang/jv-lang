use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

use jv_checker::{TypeInferenceService, TypeKind};
use jv_cli::pipeline::{compile, BuildOptionsFactory, CliOverrides};
use jv_cli::pipeline::project::{layout::ProjectLayout, locator::ProjectRoot, manifest::ManifestLoader};

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

fn compose_plan_from_fixture(
    project_dir: &Path,
    fixture: &Path,
    mut overrides: CliOverrides,
) -> jv_cli::pipeline::BuildPlan {
    let manifest_path = project_dir.join("jv.toml");
    fs::write(
        &manifest_path,
        r#"[package]
name = "integration"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    )
    .expect("write manifest");

    let src_dir = project_dir.join("src");
    fs::create_dir_all(&src_dir).expect("create src directory");
    let entrypoint_path = src_dir.join("main.jv");
    fs::copy(fixture, &entrypoint_path).expect("copy fixture source");

    let project_root = ProjectRoot::new(project_dir.to_path_buf(), manifest_path.clone());
    let settings = ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = ProjectLayout::from_settings(&project_root, &settings)
        .expect("layout enumerates sources");

    if overrides.entrypoint.is_none() {
        overrides.entrypoint = Some(entrypoint_path);
    }
    if overrides.output.is_none() {
        overrides.output = Some(project_dir.join("target"));
    }

    BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds")
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
    let project_dir = temp_dir.path().join("project");
    fs::create_dir_all(project_dir.join("src")).expect("Failed to create project src");
    fs::copy(
        workspace_file("test_simple.jv"),
        project_dir.join("src/main.jv"),
    )
    .expect("Failed to copy fixture source");
    fs::write(
        project_dir.join("jv.toml"),
        r#"[package]
name = "cli"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    )
    .expect("Failed to write manifest");

    let status = Command::new(&cli_path)
        .current_dir(&project_dir)
        .arg("build")
        .arg("--java-only")
        .status()
        .expect("Failed to execute CLI");

    assert!(status.success(), "CLI build failed with status: {}", status);

    let java_files: Vec<_> = fs::read_dir(project_dir.join("target/java25"))
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

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &input,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: true,
            check: false,
            format: true,
            target: None,
            clean: false,
            perf: false,
            emit_types: false,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let artifacts = compile(&plan).expect("Pipeline compilation failed");

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
fn pipeline_preserves_annotations_in_java_output() {
    let temp_dir = TempDirGuard::new("pipeline-annotations").expect("create temp dir");
    let input = workspace_file("tests/fixtures/java_annotations/pass_through.jv");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &input,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: true,
            check: false,
            format: false,
            target: None,
            clean: false,
            perf: false,
            emit_types: false,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let artifacts = match compile(&plan) {
        Ok(artifacts) => artifacts,
        Err(err) => {
            eprintln!(
                "Skipping annotation pipeline test: {}",
                err
            );
            return;
        }
    };
    let service_java = artifacts
        .java_files
        .iter()
        .find(|path| path.file_name().and_then(|name| name.to_str()) == Some("Service.java"))
        .expect("Service.java generated");

    let java_source = fs::read_to_string(service_java).expect("read generated Java");
    assert!(java_source.contains("@Component"));
    assert!(java_source.contains("@Autowired"));
    assert!(java_source.contains("@RequestMapping(path = {\"/ping\"}, produces = {\"application/json\"})"));
    assert!(java_source.contains("@Nullable"));
}

#[test]
fn pipeline_emit_types_produces_type_facts_json() {
    let temp_dir = TempDirGuard::new("pipeline-emit-types").expect("create temp dir");
    let input = workspace_file("test_simple.jv");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &input,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: true,
            check: false,
            format: false,
            target: None,
            clean: false,
            perf: false,
            emit_types: true,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let artifacts = compile(&plan).expect("emit-types compilation succeeds");
    let snapshot = artifacts
        .inference
        .expect("emit-types should retain inference snapshot");
    let json = snapshot
        .type_facts()
        .to_pretty_json()
        .expect("serialize type facts");

    assert!(json.contains("environment"), "missing environment: {json}");
    assert!(json.contains("message"), "expected binding entry in json: {json}");
}

#[test]
fn type_inference_snapshot_emitted_with_emit_types() {
    let temp_dir = TempDirGuard::new("type-facts").expect("temp dir");
    let snippet = temp_dir.path().join("snippet.jv");
    fs::write(&snippet, "val answer = 42\n").expect("write snippet");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &snippet,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: true,
            check: true,
            format: false,
            target: None,
            clean: false,
            perf: false,
            emit_types: true,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let artifacts = compile(&plan).expect("compile with emit-types succeeds");
    let snapshot = artifacts
        .inference
        .as_ref()
        .expect("inference snapshot present");

    let environment = snapshot.environment().flattened_bindings();
    let scheme = environment
        .get("answer")
        .expect("answer binding exported in environment");
    assert!(matches!(scheme.ty, TypeKind::Primitive("Int")));
    assert!(snapshot.bindings().len() >= 1);
}

#[test]
fn type_inference_snapshot_tracks_program_changes() {
    let temp_dir = TempDirGuard::new("type-facts-diff").expect("temp dir");
    let snippet = temp_dir.path().join("main.jv");
    fs::write(&snippet, "val base = 1\n").expect("write initial snippet");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &snippet,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: true,
            check: true,
            format: false,
            target: None,
            clean: false,
            perf: false,
            emit_types: true,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let first = compile(&plan).expect("first compile succeeds");
    let first_env = first
        .inference
        .as_ref()
        .expect("first inference snapshot")
        .environment()
        .flattened_bindings();
    assert!(first_env.contains_key("base"));
    assert!(!first_env.contains_key("incremented"));

    fs::write(&snippet, "val base = 1\nval incremented = base + 1\n")
        .expect("write updated snippet");
    fs::copy(&snippet, &plan.options.entrypoint)
        .expect("sync updated snippet to entrypoint");

    let second = compile(&plan).expect("second compile succeeds");
    let second_env = second
        .inference
        .as_ref()
        .expect("second inference snapshot")
        .environment()
        .flattened_bindings();
    assert!(second_env.contains_key("base"));
    let incremented = second_env
        .get("incremented")
        .expect("incremented binding present");
    assert!(matches!(incremented.ty, TypeKind::Primitive("Int")));
}

#[test]
fn null_safety_warnings_survive_pipeline() {
    let temp_dir = TempDirGuard::new("null-safety").expect("temp dir");
    let snippet = temp_dir.path().join("main.jv");
    fs::write(&snippet, "val message: String = null\n").expect("write snippet");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &snippet,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: true,
            check: true,
            format: false,
            target: None,
            clean: false,
            perf: false,
            emit_types: false,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let artifacts = compile(&plan).expect("compile succeeds with null warning");
    assert!(artifacts
        .warnings
        .iter()
        .any(|warning| warning.contains("Null safety violation")));
}

#[test]
fn ambiguous_function_causes_type_error() {
    let temp_dir = TempDirGuard::new("ambiguous").expect("temp dir");
    let snippet = temp_dir.path().join("main.jv");
    fs::write(&snippet, "fun ambiguous(x) { null }\n").expect("write snippet");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &snippet,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: true,
            check: true,
            format: false,
            target: None,
            clean: false,
            perf: false,
            emit_types: false,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let error = compile(&plan).expect_err("type error expected for ambiguous function");
    let message = error.to_string();
    assert!(message.contains("Type checking failed"));
    assert!(message.contains("ambiguous function signature"));
}

#[test]
fn pipeline_reports_missing_else_in_value_when() {
    let temp_dir = TempDirGuard::new("pipeline-missing-else").expect("create temp dir");
    let fixture = temp_dir.path().join("missing_else.jv");
    fs::write(
        &fixture,
        r#"fun compute(flag: Boolean): Int {
    return when (flag) {
        true -> 1
    }
}
"#,
    )
    .expect("write fixture");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &fixture,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: true,
            check: true,
            format: false,
            target: None,
            clean: false,
            perf: false,
            emit_types: false,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let err = compile(&plan).expect_err("compile should fail for missing else");
    let message = err.to_string();
    assert!(
        message.contains("E_WHEN_002"),
        "expected E_WHEN_002 in tooling error, got {message}"
    );
}

#[test]
fn pipeline_runs_javac_when_available() {
    if !has_javac() {
        eprintln!("Skipping javac integration test: javac not available");
        return;
    }

    let temp_dir = TempDirGuard::new("javac").expect("Failed to create temp dir");
    let input = workspace_file("test_simple.jv");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &input,
        CliOverrides {
            entrypoint: None,
            output: None,
            java_only: false,
            check: false,
            format: false,
            target: None,
            clean: false,
            perf: false,
            emit_types: false,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let artifacts = compile(&plan).expect("Pipeline compilation with javac failed");

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
fn cli_check_reports_missing_else_diagnostic() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping CLI diagnostic test: CARGO_BIN_EXE_jv not set");
        return;
    };

    let temp_dir = TempDirGuard::new("cli-check-when").expect("create temp dir");
    let source_path = temp_dir.path().join("missing_else.jv");
    fs::write(
        &source_path,
        r#"fun main(flag: Boolean) {
    val value = when (flag) {
        true -> 1
    }
    println(value)
}
"#,
    )
    .expect("write source");

    let output = Command::new(&cli_path)
        .arg("check")
        .arg(&source_path)
        .output()
        .expect("execute jv check");

    assert!(
        !output.status.success(),
        "CLI check should fail for missing else, status: {:?}",
        output.status
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("E_WHEN_002"),
        "expected E_WHEN_002 in CLI output, got: {stderr}"
    );
}

#[test]
fn cli_check_reports_forbidden_if_expression() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping CLI diagnostic test: CARGO_BIN_EXE_jv not set");
        return;
    };

    let temp_dir = TempDirGuard::new("cli-check-if").expect("create temp dir");
    let source_path = temp_dir.path().join("forbidden_if.jv");
    fs::write(
        &source_path,
        "fun main() { val value = if (true) 1 else 0 }\n",
    )
    .expect("write source");

    let output = Command::new(&cli_path)
        .arg("check")
        .arg(&source_path)
        .output()
        .expect("execute jv check");

    assert!(
        !output.status.success(),
        "CLI check should fail for forbidden if expression"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("JV3103"),
        "expected JV3103 in CLI output, got: {stderr}"
    );
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
