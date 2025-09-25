use jv_cli::pipeline::{compile, BuildOptionsFactory, CliOverrides};
use jv_cli::pipeline::project::{layout::ProjectLayout, locator::ProjectRoot, manifest::ManifestLoader};
use jv_pm::JavaTarget;
use serde_json::Value;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

const SAMPLE_SOURCE: &str = r#"
fun render(numbers: List<Int>) {
    for (n in numbers) {
        println(n)
    }
}

fun main() {
    val layout = [1 2 3]
    render(layout)
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
        let path = std::env::temp_dir().join(format!("jv-target-matrix-{}-{}", label, timestamp));
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

fn compile_with_target(label: &str, target: JavaTarget) -> (String, Value) {
    let fixture = TempDirGuard::new(label).expect("temp dir");
    let root = fixture.path();
    let src_dir = root.join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    let input_path = src_dir.join("main.jv");
    fs::write(&input_path, SAMPLE_SOURCE).expect("write sample source");

    let manifest_path = root.join("jv.toml");
    fs::write(
        &manifest_path,
        r#"[package]
name = "matrix"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    )
    .expect("write manifest");

    let project_root = ProjectRoot::new(root.to_path_buf(), manifest_path.clone());
    let settings = ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = ProjectLayout::from_settings(&project_root, &settings)
        .expect("layout resolves sources");

    let output_dir = root.join("out");
    let overrides = CliOverrides {
        entrypoint: Some(input_path.clone()),
        output: Some(output_dir.clone()),
        java_only: true,
        check: false,
        format: false,
        target: Some(target),
    };

    let plan = BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds");

    let artifacts = compile(&plan).expect("pipeline compile succeeds");
    let java_path = artifacts
        .java_files
        .first()
        .expect("java file emitted")
        .clone();
    let java_source = fs::read_to_string(&java_path).expect("read generated java");

    let report = artifacts
        .compatibility
        .expect("compatibility report is recorded");
    assert!(report.json_path.exists());
    let json = fs::read_to_string(&report.json_path).expect("read compatibility json");
    let value: Value = serde_json::from_str(&json).expect("parse compatibility json");

    (java_source, value)
}

#[test]
fn java21_builds_apply_collections_fallback() {
    let (java_source, report) = compile_with_target("java21", JavaTarget::Java21);

    assert!(
        java_source.contains("Arrays.asList(1, 2, 3).stream().toList()"),
        "java21 fallback should materialise Arrays.asList():\n{}",
        java_source
    );
    assert_eq!(report["target"], "21");
    assert_eq!(report["target_release"], "21");
}

#[test]
fn java25_builds_emit_list_of_literals() {
    let (java_source, report) = compile_with_target("java25", JavaTarget::Java25);

    assert!(
        java_source.contains("List.of(1, 2, 3)"),
        "java25 target should retain List.of():\n{}",
        java_source
    );
    assert_eq!(report["target"], "25");
    assert_eq!(report["target_release"], "25");
}
