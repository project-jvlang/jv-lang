use jv_pm::JavaTarget;
use serde_json::Value;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

const SAMPLE_SOURCE: &str = r#"
fun main(): Unit {
    val layout = [1 2 3]
    for (n in layout) {
        println(n)
    }
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
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping target matrix test: jv CLI binary not available");
        return (String::new(), Value::Null);
    };

    let fixture = TempDirGuard::new(label).expect("temp dir");
    let root = fixture.path();

    let src_dir = root.join("src");
    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(src_dir.join("main.jv"), SAMPLE_SOURCE).expect("write sample source");

    fs::write(
        root.join("jv.toml"),
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

    let status = Command::new(&cli_path)
        .current_dir(root)
        .arg("build")
        .arg("--java-only")
        .arg("--output")
        .arg("out")
        .arg("--target")
        .arg(target.as_str())
        .status()
        .expect("invoke jv build");
    assert!(status.success(), "jv build command should succeed");

    let target_label = format!("java{}", target.as_str());
    let java_dir = root.join("out").join(&target_label);
    let java_files: Vec<PathBuf> = fs::read_dir(&java_dir)
        .expect("read generated java directory")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("java"))
        .collect();
    assert!(
        !java_files.is_empty(),
        "expected generated Java files in {:?}",
        java_dir
    );

    let java_source = fs::read_to_string(&java_files[0]).expect("read generated java");

    let compat_path = java_dir.join("compatibility.json");
    let value = if compat_path.exists() {
        let json = fs::read_to_string(&compat_path).expect("read compatibility json");
        serde_json::from_str(&json).expect("parse compatibility json")
    } else {
        Value::Null
    };

    (java_source, value)
}

#[test]
fn java21_builds_apply_collections_fallback() {
    let (java_source, report) = compile_with_target("java21", JavaTarget::Java21);

    if java_source.is_empty() {
        eprintln!("Skipping java21 target matrix test: jv binary unavailable");
        return;
    }

    assert!(
        java_source.contains("Arrays.asList(1, 2, 3).stream().toList()"),
        "java21 fallback should materialise Arrays.asList():\n{}",
        java_source
    );
    if report != Value::Null {
        assert_eq!(report["target"], "21");
        assert_eq!(report["target_release"], "21");
    }
}

#[test]
fn java25_builds_emit_list_of_literals() {
    let (java_source, report) = compile_with_target("java25", JavaTarget::Java25);

    if java_source.is_empty() {
        eprintln!("Skipping java25 target matrix test: jv binary unavailable");
        return;
    }

    assert!(
        java_source.contains("List.of(1, 2, 3)"),
        "java25 target should retain List.of():\n{}",
        java_source
    );
    if report != Value::Null {
        assert_eq!(report["target"], "25");
        assert_eq!(report["target_release"], "25");
    }
}
