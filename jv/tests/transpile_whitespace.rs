use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};

struct TempDirGuard {
    path: PathBuf,
}

fn workspace_temp_root() -> PathBuf {
    static ROOT: OnceLock<PathBuf> = OnceLock::new();
    ROOT.get_or_init(|| {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let base = manifest_dir
            .ancestors()
            .find_map(|ancestor| {
                let candidate = ancestor.join("target");
                candidate.exists().then_some(candidate)
            })
            .unwrap_or_else(|| manifest_dir.join("target"));
        let temp_root = base.join("test-temp");
        let _ = fs::remove_dir_all(&temp_root);
        temp_root
    })
    .clone()
}

impl TempDirGuard {
    fn new(prefix: &str) -> std::io::Result<Self> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let base = workspace_temp_root();
        fs::create_dir_all(&base)?;
        let path = base.join(format!("jv-whitespace-{}-{}", prefix, timestamp));
        fs::create_dir_all(&path)?;
        Ok(Self { path })
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        // Keep build artifacts for post-test inspection.
    }
}

fn cli_binary() -> Option<PathBuf> {
    std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from)
}

#[test]
fn cli_check_reports_jv2101_for_comma_arrays() {
    let Some(cli_path) = cli_binary() else {
        eprintln!("Skipping CLI diagnostic snapshot: binary not available");
        return;
    };

    let temp_dir = TempDirGuard::new("cli-diag").expect("temp dir");
    let source_path = temp_dir.path().join("mixed_layout.jv");
    let source = r#"
fun main() {
    val numbers = [1, 2 3]
}
"#;
    fs::write(&source_path, source).expect("write source");

    let output = Command::new(&cli_path)
        .arg("check")
        .arg(&source_path)
        .stderr(Stdio::piped())
        .stdout(Stdio::piped())
        .output()
        .expect("run jv check");

    assert!(
        !output.status.success(),
        "expected CLI to fail on mixed delimiters"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{}\n{}", stdout, stderr);

    assert!(combined.contains("JV2101"), "diagnostic should mention JV2101");
    assert!(
        combined.contains("カンマ") || combined.contains("comma"),
        "diagnostic should mention comma guidance"
    );
}

#[test]
fn cli_build_transpiles_whitespace_sequences_to_java() {
    let Some(cli_path) = cli_binary() else {
        eprintln!("Skipping CLI build snapshot: binary not available");
        return;
    };

    let temp_dir = TempDirGuard::new("cli-build").expect("temp dir");
    let source_path = temp_dir.path().join("layout_sequences.jv");
    let output_dir = temp_dir.path().join("java-out");

    let source = r#"
fun plot(numbers: List<Int>, label: String) {
    println(label)
    for (n in numbers) {
        println(n)
    }
}

fun main() {
    val layout = [1 2 3]
    plot(layout, "sequence")
}
"#;
    fs::write(&source_path, source).expect("write whitespace source");

    let status = Command::new(&cli_path)
        .arg("build")
        .arg(&source_path)
        .arg("--java-only")
        .arg("-o")
        .arg(&output_dir)
        .status()
        .expect("run jv build");
    assert!(status.success(), "CLI build should succeed");

    let java_files: Vec<_> = fs::read_dir(&output_dir)
        .expect("list java output")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("java"))
        .collect();
    assert!(
        !java_files.is_empty(),
        "expected generated Java files in {:?}",
        output_dir
    );

    let java_source = fs::read_to_string(&java_files[0]).expect("read java output");
    assert!(
        java_source.contains("List.of(1, 2, 3)")
            || java_source.contains("Arrays.asList(1, 2, 3).stream().toList()"),
        "generated Java should materialize whitespace sequence"
    );
    assert!(
        java_source.contains("plot"),
        "expected generated Java to reference plot invocation"
    );
}
