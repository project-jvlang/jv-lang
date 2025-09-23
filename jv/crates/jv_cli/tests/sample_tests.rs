use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use jv_ir::transform::{fetch_sample_data, SampleFetchError, SampleFetchRequest, SampleSourceKind};
use sha2::{Digest, Sha256};
use tempfile::tempdir;

fn fixtures_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../tests/fixtures")
        .canonicalize()
        .expect("fixtures directory exists")
}

fn fixture_path(name: &str) -> PathBuf {
    fixtures_dir().join(name)
}

fn compute_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    digest.iter().map(|b| format!("{:02x}", b)).collect()
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

fn escape_path_for_annotation(path: &Path) -> String {
    path.to_str()
        .expect("fixture path should be valid UTF-8")
        .replace('\\', "\\\\")
}

fn java_sources_in(output_dir: &Path) -> Vec<PathBuf> {
    fs::read_dir(output_dir)
        .expect("enumerate build outputs")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("java"))
        .collect()
}

fn build_sample_program(cli_path: &Path, main_path: &Path, output_dir: &Path) -> Vec<PathBuf> {
    let status = Command::new(cli_path)
        .arg("build")
        .arg(main_path)
        .arg("-o")
        .arg(output_dir)
        .status()
        .expect("invoke jv build");
    assert!(status.success(), "jv build failed: {:?}", status);

    java_sources_in(output_dir)
}

fn run_generated_main(output_dir: &Path) -> std::process::Output {
    Command::new("java")
        .arg("-cp")
        .arg(output_dir)
        .arg("GeneratedMain")
        .output()
        .expect("execute generated Java")
}

#[test]
fn fetches_local_json_and_returns_expected_sha() {
    let sample = "sample_users.json";
    let fixtures = fixtures_dir();
    let expected_bytes = fs::read(fixture_path(sample)).expect("read fixture");
    let expected_sha = compute_sha256(&expected_bytes);

    let mut request = SampleFetchRequest::new(sample);
    request.base_dir = Some(fixtures);

    let result = fetch_sample_data(&request).expect("fetch local sample data");

    assert_eq!(result.source_kind, SampleSourceKind::LocalFile);
    assert_eq!(result.bytes, expected_bytes);
    assert_eq!(result.sha256, expected_sha);
    assert!(result.cache_path.is_none());
}

#[test]
fn reuses_cache_when_sha256_known() {
    let sample = "sample_users.json";
    let fixtures = fixtures_dir();
    let cache = tempdir().expect("create cache dir");

    let mut request = SampleFetchRequest::new(sample);
    request.base_dir = Some(fixtures.clone());
    request.cache_dir = Some(cache.path().to_path_buf());

    let first = fetch_sample_data(&request).expect("initial fetch");
    assert_eq!(first.source_kind, SampleSourceKind::LocalFile);

    let cached_file = cache.path().join(&first.sha256);
    assert!(cached_file.exists(), "cache entry should be written");

    request.expected_sha256 = Some(first.sha256.clone());
    let second = fetch_sample_data(&request).expect("cached fetch");

    assert_eq!(second.source_kind, SampleSourceKind::CachedFile);
    assert_eq!(second.bytes, first.bytes);
    assert_eq!(second.sha256, first.sha256);
    assert_eq!(second.cache_path.as_ref(), Some(&cached_file));
}

#[test]
fn enforce_size_limit_returns_error() {
    let sample = "sample_users.json";
    let fixtures = fixtures_dir();

    let mut request = SampleFetchRequest::new(sample);
    request.base_dir = Some(fixtures);
    request.max_bytes = Some(8);

    let error = fetch_sample_data(&request).expect_err("size limit should trigger error");
    match error {
        SampleFetchError::SizeLimitExceeded { limit, actual } => {
            assert_eq!(limit, 8);
            assert!(actual > limit);
        }
        other => panic!("expected size limit error, got {:?}", other),
    }
}

#[test]
fn sha256_mismatch_is_reported() {
    let sample = "sample_users.json";
    let fixtures = fixtures_dir();

    let mut request = SampleFetchRequest::new(sample);
    request.base_dir = Some(fixtures);
    request.expected_sha256 = Some("deadbeef".to_string());

    let error = fetch_sample_data(&request).expect_err("sha256 mismatch should error");
    match error {
        SampleFetchError::Sha256Mismatch { expected, actual } => {
            assert_eq!(expected, "deadbeef".to_string());
            assert!(actual.len() == 64);
            assert_ne!(expected, actual);
        }
        other => panic!("expected sha mismatch error, got {:?}", other),
    }
}

#[test]
fn absolute_path_fetches_without_base_dir() {
    let path = fixture_path("sample_metrics.csv");
    let source = path
        .to_str()
        .expect("path should be valid UTF-8")
        .to_string();

    let request = SampleFetchRequest::new(source);
    // No base_dir required for absolute path

    let result = fetch_sample_data(&request).expect("fetch absolute path");
    assert_eq!(result.source_kind, SampleSourceKind::LocalFile);
    assert_eq!(result.bytes, fs::read(path).expect("read fixture"));
}

#[test]
fn end_to_end_sample_annotation_embed_workflow() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping sample end-to-end test: CLI binary path not found");
        return;
    };

    if !has_javac() || !has_java_runtime() {
        eprintln!("Skipping sample end-to-end test: javac or java not available");
        return;
    }

    let workspace = tempdir().expect("create temp workspace");
    let main_path = workspace.path().join("sample_main.jv");
    let output_dir = workspace.path().join("out");

    let sample_path = fixture_path("sample_users.json");
    let sample_literal = escape_path_for_annotation(&sample_path);

    let program = format!(
        "@Sample(\"{sample}\") val users = null\n\nfun main() {{\n    println(users)\n}}\n",
        sample = sample_literal
    );

    fs::write(&main_path, program).expect("write sample program");

    let java_sources = build_sample_program(&cli_path, &main_path, &output_dir);
    assert!(
        !java_sources.is_empty(),
        "expected generated Java sources in {}",
        output_dir.display()
    );

    let embedded_data_present = java_sources.iter().any(|path| {
        fs::read_to_string(path)
            .map(|contents| contents.contains("UserSampleData") && contents.contains("Alice"))
            .unwrap_or(false)
    });
    assert!(
        embedded_data_present,
        "embedded sample data should appear in generated Java sources"
    );

    assert!(
        output_dir.join("GeneratedMain.class").exists(),
        "javac should emit GeneratedMain.class in {}",
        output_dir.display()
    );

    let run_output = run_generated_main(&output_dir);
    assert!(
        run_output.status.success(),
        "java execution failed: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let stdout = String::from_utf8(run_output.stdout).expect("stdout is valid UTF-8");
    assert!(
        stdout.contains("Alice"),
        "runtime output should include embedded sample data: {}",
        stdout
    );
    assert!(
        stdout.contains("UserSample"),
        "runtime output should reference inferred record type: {}",
        stdout
    );
}

#[test]
fn end_to_end_sample_annotation_load_refreshes_runtime_data() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping sample load end-to-end test: CLI binary path not found");
        return;
    };

    if !has_javac() || !has_java_runtime() {
        eprintln!("Skipping sample load end-to-end test: javac or java not available");
        return;
    }

    let workspace = tempdir().expect("create temp workspace");
    let main_path = workspace.path().join("sample_load.jv");
    let output_dir = workspace.path().join("out");

    let fixture_json = fixture_path("sample_users.json");
    let runtime_sample = workspace.path().join("runtime_users.json");
    fs::copy(&fixture_json, &runtime_sample).expect("copy sample fixture for runtime use");

    let sample_literal = escape_path_for_annotation(&runtime_sample);
    let program = format!(
        "@Sample(\"{sample}\", mode=Load) val users = null\n\nfun main() {{\n    println(users)\n}}\n",
        sample = sample_literal
    );

    fs::write(&main_path, program).expect("write load sample program");

    let java_sources = build_sample_program(&cli_path, &main_path, &output_dir);
    assert!(
        !java_sources.is_empty(),
        "expected generated Java sources in {}",
        output_dir.display()
    );

    let embeds_raw_literal = java_sources.iter().any(|path| {
        fs::read_to_string(path)
            .map(|contents| contents.contains("RAW_JSON") || contents.contains("RAW_CSV"))
            .unwrap_or(false)
    });
    assert!(
        !embeds_raw_literal,
        "load mode should not emit embedded RAW_* literals"
    );

    let uses_runtime_loader = java_sources.iter().any(|path| {
        fs::read_to_string(path)
            .map(|contents| contents.contains("Files.readString") || contents.contains("loadFrom"))
            .unwrap_or(false)
    });
    assert!(
        uses_runtime_loader,
        "load mode should emit runtime loader code that reads from disk"
    );

    assert!(
        output_dir.join("GeneratedMain.class").exists(),
        "javac should emit GeneratedMain.class in {}",
        output_dir.display()
    );

    let runtime_dataset = r#"[{"name":"Charlie","age":41,"email":"charlie@example.com"}]"#;
    fs::write(&runtime_sample, runtime_dataset).expect("update runtime dataset before execution");

    let run_output = run_generated_main(&output_dir);
    assert!(
        run_output.status.success(),
        "java execution failed: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let stdout = String::from_utf8(run_output.stdout).expect("stdout is valid UTF-8");
    assert!(
        stdout.contains("Charlie"),
        "runtime output should reflect freshly loaded data: {}",
        stdout
    );
    assert!(
        !stdout.contains("Alice"),
        "runtime output should not depend on embedded compile-time data: {}",
        stdout
    );
    assert!(
        stdout.contains("UserSample"),
        "runtime output should reference inferred record type: {}",
        stdout
    );
}

#[test]
fn end_to_end_sample_annotation_csv_embed_workflow() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping sample CSV end-to-end test: CLI binary path not found");
        return;
    };

    if !has_javac() || !has_java_runtime() {
        eprintln!("Skipping sample CSV end-to-end test: javac or java not available");
        return;
    }

    let workspace = tempdir().expect("create temp workspace");
    let main_path = workspace.path().join("sample_metrics.jv");
    let output_dir = workspace.path().join("out");

    let sample_path = fixture_path("sample_metrics.csv");
    let sample_literal = escape_path_for_annotation(&sample_path);
    let program = format!(
        "@Sample(\"{sample}\") val metrics = null\n\nfun main() {{\n    println(metrics)\n}}\n",
        sample = sample_literal
    );

    fs::write(&main_path, program).expect("write csv sample program");

    let java_sources = build_sample_program(&cli_path, &main_path, &output_dir);
    assert!(
        !java_sources.is_empty(),
        "expected generated Java sources in {}",
        output_dir.display()
    );

    let embeds_csv_literal = java_sources.iter().any(|path| {
        fs::read_to_string(path)
            .map(|contents| contents.contains("RAW_CSV"))
            .unwrap_or(false)
    });
    assert!(
        embeds_csv_literal,
        "embed mode should emit RAW_CSV literal for tabular data"
    );

    assert!(
        output_dir.join("GeneratedMain.class").exists(),
        "javac should emit GeneratedMain.class in {}",
        output_dir.display()
    );

    let run_output = run_generated_main(&output_dir);
    assert!(
        run_output.status.success(),
        "java execution failed: {}",
        String::from_utf8_lossy(&run_output.stderr)
    );

    let stdout = String::from_utf8(run_output.stdout).expect("stdout is valid UTF-8");
    assert!(
        stdout.contains("MetricSample"),
        "runtime output should reference inferred metric record type: {}",
        stdout
    );
    assert!(
        stdout.contains("ingest"),
        "runtime output should include CSV data values: {}",
        stdout
    );
}

#[test]
fn sample_network_sources_require_explicit_allow() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping network policy test: CLI binary path not found");
        return;
    };

    let workspace = tempdir().expect("create temp workspace");
    let main_path = workspace.path().join("sample_remote.jv");
    let output_dir = workspace.path().join("out");

    let program = "@Sample(\"https://example.com/data.json\") val remote = null\n\nfun main() {\n    println(remote)\n}\n";
    fs::write(&main_path, program).expect("write remote sample program");

    let output = Command::new(&cli_path)
        .arg("build")
        .arg(&main_path)
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("invoke jv build");

    assert!(
        !output.status.success(),
        "network-based sample source should be rejected without explicit allow flag"
    );

    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        combined.contains("ネットワークアクセスが許可されていません")
            || combined.contains("Network access for @Sample protocol"),
        "expected network denial message, got: {}",
        combined
    );
}
