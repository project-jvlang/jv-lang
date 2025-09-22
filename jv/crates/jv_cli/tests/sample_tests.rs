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
    let sample_literal = sample_path
        .to_str()
        .expect("fixture path should be valid UTF-8")
        .replace('\\', "\\\\");

    let program = format!(
        "@Sample(\"{sample}\") val users = null\n\nfun main() {{\n    println(users)\n}}\n",
        sample = sample_literal
    );

    fs::write(&main_path, program).expect("write sample program");

    let build_status = Command::new(&cli_path)
        .arg("build")
        .arg(&main_path)
        .arg("-o")
        .arg(&output_dir)
        .status()
        .expect("invoke jv build");
    assert!(build_status.success(), "jv build failed: {build_status:?}");

    let java_sources: Vec<PathBuf> = fs::read_dir(&output_dir)
        .expect("enumerate build outputs")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("java"))
        .collect();
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

    let run_output = Command::new("java")
        .arg("-cp")
        .arg(&output_dir)
        .arg("GeneratedMain")
        .output()
        .expect("execute generated Java");
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
