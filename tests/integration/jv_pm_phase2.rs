use assert_cmd::prelude::*;
use predicates::prelude::*;
use serde_json::Value;
use sha2::{Digest, Sha256};
use tempfile::{tempdir, TempDir};

use jv_pm::{
    ArtifactCoordinates, ArtifactDownloadRequest, BuildInfo, DependencyCache, DownloadManager,
    DownloadedJar, Manifest, MavenProjectMetadata, PackageInfo, ProjectSection, RepositorySection,
    ResolverDispatcher, ResolverOptions,
};

use std::collections::HashMap;
use std::env;
use std::ffi::OsString;
use std::fs;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::process::Command;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

const SAMPLE_MAIN: &str = r#"fun main() {
    println("Hello, jv!")
}
"#;

fn cargo_bin_path(name: &str) -> Option<PathBuf> {
    let var_name = format!("CARGO_BIN_EXE_{name}");
    env::var_os(&var_name).map(PathBuf::from)
}

fn jv_command() -> Option<Command> {
    cargo_bin_path("jv").map(Command::new)
}

fn jvpm_bin() -> Option<OsString> {
    cargo_bin_path("jvpm").map(|path| path.into_os_string())
}

#[test]
fn cli_add_then_build_exports_java_project() {
    let registry_url = "https://registry.integration.test/";
    let sandbox = TestSandbox::new(registry_url);

    let mut add_cmd = match jv_command() {
        Some(cmd) => cmd,
        None => {
            eprintln!("skipping cli_add_then_build_exports_java_project: jv binary unavailable");
            return;
        }
    };
    if !sandbox.configure_jv_command(&mut add_cmd) {
        eprintln!("skipping cli_add_then_build_exports_java_project: jvpm binary unavailable");
        return;
    }
    add_cmd
        .arg("add")
        .arg("--non-interactive")
        .arg("org.example:demo:1.0.0")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "依存関係 'org.example:demo' をバージョン 1.0.0 で追加しました。",
        ))
        .stdout(predicate::str::contains("更新: jv.toml / jv.lock"));

    let manifest = fs::read_to_string(sandbox.project_root().join("jv.toml"))
        .expect("failed to read manifest");
    assert!(manifest.contains("org.example:demo"));

    let lockfile = sandbox.project_root().join("jv.lock");
    assert!(lockfile.exists(), "jv.lock が生成されていません");

    // 擬似的なローカルリポジトリ成果物を配置してエクスポート対象を作成
    sandbox.write_local_repository_artifact("org.example", "demo", "1.0.0");

    let mut build_cmd = match jv_command() {
        Some(cmd) => cmd,
        None => {
            eprintln!("skipping cli_add_then_build_exports_java_project: jv binary unavailable");
            return;
        }
    };
    if !sandbox.configure_jv_command(&mut build_cmd) {
        eprintln!("skipping cli_add_then_build_exports_java_project: jvpm binary unavailable");
        return;
    }
    build_cmd
        .arg("build")
        .arg("--java-only")
        .arg("src/main.jv")
        .assert()
        .success()
        .stdout(predicate::str::contains("出力ディレクトリ"));

    let output_dir = sandbox.project_root().join("target/java-project");
    assert!(output_dir.join("pom.xml").exists(), "pom.xml が生成されていません");
    assert!(output_dir.join(".jv/settings.xml").exists(), "settings.xml が生成されていません");
    assert!(output_dir.join("classpath.txt").exists(), "classpath.txt が生成されていません");

    let settings = fs::read_to_string(output_dir.join(".jv/settings.xml"))
        .expect("failed to read settings.xml");
    assert!(
        settings.contains(registry_url),
        "settings.xml にレジストリURLが含まれていません"
    );

    let exported_repo = output_dir.join(".jv/repository");
    assert!(exported_repo.exists(), ".jv/repository がエクスポートされていません");
}

#[test]
fn cli_resolver_list_and_info_emit_expected_json() {
    let sandbox = TestSandbox::new("https://registry.integration.test/");

    let mut list_cmd = match jv_command() {
        Some(cmd) => cmd,
        None => {
            eprintln!("skipping cli_resolver_list_and_info_emit_expected_json: jv binary unavailable");
            return;
        }
    };
    if !sandbox.configure_jv_command(&mut list_cmd) {
        eprintln!("skipping cli_resolver_list_and_info_emit_expected_json: jvpm binary unavailable");
        return;
    }
    let output = list_cmd
        .arg("resolver")
        .arg("list")
        .arg("--json")
        .output()
        .expect("resolver list failed");
    assert!(output.status.success(), "resolver list exited with error");
    let strategies: Value = serde_json::from_slice(&output.stdout).expect("invalid JSON");
    let names = strategies
        .as_array()
        .expect("list should be array")
        .iter()
        .filter_map(|entry| entry.get("name"))
        .filter_map(Value::as_str)
        .collect::<Vec<_>>();
    assert!(names.contains(&"pubgrub"));
    assert!(names.contains(&"breadth-first"));
    assert!(names.contains(&"maven"));

    let mut info_cmd = match jv_command() {
        Some(cmd) => cmd,
        None => {
            eprintln!("skipping cli_resolver_list_and_info_emit_expected_json: jv binary unavailable");
            return;
        }
    };
    if !sandbox.configure_jv_command(&mut info_cmd) {
        eprintln!("skipping cli_resolver_list_and_info_emit_expected_json: jvpm binary unavailable");
        return;
    }
    let info = info_cmd
        .arg("resolver")
        .arg("info")
        .arg("pubgrub")
        .arg("--json")
        .output()
        .expect("resolver info failed");
    assert!(info.status.success(), "resolver info exited with error");
    let detail: Value = serde_json::from_slice(&info.stdout).expect("invalid JSON");
    assert_eq!(detail.get("name").and_then(Value::as_str), Some("pubgrub"));
    assert_eq!(
        detail.get("algorithm").and_then(Value::as_str),
        Some("pub_grub"),
        "pubgrub strategy should expose pub_grub algorithm"
    );
}

#[test]
fn cli_repo_commands_manage_project_repositories() {
    if cargo_bin_path("jv").is_none() {
        eprintln!("skipping cli_repo_commands_manage_project_repositories: jv binary unavailable");
        return;
    }
    if cargo_bin_path("jvpm").is_none() {
        eprintln!("skipping cli_repo_commands_manage_project_repositories: jvpm binary unavailable");
        return;
    }

    let sandbox = TestSandbox::new("https://registry.integration.test/");

    let mut add_cmd = jv_command().expect("jv binary should exist after availability check");
    if !sandbox.configure_jv_command(&mut add_cmd) {
        eprintln!("skipping cli_repo_commands_manage_project_repositories: jvpm binary unavailable");
        return;
    }
    add_cmd
        .args([
            "repo",
            "add",
            "company",
            "https://artifact.example.com/repo",
            "--priority",
            "5",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("プロジェクトリポジトリ 'company' を追加しました。"));

    let mut list_cmd = jv_command().expect("jv binary should exist");
    if !sandbox.configure_jv_command(&mut list_cmd) {
        eprintln!("skipping cli_repo_commands_manage_project_repositories: jvpm binary unavailable");
        return;
    }
    let repo_list = list_cmd
        .args(["repo", "list", "--json"])
        .output()
        .expect("repo list failed");
    assert!(repo_list.status.success());
    let entries: Value = serde_json::from_slice(&repo_list.stdout).expect("invalid JSON");
    let entry = entries
        .as_array()
        .and_then(|arr| arr.iter().find(|item| item.get("name") == Some(&Value::String("company".into()))))
        .cloned()
        .expect("company repo missing");
    assert_eq!(entry.get("configured_url").and_then(Value::as_str), Some("https://artifact.example.com/repo"));

    let mut mirror_cmd = jv_command().expect("jv binary should exist");
    if !sandbox.configure_jv_command(&mut mirror_cmd) {
        eprintln!("skipping cli_repo_commands_manage_project_repositories: jvpm binary unavailable");
        return;
    }
    mirror_cmd
        .args(["repo", "mirror", "company", "https://mirror.example.com/", "--name", "internal"])
        .assert()
        .success()
        .stdout(predicate::str::contains("プロジェクト スコープのミラー 'company' を追加しました。"));

    let mut mirror_list = jv_command().expect("jv binary should exist");
    if !sandbox.configure_jv_command(&mut mirror_list) {
        eprintln!("skipping cli_repo_commands_manage_project_repositories: jvpm binary unavailable");
        return;
    }
    let mirrors = mirror_list
        .args(["repo", "mirror", "--json"])
        .output()
        .expect("repo mirror list failed");
    assert!(mirrors.status.success());
    let mirror_entries: Value = serde_json::from_slice(&mirrors.stdout).expect("invalid JSON");
    let mirror_entry = mirror_entries
        .as_array()
        .and_then(|arr| arr.iter().find(|item| item.get("mirror_of").and_then(Value::as_str) == Some("company")))
        .cloned()
        .expect("mirror entry missing");
    assert_eq!(
        mirror_entry.get("url").and_then(Value::as_str),
        Some("https://mirror.example.com/")
    );

    let mut remove_cmd = jv_command().expect("jv binary should exist");
    if !sandbox.configure_jv_command(&mut remove_cmd) {
        eprintln!("skipping cli_repo_commands_manage_project_repositories: jvpm binary unavailable");
        return;
    }
    remove_cmd
        .args(["repo", "remove", "company"])
        .assert()
        .success()
        .stdout(predicate::str::contains("プロジェクトリポジトリ 'company' を削除しました。"));

    let mut list_after = jv_command().expect("jv binary should exist");
    if !sandbox.configure_jv_command(&mut list_after) {
        eprintln!("skipping cli_repo_commands_manage_project_repositories: jvpm binary unavailable");
        return;
    }
    let repo_list_after = list_after
        .args(["repo", "list", "--json"])
        .output()
        .expect("repo list after remove failed");
    assert!(repo_list_after.status.success());
    let entries_after: Value = serde_json::from_slice(&repo_list_after.stdout).expect("invalid JSON");
    let has_company = entries_after
        .as_array()
        .map(|arr| arr.iter().any(|item| item.get("name") == Some(&Value::String("company".into()))))
        .unwrap_or(false);
    assert!(!has_company, "company repository should be removed");
}

#[tokio::test]
async fn download_manager_emits_concurrency_warnings() {
    let registry = MockRegistry::new();
    for index in 0..12 {
        let artifact = format!("library{}", index);
        let coords = ArtifactCoordinates::new("org.integration", &artifact, "1.0.0");
        registry.insert_success(&coords, format!("jar-bytes-{index}").into_bytes());
    }

    let cache_dir = tempdir().expect("failed to create cache dir");
    let cache = Arc::new(
        DependencyCache::with_dir(cache_dir.path().to_path_buf())
            .expect("failed to initialise cache"),
    );

    let registry_client = Arc::new(registry.clone());

    let mut build = BuildInfo::default();
    build.max_concurrent_downloads = Some(32);
    build.max_concurrent_warning = Some(8);

    let manager = DownloadManager::new(registry_client, cache).apply_manifest(Some(&build));

    let requests = (0..12)
        .map(|index| {
            ArtifactDownloadRequest::new(ArtifactCoordinates::new(
                "org.integration",
                format!("library{}", index),
                "1.0.0",
            ))
        })
        .collect::<Vec<_>>();

    let report = manager.download_artifacts(requests).await;
    assert!(report.failures.is_empty(), "ダウンロードが失敗しました");
    assert_eq!(report.successes.len(), 12);
    assert!(
        report
            .warnings
            .iter()
            .any(|message| message.contains("警告閾値")),
        "警告メッセージが期待どおりに出力されていません"
    );
}

#[test]
fn resolver_performance_benchmark_completes_quickly() {
    let mut dependencies = HashMap::new();
    for index in 0..80 {
        let name = format!("dep{:02}", index);
        let requirement = format!("^{}.{}.{}", 1 + (index % 3), index % 5, index % 7);
        dependencies.insert(name, requirement);
    }

    let manifest = Manifest {
        package: PackageInfo {
            name: "benchmark-subsystem".into(),
            version: "0.1.0".into(),
            description: None,
            dependencies,
        },
        project: ProjectSection::default(),
        repositories: RepositorySection::default(),
        mirrors: Vec::new(),
        build: None,
        maven: MavenProjectMetadata::default(),
    };

    let dispatcher = ResolverDispatcher::with_default_strategies();
    let iterations = 50u32;
    let start = Instant::now();
    for _ in 0..iterations {
        dispatcher
            .resolve_manifest(&manifest, ResolverOptions::default())
            .expect("resolution should succeed");
    }
    let elapsed = start.elapsed();
    assert!(
        elapsed < Duration::from_secs(5),
        "Resolver performance regression detected: {:?}",
        elapsed
    );
}

struct TestSandbox {
    home: TempDir,
    project: TempDir,
}

impl TestSandbox {
    fn new(registry_url: &str) -> Self {
        let home = tempdir().expect("failed to create home tempdir");
        let project = tempdir().expect("failed to create project tempdir");

        fs::create_dir_all(project.path().join("src"))
            .expect("failed to create source directory");
        fs::write(project.path().join("src/main.jv"), SAMPLE_MAIN)
            .expect("failed to write sample program");

        let manifest = format!(
            "[package]\nname = \"integration-app\"\nversion = \"0.1.0\"\n\n[package.dependencies]\n\n[project]\nentrypoint = \"src/main.jv\"\n\n[project.sources]\ninclude = [\"src/**/*.jv\"]\n\n[project.output]\ndirectory = \"target\"\n\n[[repositories]]\nname = \"test-registry\"\nurl = \"{registry_url}\"\npriority = 5\n"
        );
        fs::write(project.path().join("jv.toml"), manifest)
            .expect("failed to write manifest");

        Self { home, project }
    }

    fn project_root(&self) -> &Path {
        self.project.path()
    }

    fn home_path(&self) -> &Path {
        self.home.path()
    }

    fn configure_jv_command(&self, command: &mut Command) -> bool {
        self.configure_common_env(command);
        if let Some(bin) = jvpm_bin() {
            command.env("JVPM_BIN", bin);
            true
        } else {
            false
        }
    }

    fn configure_common_env(&self, command: &mut Command) {
        command.current_dir(self.project_root());
        command.env("HOME", self.home_path());

        let root = repo_root();
        let java25 = root.join("toolchains/jdk25");
        let java21 = root.join("toolchains/jdk21");

        command.env("JAVA_HOME", java25.as_os_str());
        command.env("JAVA25_HOME", java25.as_os_str());
        command.env("JAVA21_HOME", java21.as_os_str());

        let mut path_entries = vec![java25.join("bin"), java21.join("bin")];
        if let Some(existing) = env::var_os("PATH") {
            path_entries.extend(env::split_paths(&existing));
        }
        let joined = env::join_paths(path_entries).expect("failed to join PATH entries");
        command.env("PATH", joined);
    }

    fn write_local_repository_artifact(&self, group: &str, artifact: &str, version: &str) {
        let group_path = group.replace('.', "/");
        let artifact_dir = self
            .project_root()
            .join(".jv/repository")
            .join(group_path)
            .join(artifact)
            .join(version);
        fs::create_dir_all(&artifact_dir).expect("failed to create local repo directory");
        let jar_name = format!("{artifact}-{version}.jar");
        fs::write(artifact_dir.join(jar_name), b"local-jar").expect("failed to write local jar");
    }
}

#[derive(Clone, Default)]
struct MockRegistry {
    artifacts: Arc<Mutex<HashMap<String, Vec<u8>>>>,
}

impl MockRegistry {
    fn new() -> Self {
        Self::default()
    }

    fn insert_success(&self, coords: &ArtifactCoordinates, bytes: Vec<u8>) {
        self.artifacts
            .lock()
            .expect("registry lock poisoned")
            .insert(coords.to_string(), bytes);
    }
}

impl jv_pm::download::JarFetcher for MockRegistry {
    fn download_jar<'a>(
        &'a self,
        coords: &'a ArtifactCoordinates,
    ) -> Pin<Box<dyn Future<Output = Result<DownloadedJar, jv_pm::RegistryError>> + Send + 'a>> {
        let key = coords.to_string();
        let data = self
            .artifacts
            .lock()
            .expect("registry lock poisoned")
            .get(&key)
            .cloned();
        Box::pin(async move {
            let bytes = data.unwrap_or_else(|| panic!("artifact {key} not registered"));
            let checksum = format!("{:x}", Sha256::digest(&bytes));
            Ok(DownloadedJar { bytes, checksum })
        })
    }
}

fn repo_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .ancestors()
        .find(|candidate| candidate.join("toolchains").is_dir())
        .expect("failed to locate repository root")
        .to_path_buf()
}
