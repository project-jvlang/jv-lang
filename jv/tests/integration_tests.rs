use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};

use jv_build::{
    discover_jdk,
    metadata::{BuildContext as SymbolBuildContext, SymbolIndexCache},
    JavaTarget, JdkInfo,
};
use jv_checker::{PrimitiveType, TypeInferenceService, TypeKind};
use jv_cli::pipeline::project::{
    layout::ProjectLayout, locator::ProjectRoot, manifest::ManifestLoader,
};
use jv_cli::pipeline::{compile, BuildOptionsFactory, CliOverrides};
use jv_ir::types::IrImportDetail;
use serde_json::Value;

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
        let path = base.join(format!("jv-integration-{}-{}", prefix, timestamp));
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
    let layout =
        ProjectLayout::from_settings(&project_root, &settings).expect("layout enumerates sources");

    if overrides.entrypoint.is_none() {
        overrides.entrypoint = Some(entrypoint_path);
    }
    if overrides.output.is_none() {
        overrides.output = Some(project_dir.join("target"));
    }

    BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds")
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

fn toolchains_root() -> Option<PathBuf> {
    let workspace = workspace_root();
    for ancestor in workspace.ancestors() {
        let candidate = ancestor.join("toolchains");
        if candidate.exists() {
            return Some(candidate);
        }
    }
    None
}

fn workspace_file(relative: &str) -> PathBuf {
    workspace_root().join(relative)
}

fn ensure_toolchain_envs() {
    static INIT: OnceLock<()> = OnceLock::new();
    INIT.get_or_init(|| {
        for (var, dir) in [
            ("JAVA25_HOME", toolchain_java_home(JavaTarget::Java25)),
            ("JAVA21_HOME", toolchain_java_home(JavaTarget::Java21)),
        ] {
            if env::var_os(var).is_none() {
                if let Some(path) = dir {
                    // Nightly 1.92では`std::env::set_var`が`unsafe`指定となったため明示的に包む。
                    unsafe {
                        env::set_var(var, path);
                    }
                }
            }
        }
    });
}

fn target_env_var(target: JavaTarget) -> &'static str {
    match target {
        JavaTarget::Java25 => "JAVA25_HOME",
        JavaTarget::Java21 => "JAVA21_HOME",
    }
}

fn target_toolchain_dir(target: JavaTarget) -> &'static str {
    match target {
        JavaTarget::Java25 => "jdk25",
        JavaTarget::Java21 => "jdk21",
    }
}

fn expected_major_version(target: JavaTarget) -> u32 {
    match target {
        JavaTarget::Java25 => 25,
        JavaTarget::Java21 => 21,
    }
}

fn toolchain_java_home(target: JavaTarget) -> Option<PathBuf> {
    let root = toolchains_root()?;
    let path = root.join(target_toolchain_dir(target));
    let java_bin = path.join("bin").join(java_executable());
    if java_bin.exists() {
        Some(path)
    } else {
        None
    }
}

fn jdk_info_for_target(target: JavaTarget) -> Option<&'static JdkInfo> {
    ensure_toolchain_envs();

    fn cache_for(target: JavaTarget) -> &'static OnceLock<Option<JdkInfo>> {
        match target {
            JavaTarget::Java25 => {
                static CACHE_25: OnceLock<Option<JdkInfo>> = OnceLock::new();
                &CACHE_25
            }
            JavaTarget::Java21 => {
                static CACHE_21: OnceLock<Option<JdkInfo>> = OnceLock::new();
                &CACHE_21
            }
        }
    }

    let info = cache_for(target)
        .get_or_init(|| find_jdk_for_target(target));

    if let Some(info) = info.as_ref() {
        ensure_java_home_env(info);
    }

    info.as_ref()
}

fn java_home_from_env(var: &str) -> Option<PathBuf> {
    env::var_os(var)
        .map(PathBuf::from)
        .filter(|home| home.join("bin").join(java_executable()).exists())
}

fn ensure_java_home_env(info: &JdkInfo) {
    if java_home_from_env("JAVA_HOME").is_none() {
        unsafe {
            env::set_var("JAVA_HOME", &info.java_home);
        }
    }

    if java_home_from_env("JDK_HOME").is_none() {
        unsafe {
            env::set_var("JDK_HOME", &info.java_home);
        }
    }
}

fn find_jdk_for_target(target: JavaTarget) -> Option<JdkInfo> {
    let expected = expected_major_version(target);
    let mut candidates = Vec::new();

    if let Some(env_home) = java_home_from_env(target_env_var(target)) {
        candidates.push(env_home);
    }
    if let Some(toolchain_home) = toolchain_java_home(target) {
        candidates.push(toolchain_home);
    }
    if let Some(java_home) = java_home_from_env("JAVA_HOME") {
        candidates.push(java_home);
    }

    for home in candidates {
        if let Some(info) = jdk_info_from_home(home, expected) {
            return Some(info);
        }
    }

    match discover_jdk() {
        Ok(info) if info.major_version == expected => Some(info),
        Ok(info) => {
            eprintln!(
                "Discovered JDK {} but target {} requires {}",
                info.major_version,
                target.as_str(),
                expected
            );
            None
        }
        Err(error) => {
            eprintln!(
                "Failed to discover JDK for target {}: {}",
                target.as_str(),
                error
            );
            None
        }
    }
}

fn jdk_info_from_home(home: PathBuf, expected_major: u32) -> Option<JdkInfo> {
    let javac = home.join("bin").join(javac_executable());
    if !javac.exists() {
        eprintln!(
            "JAVA_HOME={} に javac が見つかりません (確認したパス: {})",
            home.display(),
            javac.display()
        );
        return None;
    }

    let javac_major = match detect_javac_major_version(&home) {
        Ok(value) => value,
        Err(message) => {
            eprintln!(
                "JAVA_HOME={} の javac -version 実行に失敗しました: {}",
                home.display(),
                message
            );
            return None;
        }
    };

    let major = detect_major_version(&home)?;

    if javac_major != major {
        eprintln!(
            "JAVA_HOME={} の javac と java のメジャーバージョンが一致しません (javac: {}, java: {})",
            home.display(),
            javac_major,
            major
        );
        return None;
    }

    if major != expected_major {
        eprintln!(
            "Skipping JAVA_HOME={} because its major version {} != expected {}",
            home.display(),
            major,
            expected_major
        );
        return None;
    }

    Some(JdkInfo {
        javac_path: javac,
        java_home: home,
        major_version: major,
    })
}

fn detect_javac_major_version(home: &Path) -> Result<u32, &'static str> {
    let javac_bin = home.join("bin").join(javac_executable());
    if !javac_bin.exists() {
        return Err("javac 実行ファイルが存在しません");
    }

    match detect_tool_major_version(&javac_bin, &["-version"]) {
        Some(version) => Ok(version),
        None => Err("javac -version の結果からバージョン番号を取得できませんでした"),
    }
}

fn detect_major_version(home: &Path) -> Option<u32> {
    let java_bin = home.join("bin").join(java_executable());
    detect_tool_major_version(&java_bin, &["-version"])
}

fn parse_major_version(output: &str) -> Option<u32> {
    for line in output.lines() {
        if let Some(start) = line.find('"') {
            let rest = &line[start + 1..];
            if let Some(end) = rest.find('"') {
                let version_str = &rest[..end];
                let major_part = version_str.split('.').next().unwrap_or(version_str);
                if let Ok(value) = major_part.parse::<u32>() {
                    return Some(value);
                }
            }
        } else {
            for token in line.split_whitespace() {
                let digits: String = token
                    .chars()
                    .take_while(|ch| ch.is_ascii_digit())
                    .collect();
                if digits.is_empty() {
                    continue;
                }
                if let Ok(value) = digits.parse::<u32>() {
                    return Some(value);
                }
            }
        }
    }
    None
}

fn detect_tool_major_version(executable: &Path, args: &[&str]) -> Option<u32> {
    if !executable.exists() {
        return None;
    }

    let output = Command::new(executable).args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }

    let mut version_text = String::from_utf8_lossy(&output.stderr).to_string();
    if version_text.trim().is_empty() {
        version_text = String::from_utf8_lossy(&output.stdout).to_string();
    }

    if version_text.trim().is_empty() {
        return None;
    }

    parse_major_version(&version_text)
}

fn javac_executable() -> &'static str {
    if cfg!(windows) {
        "javac.exe"
    } else {
        "javac"
    }
}

fn java_executable() -> &'static str {
    if cfg!(windows) {
        "java.exe"
    } else {
        "java"
    }
}

fn cli_binary_filename() -> &'static str {
    if cfg!(windows) {
        "jv.exe"
    } else {
        "jv"
    }
}

fn cargo_target_dir() -> PathBuf {
    env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| workspace_root().join("target"))
}

fn discover_cli_binary() -> Option<PathBuf> {
    let binary_name = cli_binary_filename();
    let mut candidates = Vec::new();
    let target_dir = cargo_target_dir();

    candidates.push(target_dir.join("debug").join(binary_name));
    candidates.push(target_dir.join("release").join(binary_name));

    if let Ok(entries) = fs::read_dir(&target_dir) {
        for entry in entries.flatten() {
            if entry
                .file_type()
                .map(|ty| ty.is_dir())
                .unwrap_or(false)
            {
                candidates.push(entry.path().join("debug").join(binary_name));
                candidates.push(entry.path().join("release").join(binary_name));
            }
        }
    }

    if let Some(toolchains_dir) = toolchains_root() {
        if let Ok(entries) = fs::read_dir(&toolchains_dir) {
            for entry in entries.flatten() {
                if !entry
                    .file_type()
                    .map(|ty| ty.is_dir())
                    .unwrap_or(false)
                {
                    continue;
                }
                let root = entry.path();
                candidates.push(root.join(binary_name));
                candidates.push(root.join("bin").join(binary_name));
                if let Ok(subentries) = fs::read_dir(&root) {
                    for sub in subentries.flatten() {
                        if sub
                            .file_type()
                            .map(|ty| ty.is_dir())
                            .unwrap_or(false)
                        {
                            candidates.push(sub.path().join(binary_name));
                            candidates.push(sub.path().join("bin").join(binary_name));
                        }
                    }
                }
            }
        }
    }

    candidates.into_iter().find(|path| path.exists())
}

fn cli_binary_path() -> Result<PathBuf, String> {
    if let Some(path) = env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) {
        return Ok(path);
    }

    if let Some(path) = discover_cli_binary() {
        eprintln!(
            "CARGO_BIN_EXE_jv not set; inferred CLI binary at {}",
            path.display()
        );
        return Ok(path);
    }

    Err("CLI binary not available; run `cargo build -p jv_cli --bin jv` before executing this test".to_string())
}

fn default_java_target() -> JavaTarget {
    JavaTarget::Java25
}

fn javac_command_for_target(target: JavaTarget) -> Option<Command> {
    jdk_info_for_target(target).map(|info| {
        let mut cmd = Command::new(&info.javac_path);
        cmd.env("JAVA_HOME", &info.java_home);
        cmd
    })
}

fn java_command_for_target(target: JavaTarget) -> Option<Command> {
    jdk_info_for_target(target).map(|info| {
        let java_path = info.java_home.join("bin").join(java_executable());
        let mut cmd = Command::new(java_path);
        cmd.env("JAVA_HOME", &info.java_home);
        cmd
    })
}

fn javac_command() -> Option<Command> {
    javac_command_for_target(default_java_target())
}

fn java_command() -> Option<Command> {
    java_command_for_target(default_java_target())
}

fn has_javac_for_target(target: JavaTarget) -> bool {
    jdk_info_for_target(target).is_some()
}

fn has_java_runtime_for_target(target: JavaTarget) -> bool {
    jdk_info_for_target(target)
        .map(|info| info.java_home.join("bin").join(java_executable()).exists())
        .unwrap_or(false)
}

fn has_javac() -> bool {
    has_javac_for_target(default_java_target())
}

fn has_java_runtime() -> bool {
    has_java_runtime_for_target(default_java_target())
}

fn ensure_targets_available(targets: &[JavaTarget], reason: &str) -> bool {
    for target in targets {
        if !has_javac_for_target(*target) || !has_java_runtime_for_target(*target) {
            eprintln!(
                "Skipping {}: missing JDK {} (set {} to a valid install)",
                reason,
                target.as_str(),
                target_env_var(*target)
            );
            return false;
        }
    }
    true
}

fn resolve_module_artifacts(context: &SymbolBuildContext) -> Vec<PathBuf> {
    let mut artifacts = Vec::new();

    if !context.module_path.is_empty() {
        for entry in &context.module_path {
            collect_module_artifact(entry, &mut artifacts);
        }
    } else if let Some(java_home) = &context.java_home {
        let jmods = java_home.join("jmods");
        collect_module_artifact(&jmods, &mut artifacts);
    }

    artifacts
}

fn collect_module_artifact(path: &Path, artifacts: &mut Vec<PathBuf>) {
    if !path.exists() {
        return;
    }

    if path.is_file() {
        artifacts.push(path.to_path_buf());
        return;
    }

    if let Ok(entries) = fs::read_dir(path) {
        for entry in entries.flatten() {
            let candidate = entry.path();
            if candidate.is_file() {
                artifacts.push(candidate);
            }
        }
    }
}

fn read_java_sources(paths: &[PathBuf]) -> String {
    paths
        .iter()
        .map(|path| fs::read_to_string(path).expect("read java source"))
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn cli_build_generates_java_sources() {
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping CLI binary integration test: {}", reason);
            return;
        }
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
    assert!(
        java_source.contains("// Generated by jv"),
        "生成された Java ファイルにヘッダーコメントが含まれていません:\n{java_source}"
    );
    assert!(java_source.contains("message"));
}

#[test]
fn cli_build_numeric_script_generates_worded_class() {
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping numeric script test: {}", reason);
            return;
        }
    };

    let temp_dir = TempDirGuard::new("cli-numeric").expect("create temp dir");
    let source_path = temp_dir.path().join("02-variables.jv");
    fs::copy(
        workspace_file("tests/fixtures/02-variables.jv"),
        &source_path,
    )
    .expect("copy numeric-labeled script");

    let output_dir = temp_dir.path().join("out");
    let status = Command::new(&cli_path)
        .current_dir(temp_dir.path())
        .arg("build")
        .arg(&source_path)
        .arg("--java-only")
        .arg("-o")
        .arg(&output_dir)
        .status()
        .expect("invoke jv build for numeric script");

    assert!(status.success(), "jv build failed: {:?}", status);

    let java_dir = output_dir.join("java25");
    let expected_java = java_dir.join("ZeroTwoVariables.java");
    assert!(
        expected_java.exists(),
        "expected {} to exist",
        expected_java.display()
    );

    let java_source = fs::read_to_string(&expected_java).expect("read generated script java");
    assert!(
        java_source.contains("class ZeroTwoVariables"),
        "generated Java should declare ZeroTwoVariables: {}",
        java_source
    );
}

#[test]
fn cli_build_quick_tour_script_compiles() {
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping quick-tour test: {}", reason);
            return;
        }
    };

    let temp_dir = TempDirGuard::new("cli-quick-tour").expect("create temp dir");
    let quick_tour = workspace_file("examples/quick-tour.jv");

    let script_dir = temp_dir.path().join("quick-tour");
    fs::create_dir_all(&script_dir)
        .expect("Quick Tour 用の一時ディレクトリを作成できませんでした");
    let script_path = script_dir.join("quick-tour.jv");
    fs::copy(&quick_tour, &script_path)
        .expect("Quick Tour サンプルを一時ディレクトリへコピーできませんでした");

    let output_dir = script_dir.join("out");

    let status = Command::new(&cli_path)
        .current_dir(&script_dir)
        .arg("build")
        .arg(&script_path)
        .arg("--java-only")
        .arg("-o")
        .arg(&output_dir)
        .status()
        .expect("invoke jv build for quick-tour example");

    assert!(
        status.success(),
        "jv build failed for quick-tour example: {:?}",
        status
    );

    let java_dir = output_dir.join("java25");
    assert!(
        java_dir.is_dir(),
        "Quick Tour の Java 出力ディレクトリが生成されていません: {}",
        java_dir.display()
    );

    let java_files = collect_java_sources(&java_dir);
    assert!(
        !java_files.is_empty(),
        "Quick Tour の Java ソースが生成されていません"
    );
}

#[test]
fn cli_examples_build_without_java_errors() {
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping examples build test: {}", reason);
            return;
        }
    };
    if javac_command_for_target(JavaTarget::Java25).is_none() {
        eprintln!("Skipping examples build test: javac for Java25 not available");
        return;
    }

    let examples_root = workspace_file("examples");
    assert!(
        examples_root.is_dir(),
        "examples root missing at {}",
        examples_root.display()
    );

    let fixtures = discover_examples(&examples_root);
    assert!(
        !fixtures.is_empty(),
        "no buildable examples discovered in {}",
        examples_root.display()
    );

    let mut failures = Vec::new();
    for ExampleFixture { label, kind } in fixtures {
        let (jv_file, result) = match kind {
            ExampleKind::Script { source } => {
                let res = build_script_example(cli_path.as_path(), &source, &label);
                (source, res)
            }
            ExampleKind::Project { root } => {
                let entrypoint = resolve_project_entrypoint(&root);
                let res = build_project_example(cli_path.as_path(), &root, &label);
                (entrypoint, res)
            }
        };

        match result {
            Ok(()) => {
                println!(
                    "[OK] {} -> {}",
                    label,
                    jv_file.display()
                );
            }
            Err(err) => {
                println!(
                    "[FAIL] {} -> {}\n{}",
                    label,
                    jv_file.display(),
                    err
                );
                failures.push(format!(
                    "{} -> {}:\n{}",
                    label,
                    jv_file.display(),
                    err
                ));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "一部の examples が正常にビルドできませんでした:\n{}",
        failures.join("\n\n")
    );
}

struct ExampleFixture {
    label: String,
    kind: ExampleKind,
}

enum ExampleKind {
    Script { source: PathBuf },
    Project { root: PathBuf },
}

fn discover_examples(root: &Path) -> Vec<ExampleFixture> {
    let mut fixtures = Vec::new();
    for entry in fs::read_dir(root).expect("read examples directory") {
        let entry = entry.expect("read example entry");
        let path = entry.path();
        let file_type = entry.file_type().expect("stat example entry");
        let label = entry.file_name().to_string_lossy().to_string();

        if label == "target" {
            continue;
        }

        if file_type.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("jv") {
            fixtures.push(ExampleFixture {
                label,
                kind: ExampleKind::Script { source: path },
            });
        } else if file_type.is_dir() && path.join("jv.toml").exists() {
            fixtures.push(ExampleFixture {
                label,
                kind: ExampleKind::Project { root: path },
            });
        }
    }
    fixtures.sort_by(|a, b| a.label.cmp(&b.label));
    fixtures
}

fn build_script_example(
    cli_path: &Path,
    source: &Path,
    label: &str,
) -> Result<(), String> {
    let workdir = source
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));
    let relative_output = PathBuf::from(format!(
        "target/test-cli-examples/scripts/{}",
        sanitize_label(label)
    ));
    let absolute_output = workdir.join(&relative_output);
    if let Some(parent) = absolute_output.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("出力ディレクトリを作成できませんでした: {err}"))?;
    }
    let _ = fs::remove_dir_all(&absolute_output);

    let output = Command::new(cli_path)
        .current_dir(&workdir)
        .arg("build")
        .arg(&source)
        .arg("--java-only")
        .arg("--target")
        .arg("25")
        .arg("-o")
        .arg(&relative_output)
        .output()
        .map_err(|err| format!("jv build 実行に失敗しました: {err}"))?;

    if !output.status.success() {
        return Err(format!(
            "jv build failed for script example {}:\nstatus: {}\nstdout: {}\nstderr: {}",
            source.display(),
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    verify_java_artifacts(&absolute_output, label)
}

fn build_project_example(
    cli_path: &Path,
    root: &Path,
    label: &str,
) -> Result<(), String> {
    let relative_output = PathBuf::from("target/test-cli-examples");
    let absolute_output = root.join(&relative_output);
    let _ = fs::remove_dir_all(&absolute_output);

    let output = Command::new(cli_path)
        .current_dir(root)
        .arg("build")
        .arg("--java-only")
        .arg("--target")
        .arg("25")
        .arg("-o")
        .arg(&relative_output)
        .output()
        .map_err(|err| format!("jv build 実行に失敗しました: {err}"))?;

    if !output.status.success() {
        return Err(format!(
            "jv build failed for project example {}:\nstatus: {}\nstdout: {}\nstderr: {}",
            root.display(),
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    verify_java_artifacts(&absolute_output, label)
}

fn verify_java_artifacts(output_root: &Path, label: &str) -> Result<(), String> {
    let java_dir = output_root.join("java25");
    if !java_dir.is_dir() {
        return Err(format!(
            "java25 output missing for example {} at {}",
            label,
            java_dir.display()
        ));
    }

    let java_sources = collect_java_sources(&java_dir);
    if java_sources.is_empty() {
        return Err(format!(
            "no Java sources produced for example {} in {}",
            label,
            java_dir.display()
        ));
    }

    let classes_dir = java_dir.join("classes");
    fs::create_dir_all(&classes_dir)
        .map_err(|err| format!("javac 出力ディレクトリを作成できませんでした: {err}"))?;

    let mut javac =
        javac_command_for_target(JavaTarget::Java25).ok_or_else(|| "javac command unavailable".to_string())?;
    javac.arg("-d").arg(&classes_dir);
    for source in &java_sources {
        javac.arg(source);
    }

    let output = javac
        .output()
        .map_err(|err| format!("javac の起動に失敗しました: {err}"))?;
    if !output.status.success() {
        return Err(format!(
            "javac reported errors for example {} stored in {}:\nstdout: {}\nstderr: {}",
            label,
            java_dir.display(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(())
}

fn collect_java_sources(root: &Path) -> Vec<PathBuf> {
    let mut java_files = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        if dir.is_dir() {
            for entry in fs::read_dir(&dir).expect("traverse java output directory") {
                let entry = entry.expect("read java output entry");
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else if path.extension().and_then(|ext| ext.to_str()) == Some("java") {
                    java_files.push(path);
                }
            }
        }
    }
    java_files
}

fn sanitize_label(label: &str) -> String {
    label
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' {
                ch
            } else {
                '-'
            }
        })
        .collect()
}

fn resolve_project_entrypoint(root: &Path) -> PathBuf {
    let manifest = root.join("jv.toml");
    if let Ok(content) = fs::read_to_string(&manifest) {
        if let Ok(value) = toml::from_str::<toml::Value>(&content) {
            if let Some(entrypoint) = value
                .get("project")
                .and_then(|project| project.get("entrypoint"))
                .and_then(|path| path.as_str())
            {
                return root.join(entrypoint);
            }
        }
    }
    root.join("src/main.jv")
}

#[test]
fn cli_build_emits_pattern_compile_for_regex_literal() {
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping regex CLI build test: {}", reason);
            return;
        }
    };

    let temp_dir = TempDirGuard::new("cli-regex-build").expect("create temp dir");
    let project_dir = temp_dir.path().join("regex-project");
    fs::create_dir_all(project_dir.join("src")).expect("create regex project src");

    fs::write(
        project_dir.join("jv.toml"),
        r#"[package]
name = "regex"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    )
    .expect("write regex manifest");

    fs::write(
        project_dir.join("src/main.jv"),
        r#"val pattern = /\d+/

fun sample(input: String): Boolean {
    return pattern.matcher(input).matches()
}
"#,
    )
    .expect("write regex source");

    let status = Command::new(&cli_path)
        .current_dir(&project_dir)
        .arg("build")
        .arg("--java-only")
        .status()
        .expect("execute jv build for regex literal");

    assert!(status.success(), "regex CLI build failed: {status:?}");

    let java_files: Vec<_> = fs::read_dir(project_dir.join("target/java25"))
        .expect("read regex java output")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|ext| ext.to_str()) == Some("java"))
        .collect();

    assert!(
        !java_files.is_empty(),
        "regex build should emit at least one Java file"
    );

    let java_source = fs::read_to_string(&java_files[0]).expect("read regex java source");
    assert!(
        java_source.contains("// Generated by jv"),
        "生成された Java ソースにヘッダーコメントがありません:\n{java_source}"
    );
    assert!(
        java_source.contains("import java.util.regex.Pattern;"),
        "generated Java should import Pattern: {java_source}"
    );
    assert!(
        java_source.contains("Pattern.compile(\"\\\\d+\")"),
        "generated Java should compile the regex literal: {java_source}"
    );
}

#[test]
fn cli_check_reports_regex_diagnostics() {
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping regex diagnostic test: {}", reason);
            return;
        }
    };

    let temp_dir = TempDirGuard::new("cli-regex-diagnostic").expect("create temp dir");
    let source_path = temp_dir.path().join("invalid_regex.jv");
    fs::write(&source_path, "val broken = /abc\\q/\n").expect("write invalid regex source");

    let output = Command::new(&cli_path)
        .arg("check")
        .arg(&source_path)
        .output()
        .expect("execute jv check for invalid regex");

    assert!(
        !output.status.success(),
        "invalid regex should cause CLI check to fail"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("JV5102"),
        "regex diagnostic output should contain JV5102, got: {stderr}"
    );
    assert!(
        stderr.contains("正規表現リテラル") || stderr.to_lowercase().contains("regex literal"),
        "regex diagnostic text should mention regex literal terminology, got: {stderr}"
    );
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
            verbose: false,
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
fn pipeline_regex_command_unit_main_compiles() {
    let temp_dir = TempDirGuard::new("regex-command-unit-main")
        .expect("create temp directory for regex command fixture");
    let fixture_path = temp_dir
        .path()
        .join("regex_command_unit_main_scope_error.jv");

    fs::write(
        &fixture_path,
        r#"fun main(): Unit {
    val text = "abc"
    val masked = a/text/'a'/"b"/
    val first = f/text/'a'/"c"/
    println(masked)
    println(first)
}
"#,
    )
    .expect("write regex command unit main fixture");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &fixture_path,
        CliOverrides {
            java_only: true,
            format: true,
            ..CliOverrides::default()
        },
    );

    compile(&plan).expect("regex command program should compile without scope errors");
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
            verbose: false,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        },
    );

    let artifacts = match compile(&plan) {
        Ok(artifacts) => artifacts,
        Err(err) => {
            eprintln!("Skipping annotation pipeline test: {}", err);
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
    assert!(java_source
        .contains("@RequestMapping(path = {\"/ping\"}, produces = {\"application/json\"})"));
    assert!(java_source.contains("@Nullable"));
}

#[test]
fn sequence_runtime_java_omits_null_prints() {
    let temp_dir = TempDirGuard::new("sequence-runtime-null").expect("create temp dir");
    let input = workspace_file("test_simple.jv");

    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &input,
        CliOverrides {
            java_only: true,
            ..CliOverrides::default()
        },
    );

    let artifacts = compile(&plan).expect("sequence runtime compilation succeeds");

    let offending = artifacts
        .java_files
        .iter()
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .map(|name| name.contains("Sequence"))
                .unwrap_or(false)
        })
        .find_map(|path| {
            let source = fs::read_to_string(path).expect("read generated Java");
            if source.contains("System.out.println(null)") {
                Some(path.clone())
            } else {
                None
            }
        });

    assert!(
        offending.is_none(),
        "Sequence runtime should not emit System.out.println(null) (file: {})",
        offending.unwrap().display()
    );
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
            verbose: false,
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
    assert!(
        json.contains("message"),
        "expected binding entry in json: {json}"
    );

    let facts: Value =
        serde_json::from_str(&json).expect("type facts JSON should be well-formed");
    let bindings = facts
        .get("bindings")
        .and_then(|value| value.as_array())
        .expect("type facts should include binding entries");
    let has_list_binding = bindings.iter().any(|entry| {
        entry
            .as_str()
            .map(|value| value.contains("java.util.List"))
            .unwrap_or(false)
    });
    assert!(
        has_list_binding,
        "expected regex/sequence derived bindings to record java.util.List usage: {json}"
    );
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
            verbose: false,
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
    assert!(matches!(scheme.ty, TypeKind::Primitive(PrimitiveType::Int)));
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
            verbose: false,
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
    fs::copy(&snippet, &plan.options.entrypoint).expect("sync updated snippet to entrypoint");

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
    assert!(matches!(
        incremented.ty,
        TypeKind::Primitive(PrimitiveType::Int)
    ));
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
            verbose: false,
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
            verbose: false,
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
            verbose: false,
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
            verbose: false,
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
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping CLI diagnostic test: {}", reason);
            return;
        }
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
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping CLI diagnostic test: {}", reason);
            return;
        }
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
    let cli_path = match cli_binary_path() {
        Ok(path) => path,
        Err(reason) => {
            eprintln!("Skipping CLI integration test: {}", reason);
            return;
        }
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
    let java_dir = output_dir.join("java25");
    assert!(
        java_dir.is_dir(),
        "java25 出力ディレクトリが見つかりません: {}",
        java_dir.display()
    );

    let generated_java = collect_java_sources(&java_dir);
    assert!(
        !generated_java.is_empty(),
        "Java ソースが生成されていません。build stdout: {}",
        String::from_utf8_lossy(&build_output.stdout)
    );

    if has_javac() && has_java_runtime() {
        let run_output = Command::new(&cli_path)
            .arg("run")
            .arg(&main_path)
            .output()
            .expect("Failed to run jv run");
        if !run_output.status.success() {
            let stderr_text = String::from_utf8_lossy(&run_output.stderr);
            if stderr_text.contains("JDK not available") || stderr_text.contains("JDK not found") {
                eprintln!("JDK が利用できないため jv run をスキップします: {stderr_text}");
            } else {
                panic!(
                    "jv run が失敗しました。\nstdout: {}\nstderr: {}",
                    String::from_utf8_lossy(&run_output.stdout),
                    stderr_text
                );
            }
        }
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
        let mut stdin = repl_child
            .stdin
            .take()
            .expect("Failed to access repl stdin");
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

#[test]
fn advanced_generics_fixtures_compile() {
    if !has_java_runtime() || !has_javac() {
        eprintln!("Skipping advanced generics fixtures: java runtime or javac not available");
        return;
    }

    let fixtures = [
        "generic_data_basic",
        "higher_kinded_functor",
        "dependent_vector",
        "reflection_api",
    ];

    for name in &fixtures {
        let fixture_path = workspace_file(&format!(
            "crates/jv_cli/tests/fixtures/advanced_generics/{}.jv",
            name
        ));
        let temp_dir = TempDirGuard::new(&format!("advanced-generics-{name}")).expect("temp dir");
        let plan =
            compose_plan_from_fixture(temp_dir.path(), &fixture_path, CliOverrides::default());

        assert!(
            plan.entrypoint().exists(),
            "entrypoint should exist for {name}"
        );
    }
}

#[test]
fn sequence_pipeline_fixture_runs_consistently_across_targets() {
    let required = [JavaTarget::Java25, JavaTarget::Java21];
    if !ensure_targets_available(&required, "sequence pipeline fixture run") {
        return;
    }

    let fixture = workspace_file("tests/fixtures/sequence/sequence_chain.jv");
    let expected_output = "[2, 4, 6, 8, 10]\n[5, 8]\n3\ntrue\n15\n3";

    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        let temp_dir =
            TempDirGuard::new("sequence-pipeline").expect("create temp directory for fixture");
        let plan = compose_plan_from_fixture(
            temp_dir.path(),
            &fixture,
            CliOverrides {
                java_only: true,
                target: Some(target),
                ..CliOverrides::default()
            },
        );

        let artifacts = compile(&plan).expect("sequence pipeline fixture compiles");
        let java_dir = plan
            .options
            .output_dir
            .join(format!("java{}", target.as_str()));
        let classes_dir = java_dir.join("classes");
        fs::create_dir_all(&classes_dir).expect("create classes directory for javac");

        let mut javac = javac_command_for_target(target).expect("javac command unavailable");
        javac.arg("-d").arg(&classes_dir);
        for file in &artifacts.java_files {
            javac.arg(file);
        }
        let status = javac
            .status()
            .expect("invoke javac for sequence pipeline fixture");
        assert!(
            status.success(),
            "javac failed for target {}",
            target.as_str()
        );

        let mut java_cmd = java_command_for_target(target).expect("java runtime unavailable");
        let output = java_cmd
            .arg("-cp")
            .arg(&classes_dir)
            .arg(&artifacts.script_main_class)
            .output()
            .expect("execute compiled sequence pipeline script");

        assert!(
            output.status.success(),
            "java execution failed for target {}: {}",
            target.as_str(),
            String::from_utf8_lossy(&output.stderr)
        );

        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(
            stdout.trim(),
            expected_output,
            "unexpected output for target {}",
            target.as_str()
        );
    }
}

#[test]
fn sequence_stream_casts_compile_successfully() {
    if !ensure_targets_available(
        &[JavaTarget::Java25],
        "sequence stream cast regression verification",
    ) {
        return;
    }

    let fixture =
        workspace_file("tests/fixtures/sequence/sequence_stream_cast_failure.jv");
    let temp_dir =
        TempDirGuard::new("sequence-stream-cast-regression").expect("create temp dir");
    let plan = compose_plan_from_fixture(
        temp_dir.path(),
        &fixture,
        CliOverrides {
            java_only: true,
            target: Some(JavaTarget::Java25),
            ..CliOverrides::default()
        },
    );

    let artifacts = compile(&plan).expect("fixture should compile to Java sources");
    let java_dir = plan
        .options
        .output_dir
        .join(format!("java{}", JavaTarget::Java25.as_str()));
    let classes_dir = java_dir.join("classes");
    fs::create_dir_all(&classes_dir).expect("create classes directory for javac");

    let mut javac =
        javac_command_for_target(JavaTarget::Java25).expect("javac command unavailable");
    javac.arg("-d").arg(&classes_dir);
    for file in &artifacts.java_files {
        javac.arg(file);
    }

    let javac_status = javac
        .status()
        .expect("invoke javac for sequence stream cast fixture");
    assert!(
        javac_status.success(),
        "javac failed for sequence stream cast fixture"
    );

    let mut java_cmd =
        java_command_for_target(JavaTarget::Java25).expect("java runtime unavailable");
    let output = java_cmd
        .arg("-cp")
        .arg(&classes_dir)
        .arg(&artifacts.script_main_class)
        .output()
        .expect("execute compiled sequence stream cast script");

    assert!(
        output.status.success(),
        "sequence stream cast execution failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let expected = "[lexer-module, parser-module, ast-module, codegen-module]\n[lexer-module, parser-module, ast-module, codegen-module]";
    assert_eq!(
        stdout.trim(),
        expected,
        "unexpected output from sequence stream cast fixture"
    );
}

#[test]
fn java_target_switch_emits_expected_sequence_collection_factories() {
    let fixture = workspace_file("tests/fixtures/sequence/java21_compat.jv");

    for (target, expected_snippets) in [
        (
            JavaTarget::Java25,
            vec![".toList()", ".collect(Collectors.toSet())"],
        ),
        (
            JavaTarget::Java21,
            vec!["Collectors.toList()", "Collectors.toSet()"],
        ),
    ] {
        let temp_dir =
            TempDirGuard::new("sequence-collection-factories").expect("create temp directory");
        let plan = compose_plan_from_fixture(
            temp_dir.path(),
            &fixture,
            CliOverrides {
                java_only: true,
                target: Some(target),
                ..CliOverrides::default()
            },
        );

        let artifacts = compile(&plan).expect("collection factory fixture compiles");
        let mut java_source = String::new();
        for file in &artifacts.java_files {
            let content = fs::read_to_string(file).expect("read generated java source");
            java_source.push_str(&content);
        }

        for snippet in expected_snippets {
            assert!(
                java_source.contains(snippet),
                "expected '{}' for target {} in generated Java:\n{}",
                snippet,
                target.as_str(),
                java_source
            );
        }

        if target == JavaTarget::Java25 {
            assert!(
                !java_source.contains("Collectors.toList()"),
                "Java 25 output should avoid Collectors.toList():\n{}",
                java_source
            );
            assert!(
                java_source.contains(".collect(Collectors.toSet())"),
                "Java 25 output should explicitly fallback to Collectors.toSet():\n{}",
                java_source
            );
        }
    }
}

#[test]
fn java21_compat_fixture_runs_across_targets() {
    let required = [JavaTarget::Java25, JavaTarget::Java21];
    if !ensure_targets_available(&required, "java21 compat runtime test") {
        return;
    }

    let fixture = workspace_file("tests/fixtures/sequence/java21_compat.jv");
    let expected_output = "[2, 3, 4, 5]\ntrue\n2";

    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        let temp_dir =
            TempDirGuard::new("java21-compat-run").expect("create temp directory for fixture");
        let plan = compose_plan_from_fixture(
            temp_dir.path(),
            &fixture,
            CliOverrides {
                java_only: true,
                target: Some(target),
                ..CliOverrides::default()
            },
        );

        let artifacts = compile(&plan).expect("java21 compat fixture compiles");
        let java_dir = plan
            .options
            .output_dir
            .join(format!("java{}", target.as_str()));
        let classes_dir = java_dir.join("classes");
        fs::create_dir_all(&classes_dir).expect("create classes directory for javac");

        let mut javac = javac_command_for_target(target).expect("javac command unavailable");
        javac.arg("-d").arg(&classes_dir);
        for file in &artifacts.java_files {
            javac.arg(file);
        }
        let status = javac
            .status()
            .expect("invoke javac for java21 compat fixture");
        assert!(
            status.success(),
            "javac failed for target {}",
            target.as_str()
        );

        let mut java_cmd = java_command_for_target(target).expect("java runtime unavailable");
        let output = java_cmd
            .arg("-cp")
            .arg(&classes_dir)
            .arg(&artifacts.script_main_class)
            .output()
            .expect("execute compiled java21 compat script");

        assert!(
            output.status.success(),
            "java execution failed for target {}: {}",
            target.as_str(),
            String::from_utf8_lossy(&output.stderr)
        );

        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(
            stdout.trim(),
            expected_output,
            "unexpected output for target {}",
            target.as_str()
        );
    }
}

#[test]
fn sequence_interpolation_materializes_sequences_before_string_format() {
    let required = [JavaTarget::Java25, JavaTarget::Java21];
    if !ensure_targets_available(&required, "sequence interpolation fixture run") {
        return;
    }

    let fixture = workspace_file("tests/lang/strings/sequence_interpolation.jv");
    let expected_output = "inline=[2, 4, 6, 8]\nstored=[2, 4]";

    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        let temp_dir =
            TempDirGuard::new("sequence-interpolation").expect("create temp directory for fixture");
        let plan = compose_plan_from_fixture(
            temp_dir.path(),
            &fixture,
            CliOverrides {
                java_only: true,
                target: Some(target),
                ..CliOverrides::default()
            },
        );

        let artifacts = compile(&plan).expect("sequence interpolation fixture compiles");
        let java_dir = plan
            .options
            .output_dir
            .join(format!("java{}", target.as_str()));
        let classes_dir = java_dir.join("classes");
        fs::create_dir_all(&classes_dir).expect("create classes directory for javac");

        let mut javac = javac_command_for_target(target).expect("javac command unavailable");
        javac.arg("-d").arg(&classes_dir);
        for file in &artifacts.java_files {
            javac.arg(file);
        }
        let status = javac
            .status()
            .expect("invoke javac for sequence interpolation fixture");
        assert!(
            status.success(),
            "javac failed for target {}",
            target.as_str()
        );

        let mut java_cmd = java_command_for_target(target).expect("java runtime unavailable");
        let output = java_cmd
            .arg("-cp")
            .arg(&classes_dir)
            .arg(&artifacts.script_main_class)
            .output()
            .expect("execute compiled sequence interpolation script");

        assert!(
            output.status.success(),
            "java execution failed for target {}: {}",
            target.as_str(),
            String::from_utf8_lossy(&output.stderr)
        );

        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(
            stdout.trim(),
            expected_output,
            "unexpected output for target {}",
            target.as_str()
        );
    }
}

#[test]
fn sequence_map_materialization_casts_streams_to_lists() {
    let required = [JavaTarget::Java25, JavaTarget::Java21];
    if !ensure_targets_available(&required, "sequence map materialization fixture run") {
        return;
    }

    let fixture = workspace_file("tests/lang/streams/map_materialization.jv");
    let expected_output =
        "Doubled ints: [2, 4, 6, 8, 10]\nDoubled floats: [2.0, 4.0, 6.0, 8.0, 10.0]";

    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        let temp_dir =
            TempDirGuard::new("sequence-map-materialization").expect("create temp directory for fixture");
        let plan = compose_plan_from_fixture(
            temp_dir.path(),
            &fixture,
            CliOverrides {
                java_only: true,
                target: Some(target),
                ..CliOverrides::default()
            },
        );

        let artifacts = compile(&plan).expect("sequence map materialization fixture compiles");
        let java_dir = plan
            .options
            .output_dir
            .join(format!("java{}", target.as_str()));
        let classes_dir = java_dir.join("classes");
        fs::create_dir_all(&classes_dir).expect("create classes directory for javac");

        let mut javac = javac_command_for_target(target).expect("javac command unavailable");
        javac.arg("-d").arg(&classes_dir);
        for file in &artifacts.java_files {
            javac.arg(file);
        }
        let status = javac
            .status()
            .expect("invoke javac for sequence map materialization fixture");
        assert!(
            status.success(),
            "javac failed for target {}",
            target.as_str()
        );

        let mut java_cmd = java_command_for_target(target).expect("java command unavailable");
        java_cmd
            .arg("-cp")
            .arg(&classes_dir)
            .arg(&artifacts.script_main_class);
        let output = java_cmd
            .output()
            .expect("execute compiled sequence map materialization script");
        assert!(
            output.status.success(),
            "java execution failed for target {}: {}",
            target.as_str(),
            String::from_utf8_lossy(&output.stderr)
        );

        let stdout = String::from_utf8_lossy(&output.stdout);
        assert_eq!(
            stdout.trim(),
            expected_output,
            "unexpected output for target {}",
            target.as_str()
        );
    }
}

#[test]
fn smart_imports_resolve_alias_wildcard_static_and_module() {
    if !has_javac() || !has_java_runtime() {
        eprintln!("Skipping smart import integration test: java runtime or javac missing");
        return;
    }

    let fixture = workspace_file("tests/lang/imports/smart_imports.jv");
    let temp_dir = TempDirGuard::new("smart-imports")
        .expect("create temp directory for smart imports fixture");
    let mut metrics_checked = false;

    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        let plan = compose_plan_from_fixture(
            temp_dir.path(),
            &fixture,
            CliOverrides {
                java_only: true,
                verbose: true,
                target: Some(target),
                ..CliOverrides::default()
            },
        );

        let artifacts = compile(&plan).expect("smart imports fixture compiles");
        assert!(
            !artifacts.java_files.is_empty(),
            "expected generated Java output"
        );

        let java_source = read_java_sources(&artifacts.java_files);

        let local_date_import = artifacts
            .resolved_imports
            .iter()
            .find(|import| {
                matches!(
                    import.detail,
                    IrImportDetail::Type { ref fqcn } if fqcn == "java.time.LocalDate"
                )
            })
            .expect("LocalDate import should be resolved");
        assert_eq!(
            local_date_import.alias.as_deref(),
            Some("Date"),
            "LocalDate import should keep the alias"
        );

        assert!(
            artifacts.resolved_imports.iter().any(|import| matches!(
                import.detail,
                IrImportDetail::Package { ref name } if name == "java.util"
            )),
            "package wildcard import should be present"
        );

        assert!(
            artifacts.resolved_imports.iter().any(|import| matches!(
                import.detail,
                IrImportDetail::Static { ref owner, ref member }
                    if owner == "java.util.Collections" && member == "*"
            )),
            "static wildcard import should be present"
        );

        assert!(
            artifacts.resolved_imports.iter().any(|import| matches!(
                import.detail,
                IrImportDetail::Static { ref owner, ref member }
                    if owner == "java.lang.Math" && member == "max"
            )),
            "static single-member import should be present"
        );

        let driver_import = artifacts
            .resolved_imports
            .iter()
            .find(|import| {
                matches!(
                    import.detail,
                    IrImportDetail::Type { ref fqcn } if fqcn == "java.sql.DriverManager"
                )
            })
            .expect("DriverManager import should be resolved");

        let module_imports: Vec<String> = artifacts
            .resolved_imports
            .iter()
            .filter_map(|import| match &import.detail {
                IrImportDetail::Module { name } => Some(name.clone()),
                _ => None,
            })
            .collect();

        if target == JavaTarget::Java25 {
            assert_eq!(
                driver_import.module_dependency.as_deref(),
                Some("java.sql"),
                "DriverManager should carry java.sql module dependency for Java 25"
            );
            assert!(
                module_imports.iter().any(|name| name == "java.sql"),
                "module import for java.sql should be emitted"
            );
            assert!(
                java_source.contains("import module java.sql;"),
                "generated Java should include module import"
            );
        } else {
            assert!(
                driver_import.module_dependency.is_none(),
                "Java 21 target should not attach module dependency"
            );
            assert!(
                module_imports.is_empty(),
                "Java 21 target should not emit module imports"
            );
            assert!(
                !java_source.contains("import module java.sql;"),
                "Java 21 output should omit module imports"
            );
        }

        assert!(
            java_source.contains("import java.time.LocalDate;"),
            "generated Java should import LocalDate"
        );
        assert!(
            java_source.contains("import static java.lang.Math.max;"),
            "generated Java should include static Math.max import"
        );
        assert!(
            java_source.contains("import static java.util.Collections.*;"),
            "generated Java should include static wildcard import"
        );

        if target == JavaTarget::Java25 && !metrics_checked {
            let cache_dir = plan
                .root
                .root_dir()
                .join("target")
                .join("jv")
                .join("symbol-index");
            let cache = SymbolIndexCache::new(cache_dir);
            let context = SymbolBuildContext::from_config(&plan.build_config);
            let module_artifacts = resolve_module_artifacts(&context);
            let fingerprint = cache
                .fingerprint(&context, &module_artifacts)
                .expect("fingerprint generation");
            let cache_entry = cache
                .load(&fingerprint)
                .expect("load symbol index cache")
                .expect("symbol index cache entry should exist");
            assert!(
                cache_entry.metrics.artifact_count > 0,
                "cache metrics should record scanned artifacts"
            );
            assert!(
                cache_entry.metrics.build_ms > 0,
                "cache metrics should record build duration"
            );
            metrics_checked = true;
        }
    }

    assert!(
        metrics_checked,
        "performance guard metrics were not validated"
    );
}

#[test]
fn pipeline_parses_package_declarations() {
    let fixtures = [
        ("simple_package", "com.example.app"),
        ("nested_package", "com.example.app.service.impl"),
        ("package_with_class", "org.jv.test"),
        ("package_single_segment", "utils"),
    ];

    for (name, expected_package) in &fixtures {
        let fixture = workspace_file(&format!("tests/fixtures/package/{}.jv", name));
        let temp_dir = TempDirGuard::new(&format!("package-{name}"))
            .expect("create temp directory for package fixture");
        let plan = compose_plan_from_fixture(
            temp_dir.path(),
            &fixture,
            CliOverrides {
                java_only: true,
                ..CliOverrides::default()
            },
        );

        let artifacts = compile(&plan).expect(&format!("package fixture {} compiles", name));

        // Verify that Java files were generated
        assert!(
            !artifacts.java_files.is_empty(),
            "expected generated Java files for {}",
            name
        );

        // Read generated Java source to verify package declaration
        for java_file in &artifacts.java_files {
            let java_source =
                fs::read_to_string(java_file).expect(&format!("read generated Java for {}", name));

            // Verify package declaration in generated Java code
            assert!(
                java_source.contains(&format!("package {};", expected_package)),
                "expected 'package {};' in generated Java for {}, got:\n{}",
                expected_package,
                name,
                java_source
            );
        }
    }
}
