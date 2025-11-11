use crate::pipeline::project::layout::ProjectLayout;
use crate::pipeline::project::locator::ProjectLocator;
use crate::pipeline::project::manifest::ManifestLoader;
use crate::pipeline::{BuildOptionsFactory, CliOverrides, OutputManager, compile};
use crate::tooling_failure;
use anyhow::{Context, Result, bail};
use clap::Args;
use jv_pm::JavaTarget;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// Generate JUnit sources from the test DSL and execute `mvn test`.
#[derive(Args, Debug, Clone)]
pub struct TestArgs {
    /// Optional .jv entrypoint (defaults to manifest entrypoint)
    #[arg(value_name = "entrypoint")]
    pub input: Option<String>,
    /// Clean the generated test output directory before building
    #[arg(long)]
    pub clean: bool,
    /// Override the Java target (21 or 25)
    #[arg(long, value_name = "java-target")]
    pub target: Option<JavaTarget>,
    /// Explicit path to the Maven executable to use
    #[arg(long, value_name = "path")]
    pub maven: Option<String>,
}

pub fn run(args: TestArgs) -> Result<()> {
    let start_path = compute_start_path(args.input.as_deref())?;
    let locator = ProjectLocator::new();
    let (project_root, settings, diagnostic_path) = match locator.locate(&start_path) {
        Ok(root) => {
            let manifest_path = root.manifest_path().to_path_buf();
            let settings = ManifestLoader::load(&manifest_path)
                .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;
            (root, settings, manifest_path)
        }
        Err(diagnostic) => return Err(tooling_failure(&start_path, diagnostic)),
    };

    let entrypoint_override = determine_entrypoint_override(args.input.is_some(), &start_path);
    let layout = ProjectLayout::from_settings(&project_root, &settings)
        .map_err(|diagnostic| tooling_failure(&diagnostic_path, diagnostic))?;

    let generated_root = project_root.join("target/generated-tests");
    let mut overrides = CliOverrides::default();
    overrides.entrypoint = entrypoint_override;
    overrides.output = Some(generated_root.clone());
    overrides.java_only = true;
    overrides.clean = args.clean;
    overrides.target = args.target;

    let mut plan = BuildOptionsFactory::compose(project_root.clone(), settings, layout, overrides)
        .map_err(|diagnostic| tooling_failure(&diagnostic_path, diagnostic))?;

    let sample_cache = generated_root.join(".sample-cache");
    plan.build_config.sample.cache_dir = Some(sample_cache);

    let mut prepared_output = OutputManager::prepare(plan)
        .map_err(|diagnostic| tooling_failure(&diagnostic_path, diagnostic))?;
    let plan = prepared_output.plan();

    println!(
        "テストJava生成先 / Test generation dir: {}",
        prepared_output.target_dir().display()
    );

    let artifacts = compile(plan)
        .with_context(|| format!("Failed to compile {}", plan.entrypoint().display()))?;

    if artifacts.java_files.is_empty() {
        println!("テストDSLから生成された Java クラスはありません / No test classes emitted");
    } else {
        for java_file in &artifacts.java_files {
            println!("Generated test: {}", java_file.display());
        }
    }

    for diagnostic in &artifacts.compatibility_diagnostics {
        println!(
            "{}",
            crate::format_tooling_diagnostic(plan.entrypoint(), diagnostic)
        );
    }

    for warning in &artifacts.warnings {
        println!("Warning: {}", warning);
    }

    let target = plan.build_config.target;
    let java_home = resolve_java_home(project_root.root_dir(), target)?;
    let maven_exec = resolve_maven_executable(project_root.root_dir(), args.maven.as_deref())?;
    let generated_dir = prepared_output.target_dir().to_path_buf();

    run_maven_test(
        &maven_exec,
        project_root.root_dir(),
        &java_home,
        &generated_dir,
        target,
    )?;
    prepared_output.mark_success();

    println!(
        "mvn test 完了 (Java {}) / Maven tests finished (Java {})",
        target, target,
    );

    Ok(())
}

fn compute_start_path(input: Option<&str>) -> Result<PathBuf> {
    let cwd = env::current_dir().context("カレントディレクトリを取得できません")?;
    match input {
        Some(value) => {
            let candidate = PathBuf::from(value);
            if candidate.is_absolute() {
                Ok(candidate)
            } else {
                Ok(cwd.join(candidate))
            }
        }
        None => Ok(cwd),
    }
}

fn determine_entrypoint_override(explicit: bool, start_path: &Path) -> Option<PathBuf> {
    if !explicit {
        return None;
    }
    match fs::metadata(start_path) {
        Ok(metadata) if metadata.is_file() => Some(start_path.to_path_buf()),
        _ => None,
    }
}

fn resolve_java_home(root: &Path, target: JavaTarget) -> Result<PathBuf> {
    let env_var = java_env_var(target);
    if let Some(value) = env::var_os(env_var) {
        let candidate = PathBuf::from(value);
        if is_valid_java_home(&candidate) {
            return Ok(candidate);
        }
    }

    if let Some(toolchains) = toolchains_root(root) {
        for dir in java_toolchain_dirs(target) {
            let candidate = toolchains.join(dir);
            if is_valid_java_home(&candidate) {
                return Ok(candidate);
            }
        }
    }

    if let Some(value) = env::var_os("JAVA_HOME") {
        let candidate = PathBuf::from(value);
        if is_valid_java_home(&candidate) {
            return Ok(candidate);
        }
    }

    bail!(
        "Java {} のツールチェーンが見つかりません。toolchains ディレクトリまたは JAVA{}_HOME を確認してください。",
        target.as_str(),
        target.as_str()
    );
}

fn resolve_maven_executable(root: &Path, override_path: Option<&str>) -> Result<PathBuf> {
    if let Some(path) = override_path {
        let candidate = PathBuf::from(path);
        if candidate.is_file() {
            return Ok(candidate);
        }
        bail!(
            "指定された Maven パス '{}' が見つかりません。",
            candidate.display()
        );
    }

    if let Some(home) = env::var_os("MAVEN_HOME") {
        let candidate = PathBuf::from(home)
            .join("bin")
            .join(maven_executable_name());
        if candidate.is_file() {
            return Ok(candidate);
        }
    }

    if let Some(toolchains) = toolchains_root(root) {
        let candidate = toolchains
            .join("maven")
            .join("bin")
            .join(maven_executable_name());
        if candidate.is_file() {
            return Ok(candidate);
        }
    }

    bail!("Maven 実行ファイルが見つかりません。--maven で明示的なパスを指定してください。");
}

fn toolchains_root(root: &Path) -> Option<PathBuf> {
    let mut current = Some(root.to_path_buf());
    while let Some(dir) = current {
        let candidate = dir.join("toolchains");
        if candidate.is_dir() {
            return Some(candidate);
        }
        current = dir.parent().map(Path::to_path_buf);
    }
    None
}

fn is_valid_java_home(path: &Path) -> bool {
    path.join("bin").join(java_executable_name()).exists()
}

fn java_env_var(target: JavaTarget) -> &'static str {
    match target {
        JavaTarget::Java25 => "JAVA25_HOME",
        JavaTarget::Java21 => "JAVA21_HOME",
    }
}

fn java_toolchain_dirs(target: JavaTarget) -> &'static [&'static str] {
    match target {
        JavaTarget::Java25 => &["java25", "jdk25"],
        JavaTarget::Java21 => &["java21", "jdk21"],
    }
}

fn java_executable_name() -> &'static str {
    if cfg!(windows) { "java.exe" } else { "java" }
}

fn maven_executable_name() -> &'static str {
    if cfg!(windows) { "mvn.cmd" } else { "mvn" }
}

fn run_maven_test(
    executable: &Path,
    working_dir: &Path,
    java_home: &Path,
    generated_dir: &Path,
    target: JavaTarget,
) -> Result<()> {
    let mut command = Command::new(executable);
    command.current_dir(working_dir);
    command.arg("test");
    command.arg(format!("-Djv.generated.tests={}", generated_dir.display()));
    command.env("JAVA_HOME", java_home);
    command.env("JV_GENERATED_TESTS", generated_dir);
    command.env("JV_TEST_TARGET", target.as_str());
    command.stdout(Stdio::inherit());
    command.stderr(Stdio::inherit());

    let status = command
        .status()
        .with_context(|| format!("{} の実行に失敗しました", executable.display()))?;
    if !status.success() {
        bail!(
            "mvn test が失敗しました (status: {})",
            status
                .code()
                .map(|code| code.to_string())
                .unwrap_or_else(|| "terminated by signal".to_string())
        );
    }
    Ok(())
}
