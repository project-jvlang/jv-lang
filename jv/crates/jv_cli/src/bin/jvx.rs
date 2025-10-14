use anyhow::Result;
use clap::Parser;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use jv_cli::pipeline::project::{
    layout::ProjectLayout, locator::ProjectLocator, manifest::ManifestLoader,
};
use jv_cli::pipeline::{run_program, BuildOptionsFactory, BuildPlan, CliOverrides};
use jv_cli::tooling_failure;

#[derive(Parser)]
#[command(name = "jvx", about = "Quickly execute a jv file or snippet")]
struct JvxCli {
    /// Path to .jv file, '-' for stdin, or inline snippet
    target: String,
    /// Arguments passed to the program
    #[arg(trailing_var_arg = true)]
    args: Vec<String>,
}

fn main() -> Result<()> {
    let cli = JvxCli::parse();
    exec_quick(&cli.target, cli.args)
}

fn exec_quick(target: &str, args: Vec<String>) -> Result<()> {
    if target == "-" {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf)?;
        return run_snippet(&buf, &args);
    }

    if Path::new(target).exists() {
        return run_jv_file(target, &args);
    }

    run_snippet(target, &args)
}

fn run_snippet(source: &str, args: &[String]) -> Result<()> {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    let temp_dir = std::env::temp_dir().join(format!("jvx-snippet-{}", timestamp));
    fs::create_dir_all(&temp_dir)?;

    let temp_file = temp_dir.join("snippet.jv");
    fs::write(&temp_file, source)?;
    write_ephemeral_manifest(&temp_dir, "snippet.jv")?;

    let result = run_jv_file(&temp_file, args);
    let _ = fs::remove_dir_all(temp_dir);
    result
}

fn run_jv_file<P: AsRef<Path>>(input: P, args: &[String]) -> Result<()> {
    let input_path = to_absolute(input.as_ref())?;

    if !input_path.exists() {
        anyhow::bail!("Input file '{}' not found", input_path.display());
    }

    let plan = build_plan_for_input(&input_path)?;

    run_program(&plan, args)
}

fn build_plan_for_input(input_path: &Path) -> Result<BuildPlan> {
    let project_root = ProjectLocator::new()
        .locate(input_path)
        .map_err(|diagnostic| tooling_failure(input_path, diagnostic))?;
    let manifest_path = project_root.manifest_path().to_path_buf();

    let settings = ManifestLoader::load(&manifest_path)
        .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;
    let layout = ProjectLayout::from_settings(&project_root, &settings)
        .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;

    let overrides = CliOverrides {
        entrypoint: Some(input_path.to_path_buf()),
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
    };

    BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))
}

fn to_absolute(path: &Path) -> Result<PathBuf> {
    if path.is_absolute() {
        Ok(path.to_path_buf())
    } else {
        Ok(std::env::current_dir()?.join(path))
    }
}

fn write_ephemeral_manifest(dir: &Path, entry: &str) -> Result<()> {
    let manifest_path = dir.join("jv.toml");
    if manifest_path.exists() {
        return Ok(());
    }

    let manifest = format!(
        r#"[package]
name = "jvx-snippet"
version = "0.0.0"

[package.dependencies]

[project]
entrypoint = "{}"

[project.sources]
include = ["*.jv"]
"#,
        entry
    );

    fs::write(manifest_path, manifest)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn ephemeral_manifest_uses_relative_entrypoint() {
        let temp = tempdir().expect("temp dir");
        write_ephemeral_manifest(temp.path(), "snippet.jv").expect("manifest write");

        let manifest = fs::read_to_string(temp.path().join("jv.toml")).expect("read manifest");
        assert!(manifest.contains("entrypoint = \"snippet.jv\""));
    }

    #[test]
    fn build_plan_respects_manifest_entrypoint() {
        let temp = tempdir().expect("temp dir");
        let root = temp.path();
        let src_dir = root.join("src");
        fs::create_dir_all(&src_dir).expect("create src");
        let entrypoint = src_dir.join("main.jv");
        fs::write(&entrypoint, "fun main() {}\n").expect("write entrypoint");

        let manifest = r#"[package]
name = "jvx-test"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#;
        fs::write(root.join("jv.toml"), manifest).expect("write manifest");

        let plan = build_plan_for_input(&entrypoint).expect("plan");

        assert_eq!(plan.entrypoint(), entrypoint.as_path());
        assert_eq!(plan.root.root_dir(), root);
    }
}
