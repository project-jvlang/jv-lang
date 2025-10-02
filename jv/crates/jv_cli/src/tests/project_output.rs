use std::fs;

use crate::pipeline::{
    project::{layout::ProjectLayout, locator::ProjectRoot, manifest::ManifestLoader},
    BuildOptionsFactory, CliOverrides, OutputManager,
};

use super::TempDirGuard;

fn write_manifest(path: &std::path::Path) {
    fs::write(
        path,
        r#"[package]
name = "output"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]

[project.output]
directory = "target"
clean = false
"#,
    )
    .expect("write manifest");
}

fn compose_plan(root: &TempDirGuard, clean: bool) -> crate::pipeline::BuildPlan {
    let manifest_path = root.path().join("jv.toml");
    write_manifest(&manifest_path);

    let src_dir = root.path().join("src");
    fs::create_dir_all(&src_dir).expect("create src directory");
    fs::write(src_dir.join("main.jv"), "fun main() {}\n").expect("write entrypoint");

    let project_root = ProjectRoot::new(root.path().to_path_buf(), manifest_path.clone());
    let settings = ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = ProjectLayout::from_settings(&project_root, &settings).expect("layout resolves");

    let mut overrides = CliOverrides::default();
    overrides.clean = clean;

    BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds")
}

#[test]
fn output_manager_creates_target_directory() {
    let temp_dir = TempDirGuard::new("output-create");
    let plan = compose_plan(&temp_dir, false);

    let mut prepared = OutputManager::prepare(plan).expect("prepare succeeds");
    let target_dir = prepared.target_dir().to_path_buf();

    assert!(target_dir.ends_with("java25"));
    assert!(target_dir.exists());
    assert!(prepared.base_dir().ends_with("target"));

    prepared.mark_success();
}

#[test]
fn output_manager_cleans_when_requested() {
    let temp_dir = TempDirGuard::new("output-clean");
    compose_plan(&temp_dir, false);
    let target_dir_hint = temp_dir.path().join("target/java25");
    fs::create_dir_all(&target_dir_hint).expect("create existing target");
    fs::write(target_dir_hint.join("stale.txt"), "old").expect("write stale file");

    let plan_with_clean = compose_plan(&temp_dir, true);
    let mut prepared =
        OutputManager::prepare(plan_with_clean).expect("prepare with clean succeeds");

    assert!(prepared.clean_applied());
    assert!(!prepared.target_dir().join("stale.txt").exists());

    prepared.mark_success();
}

#[test]
fn output_manager_removes_outputs_on_failure() {
    let temp_dir = TempDirGuard::new("output-failure");
    let plan = compose_plan(&temp_dir, false);
    let target_dir_path;
    {
        let prepared = OutputManager::prepare(plan).expect("prepare succeeds");
        target_dir_path = prepared.target_dir().to_path_buf();
        // drop without marking success
    }

    assert!(!target_dir_path.exists());
}
