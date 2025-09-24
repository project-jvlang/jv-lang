use std::fs;

use crate::pipeline::project::{
    layout::ProjectLayout, locator::ProjectRoot, manifest::ManifestLoader,
};

use super::TempDirGuard;

fn create_manifest(path: &std::path::Path, contents: &str) {
    fs::write(path, contents).expect("failed to write manifest");
}

fn to_relative_strings(root: &std::path::Path, paths: &[std::path::PathBuf]) -> Vec<String> {
    paths
        .iter()
        .map(|path| {
            path.strip_prefix(root)
                .expect("path should be inside project root")
                .to_string_lossy()
                .replace('\\', "/")
        })
        .collect()
}

#[test]
fn layout_filters_sources_with_include_and_exclude() {
    let temp_dir = TempDirGuard::new("project-layout-filters");
    let root = temp_dir.path();
    let manifest_path = root.join("jv.toml");

    create_manifest(
        &manifest_path,
        r#"[package]
name = "layout"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv", "tests/**/*.jv"]
exclude = ["tests/**"]
"#,
    );

    fs::create_dir_all(root.join("src/app")).unwrap();
    fs::create_dir_all(root.join("tests")).expect("failed to create tests directory");
    fs::write(root.join("src/main.jv"), "fun main() {}\n").unwrap();
    fs::write(root.join("src/app/util.jv"), "fun util() {}\n").unwrap();
    fs::write(root.join("tests/helper.jv"), "fun helper() {}\n").unwrap();

    let project_root = ProjectRoot::new(root.to_path_buf(), manifest_path.clone());
    let settings = ManifestLoader::load(&manifest_path).expect("manifest should load");
    let layout = ProjectLayout::from_settings(&project_root, &settings)
        .expect("layout should resolve sources");

    let relatives = to_relative_strings(layout.root().root_dir(), layout.sources());
    assert_eq!(relatives, vec!["src/app/util.jv", "src/main.jv"]);
    assert_eq!(layout.entrypoint(), &root.join("src/main.jv"));
}

#[test]
fn layout_returns_error_when_no_sources_detected() {
    let temp_dir = TempDirGuard::new("project-layout-no-sources");
    let root = temp_dir.path();
    let manifest_path = root.join("jv.toml");

    create_manifest(
        &manifest_path,
        r#"[package]
name = "empty"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
exclude = ["tests/**"]
"#,
    );

    fs::create_dir_all(root.join("src")).expect("failed to create src directory");

    let project_root = ProjectRoot::new(root.to_path_buf(), manifest_path.clone());
    let settings = ManifestLoader::load(&manifest_path).expect("manifest should load");
    let error = ProjectLayout::from_settings(&project_root, &settings)
        .expect_err("expected diagnostic when no sources are found");

    assert_eq!(error.code, "JV1002");
    assert!(error.message.contains("ソース"));
}

#[test]
fn layout_infers_entrypoint_without_manifest_override() {
    let temp_dir = TempDirGuard::new("project-layout-infer-entrypoint");
    let root = temp_dir.path();
    let manifest_path = root.join("jv.toml");

    create_manifest(
        &manifest_path,
        r#"[package]
name = "infer"
version = "0.1.0"

[package.dependencies]
"#,
    );

    fs::create_dir_all(root.join("src/features")).expect("failed to create nested directory");
    fs::write(root.join("src/main.jv"), "fun main() {}\n").unwrap();
    fs::write(root.join("src/features/extra.jv"), "fun extra() {}\n").unwrap();

    let project_root = ProjectRoot::new(root.to_path_buf(), manifest_path.clone());
    let settings = ManifestLoader::load(&manifest_path).expect("manifest should load");
    let layout = ProjectLayout::from_settings(&project_root, &settings)
        .expect("layout should infer entrypoint");

    assert_eq!(layout.entrypoint(), &root.join("src/main.jv"));
}

#[test]
fn layout_errors_when_explicit_entrypoint_missing() {
    let temp_dir = TempDirGuard::new("project-layout-missing-entrypoint");
    let root = temp_dir.path();
    let manifest_path = root.join("jv.toml");

    create_manifest(
        &manifest_path,
        r#"[package]
name = "missing"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/bin/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    );

    fs::create_dir_all(root.join("src")).expect("failed to create src directory");
    fs::write(root.join("src/main.jv"), "fun main() {}\n").unwrap();

    let project_root = ProjectRoot::new(root.to_path_buf(), manifest_path.clone());
    let settings = ManifestLoader::load(&manifest_path).expect("manifest should load");
    let error = ProjectLayout::from_settings(&project_root, &settings)
        .expect_err("expected diagnostic when entrypoint is missing");

    assert_eq!(error.code, "JV1002");
    assert!(error.message.contains("エントリポイント"));
}
