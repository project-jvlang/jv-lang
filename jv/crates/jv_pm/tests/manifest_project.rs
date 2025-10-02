use std::fs;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

use jv_pm::Manifest;

fn temp_manifest_path(suffix: &str) -> PathBuf {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    std::env::temp_dir().join(format!("jv-pm-project-{suffix}-{timestamp}.toml"))
}

#[test]
fn project_section_defaults_when_missing() {
    let path = temp_manifest_path("defaults");
    fs::write(
        &path,
        r#"[package]
name = "sample"
version = "0.1.0"

[package.dependencies]
"#,
    )
    .unwrap();

    let manifest = Manifest::load_from_path(&path).expect("manifest should load");
    assert_eq!(
        manifest.project.sources.include,
        vec!["src/**/*.jv".to_string()]
    );
    assert!(manifest.project.sources.exclude.is_empty());
    assert_eq!(manifest.project.output.directory, "target");
    assert!(!manifest.project.output.clean);
    assert!(manifest.project.entrypoint.is_none());

    let _ = fs::remove_file(path);
}

#[test]
fn project_section_honours_custom_values() {
    let path = temp_manifest_path("custom");
    fs::write(
        &path,
        r#"[package]
name = "custom"
version = "1.2.3"

[package.dependencies]

[project]
entrypoint = "app/main.jv"

[project.sources]
include = ["app/**/*.jv", "shared/**/*.jv"]
exclude = ["tests/**"]

[project.output]
directory = "build/java"
clean = true
"#,
    )
    .unwrap();

    let manifest = Manifest::load_from_path(&path).expect("manifest should load");
    assert_eq!(
        manifest.project.sources.include,
        vec!["app/**/*.jv".to_string(), "shared/**/*.jv".to_string()]
    );
    assert_eq!(
        manifest.project.sources.exclude,
        vec!["tests/**".to_string()]
    );
    assert_eq!(manifest.project.entrypoint.as_deref(), Some("app/main.jv"));
    assert_eq!(manifest.project.output.directory, "build/java");
    assert!(manifest.project.output.clean);

    let _ = fs::remove_file(path);
}
