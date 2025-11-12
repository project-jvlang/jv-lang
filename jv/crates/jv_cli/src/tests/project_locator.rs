use std::fs;
use std::path::Path;

use crate::pipeline::project::locator::ProjectLocator;

use super::{lock_current_dir, current_dir_lock, TempDirGuard};

struct WorkingDirGuard {
    previous: std::path::PathBuf,
}

impl WorkingDirGuard {
    fn change_to(path: &Path) -> Self {
        let previous = std::env::current_dir().unwrap();
        std::env::set_current_dir(path).unwrap();
        Self { previous }
    }
}

impl Drop for WorkingDirGuard {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(&self.previous);
    }
}

#[test]
fn locate_returns_root_from_nested_directory() {
    let temp_dir = TempDirGuard::new("project-locator-success");
    let root = temp_dir.path();
    let manifest = root.join("jv.toml");
    fs::write(&manifest, "[package]\nname = \"demo\"\n").unwrap();

    let nested = root.join("src/app");
    fs::create_dir_all(&nested).unwrap();

    let locator = ProjectLocator::new();
    let project_root = locator
        .locate(&nested)
        .expect("expected manifest discovery to succeed");

    let expected_root = fs::canonicalize(root).unwrap();
    let expected_manifest = fs::canonicalize(&manifest).unwrap();
    assert_eq!(project_root.root_dir(), expected_root.as_path());
    assert_eq!(project_root.manifest_path(), expected_manifest.as_path());
}

#[test]
fn locate_fails_without_manifest() {
    let temp_dir = TempDirGuard::new("project-locator-missing");

    let locator = ProjectLocator::new();
    let error = locator
        .locate(temp_dir.path())
        .expect_err("expected diagnostic when manifest is missing");

    assert_eq!(error.code, "JV1001");
    assert!(error.message.contains("jv.toml"));
}

#[test]
fn locate_blocks_relative_escape_outside_workspace() {
    let _lock = lock_current_dir();

    let temp_dir = TempDirGuard::new("project-locator-escape");
    let nested = temp_dir.path().join("nested/inner");
    fs::create_dir_all(&nested).unwrap();
    fs::write(
        temp_dir.path().join("jv.toml"),
        "[package]\nname = \"demo\"\n",
    )
    .unwrap();

    let _dir_guard = WorkingDirGuard::change_to(&nested);

    let locator = ProjectLocator::new();
    let error = locator
        .locate(Path::new("../.."))
        .expect_err("expected diagnostic when escaping workspace");

    assert_eq!(error.code, "JV1001");
    assert!(error.message.contains("探索開始パス"));
}
