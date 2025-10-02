use super::*;
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn test_package_info_creation() {
    let package = PackageInfo {
        name: "test-package".to_string(),
        version: "1.0.0".to_string(),
        description: Some("Test package".to_string()),
        dependencies: HashMap::new(),
    };

    assert_eq!(package.name, "test-package");
    assert_eq!(package.version, "1.0.0");
    assert_eq!(package.description, Some("Test package".to_string()));
    assert!(package.dependencies.is_empty());
}

#[test]
fn test_package_with_dependencies() {
    let mut dependencies = HashMap::new();
    dependencies.insert("dep1".to_string(), "^1.0".to_string());
    dependencies.insert("dep2".to_string(), "~2.1".to_string());

    let package = PackageInfo {
        name: "my-package".to_string(),
        version: "0.1.0".to_string(),
        description: None,
        dependencies,
    };

    assert_eq!(package.dependencies.len(), 2);
    assert_eq!(package.dependencies.get("dep1"), Some(&"^1.0".to_string()));
    assert_eq!(package.dependencies.get("dep2"), Some(&"~2.1".to_string()));
}

#[test]
fn test_manifest_creation() {
    let package = PackageInfo {
        name: "test".to_string(),
        version: "1.0.0".to_string(),
        description: None,
        dependencies: HashMap::new(),
    };

    let build_info = BuildInfo {
        java_version: JavaTarget::Java25,
    };

    let manifest = Manifest {
        package,
        project: ProjectSection::default(),
        build: Some(build_info),
    };

    assert_eq!(manifest.package.name, "test");
    assert_eq!(
        manifest.build.as_ref().unwrap().java_version,
        JavaTarget::Java25
    );
}

#[test]
fn test_package_manager_creation() {
    let pm = PackageManager::new("https://registry.jv-lang.org".to_string());
    assert_eq!(pm.registry_url, "https://registry.jv-lang.org");
}

#[test]
fn test_package_error_display() {
    let not_found = PackageError::PackageNotFound("test-pkg".to_string());
    let version_conflict = PackageError::VersionConflict("version mismatch".to_string());
    let invalid_manifest = PackageError::InvalidManifest("bad toml".to_string());
    let network_error = PackageError::NetworkError("connection timeout".to_string());

    assert!(not_found.to_string().contains("Package not found"));
    assert!(version_conflict.to_string().contains("Version conflict"));
    assert!(invalid_manifest
        .to_string()
        .contains("Invalid package manifest"));
    assert!(network_error.to_string().contains("Network error"));
}

#[test]
fn test_package_serialization() {
    let package = PackageInfo {
        name: "serialize-test".to_string(),
        version: "1.2.3".to_string(),
        description: Some("Serialization test".to_string()),
        dependencies: HashMap::new(),
    };

    let json = serde_json::to_string(&package).unwrap();
    assert!(json.contains("serialize-test"));
    assert!(json.contains("1.2.3"));

    let deserialized: PackageInfo = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized.name, package.name);
    assert_eq!(deserialized.version, package.version);
}

#[test]
fn test_install_package_placeholder() {
    let pm = PackageManager::new("test".to_string());
    let result = pm.install_package("test-pkg", "1.0.0");
    assert!(result.is_ok());
}

#[test]
fn test_resolve_dependencies_placeholder() {
    let pm = PackageManager::new("test".to_string());
    let manifest = Manifest {
        package: PackageInfo {
            name: "test".to_string(),
            version: "1.0.0".to_string(),
            description: None,
            dependencies: HashMap::new(),
        },
        project: ProjectSection::default(),
        build: None,
    };

    let result = pm.resolve_dependencies(&manifest);
    assert!(result.is_ok());
    assert!(result.unwrap().is_empty());
}

#[test]
fn manifest_loads_java_target_with_default() {
    let path = manifest_path("java25");
    fs::write(
        &path,
        r#"[package]
name = "sample"
version = "0.1.0"

[package.dependencies]

[build]
java_version = "25"
"#,
    )
    .unwrap();

    let manifest = Manifest::load_from_path(&path).expect("load manifest");
    assert_eq!(manifest.java_target(), JavaTarget::Java25);

    let _ = fs::remove_file(path);
}

#[test]
fn manifest_allows_missing_dependencies_section() {
    let path = manifest_path("nodeps");
    fs::write(
        &path,
        r#"[package]
name = "sample"
version = "0.1.0"

[build]
java_version = "25"
"#,
    )
    .unwrap();

    let manifest = Manifest::load_from_path(&path).expect("load manifest without dependencies");
    assert!(manifest.package.dependencies.is_empty());

    let _ = fs::remove_file(path);
}

#[test]
fn manifest_rejects_invalid_java_target() {
    let path = manifest_path("invalid");
    fs::write(
        &path,
        r#"[package]
name = "broken"
version = "0.1.0"

[package.dependencies]

[build]
java_version = "99"
"#,
    )
    .unwrap();

    let error = Manifest::load_from_path(&path).expect_err("invalid java target should error");
    match error {
        PackageError::InvalidManifest(message) => {
            assert!(message.contains("Unsupported java target"));
        }
        other => panic!("unexpected error: {other:?}"),
    }

    let _ = fs::remove_file(path);
}

fn manifest_path(suffix: &str) -> std::path::PathBuf {
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    std::env::temp_dir().join(format!("jv-pm-manifest-{suffix}-{timestamp}.toml"))
}
