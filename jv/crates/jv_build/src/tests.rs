use super::*;

#[test]
fn test_build_config_default() {
    let config = BuildConfig::default();
    assert_eq!(config.java_version, "25");
    assert_eq!(config.output_dir, "./out");
    assert!(config.classpath.is_empty());
    assert_eq!(config.compiler_options, vec!["--release", "25"]);
    assert_eq!(config.sample.network_policy, NetworkPolicy::Deny);
    assert_eq!(config.sample.embed_max_bytes, Some(1_048_576));
}

#[test]
fn test_build_system_creation() {
    let config = BuildConfig::default();
    let build_system = BuildSystem::new(config);
    assert_eq!(build_system.config.java_version, "25");
    assert!(!build_system.config.sample.network_allowed());
}

#[test]
fn build_config_maps_sample_errors() {
    let config = BuildConfig::default();
    let error = config
        .enforce_sample_source("https://example.com/data.json")
        .expect_err("network access should be denied");
    assert!(matches!(error, BuildError::ConfigError(_)));
}

#[test]
fn ensure_protocol_dependencies_reports_missing_cli() {
    let mut config = BuildConfig::default();
    config.sample.network_policy = NetworkPolicy::Allow;
    config.sample.cli.aws.override_path = Some(std::path::PathBuf::from("/non/existent/aws"));

    let build_system = BuildSystem::new(config);
    let error = build_system
        .ensure_protocol_dependencies(SampleProtocol::S3)
        .expect_err("missing CLI should be reported");
    assert!(matches!(error, BuildError::ConfigError(_)));
}

#[cfg(unix)]
#[test]
fn resolve_sample_dependency_uses_override_path() {
    let script = write_exec_script("#!/bin/sh\necho override\n");

    let mut config = BuildConfig::default();
    config.sample.cli.aws.override_path = Some(script.clone());

    let build_system = BuildSystem::new(config);
    let resolved = build_system
        .resolve_sample_dependency(SampleDependency::AwsCli)
        .expect("resolve CLI");

    assert_eq!(resolved.command, "aws");
    assert_eq!(resolved.path, script);

    let _ = std::fs::remove_file(resolved.path);
}

#[cfg(unix)]
#[test]
fn execute_sample_command_captures_stdout() {
    let script = write_exec_script("#!/bin/sh\necho \"payload:$@\"\n");

    let mut config = BuildConfig::default();
    config.sample.cli.aws.override_path = Some(script.clone());

    let build_system = BuildSystem::new(config);
    let output = build_system
        .execute_sample_command(SampleDependency::AwsCli, &["arg1", "arg2"], None)
        .expect("execute CLI");

    let stdout = String::from_utf8(output.stdout).expect("utf8 stdout");
    assert!(stdout.contains("payload:arg1 arg2"));

    let _ = std::fs::remove_file(script);
}

#[cfg(unix)]
#[test]
fn execute_sample_command_reports_non_zero_exit() {
    let script = write_exec_script("#!/bin/sh\necho problem 1>&2\nexit 42\n");

    let mut config = BuildConfig::default();
    config.sample.cli.aws.override_path = Some(script.clone());

    let build_system = BuildSystem::new(config);
    let error = build_system
        .execute_sample_command(SampleDependency::AwsCli, &[], None)
        .expect_err("non-zero exit should fail");

    match error {
        BuildError::CliExecutionError {
            command,
            status,
            stderr,
        } => {
            assert_eq!(command, "aws");
            assert_eq!(status, Some(42));
            assert_eq!(stderr, "problem");
        }
        other => panic!("unexpected error: {other:?}"),
    }

    let _ = std::fs::remove_file(script);
}

#[cfg(unix)]
fn write_exec_script(content: &str) -> std::path::PathBuf {
    use std::fs;
    use std::os::unix::fs::PermissionsExt;
    use std::time::{SystemTime, UNIX_EPOCH};

    let filename = format!(
        "jv-build-cli-test-{}",
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    );
    let path = std::env::temp_dir().join(filename);
    fs::write(&path, content).expect("write script");

    let mut permissions = fs::metadata(&path)
        .expect("metadata")
        .permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&path, permissions).expect("set permissions");

    path
}
