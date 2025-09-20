use super::*;

#[test]
fn test_build_config_default() {
    let config = BuildConfig::default();
    assert_eq!(config.java_version, "25");
    assert_eq!(config.output_dir, "./out");
    assert!(config.classpath.is_empty());
    assert_eq!(config.compiler_options, vec!["--release", "25"]);
}

#[test]
fn test_build_system_creation() {
    let config = BuildConfig::default();
    let build_system = BuildSystem::new(config);
    assert_eq!(build_system.config.java_version, "25");
}
