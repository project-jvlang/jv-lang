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
