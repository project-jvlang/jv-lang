//! Install command implementation for Maven wrapper mode.

use std::ffi::OsString;

use crate::cli::InstallArgs;

/// Configuration for install command.
#[derive(Debug, Clone)]
pub struct InstallConfig {
    /// Additional arguments to pass to Maven.
    pub maven_args: Vec<OsString>,
}

impl InstallConfig {
    /// Create InstallConfig from CLI args.
    pub fn from_args(args: &InstallArgs) -> Self {
        Self {
            maven_args: args.maven_args.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_install_config_from_args() {
        let args = InstallArgs {
            maven_args: vec![
                OsString::from("-DskipTests"),
                OsString::from("-Dmaven.javadoc.skip=true"),
            ],
        };

        let config = InstallConfig::from_args(&args);
        assert_eq!(config.maven_args.len(), 2);
        assert_eq!(config.maven_args[0], OsString::from("-DskipTests"));
        assert_eq!(config.maven_args[1], OsString::from("-Dmaven.javadoc.skip=true"));
    }

    #[test]
    fn test_install_config_empty_args() {
        let args = InstallArgs {
            maven_args: vec![],
        };

        let config = InstallConfig::from_args(&args);
        assert!(config.maven_args.is_empty());
    }

    #[test]
    fn test_install_config_common_maven_options() {
        // Test common Maven options mentioned in R2-5
        let args = InstallArgs {
            maven_args: vec![
                OsString::from("-DskipTests"),  // Skip tests
                OsString::from("-o"),            // Offline mode
                OsString::from("-U"),            // Force update snapshots
            ],
        };

        let config = InstallConfig::from_args(&args);
        assert_eq!(config.maven_args.len(), 3);
        assert!(config.maven_args.contains(&OsString::from("-DskipTests")));
        assert!(config.maven_args.contains(&OsString::from("-o")));
        assert!(config.maven_args.contains(&OsString::from("-U")));
    }

    #[test]
    fn test_install_config_with_profiles() {
        let args = InstallArgs {
            maven_args: vec![
                OsString::from("-Pproduction"),
                OsString::from("-Prelease"),
            ],
        };

        let config = InstallConfig::from_args(&args);
        assert_eq!(config.maven_args.len(), 2);
    }

    #[test]
    fn test_install_config_with_system_properties() {
        let args = InstallArgs {
            maven_args: vec![
                OsString::from("-Dproject.version=1.0.0"),
                OsString::from("-Djava.awt.headless=true"),
            ],
        };

        let config = InstallConfig::from_args(&args);
        assert_eq!(config.maven_args.len(), 2);
    }

    #[test]
    fn test_install_config_preserves_order() {
        let args = InstallArgs {
            maven_args: vec![
                OsString::from("first"),
                OsString::from("second"),
                OsString::from("third"),
            ],
        };

        let config = InstallConfig::from_args(&args);
        assert_eq!(config.maven_args[0], OsString::from("first"));
        assert_eq!(config.maven_args[1], OsString::from("second"));
        assert_eq!(config.maven_args[2], OsString::from("third"));
    }

    #[test]
    fn test_install_config_clone() {
        let args = InstallArgs {
            maven_args: vec![OsString::from("-DskipTests")],
        };

        let config = InstallConfig::from_args(&args);
        let cloned = config.clone();

        assert_eq!(config.maven_args, cloned.maven_args);
    }
}
