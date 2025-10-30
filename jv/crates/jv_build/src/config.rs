use crate::BuildError;
pub use jv_pm::JavaTarget;
use std::fmt;
use std::path::{Path, PathBuf};
use std::time::Duration;
use thiserror::Error;

/// Top-level configuration shared by the build system.
#[derive(Debug, Clone)]
pub struct BuildConfig {
    pub target: JavaTarget,
    pub output_dir: String,
    pub classpath: Vec<String>,
    pub compiler_options: Vec<String>,
    pub sample: SampleConfig,
}

impl Default for BuildConfig {
    fn default() -> Self {
        let target = JavaTarget::default();
        Self {
            target,
            output_dir: "./out".to_string(),
            classpath: vec![],
            compiler_options: compiler_options_for(target),
            sample: SampleConfig::default(),
        }
    }
}

/// Security-focused configuration for the @Sample feature.
#[derive(Debug, Clone)]
pub struct SampleConfig {
    /// Network policy controlling remote access during compilation.
    pub network_policy: NetworkPolicy,
    /// Maximum bytes allowed when embedding data at compile time.
    pub embed_max_bytes: Option<u64>,
    /// Enable persistent caching for remote fetches.
    pub cache_enabled: bool,
    /// Optional cache directory override.
    pub cache_dir: Option<PathBuf>,
    /// Timeout applied to remote fetch operations.
    pub fetch_timeout: Duration,
    /// CLI dependencies required for protocol handlers.
    pub cli: SampleCliDependencies,
}

impl SampleConfig {
    /// Enforce security policy for a given @Sample source string.
    ///
    /// Returns the detected protocol when the source is permitted. Any violation
    /// (network disabled or missing CLI dependency) yields a `SampleConfigError`.
    pub fn enforce_source_security(
        &self,
        source: &str,
    ) -> Result<SampleProtocol, SampleConfigError> {
        let protocol = SampleProtocol::detect(source);

        if protocol.requires_network() && self.network_policy == NetworkPolicy::Deny {
            return Err(SampleConfigError::NetworkNotAllowed { protocol });
        }

        match protocol {
            SampleProtocol::S3 => {
                self.cli.aws.resolve()?;
            }
            SampleProtocol::GitSsh => {
                self.cli.git.resolve()?;
            }
            _ => {}
        }

        Ok(protocol)
    }

    /// Returns true when the configuration allows network access for @Sample sources.
    pub fn network_allowed(&self) -> bool {
        self.network_policy == NetworkPolicy::Allow
    }
}

impl BuildConfig {
    pub fn with_target(target: JavaTarget) -> Self {
        let mut config = Self::default();
        config.set_target(target);
        config
    }

    pub fn set_target(&mut self, target: JavaTarget) {
        self.target = target;
        self.compiler_options = compiler_options_for(target);
    }

    pub fn java_release(&self) -> &'static str {
        self.target.release_flag()
    }

    /// Convenience wrapper that enforces security controls for a source and
    /// maps violations into the crate-level `BuildError` type.
    pub fn enforce_sample_source(&self, source: &str) -> Result<SampleProtocol, BuildError> {
        self.sample
            .enforce_source_security(source)
            .map_err(|error| BuildError::ConfigError(error.to_string()))
    }
}

fn compiler_options_for(target: JavaTarget) -> Vec<String> {
    vec!["--release".to_string(), target.release_flag().to_string()]
}

impl Default for SampleConfig {
    fn default() -> Self {
        Self {
            network_policy: NetworkPolicy::Deny,
            embed_max_bytes: Some(1_048_576), // 1 MiB default limit
            cache_enabled: false,
            cache_dir: None,
            fetch_timeout: Duration::from_secs(30),
            cli: SampleCliDependencies::default(),
        }
    }
}

/// Network access policy for @Sample data sources.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NetworkPolicy {
    Allow,
    Deny,
}

impl Default for NetworkPolicy {
    fn default() -> Self {
        NetworkPolicy::Deny
    }
}

/// Protocol classification for @Sample sources.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SampleProtocol {
    Path,
    FileUri,
    Http,
    Https,
    S3,
    GitSsh,
    Unknown,
}

impl SampleProtocol {
    /// Detect protocol from a source string.
    pub fn detect(source: &str) -> Self {
        let trimmed = source.trim();
        if trimmed.starts_with("git+ssh://") {
            SampleProtocol::GitSsh
        } else if trimmed.starts_with("s3://") {
            SampleProtocol::S3
        } else if trimmed.starts_with("https://") {
            SampleProtocol::Https
        } else if trimmed.starts_with("http://") {
            SampleProtocol::Http
        } else if trimmed.starts_with("file://") {
            SampleProtocol::FileUri
        } else if trimmed.contains("://") {
            SampleProtocol::Unknown
        } else {
            SampleProtocol::Path
        }
    }

    pub fn requires_network(self) -> bool {
        matches!(
            self,
            SampleProtocol::Http
                | SampleProtocol::Https
                | SampleProtocol::S3
                | SampleProtocol::GitSsh
                | SampleProtocol::Unknown
        )
    }
}

impl fmt::Display for SampleProtocol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let label = match self {
            SampleProtocol::Path => "path",
            SampleProtocol::FileUri => "file",
            SampleProtocol::Http => "http",
            SampleProtocol::Https => "https",
            SampleProtocol::S3 => "s3",
            SampleProtocol::GitSsh => "git+ssh",
            SampleProtocol::Unknown => "unknown",
        };
        write!(f, "{}", label)
    }
}

/// CLI dependency descriptors for @Sample integrations.
#[derive(Debug, Clone)]
pub struct SampleCliDependencies {
    pub aws: CliRequirement,
    pub git: CliRequirement,
}

impl SampleCliDependencies {
    pub fn dependency(&self, dependency: SampleDependency) -> &CliRequirement {
        match dependency {
            SampleDependency::AwsCli => &self.aws,
            SampleDependency::GitCli => &self.git,
        }
    }

    pub fn dependency_mut(&mut self, dependency: SampleDependency) -> &mut CliRequirement {
        match dependency {
            SampleDependency::AwsCli => &mut self.aws,
            SampleDependency::GitCli => &mut self.git,
        }
    }
}

impl Default for SampleCliDependencies {
    fn default() -> Self {
        Self {
            aws: CliRequirement::new("aws")
                .with_hint("Install the AWS CLI and ensure it is on PATH."),
            git: CliRequirement::new("git").with_hint("Install Git and ensure it is on PATH."),
        }
    }
}

/// Individual CLI requirement description.
#[derive(Debug, Clone)]
pub struct CliRequirement {
    pub command: String,
    pub override_path: Option<PathBuf>,
    pub hint: Option<String>,
}

impl CliRequirement {
    pub fn new(command: impl Into<String>) -> Self {
        Self {
            command: command.into(),
            override_path: None,
            hint: None,
        }
    }

    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hint = Some(hint.into());
        self
    }

    pub fn with_override_path(mut self, path: PathBuf) -> Self {
        self.override_path = Some(path);
        self
    }

    /// Resolve the CLI path honoring any override and ensuring existence.
    pub fn resolve(&self) -> Result<PathBuf, SampleConfigError> {
        if let Some(path) = &self.override_path {
            return validate_override(&self.command, path);
        }

        which::which(&self.command).map_err(|_| SampleConfigError::DependencyMissing {
            command: self.command.clone(),
            hint: self
                .hint
                .as_ref()
                .map(|value| format!(" ({value})"))
                .unwrap_or_default(),
        })
    }
}

fn validate_override(command: &str, path: &Path) -> Result<PathBuf, SampleConfigError> {
    match std::fs::metadata(path) {
        Ok(metadata) => {
            if metadata.is_file() {
                Ok(path.to_path_buf())
            } else {
                Err(SampleConfigError::InvalidOverride {
                    command: command.to_string(),
                    path: path.to_path_buf(),
                })
            }
        }
        Err(_) => Err(SampleConfigError::InvalidOverride {
            command: command.to_string(),
            path: path.to_path_buf(),
        }),
    }
}

/// Marker for supported CLI dependencies.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SampleDependency {
    AwsCli,
    GitCli,
}

impl fmt::Display for SampleDependency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SampleDependency::AwsCli => write!(f, "aws"),
            SampleDependency::GitCli => write!(f, "git"),
        }
    }
}

/// Errors raised while enforcing @Sample configuration security.
#[derive(Debug, Error)]
pub enum SampleConfigError {
    #[error(
        "Network access for @Sample protocol '{protocol}' is disabled. Enable with --sample-network=allow or set sample.allow_network=true in jv.toml."
    )]
    NetworkNotAllowed { protocol: SampleProtocol },

    #[error("CLI dependency '{command}' is not available{hint}.")]
    DependencyMissing { command: String, hint: String },

    #[error("CLI override path for '{command}' is invalid: {path}")]
    InvalidOverride { command: String, path: PathBuf },
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn sample_config_denies_network_by_default() {
        let config = SampleConfig::default();
        let error = config
            .enforce_source_security("https://example.com/data.json")
            .expect_err("network should be disabled by default");
        assert!(matches!(error, SampleConfigError::NetworkNotAllowed { .. }));
    }

    #[test]
    fn sample_config_validates_cli_override() {
        let mut config = SampleConfig::default();
        config.network_policy = NetworkPolicy::Allow;

        let temp_dir = std::env::temp_dir();
        let override_path = temp_dir.join(format!(
            "jv-build-cli-{}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        fs::write(&override_path, "#!/bin/sh\nexit 0\n").unwrap();

        config.cli.aws.override_path = Some(override_path.clone());
        let protocol = config
            .enforce_source_security("s3://bucket/data.json")
            .expect("override CLI should satisfy dependency");
        assert_eq!(protocol, SampleProtocol::S3);

        let _ = fs::remove_file(override_path);
    }

    #[test]
    fn sample_config_reports_missing_cli() {
        let mut config = SampleConfig::default();
        config.network_policy = NetworkPolicy::Allow;
        config.cli.aws.override_path = Some(PathBuf::from("/non/existent/aws"));

        let error = config
            .enforce_source_security("s3://bucket/data.json")
            .expect_err("missing CLI should be reported");
        assert!(matches!(error, SampleConfigError::InvalidOverride { .. }));
    }
}
