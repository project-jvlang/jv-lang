// jv_pm - Package Manager functionality
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path::Path;
use std::str::FromStr;
use thiserror::Error;

pub mod cache;
pub mod download;
pub mod lockfile;
pub mod maven;
pub mod registry;
pub mod resolver;

pub use cache::{
    CacheError, CacheStatsSnapshot, CachedArtifact, CachedMetadata, CachedPom, DependencyCache,
};
pub use download::{
    ArtifactDownloadRequest, DownloadError, DownloadFailure, DownloadManager, DownloadReport,
    DownloadSettings, DownloadSource, DownloadSuccess, ParallelDownloader,
};
pub use lockfile::{
    DependencyRequirementChange, LOCKFILE_VERSION, LockedDependency, LockedPackage, LockedSource,
    Lockfile, LockfileDiff, LockfileError, LockfileManifestDependency, LockfileManifestSnapshot,
    LockfileService, VersionChange,
};
pub use maven::{
    MavenIntegrationConfig, MavenIntegrationDispatcher, MavenIntegrationError,
    MavenIntegrationFiles, MavenMirrorConfig, MavenRepositoryConfig, PomGenerator,
};
pub use registry::{
    ArtifactCoordinates, ArtifactResource, DownloadedJar, MavenCoordinates, MavenMetadata,
    MavenRegistry, MetadataParseError, RegistryError, RetryConfig,
};
pub use resolver::{
    DependencyScope, ManifestDependencyProvider, RequestedDependency, ResolutionDiagnostic,
    ResolutionDiagnosticLevel, ResolutionSource, ResolutionStats, ResolvedDependencies,
    ResolvedDependency, ResolverAlgorithmKind, ResolverDispatcher, ResolverError, ResolverOptions,
    ResolverStrategy, ResolverStrategyInfo, StrategyStability, VersionDecision,
};

#[derive(Error, Debug)]
pub enum PackageError {
    #[error("Package not found: {0}")]
    PackageNotFound(String),
    #[error("Version conflict: {0}")]
    VersionConflict(String),
    #[error("Invalid package manifest: {0}")]
    InvalidManifest(String),
    #[error("Network error: {0}")]
    NetworkError(String),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JavaTarget {
    Java21,
    Java25,
}

impl JavaTarget {
    pub const fn as_str(self) -> &'static str {
        match self {
            JavaTarget::Java21 => "21",
            JavaTarget::Java25 => "25",
        }
    }

    pub fn variants() -> &'static [&'static str] {
        &["21", "25"]
    }

    pub const fn enables_modern_features(self) -> bool {
        matches!(self, JavaTarget::Java25)
    }

    pub const fn release_flag(self) -> &'static str {
        self.as_str()
    }
}

impl Default for JavaTarget {
    fn default() -> Self {
        JavaTarget::Java25
    }
}

impl fmt::Display for JavaTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct JavaTargetParseError {
    invalid: String,
}

impl JavaTargetParseError {
    fn new(value: impl Into<String>) -> Self {
        Self {
            invalid: value.into(),
        }
    }
}

impl fmt::Display for JavaTargetParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Unsupported java target '{}'. Supported targets: {}",
            self.invalid,
            JavaTarget::variants().join(", ")
        )
    }
}

impl std::error::Error for JavaTargetParseError {}

impl FromStr for JavaTarget {
    type Err = JavaTargetParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let normalised = s.trim().to_ascii_lowercase();
        match normalised.as_str() {
            "21" | "java21" | "jdk21" => Ok(JavaTarget::Java21),
            "25" | "java25" | "jdk25" | "latest" | "lts" => Ok(JavaTarget::Java25),
            other => Err(JavaTargetParseError::new(other)),
        }
    }
}

impl Serialize for JavaTarget {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for JavaTarget {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;

        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = JavaTarget;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a Java target like '21' or '25'")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                JavaTarget::from_str(value).map_err(E::custom)
            }

            fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_str(&value)
            }

            fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                JavaTarget::from_str(&value.to_string()).map_err(E::custom)
            }

            fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if value < 0 {
                    return Err(E::custom(JavaTargetParseError::new(value.to_string())));
                }
                self.visit_u64(value as u64)
            }
        }

        deserializer.deserialize_any(Visitor)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    pub description: Option<String>,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: PackageInfo,
    #[serde(default)]
    pub project: ProjectSection,
    pub build: Option<BuildInfo>,
    #[serde(default)]
    pub maven: MavenProjectMetadata,
}

impl Manifest {
    pub fn load_from_path(path: impl AsRef<Path>) -> Result<Self, PackageError> {
        let path = path.as_ref();
        let content = fs::read_to_string(path)?;
        toml::from_str(&content).map_err(|error| PackageError::InvalidManifest(error.to_string()))
    }

    pub fn java_target(&self) -> JavaTarget {
        self.build
            .as_ref()
            .map(|info| info.java_version)
            .unwrap_or_default()
    }

    pub fn max_concurrent_downloads(&self) -> Option<usize> {
        self.build
            .as_ref()
            .and_then(|info| info.max_concurrent_downloads)
    }

    pub fn max_concurrent_warning(&self) -> Option<usize> {
        self.build
            .as_ref()
            .and_then(|info| info.max_concurrent_warning)
    }

    pub fn maven_group_id(&self) -> Option<&str> {
        let group = self.maven.group_id.trim();
        if group.is_empty() { None } else { Some(group) }
    }

    pub fn maven_artifact_id(&self) -> &str {
        self.maven
            .artifact_id
            .as_deref()
            .unwrap_or(&self.package.name)
    }

    pub fn maven_packaging(&self) -> &str {
        self.maven.packaging.as_deref().unwrap_or("jar")
    }

    pub fn maven_description(&self) -> Option<&str> {
        if let Some(desc) = self.maven.description.as_deref() {
            if !desc.trim().is_empty() {
                return Some(desc);
            }
        }
        self.package.description.as_deref()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct BuildInfo {
    #[serde(default)]
    #[serde(rename = "java_version")]
    pub java_version: JavaTarget,
    #[serde(default)]
    pub max_concurrent_downloads: Option<usize>,
    #[serde(default)]
    pub max_concurrent_warning: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct MavenProjectMetadata {
    pub group_id: String,
    pub artifact_id: Option<String>,
    pub packaging: Option<String>,
    pub description: Option<String>,
    pub url: Option<String>,
}

impl Default for MavenProjectMetadata {
    fn default() -> Self {
        Self {
            group_id: String::new(),
            artifact_id: None,
            packaging: None,
            description: None,
            url: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct ProjectSection {
    pub sources: SourceSection,
    pub output: OutputSection,
    pub entrypoint: Option<String>,
}

impl Default for ProjectSection {
    fn default() -> Self {
        Self {
            sources: SourceSection::default(),
            output: OutputSection::default(),
            entrypoint: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct SourceSection {
    pub include: Vec<String>,
    pub exclude: Vec<String>,
}

impl Default for SourceSection {
    fn default() -> Self {
        Self {
            include: vec!["src/**/*.jv".to_string()],
            exclude: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct OutputSection {
    #[serde(default = "default_output_directory")]
    pub directory: String,
    #[serde(default)]
    pub clean: bool,
}

impl Default for OutputSection {
    fn default() -> Self {
        Self {
            directory: default_output_directory(),
            clean: false,
        }
    }
}

fn default_output_directory() -> String {
    "target".to_string()
}

pub struct PackageManager {
    #[allow(dead_code)]
    registry_url: String,
}

impl PackageManager {
    pub fn new(registry_url: String) -> Self {
        Self { registry_url }
    }

    pub fn install_package(&self, _name: &str, _version: &str) -> Result<(), PackageError> {
        // Placeholder implementation
        Ok(())
    }

    pub fn resolve_dependencies(
        &self,
        _manifest: &Manifest,
    ) -> Result<Vec<PackageInfo>, PackageError> {
        // Placeholder implementation
        Ok(vec![])
    }
}

#[cfg(test)]
mod tests;
