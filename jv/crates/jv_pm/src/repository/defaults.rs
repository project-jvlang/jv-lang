use once_cell::sync::OnceCell;
use serde::Deserialize;

use super::config::RepositoryConfig;

static EMBEDDED_CONFIG: OnceCell<EmbeddedConfig> = OnceCell::new();
const EMBEDDED_TOML: &str = include_str!(concat!(
    env!("OUT_DIR"),
    "/embedded_default_repositories.toml"
));

#[derive(Debug, Clone, Deserialize)]
struct EmbeddedConfig {
    #[serde(default)]
    repositories: Vec<RepositoryConfig>,
    #[serde(default)]
    maven: MavenConfigSection,
}

impl Default for EmbeddedConfig {
    fn default() -> Self {
        Self {
            repositories: Vec::new(),
            maven: MavenConfigSection::default(),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct MavenConfigSection {
    #[serde(default = "default_distribution_id")]
    default_distribution: String,
    #[serde(default)]
    distributions: Vec<MavenDistribution>,
    #[serde(default, rename = "standard_plugins")]
    standard_plugins: Vec<MavenPluginDefinition>,
    #[serde(default, rename = "managed_artifacts")]
    managed_artifacts: Vec<MavenManagedArtifact>,
}

impl MavenConfigSection {
    pub(crate) fn default_distribution_id(&self) -> &str {
        self.default_distribution.as_str()
    }

    fn ensure_defaults(&mut self) {
        if self.distributions.is_empty() {
            self.distributions = fallback_distributions();
        }
        if self.standard_plugins.is_empty() {
            self.standard_plugins = fallback_standard_plugins();
        } else {
            for fallback in fallback_standard_plugins() {
                let missing = self.standard_plugins.iter().all(|plugin| {
                    plugin.group_id != fallback.group_id
                        || plugin.artifact_id != fallback.artifact_id
                });
                if missing {
                    self.standard_plugins.push(fallback);
                }
            }
        }
        if self.managed_artifacts.is_empty() {
            self.managed_artifacts = fallback_managed_artifacts();
        } else {
            for fallback in fallback_managed_artifacts() {
                let missing = self.managed_artifacts.iter().all(|artifact| {
                    artifact.group_id != fallback.group_id
                        || artifact.artifact_id != fallback.artifact_id
                });
                if missing {
                    self.managed_artifacts.push(fallback);
                }
            }
        }
    }
}

impl Default for MavenConfigSection {
    fn default() -> Self {
        Self {
            default_distribution: default_distribution_id(),
            distributions: Vec::new(),
            standard_plugins: Vec::new(),
            managed_artifacts: Vec::new(),
        }
    }
}

fn default_distribution_id() -> String {
    "maven3".to_string()
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub(crate) struct MavenDistribution {
    pub id: String,
    pub version: String,
    #[serde(default)]
    pub release_channel: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct MavenPluginDefinition {
    pub group_id: String,
    pub artifact_id: String,
    pub version: String,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct MavenManagedArtifact {
    pub group_id: String,
    pub artifact_id: String,
    pub version: String,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum EmbeddedFormat {
    Full(EmbeddedConfig),
    Wrapper { repositories: Vec<RepositoryConfig> },
    Direct(Vec<RepositoryConfig>),
}

pub(crate) fn embedded_global_repositories() -> Vec<RepositoryConfig> {
    let config = embedded_config();
    if config.repositories.is_empty() {
        fallback_repositories()
    } else {
        config.repositories.clone()
    }
}

pub(crate) fn maven_standard_plugins() -> &'static [MavenPluginDefinition] {
    &embedded_config().maven.standard_plugins
}

pub(crate) fn maven_managed_artifacts() -> &'static [MavenManagedArtifact] {
    &embedded_config().maven.managed_artifacts
}

pub(crate) fn default_maven_distribution_id() -> &'static str {
    embedded_config().maven.default_distribution_id()
}

pub(crate) fn maven_distribution(id: &str) -> Option<&'static MavenDistribution> {
    embedded_config()
        .maven
        .distributions
        .iter()
        .find(|distribution| distribution.id == id)
}

pub(crate) fn maven_distribution_version(id: &str) -> &'static str {
    maven_distribution(id)
        .map(|distribution| distribution.version.as_str())
        .unwrap_or("unknown")
}

fn embedded_config() -> &'static EmbeddedConfig {
    EMBEDDED_CONFIG.get_or_init(|| {
        let parsed = parse_embedded()
            .map(ensure_complete)
            .unwrap_or_else(|_| ensure_complete(EmbeddedConfig::default()));
        parsed
    })
}

fn parse_embedded() -> Result<EmbeddedConfig, toml::de::Error> {
    if EMBEDDED_TOML.trim().is_empty() {
        return Ok(EmbeddedConfig::default());
    }

    let parsed: EmbeddedFormat = toml::from_str(EMBEDDED_TOML)?;
    Ok(match parsed {
        EmbeddedFormat::Full(config) => config,
        EmbeddedFormat::Wrapper { repositories } => EmbeddedConfig {
            repositories,
            ..EmbeddedConfig::default()
        },
        EmbeddedFormat::Direct(items) => EmbeddedConfig {
            repositories: items,
            ..EmbeddedConfig::default()
        },
    })
}

fn ensure_complete(mut config: EmbeddedConfig) -> EmbeddedConfig {
    if config.repositories.is_empty() {
        config.repositories = fallback_repositories();
    }
    config.maven.ensure_defaults();
    config
}

fn fallback_repositories() -> Vec<RepositoryConfig> {
    vec![RepositoryConfig {
        name: "maven-central".to_string(),
        url: "https://repo.maven.apache.org/maven2".to_string(),
        priority: 100,
        auth: None,
        filter: None,
    }]
}

fn fallback_standard_plugins() -> Vec<MavenPluginDefinition> {
    vec![
        ("org.apache.maven.plugins", "maven-clean-plugin", "3.2.0"),
        (
            "org.apache.maven.plugins",
            "maven-resources-plugin",
            "3.3.1",
        ),
        (
            "org.apache.maven.plugins",
            "maven-compiler-plugin",
            "3.13.0",
        ),
        ("org.apache.maven.plugins", "maven-surefire-plugin", "3.2.5"),
        ("org.apache.maven.plugins", "maven-jar-plugin", "3.4.1"),
        ("org.apache.maven.plugins", "maven-install-plugin", "3.1.2"),
        ("org.apache.maven.plugins", "maven-deploy-plugin", "3.1.2"),
        ("org.apache.maven.plugins", "maven-site-plugin", "3.12.1"),
        ("org.apache.maven.plugins", "maven-antrun-plugin", "3.1.0"),
        ("org.apache.maven.plugins", "maven-assembly-plugin", "3.7.1"),
        (
            "org.apache.maven.plugins",
            "maven-dependency-plugin",
            "3.7.0",
        ),
    ]
    .into_iter()
    .map(|(group_id, artifact_id, version)| MavenPluginDefinition {
        group_id: group_id.to_string(),
        artifact_id: artifact_id.to_string(),
        version: version.to_string(),
    })
    .collect()
}

fn fallback_managed_artifacts() -> Vec<MavenManagedArtifact> {
    vec![MavenManagedArtifact {
        group_id: "org.apache.commons".to_string(),
        artifact_id: "commons-text".to_string(),
        version: "1.12.0".to_string(),
    }]
}

fn fallback_distributions() -> Vec<MavenDistribution> {
    vec![
        MavenDistribution {
            id: "maven3".to_string(),
            version: "3.9.11".to_string(),
            release_channel: Some("stable".to_string()),
            description: Some("Apache Maven 3.x (fallback)".to_string()),
        },
        MavenDistribution {
            id: "maven4".to_string(),
            version: "4.0.0".to_string(),
            release_channel: Some("upcoming".to_string()),
            description: Some("Apache Maven 4.x (fallback)".to_string()),
        },
    ]
}
