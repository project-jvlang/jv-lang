use once_cell::sync::OnceCell;
use serde::Deserialize;

use super::config::RepositoryConfig;

static EMBEDDED_REPOSITORIES: OnceCell<Vec<RepositoryConfig>> = OnceCell::new();

const EMBEDDED_TOML: &str = include_str!(concat!(
    env!("OUT_DIR"),
    "/embedded_default_repositories.toml"
));

#[derive(Deserialize)]
#[serde(untagged)]
enum EmbeddedFormat {
    Wrapper { repositories: Vec<RepositoryConfig> },
    Direct(Vec<RepositoryConfig>),
}

pub(crate) fn embedded_global_repositories() -> Vec<RepositoryConfig> {
    EMBEDDED_REPOSITORIES
        .get_or_init(|| match parse_embedded() {
            Ok(repositories) if !repositories.is_empty() => repositories,
            Ok(_) | Err(_) => fallback_repositories(),
        })
        .clone()
}

fn parse_embedded() -> Result<Vec<RepositoryConfig>, toml::de::Error> {
    if EMBEDDED_TOML.trim().is_empty() {
        return Ok(Vec::new());
    }

    let parsed: EmbeddedFormat = toml::from_str(EMBEDDED_TOML)?;
    Ok(match parsed {
        EmbeddedFormat::Wrapper { repositories } => repositories,
        EmbeddedFormat::Direct(items) => items,
    })
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
