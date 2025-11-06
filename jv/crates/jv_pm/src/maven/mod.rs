use std::collections::HashMap;
use std::path::{Path, PathBuf};

use thiserror::Error;

use crate::{Lockfile, Manifest, ResolvedDependencies};

pub mod pom_generator;
pub mod settings;

pub use pom_generator::{PomGenerationError, PomGenerator};
pub use settings::{SettingsGenerationError, SettingsGenerationRequest};

/// Mavenレポジトリ設定。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MavenRepositoryConfig {
    pub id: String,
    pub url: String,
    pub name: Option<String>,
    pub releases_enabled: bool,
    pub snapshots_enabled: bool,
}

impl MavenRepositoryConfig {
    pub fn new(id: impl Into<String>, url: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            url: url.into(),
            name: None,
            releases_enabled: true,
            snapshots_enabled: false,
        }
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn with_releases_enabled(mut self, enabled: bool) -> Self {
        self.releases_enabled = enabled;
        self
    }

    pub fn with_snapshots_enabled(mut self, enabled: bool) -> Self {
        self.snapshots_enabled = enabled;
        self
    }
}

/// Mavenミラー設定。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MavenMirrorConfig {
    pub id: String,
    pub mirror_of: String,
    pub url: String,
}

impl MavenMirrorConfig {
    pub fn new(
        id: impl Into<String>,
        mirror_of: impl Into<String>,
        url: impl Into<String>,
    ) -> Self {
        Self {
            id: id.into(),
            mirror_of: mirror_of.into(),
            url: url.into(),
        }
    }
}

/// Maven統合に必要な入力情報。
#[derive(Debug)]
pub struct MavenIntegrationConfig<'a> {
    pub manifest: &'a Manifest,
    pub resolved: &'a ResolvedDependencies,
    pub lockfile: Option<&'a Lockfile>,
    pub repositories: &'a [MavenRepositoryConfig],
    pub mirrors: &'a [MavenMirrorConfig],
    pub project_root: &'a Path,
    pub local_repository: &'a Path,
}

/// 生成されたファイル群。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MavenIntegrationFiles {
    pub files: Vec<(PathBuf, String)>,
}

impl MavenIntegrationFiles {
    pub fn new() -> Self {
        Self { files: Vec::new() }
    }

    pub fn push(&mut self, path: PathBuf, contents: String) {
        self.files.push((path, contents));
    }
}

/// ストラテジ情報。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MavenIntegrationStrategyInfo {
    pub name: String,
    pub description: String,
    pub maven_version: String,
    pub is_default: bool,
}

/// Maven統合戦略トレイト。
pub trait MavenIntegrationStrategy: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn maven_version(&self) -> &str;
    fn generate(
        &self,
        config: &MavenIntegrationConfig<'_>,
    ) -> Result<MavenIntegrationFiles, MavenIntegrationError>;
}

/// Maven統合ディスパッチャ。
#[derive(Default)]
pub struct MavenIntegrationDispatcher {
    strategies: HashMap<String, Box<dyn MavenIntegrationStrategy>>,
    default_strategy: String,
}

impl MavenIntegrationDispatcher {
    pub fn new() -> Self {
        let mut strategies: HashMap<String, Box<dyn MavenIntegrationStrategy>> = HashMap::new();
        strategies.insert("maven3".to_string(), Box::new(Maven3IntegrationStrategy));
        strategies.insert("maven4".to_string(), Box::new(Maven4IntegrationStrategy));

        Self {
            strategies,
            default_strategy: "maven3".to_string(),
        }
    }

    pub fn list_strategies(&self) -> Vec<MavenIntegrationStrategyInfo> {
        self.strategies
            .values()
            .map(|strategy| MavenIntegrationStrategyInfo {
                name: strategy.name().to_string(),
                description: strategy.description().to_string(),
                maven_version: strategy.maven_version().to_string(),
                is_default: strategy.name() == self.default_strategy,
            })
            .collect()
    }

    pub fn strategy_info(&self, name: &str) -> Option<MavenIntegrationStrategyInfo> {
        self.strategies
            .get(name)
            .map(|strategy| MavenIntegrationStrategyInfo {
                name: strategy.name().to_string(),
                description: strategy.description().to_string(),
                maven_version: strategy.maven_version().to_string(),
                is_default: strategy.name() == self.default_strategy,
            })
    }

    pub fn generate_default(
        &self,
        config: &MavenIntegrationConfig<'_>,
    ) -> Result<MavenIntegrationFiles, MavenIntegrationError> {
        self.generate(&self.default_strategy, config)
    }

    pub fn generate(
        &self,
        name: &str,
        config: &MavenIntegrationConfig<'_>,
    ) -> Result<MavenIntegrationFiles, MavenIntegrationError> {
        let strategy = self
            .strategies
            .get(name)
            .ok_or_else(|| MavenIntegrationError::UnknownStrategy(name.to_string()))?;

        strategy.generate(config)
    }

    pub fn register_strategy(&mut self, strategy: Box<dyn MavenIntegrationStrategy>) {
        let name = strategy.name().to_string();
        self.strategies.insert(name, strategy);
    }

    pub fn set_default_strategy(&mut self, name: impl Into<String>) {
        self.default_strategy = name.into();
    }
}

/// Maven統合エラー。
#[derive(Debug, Error)]
pub enum MavenIntegrationError {
    #[error("指定されたストラテジ '{0}' は未登録です")]
    UnknownStrategy(String),
    #[error("ストラテジ '{0}' は現在未対応です")]
    UnsupportedStrategy(String),
    #[error("pom.xml の生成に失敗しました: {0}")]
    Pom(#[from] PomGenerationError),
    #[error("settings.xml の生成に失敗しました: {0}")]
    Settings(#[from] SettingsGenerationError),
}

struct Maven3IntegrationStrategy;

impl MavenIntegrationStrategy for Maven3IntegrationStrategy {
    fn name(&self) -> &str {
        "maven3"
    }

    fn description(&self) -> &str {
        "Maven 3.x互換のpom.xmlとsettings.xmlを生成します"
    }

    fn maven_version(&self) -> &str {
        "3.x"
    }

    fn generate(
        &self,
        config: &MavenIntegrationConfig<'_>,
    ) -> Result<MavenIntegrationFiles, MavenIntegrationError> {
        let pom = PomGenerator::new(config.manifest, config.resolved)
            .with_lockfile(config.lockfile)
            .with_repositories(config.repositories)
            .generate()?;

        let settings_request = SettingsGenerationRequest {
            local_repository: config.local_repository,
            repositories: config.repositories,
            mirrors: config.mirrors,
        };
        let settings = settings::generate_settings_xml(&settings_request)?;

        let mut files = MavenIntegrationFiles::new();
        files.push(PathBuf::from("pom.xml"), pom);
        files.push(PathBuf::from(".jv/settings.xml"), settings);
        Ok(files)
    }
}

struct Maven4IntegrationStrategy;

impl MavenIntegrationStrategy for Maven4IntegrationStrategy {
    fn name(&self) -> &str {
        "maven4"
    }

    fn description(&self) -> &str {
        "Maven 4形式の出力生成 (開発中)"
    }

    fn maven_version(&self) -> &str {
        "4.x"
    }

    fn generate(
        &self,
        _config: &MavenIntegrationConfig<'_>,
    ) -> Result<MavenIntegrationFiles, MavenIntegrationError> {
        Err(MavenIntegrationError::UnsupportedStrategy(
            "maven4".to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lockfile::LockfileService;
    use crate::resolver::{
        DependencyScope, ResolutionDiagnostic, ResolutionSource, ResolutionStats,
        ResolverAlgorithmKind, VersionDecision,
    };
    use crate::{PackageInfo, ProjectSection};
    use std::collections::HashMap;
    use std::path::PathBuf;

    fn sample_manifest() -> Manifest {
        Manifest {
            package: PackageInfo {
                name: "demo-app".to_string(),
                version: "1.2.3".to_string(),
                description: Some("デモアプリケーション".to_string()),
                dependencies: HashMap::from([
                    ("org.example:demo-lib".to_string(), "^1.0".to_string()),
                    ("org.example:test-kit".to_string(), "^2.0".to_string()),
                ]),
            },
            project: ProjectSection::default(),
            repositories: crate::RepositorySection::default(),
            mirrors: Vec::new(),
            build: None,
            maven: crate::MavenProjectMetadata {
                group_id: "com.example".to_string(),
                artifact_id: Some("demo-app".to_string()),
                packaging: Some("jar".to_string()),
                description: None,
                url: Some("https://example.com".to_string()),
            },
        }
    }

    fn sample_resolved() -> ResolvedDependencies {
        ResolvedDependencies {
            strategy: "pubgrub".to_string(),
            algorithm: ResolverAlgorithmKind::PubGrub,
            dependencies: vec![
                crate::resolver::ResolvedDependency {
                    name: "org.example:demo-lib".to_string(),
                    requested: "^1.0".to_string(),
                    decision: VersionDecision::Exact("1.0.1".to_string()),
                    scope: DependencyScope::Main,
                    source: ResolutionSource::Registry,
                },
                crate::resolver::ResolvedDependency {
                    name: "org.example:test-kit".to_string(),
                    requested: "^2.0".to_string(),
                    decision: VersionDecision::Exact("2.1.0".to_string()),
                    scope: DependencyScope::Dev,
                    source: ResolutionSource::Registry,
                },
            ],
            diagnostics: Vec::<ResolutionDiagnostic>::new(),
            stats: ResolutionStats {
                elapsed_ms: 10,
                total_dependencies: 2,
                decided_dependencies: 2,
            },
        }
    }

    #[test]
    fn pom_generator_outputs_expected_sections() {
        let manifest = sample_manifest();
        let resolved = sample_resolved();
        let lockfile = LockfileService::generate(&manifest, &resolved).expect("lockfile");

        let xml = PomGenerator::new(&manifest, &resolved)
            .with_lockfile(Some(&lockfile))
            .with_repositories(&[])
            .generate()
            .expect("pom generation");

        assert!(xml.contains("<groupId>com.example</groupId>"));
        assert!(xml.contains("<artifactId>demo-app</artifactId>"));
        assert!(xml.contains("<version>1.2.3</version>"));
        assert!(xml.contains("<dependency>"));
        assert!(xml.contains("<artifactId>demo-lib</artifactId>"));
        assert!(xml.contains("<version>1.0.1</version>"));
        assert!(xml.contains("<scope>test</scope>"));
    }

    #[test]
    fn settings_generator_emits_repository_profile() {
        let repositories = vec![MavenRepositoryConfig::new(
            "central",
            "https://repo.maven.apache.org/maven2",
        )];
        let mirrors = vec![MavenMirrorConfig::new(
            "corp",
            "central",
            "https://mirror.corp/maven",
        )];
        let local_repo = PathBuf::from("/tmp/project/.jv/repository");

        let xml = settings::generate_settings_xml(&SettingsGenerationRequest {
            local_repository: &local_repo,
            repositories: &repositories,
            mirrors: &mirrors,
        })
        .expect("settings generation");

        assert!(xml.contains("<localRepository>/tmp/project/.jv/repository</localRepository>"));
        assert!(xml.contains("<id>central</id>"));
        assert!(xml.contains("<mirrorOf>central</mirrorOf>"));
    }

    #[test]
    fn dispatcher_generates_files() {
        let manifest = sample_manifest();
        let resolved = sample_resolved();
        let lockfile = LockfileService::generate(&manifest, &resolved).expect("lockfile");

        let repositories = vec![MavenRepositoryConfig::new(
            "central",
            "https://repo.maven.apache.org/maven2",
        )];
        let mirrors = Vec::<MavenMirrorConfig>::new();
        let project_root = PathBuf::from("/tmp/project");
        let local_repo = project_root.join(".jv/repository");

        let dispatcher = MavenIntegrationDispatcher::new();
        let files = dispatcher
            .generate_default(&MavenIntegrationConfig {
                manifest: &manifest,
                resolved: &resolved,
                lockfile: Some(&lockfile),
                repositories: &repositories,
                mirrors: &mirrors,
                project_root: &project_root,
                local_repository: &local_repo,
            })
            .expect("integration");

        assert!(
            files
                .files
                .iter()
                .any(|(path, _)| path == Path::new("pom.xml"))
        );
        assert!(
            files
                .files
                .iter()
                .any(|(path, _)| path == Path::new(".jv/settings.xml"))
        );
    }
}
