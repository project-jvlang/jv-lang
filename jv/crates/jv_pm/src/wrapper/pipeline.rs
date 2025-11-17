use dirs;
use std::{
    collections::{HashMap, HashSet},
    env, fs,
    future::Future,
    io::{self, Write},
    path::{Path, PathBuf},
    pin::Pin,
    sync::Arc,
    time::Duration,
};

use anyhow::{Context, anyhow};
use is_terminal::IsTerminal;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use strsim::normalized_levenshtein;
use tokio::runtime::Builder as RuntimeBuilder;
use url::{Url, form_urlencoded};

use crate::{
    DependencyCache, Manifest, MavenCoordinates, RepositoryManager,
    cli::{AddArgs, RemoveArgs},
    download::{ArtifactDownloadRequest, DownloadManager, DownloadSuccess, JarFetcher},
    lockfile::{Lockfile, LockfileService},
    maven::{
        MavenIntegrationConfig, MavenIntegrationDispatcher, MavenMirrorConfig,
        MavenRepositoryConfig,
    },
    registry::{ArtifactCoordinates, DownloadedJar, MavenMetadata, MavenRegistry, RegistryError},
    resolver::{
        ResolvedDependencies, ResolvedDependency, ResolverAlgorithmKind, ResolverDispatcher,
        ResolverOptions, VersionDecision,
    },
};

use super::{
    context::WrapperContext,
    error::WrapperError,
    metrics,
    sync::{self, WrapperUpdateSummary},
};

/// Orchestrates Maven wrapper add/remove flow.
pub struct WrapperPipeline {
    context: WrapperContext,
    runtime: tokio::runtime::Runtime,
    client: Client,
    cache: Arc<DependencyCache>,
    dispatcher: ResolverDispatcher,
    integration_dispatcher: MavenIntegrationDispatcher,
    resolver_options: ResolverOptions,
}

impl WrapperPipeline {
    pub fn new(
        context: WrapperContext,
        resolver_options: ResolverOptions,
    ) -> Result<Self, WrapperError> {
        let runtime = RuntimeBuilder::new_multi_thread()
            .enable_all()
            .build()
            .map_err(|error| {
                WrapperError::OperationFailed(format!(
                    "tokioランタイムの初期化に失敗しました: {error}"
                ))
            })?;

        let client = Client::builder()
            .user_agent(format!("jvpm-wrapper/{}", env!("CARGO_PKG_VERSION")))
            .timeout(Duration::from_secs(20))
            .build()
            .map_err(|error| {
                WrapperError::OperationFailed(format!(
                    "HTTPクライアントの初期化に失敗しました: {error}"
                ))
            })?;

        let cache = Arc::new(DependencyCache::global().map_err(|error| {
            WrapperError::OperationFailed(format!(
                "グローバルキャッシュの初期化に失敗しました: {error}"
            ))
        })?);

        let dispatcher = ResolverDispatcher::with_default_strategies();
        let integration_dispatcher = MavenIntegrationDispatcher::new();

        Ok(Self {
            context,
            runtime,
            client,
            cache,
            dispatcher,
            integration_dispatcher,
            resolver_options,
        })
    }

    pub fn add(&mut self, args: &AddArgs) -> Result<WrapperUpdateSummary, WrapperError> {
        let interactive_allowed =
            !args.non_interactive && io::stdin().is_terminal() && io::stdout().is_terminal();

        let mut manifest = self.context.manifest.clone();
        let previous_resolved = self.resolve_manifest(&self.context.manifest)?;

        let mut manager = RepositoryManager::with_project_root(self.context.project_root.clone())
            .map_err(|error| {
            WrapperError::OperationFailed(format!(
                "リポジトリマネージャーの初期化に失敗しました: {error}"
            ))
        })?;
        manager.load_project_config(&manifest);

        let mut additions = Vec::new();
        for raw in &args.packages {
            let input = parse_dependency_input(raw).map_err(|error| {
                WrapperError::OperationFailed(format!("依存指定の解析に失敗しました: {error}"))
            })?;

            let (coordinate, version_hint) = match input {
                ParsedDependencyInput::Qualified {
                    coordinate,
                    requirement,
                } => (coordinate, requirement),
                ParsedDependencyInput::ArtifactOnly { query } => {
                    let suggestion =
                        self.pick_suggestion(&query, interactive_allowed, args.non_interactive)?;
                    (suggestion.coordinate, None)
                }
            };

            let version = resolve_version_for_coordinate(
                &self.runtime,
                &self.cache,
                &manager,
                &coordinate,
                version_hint.as_deref(),
            )
            .map_err(|error| {
                WrapperError::OperationFailed(format!(
                    "{} のバージョン決定に失敗しました: {error}",
                    coordinate.display()
                ))
            })?;

            manifest
                .package
                .dependencies
                .insert(coordinate.display(), version.clone());

            additions.push(MavenCoordinates::new(
                coordinate.group_id.clone(),
                coordinate.artifact_id.clone(),
            ));
        }

        let summary =
            self.apply_manifest_update(&manifest, additions, Vec::new(), &previous_resolved)?;
        self.context.manifest = manifest;
        Ok(summary)
    }

    pub fn remove(&mut self, args: &RemoveArgs) -> Result<WrapperUpdateSummary, WrapperError> {
        let interactive_allowed =
            !args.non_interactive && io::stdin().is_terminal() && io::stdout().is_terminal();

        let mut manifest = self.context.manifest.clone();

        if manifest.package.dependencies.is_empty() {
            return Err(WrapperError::OperationFailed(
                "pom.xml に管理対象の依存関係がありません。".to_string(),
            ));
        }

        let previous_resolved = self.resolve_manifest(&self.context.manifest)?;

        let mut removals = Vec::new();

        for raw in &args.packages {
            let input = parse_dependency_input(raw).map_err(|error| {
                WrapperError::OperationFailed(format!("依存指定の解析に失敗しました: {error}"))
            })?;

            let target = match input {
                ParsedDependencyInput::Qualified {
                    coordinate,
                    requirement: _,
                } => {
                    let name = coordinate.display();
                    if manifest.package.dependencies.contains_key(&name) {
                        name
                    } else {
                        select_remove_candidate(
                            &manifest,
                            &coordinate.artifact_id,
                            &name,
                            interactive_allowed,
                            args.non_interactive,
                        )?
                    }
                }
                ParsedDependencyInput::ArtifactOnly { query } => select_remove_candidate(
                    &manifest,
                    &query,
                    &query,
                    interactive_allowed,
                    args.non_interactive,
                )?,
            };

            if let Some(_) = manifest.package.dependencies.remove(&target) {
                if let Some(coords) = maven_coordinates_from_name(&target) {
                    removals.push(coords);
                }
            } else {
                return Err(WrapperError::OperationFailed(format!(
                    "依存関係 '{}' は pom.xml に存在しません。",
                    target
                )));
            }
        }

        let summary =
            self.apply_manifest_update(&manifest, Vec::new(), removals, &previous_resolved)?;
        self.context.manifest = manifest;
        Ok(summary)
    }

    fn apply_manifest_update(
        &mut self,
        manifest: &Manifest,
        added: Vec<MavenCoordinates>,
        removed: Vec<MavenCoordinates>,
        previous_resolved: &ResolvedDependencies,
    ) -> Result<WrapperUpdateSummary, WrapperError> {
        let resolved = metrics::measure("wrapper-pipeline-dependency-resolution", || {
            self.resolve_manifest(manifest)
        })?;

        let lockfile = metrics::measure("wrapper-pipeline-lockfile-generation", || {
            LockfileService::generate(manifest, &resolved).map_err(|error| {
                WrapperError::OperationFailed(format!("jv.lock の生成に失敗しました: {error}"))
            })
        })?;

        let lockfile_content = toml::to_string_pretty(&lockfile).map_err(|error| {
            WrapperError::OperationFailed(format!("jv.lock の整形に失敗しました: {error}"))
        })?;

        let lockfile_updated = metrics::measure("wrapper-pipeline-lockfile-write", || {
            sync::write_lockfile(&self.context.lockfile_path, lockfile_content.as_bytes())
        })?;

        let mut manager = RepositoryManager::with_project_root(self.context.project_root.clone())
            .map_err(|error| {
            WrapperError::OperationFailed(format!(
                "リポジトリマネージャーの初期化に失敗しました: {error}"
            ))
        })?;
        manager.load_project_config(manifest);

        let (pom_updated, settings_updated) =
            metrics::measure("wrapper-pipeline-artifact-generation", || {
                self.generate_maven_artifacts(manifest, &resolved, &lockfile, &manager)
            })?;

        metrics::measure("wrapper-pipeline-jar-download", || {
            self.ensure_wrapper_jars(manifest, &resolved, &manager)
        })?;

        metrics::measure("wrapper-pipeline-artifact-cleanup", || {
            self.cleanup_removed_artifacts(&removed, previous_resolved)
        })?;

        Ok(WrapperUpdateSummary {
            added,
            removed,
            pom_updated,
            settings_updated,
            lockfile_updated,
        })
    }

    fn resolve_manifest(&self, manifest: &Manifest) -> Result<ResolvedDependencies, WrapperError> {
        if manifest.package.dependencies.is_empty() {
            let strategy_name = self.requested_strategy_name();
            let (strategy_label, algorithm) = self
                .dispatcher
                .strategy_info(strategy_name)
                .map(|info| (info.name.clone(), info.algorithm))
                .unwrap_or_else(|| {
                    (
                        self.dispatcher.default_strategy().to_string(),
                        ResolverAlgorithmKind::PubGrub,
                    )
                });

            return Ok(ResolvedDependencies {
                strategy: strategy_label,
                algorithm,
                dependencies: Vec::new(),
                diagnostics: Vec::new(),
                stats: crate::resolver::ResolutionStats {
                    elapsed_ms: 0,
                    total_dependencies: 0,
                    decided_dependencies: 0,
                },
            });
        }

        self.dispatcher
            .resolve_manifest(manifest, self.resolver_options.clone())
            .map_err(|error| {
                WrapperError::OperationFailed(format!("依存関係の解決に失敗しました: {error}"))
            })
    }

    fn requested_strategy_name(&self) -> &str {
        self.resolver_options
            .strategy
            .as_deref()
            .unwrap_or_else(|| self.dispatcher.default_strategy())
    }

    fn generate_maven_artifacts(
        &self,
        manifest: &Manifest,
        resolved: &ResolvedDependencies,
        lockfile: &Lockfile,
        manager: &RepositoryManager,
    ) -> Result<(bool, bool), WrapperError> {
        let repositories = collect_maven_repositories(manager);
        let mirrors = collect_effective_mirrors(manifest)?;

        let config = MavenIntegrationConfig {
            manifest: Some(manifest),
            resolved,
            lockfile: Some(lockfile),
            repositories: &repositories,
            mirrors: &mirrors,
            project_root: &self.context.project_root,
            local_repository: &self.context.local_repository,
        };

        let files = self
            .integration_dispatcher
            .generate("wrapper-default", &config)
            .map_err(|error| {
                WrapperError::OperationFailed(format!("pom/xml の生成に失敗しました: {error}"))
            })?;

        sync::sync_maven_artifacts(&self.context.project_root, &files)
    }

    fn ensure_wrapper_jars(
        &self,
        manifest: &Manifest,
        resolved: &ResolvedDependencies,
        manager: &RepositoryManager,
    ) -> Result<(), WrapperError> {
        self.download_wrapper_jars(manifest, resolved, manager)
    }

    fn download_wrapper_jars(
        &self,
        manifest: &Manifest,
        resolved: &ResolvedDependencies,
        manager: &RepositoryManager,
    ) -> Result<(), WrapperError> {
        let requests = self.build_download_requests(manifest, resolved)?;
        if requests.is_empty() {
            return Ok(());
        }

        let registries = build_registry_clients(manager)?;
        let fetcher = Arc::new(MultiRegistryFetcher::new(registries));

        let mut download_manager =
            DownloadManager::new(Arc::clone(&fetcher), Arc::clone(&self.cache));
        download_manager.apply_manifest(manifest.build.as_ref());

        let report = self
            .runtime
            .block_on(download_manager.download_artifacts(requests));

        if !report.failures.is_empty() {
            let message = report
                .failures
                .iter()
                .map(|failure| failure.error.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            return Err(WrapperError::OperationFailed(format!(
                "依存Jarのダウンロードに失敗しました: {message}"
            )));
        }

        self.sync_downloaded_jars(&report.successes)
    }

    fn build_download_requests(
        &self,
        manifest: &Manifest,
        resolved: &ResolvedDependencies,
    ) -> Result<Vec<ArtifactDownloadRequest>, WrapperError> {
        let mut seen = HashSet::new();
        let mut requests = Vec::new();

        for dependency in &resolved.dependencies {
            let coords = self.artifact_coordinates_from_dependency(manifest, dependency)?;
            if !seen.insert(coords.clone()) {
                continue;
            }
            requests.push(ArtifactDownloadRequest::new(coords));
        }

        Ok(requests)
    }

    fn artifact_coordinates_from_dependency(
        &self,
        manifest: &Manifest,
        dependency: &ResolvedDependency,
    ) -> Result<ArtifactCoordinates, WrapperError> {
        let (group_id, artifact_id) =
            parse_dependency_name(&dependency.name, manifest.maven_group_id())?;
        let version = match &dependency.decision {
            VersionDecision::Exact(value) => value.clone(),
            _ => {
                return Err(WrapperError::OperationFailed(format!(
                    "{} のバージョンが確定していません",
                    dependency.name
                )));
            }
        };
        Ok(ArtifactCoordinates::new(group_id, artifact_id, version))
    }

    fn sync_downloaded_jars(&self, successes: &[DownloadSuccess]) -> Result<(), WrapperError> {
        let mut seen = HashSet::new();
        for success in successes {
            let coordinates = success.request.coordinates().clone();
            if !seen.insert(coordinates.clone()) {
                continue;
            }
            self.copy_cached_artifact_to_local_repo(&coordinates, &success.artifact.path)?;
        }
        Ok(())
    }

    fn copy_cached_artifact_to_local_repo(
        &self,
        coords: &ArtifactCoordinates,
        source: &Path,
    ) -> Result<(), WrapperError> {
        let relative_path = PathBuf::from(coords.jar_path());
        let target = self.context.local_repository.join(relative_path);

        if let Some(parent) = target.parent() {
            fs::create_dir_all(parent).map_err(|error| {
                WrapperError::OperationFailed(format!(
                    "{} の作成に失敗しました: {error}",
                    parent.display()
                ))
            })?;
        }

        fs::copy(source, &target).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "{} へのコピーに失敗しました: {error}",
                target.display()
            ))
        })?;

        let checksum_source = source.with_extension("jar.sha256");
        if checksum_source.exists() {
            let checksum_target = target.with_extension("jar.sha256");
            fs::copy(&checksum_source, checksum_target).map_err(|error| {
                WrapperError::OperationFailed(format!(
                    "{} へのコピーに失敗しました: {error}",
                    checksum_target.display()
                ))
            })?;
        }

        Ok(())
    }

    fn cleanup_removed_artifacts(
        &self,
        removed: &[MavenCoordinates],
        previous_resolved: &ResolvedDependencies,
    ) -> Result<(), WrapperError> {
        if removed.is_empty() {
            return Ok(());
        }

        let version_map: HashMap<String, String> = previous_resolved
            .dependencies
            .iter()
            .filter_map(|dependency| match &dependency.decision {
                VersionDecision::Exact(version) => Some((dependency.name.clone(), version.clone())),
                _ => None,
            })
            .collect();

        for coord in removed {
            if let Some(version) = version_map.get(&coord.to_string()) {
                let target_dir = self
                    .context
                    .local_repository
                    .join(coord.group_path())
                    .join(&coord.artifact_id)
                    .join(version);

                if target_dir.exists() {
                    fs::remove_dir_all(&target_dir).map_err(|error| {
                        WrapperError::OperationFailed(format!(
                            "{} の削除に失敗しました: {error}",
                            target_dir.display()
                        ))
                    })?;
                }
            }
        }

        Ok(())
    }

    fn pick_suggestion(
        &self,
        query: &str,
        interactive_allowed: bool,
        non_interactive: bool,
    ) -> Result<PackageSuggestion, WrapperError> {
        let mut suggestions = search_package_candidates(&self.runtime, &self.client, query, 10)
            .map_err(|error| WrapperError::OperationFailed(error.to_string()))?;

        if suggestions.is_empty() {
            return Err(WrapperError::OperationFailed(format!(
                "依存候補が見つかりませんでした: '{}'. 完全な group:artifact を指定してください。",
                query
            )));
        }

        if suggestions.len() > 1 && (non_interactive || !interactive_allowed) {
            print_non_interactive_suggestions(&suggestions);
            return Err(WrapperError::OperationFailed(
                "複数の候補が見つかりました。完全な group:artifact を指定して再実行してください。"
                    .to_string(),
            ));
        }

        if suggestions.len() == 1 {
            return Ok(suggestions.remove(0));
        }

        print_suggestions(&suggestions);
        let index = prompt_user_selection(suggestions.len())
            .map_err(|error| WrapperError::OperationFailed(error.to_string()))?;
        Ok(suggestions.remove(index))
    }
}

fn collect_maven_repositories(manager: &RepositoryManager) -> Vec<MavenRepositoryConfig> {
    manager
        .list()
        .into_iter()
        .map(|handle| {
            let config = handle.config();
            let mut repo =
                MavenRepositoryConfig::new(config.name.clone(), handle.url().to_string());
            if !config.name.trim().is_empty() {
                repo = repo.with_name(config.name.clone());
            }
            repo
        })
        .collect()
}

fn collect_effective_mirrors(manifest: &Manifest) -> Result<Vec<MavenMirrorConfig>, WrapperError> {
    let mut mirrors = Vec::new();

    for mirror in manifest.mirrors.iter() {
        let id = mirror
            .name
            .clone()
            .unwrap_or_else(|| format!("mirror-{}", mirror.mirror_of));
        mirrors.push(MavenMirrorConfig::new(
            id,
            mirror.mirror_of.clone(),
            mirror.url.clone(),
        ));
    }

    let global_state = load_global_config_state()?;
    for (index, mirror) in global_state.data.mirrors.into_iter().enumerate() {
        let id = mirror
            .name
            .clone()
            .unwrap_or_else(|| format!("global-mirror-{}", index));
        mirrors.push(MavenMirrorConfig::new(id, mirror.mirror_of, mirror.url));
    }

    Ok(mirrors)
}

fn build_registry_clients(
    manager: &RepositoryManager,
) -> Result<Vec<Arc<MavenRegistry>>, WrapperError> {
    let mut registries = Vec::new();
    for handle in manager.list().into_iter() {
        if handle.config().name == "local" {
            continue;
        }

        if let Ok(parsed) = Url::parse(handle.url()) {
            if parsed.scheme() == "file" {
                continue;
            }
        }

        let registry = MavenRegistry::new(handle.url()).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "リポジトリ '{}' の初期化に失敗しました: {error}",
                handle.name()
            ))
        })?;
        registries.push(Arc::new(registry));
    }

    if registries.is_empty() {
        return Err(WrapperError::OperationFailed(
            "Maven Jar をダウンロードするリポジトリが構成されていません。".to_string(),
        ));
    }

    Ok(registries)
}

fn parse_dependency_name(
    raw: &str,
    default_group: Option<&str>,
) -> Result<(String, String), WrapperError> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return Err(WrapperError::OperationFailed(
            "依存指定が空です。".to_string(),
        ));
    }

    let segments: Vec<&str> = trimmed.split(':').collect();
    match segments.len() {
        1 => {
            let artifact = segments[0];
            if artifact.is_empty() {
                return Err(WrapperError::OperationFailed(format!(
                    "依存 '{}' の artifactId が空です。",
                    raw
                )));
            }
            let group = default_group.ok_or_else(|| {
                WrapperError::OperationFailed(format!(
                    "依存 '{}' の groupId を特定できません。",
                    raw
                ))
            })?;
            Ok((group.to_string(), artifact.to_string()))
        }
        2 => {
            let group = segments[0];
            let artifact = segments[1];
            if group.is_empty() || artifact.is_empty() {
                return Err(WrapperError::OperationFailed(format!(
                    "依存 '{}' の座標に空の要素が含まれています。",
                    raw
                )));
            }
            Ok((group.to_string(), artifact.to_string()))
        }
        _ => Err(WrapperError::OperationFailed(format!(
            "依存 '{}' の座標は 'group:artifact' 形式で指定してください。",
            raw
        ))),
    }
}

const CHECKSUM_RETRY_LIMIT: usize = 2;

#[derive(Clone)]
struct MultiRegistryFetcher {
    registries: Vec<Arc<MavenRegistry>>,
}

impl MultiRegistryFetcher {
    fn new(registries: Vec<Arc<MavenRegistry>>) -> Self {
        Self { registries }
    }
}

impl JarFetcher for MultiRegistryFetcher {
    fn download_jar<'a>(
        &'a self,
        coords: &'a ArtifactCoordinates,
    ) -> Pin<Box<dyn Future<Output = Result<DownloadedJar, RegistryError>> + Send + 'a>> {
        let registries = self.registries.clone();
        let coords = coords.clone();

        Box::pin(async move {
            let mut last_error = None;
            for registry in registries {
                for attempt in 1..=CHECKSUM_RETRY_LIMIT {
                    match registry.download_jar(&coords).await {
                        Ok(jar) => return Ok(jar),
                        Err(error) => {
                            if matches!(error, RegistryError::ChecksumMismatch { .. })
                                && attempt < CHECKSUM_RETRY_LIMIT
                            {
                                continue;
                            }
                            last_error = Some(error);
                            break;
                        }
                    }
                }
            }

            Err(
                last_error.unwrap_or_else(|| RegistryError::InvalidResponse {
                    resource: coords.jar_path(),
                    message: "Jar をダウンロードできませんでした".to_string(),
                }),
            )
        })
    }
}

fn load_global_config_state() -> Result<GlobalConfigState, WrapperError> {
    let path = dirs::home_dir()
        .ok_or_else(|| {
            WrapperError::OperationFailed("ホームディレクトリを特定できませんでした".to_string())
        })?
        .join(".jv")
        .join("config.toml");

    if path.exists() {
        let content = fs::read_to_string(&path).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "{} の読み込みに失敗しました: {error}",
                path.display()
            ))
        })?;
        let data = toml::from_str(&content).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "{} の解析に失敗しました: {error}",
                path.display()
            ))
        })?;
        Ok(GlobalConfigState { data })
    } else {
        Ok(GlobalConfigState {
            data: StoredGlobalConfig::default(),
        })
    }
}

struct GlobalConfigState {
    data: StoredGlobalConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct StoredGlobalConfig {
    #[serde(default)]
    repositories: Vec<crate::repository::config::RepositoryConfig>,
    #[serde(default)]
    mirrors: Vec<crate::repository::config::MirrorConfig>,
    #[serde(flatten)]
    extras: toml::value::Table,
}

#[derive(Debug, Clone)]
struct DependencyCoordinate {
    group_id: String,
    artifact_id: String,
}

impl DependencyCoordinate {
    fn new(group_id: impl Into<String>, artifact_id: impl Into<String>) -> Self {
        Self {
            group_id: group_id.into(),
            artifact_id: artifact_id.into(),
        }
    }

    fn display(&self) -> String {
        format!("{}:{}", self.group_id, self.artifact_id)
    }
}

enum ParsedDependencyInput {
    Qualified {
        coordinate: DependencyCoordinate,
        requirement: Option<String>,
    },
    ArtifactOnly {
        query: String,
    },
}

#[derive(Debug, Clone)]
struct PackageSuggestion {
    coordinate: DependencyCoordinate,
    latest_version: Option<String>,
    packaging: Option<String>,
}

#[derive(Debug, Deserialize)]
struct MavenSearchResponse {
    response: MavenSearchDocs,
}

#[derive(Debug, Deserialize)]
struct MavenSearchDocs {
    docs: Vec<MavenSearchDoc>,
}

#[derive(Debug, Deserialize)]
struct MavenSearchDoc {
    #[serde(rename = "g", default)]
    group_id: String,
    #[serde(rename = "a", default)]
    artifact_id: String,
    #[serde(rename = "latestVersion", default)]
    latest_version: Option<String>,
    #[serde(rename = "p", default)]
    packaging: Option<String>,
}

fn print_suggestions(suggestions: &[PackageSuggestion]) {
    for (index, suggestion) in suggestions.iter().enumerate() {
        let latest = suggestion.latest_version.as_deref().unwrap_or("不明");
        let packaging = suggestion.packaging.as_deref().unwrap_or("jar");
        println!(
            "  [{}] {}  最新版: {}  パッケージ: {}",
            index + 1,
            suggestion.coordinate.display(),
            latest,
            packaging
        );
    }
}

fn print_non_interactive_suggestions(suggestions: &[PackageSuggestion]) {
    println!("候補:");
    print_suggestions(suggestions);
    println!(
        "複数の候補が見つかりました。--non-interactive を指定した場合は、完全な group:artifact を指定して再実行してください。"
    );
}

fn prompt_user_selection(count: usize) -> anyhow::Result<usize> {
    loop {
        print!("番号を入力してください (1-{} / qで中止): ", count);
        io::stdout().flush().ok();

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;
        let input = buffer.trim();
        if input.eq_ignore_ascii_case("q") || input.is_empty() {
            return Err(anyhow!("選択がキャンセルされました"));
        }

        if let Ok(value) = input.parse::<usize>() {
            if (1..=count).contains(&value) {
                return Ok(value - 1);
            }
        }

        println!(
            "無効な入力です。1 から {} の数値を入力してください。",
            count
        );
    }
}

fn resolve_version_for_coordinate(
    runtime: &tokio::runtime::Runtime,
    cache: &Arc<DependencyCache>,
    manager: &RepositoryManager,
    coordinate: &DependencyCoordinate,
    explicit: Option<&str>,
) -> anyhow::Result<String> {
    if let Some(version) = explicit {
        let trimmed = version.trim();
        if trimmed.is_empty() {
            return Err(anyhow!("バージョン指定が空です。"));
        }
        return Ok(trimmed.to_string());
    }

    let coords = MavenCoordinates::new(&coordinate.group_id, &coordinate.artifact_id);

    if let Some(cached) = cache.get_metadata(&coords)? {
        if let Some(version) = select_preferred_version(&cached.metadata) {
            return Ok(version);
        }
    }

    let handles = manager.get_repositories_for_dependency(&coordinate.group_id);
    if handles.is_empty() {
        return Err(anyhow!(
            "レジストリが設定されていないため最新バージョンを取得できません ({})",
            coordinate.display()
        ));
    }

    let mut last_error: Option<anyhow::Error> = None;

    for handle in handles {
        let registry = MavenRegistry::new(handle.url()).with_context(|| {
            format!(
                "リポジトリ '{}' ({}) の初期化に失敗しました",
                handle.name(),
                handle.url()
            )
        })?;

        match runtime.block_on(registry.fetch_metadata(&coords)) {
            Ok(metadata) => {
                let preferred = select_preferred_version(&metadata);
                if let Err(error) = cache.store_metadata(&coords, &metadata) {
                    tracing::warn!(error = ?error, "メタデータのキャッシュ保存に失敗しました");
                }
                if let Some(version) = preferred {
                    return Ok(version);
                }
                last_error = Some(anyhow!(
                    "リポジトリ '{}' に利用可能なバージョンが見つかりませんでした ({})",
                    handle.name(),
                    coordinate.display()
                ));
            }
            Err(RegistryError::PackageNotFound { .. }) => {
                last_error = Some(anyhow!(
                    "リポジトリ '{}' にパッケージが見つかりませんでした ({})",
                    handle.name(),
                    coordinate.display()
                ));
            }
            Err(error) => {
                last_error = Some(error.into());
            }
        }
    }

    Err(last_error.unwrap_or_else(|| {
        anyhow!(
            "{} のバージョンを特定できませんでした。完全な座標を指定してください。",
            coordinate.display()
        )
    }))
}

fn select_preferred_version(metadata: &MavenMetadata) -> Option<String> {
    metadata
        .latest_release()
        .map(|s| s.to_string())
        .or_else(|| metadata.latest().map(|s| s.to_string()))
        .or_else(|| metadata.versions().iter().last().cloned())
}

#[derive(Debug, Clone)]
struct DependencyCandidate {
    name: String,
    requirement: String,
    score: f64,
}

fn select_remove_candidate(
    manifest: &Manifest,
    query: &str,
    display_query: &str,
    interactive_allowed: bool,
    non_interactive: bool,
) -> Result<String, WrapperError> {
    let mut candidates = gather_dependency_candidates(manifest, query);
    if candidates.is_empty() {
        return Err(WrapperError::OperationFailed(format!(
            "依存関係 '{}' は pom.xml に存在しません。",
            display_query
        )));
    }

    if candidates.len() == 1 {
        return Ok(candidates.remove(0).name);
    }

    if non_interactive || !interactive_allowed {
        print_remove_candidates(&candidates);
        return Err(WrapperError::OperationFailed(
            "複数の依存関係が該当します。完全な group:artifact を指定して再実行してください。"
                .to_string(),
        ));
    }

    println!("複数の依存関係が該当します。削除する項目を選択してください:");
    print_remove_candidates(&candidates);
    let index = prompt_user_selection(candidates.len())
        .map_err(|error| WrapperError::OperationFailed(error.to_string()))?;
    Ok(candidates.remove(index).name)
}

fn gather_dependency_candidates(manifest: &Manifest, query: &str) -> Vec<DependencyCandidate> {
    let mut entries = Vec::new();
    let lower_query = query.trim().to_ascii_lowercase();

    for (name, requirement) in &manifest.package.dependencies {
        let lower_name = name.to_ascii_lowercase();
        let score = if lower_name == lower_query {
            1.0
        } else if lower_name.contains(&lower_query) {
            0.9
        } else {
            let artifact = name.split(':').last().unwrap_or(name);
            let artifact_lower = artifact.to_ascii_lowercase();
            if artifact_lower == lower_query {
                0.95
            } else if artifact_lower.contains(&lower_query) {
                0.85
            } else {
                let distance = normalized_levenshtein(&artifact_lower, &lower_query);
                (1.0 - distance).max(0.0) * 0.5
            }
        };

        entries.push(DependencyCandidate {
            name: name.clone(),
            requirement: requirement.clone(),
            score,
        });
    }

    entries.sort_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    entries.truncate(10);
    entries
}

fn print_remove_candidates(candidates: &[DependencyCandidate]) {
    println!("候補:");
    for (index, candidate) in candidates.iter().enumerate() {
        println!(
            "  [{}] {} = {}",
            index + 1,
            candidate.name,
            candidate.requirement
        );
    }
}

fn maven_coordinates_from_name(name: &str) -> Option<MavenCoordinates> {
    let segments = name.split(':').map(|part| part.trim()).collect::<Vec<_>>();
    if segments.len() != 2 {
        return None;
    }
    Some(MavenCoordinates::new(segments[0], segments[1]))
}

fn search_package_candidates(
    runtime: &tokio::runtime::Runtime,
    client: &Client,
    query: &str,
    limit: usize,
) -> anyhow::Result<Vec<PackageSuggestion>> {
    let trimmed = query.trim();
    if trimmed.is_empty() {
        return Ok(Vec::new());
    }

    let raw_query = if trimmed.contains(':') {
        trimmed.to_string()
    } else {
        format!("a:{}*", trimmed)
    };

    let encoded_query: String = form_urlencoded::byte_serialize(raw_query.as_bytes()).collect();
    let url = format!(
        "https://search.maven.org/solrsearch/select?q={}&rows={}&wt=json",
        encoded_query, limit
    );

    let response: MavenSearchResponse = runtime.block_on(async {
        let resp = client.get(&url).send().await?;
        let checked = resp.error_for_status()?;
        checked.json::<MavenSearchResponse>().await
    })?;

    let mut suggestions = Vec::new();
    for doc in response.response.docs.into_iter().take(limit) {
        if doc.group_id.is_empty() || doc.artifact_id.is_empty() {
            continue;
        }
        suggestions.push(PackageSuggestion {
            coordinate: DependencyCoordinate::new(doc.group_id, doc.artifact_id),
            latest_version: doc.latest_version.filter(|value| !value.is_empty()),
            packaging: doc.packaging,
        });
    }

    Ok(suggestions)
}

fn parse_dependency_input(raw: &str) -> anyhow::Result<ParsedDependencyInput> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return Err(anyhow!("依存指定が空です。"));
    }

    let (coord_part, version_override) = match trimmed.rsplit_once('@') {
        Some((coord, ver)) if coord.contains(':') && !ver.contains(':') => {
            (coord.trim(), Some(ver.trim().to_string()))
        }
        _ => (trimmed, None),
    };

    let segments = coord_part
        .split(':')
        .map(|part| part.trim())
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();

    match segments.len() {
        0 => Err(anyhow!("依存指定 '{}' が無効です。", raw)),
        1 => Ok(ParsedDependencyInput::ArtifactOnly {
            query: segments[0].to_string(),
        }),
        2 => Ok(ParsedDependencyInput::Qualified {
            coordinate: DependencyCoordinate::new(segments[0], segments[1]),
            requirement: version_override,
        }),
        3 => {
            let requirement = segments[2].trim();
            if requirement.is_empty() {
                return Err(anyhow!("依存指定 '{}' のバージョン部分が空です。", raw));
            }
            Ok(ParsedDependencyInput::Qualified {
                coordinate: DependencyCoordinate::new(segments[0], segments[1]),
                requirement: Some(requirement.to_string()),
            })
        }
        _ => Err(anyhow!(
            "依存指定 '{}' が不正です。group:artifact[:version] 形式で指定してください。",
            raw
        )),
    }
}
