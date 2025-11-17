use dirs;
use std::{
    env, fs,
    io::{self, Write},
    sync::Arc,
    time::Duration,
};

use anyhow::{Context, anyhow};
use is_terminal::IsTerminal;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use strsim::normalized_levenshtein;
use tokio::runtime::Builder as RuntimeBuilder;
use url::form_urlencoded;

use crate::{
    DependencyCache, Manifest, MavenCoordinates, RepositoryManager,
    cli::{AddArgs, RemoveArgs},
    lockfile::{Lockfile, LockfileService},
    maven::{
        MavenIntegrationConfig, MavenIntegrationDispatcher, MavenMirrorConfig,
        MavenRepositoryConfig,
    },
    registry::{MavenMetadata, MavenRegistry, RegistryError},
    resolver::{ResolvedDependencies, ResolverAlgorithmKind, ResolverDispatcher, ResolverOptions},
};

use super::{
    context::WrapperContext,
    error::WrapperError,
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

        let summary = self.apply_manifest_update(&manifest, additions, Vec::new())?;
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

        let summary = self.apply_manifest_update(&manifest, Vec::new(), removals)?;
        self.context.manifest = manifest;
        Ok(summary)
    }

    fn apply_manifest_update(
        &mut self,
        manifest: &Manifest,
        added: Vec<MavenCoordinates>,
        removed: Vec<MavenCoordinates>,
    ) -> Result<WrapperUpdateSummary, WrapperError> {
        let resolved = self.resolve_manifest(manifest)?;

        let lockfile = LockfileService::generate(manifest, &resolved).map_err(|error| {
            WrapperError::OperationFailed(format!("jv.lock の生成に失敗しました: {error}"))
        })?;

        let lockfile_content = toml::to_string_pretty(&lockfile).map_err(|error| {
            WrapperError::OperationFailed(format!("jv.lock の整形に失敗しました: {error}"))
        })?;

        let lockfile_updated =
            sync::write_lockfile(&self.context.lockfile_path, lockfile_content.as_bytes())?;

        let (pom_updated, settings_updated) =
            self.generate_maven_artifacts(manifest, &resolved, &lockfile)?;

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
    ) -> Result<(bool, bool), WrapperError> {
        let mut manager = RepositoryManager::with_project_root(self.context.project_root.clone())
            .map_err(|error| {
            WrapperError::OperationFailed(format!(
                "リポジトリマネージャーの初期化に失敗しました: {error}"
            ))
        })?;
        manager.load_project_config(manifest);

        let repositories = collect_maven_repositories(&manager);
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
