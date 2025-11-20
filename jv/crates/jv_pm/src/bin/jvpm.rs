use std::collections::HashSet;
use std::env;
use std::ffi::OsString;
use std::fmt::Write as _;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result, anyhow};
use clap::Parser;
use is_terminal::IsTerminal;
use jv_pm::cli::{
    AddArgs, Cli, Commands, RemoveArgs, RepoAuthKind, RepoCommand, RepoOrigin, RepoScope,
    ResolverAlgorithm, ResolverCommand,
};
use jv_pm::wrapper::{
    CliMode, WrapperCommandFilter, context::WrapperContext, pipeline::WrapperPipeline,
    sync::WrapperUpdateSummary,
};
use jv_pm::{
    self, AuthConfig, AuthType, DependencyCache, ExportError, ExportRequest, FilterConfig,
    JavaProjectExporter, LockfileService, Manifest, MavenCoordinates, MavenMetadata,
    MavenMirrorConfig, MavenRegistry, MavenRepositoryConfig, MirrorConfig, RegistryError,
    RepositoryConfig, RepositoryManager, ResolutionStats, ResolvedDependencies,
    ResolverAlgorithmKind, ResolverDispatcher, ResolverOptions, ResolverStrategyInfo,
    StrategyStability, repository,
};
use jv_pm::resolver::MavenResolverContext;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use strsim::normalized_levenshtein;
use tokio::runtime::Builder as RuntimeBuilder;
use url::form_urlencoded;
#[derive(Clone, Debug)]
struct RepositoryEntry {
    origin: RepoOrigin,
    config: RepositoryConfig,
    effective_url: String,
}

#[derive(Clone, Debug, Serialize)]
struct RepositoryListEntry {
    name: String,
    scope: RepoOrigin,
    priority: u32,
    configured_url: String,
    effective_url: String,
    mirrored: bool,
    auth: Option<AuthSummary>,
    filter: Option<FilterSummary>,
}

#[derive(Clone, Debug, Serialize)]
struct RepositoryDetail {
    name: String,
    scope: RepoOrigin,
    priority: u32,
    configured_url: String,
    effective_url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    mirrored: Option<MirrorDetail>,
    #[serde(skip_serializing_if = "Option::is_none")]
    auth: Option<AuthSummary>,
    #[serde(skip_serializing_if = "Option::is_none")]
    filter: Option<FilterSummary>,
}

#[derive(Clone, Debug, Serialize)]
struct AuthSummary {
    mode: RepoAuthKind,
    #[serde(skip_serializing_if = "Option::is_none")]
    username_env: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    password_env: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    token_env: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
struct FilterSummary {
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    include_groups: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    exclude_groups: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
struct MirrorDetail {
    scope: RepoScope,
    mirror_of: String,
    url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
}

struct RepositoryContext {
    manifest: Manifest,
    global: GlobalConfigState,
    entries: Vec<RepositoryEntry>,
}

struct GlobalConfigState {
    path: PathBuf,
    data: StoredGlobalConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct StoredGlobalConfig {
    #[serde(default)]
    repositories: Vec<RepositoryConfig>,
    #[serde(default)]
    mirrors: Vec<MirrorConfig>,
    #[serde(flatten)]
    extras: toml::value::Table,
}

struct AddOptions {
    name: String,
    url: String,
    priority: Option<u32>,
    scope: RepoScope,
    auth: RepoAuthKind,
    username_env: Option<String>,
    password_env: Option<String>,
    token_env: Option<String>,
    include_groups: Vec<String>,
    exclude_groups: Vec<String>,
}

struct RemoveOptions {
    name: String,
    scope: RepoScope,
}

struct MirrorOptions {
    target: Option<String>,
    url: Option<String>,
    display_name: Option<String>,
    remove: bool,
    json: bool,
    scope: RepoScope,
}

enum MirrorCommandResult {
    Unchanged,
    Modified,
}

fn main() {
    if let Err(error) = real_main() {
        eprintln!("{error}");
        std::process::exit(1);
    }
}

fn real_main() -> Result<()> {
    let mode = CliMode::detect();
    if mode.is_wrapper() {
        println!("[wrapper-mode] Maven ラッパーモードで起動しました");
    }
    let cli = Cli::parse();
    WrapperCommandFilter::validate(&cli.command, mode).map_err(|err| anyhow!(err))?;

    match cli.command {
        Commands::Add(args) => handle_add_command(args, mode),
        Commands::Remove(args) => handle_remove_command(args, mode),
        Commands::Resolver(command) => handle_resolver_command(command),
        Commands::Repo(command) => handle_repo_command(command),
        Commands::Maven(args) => handle_maven_passthrough(args),
    }
}

fn handle_maven_passthrough(args: Vec<OsString>) -> Result<()> {
    let maven_cmd = resolve_maven_binary()?;
    let status = Command::new(&maven_cmd)
        .args(&args)
        .status()
        .with_context(|| format!("{} の実行に失敗しました", maven_cmd.display()))?;

    if status.success() {
        return Ok(());
    }

    if let Some(code) = status.code() {
        std::process::exit(code);
    }

    Err(anyhow!(
        "{} がシグナル割り込みで終了しました",
        maven_cmd.display()
    ))
}

fn resolver_options_from_strategy(strategy: Option<ResolverAlgorithm>) -> ResolverOptions {
    let mut options = ResolverOptions::default();
    if let Some(strategy) = strategy {
        options = options.with_strategy(strategy.strategy_name());
    }
    options
}

/// Wrapperモード専用: 明示指定が無ければ Maven 互換戦略をデフォルトで使用する。
fn resolver_options_for_wrapper(strategy: Option<ResolverAlgorithm>) -> ResolverOptions {
    match strategy {
        Some(value) => resolver_options_from_strategy(Some(value)),
        None => {
            ResolverOptions::default().with_strategy(ResolverAlgorithm::MavenCompat.strategy_name())
        }
    }
}

fn handle_add_command(args: AddArgs, mode: CliMode) -> Result<()> {
    if mode.is_wrapper() {
        return handle_wrapper_add_command(&args);
    }
    handle_native_add_command(args)
}

fn handle_wrapper_add_command(args: &AddArgs) -> Result<()> {
    let context = WrapperContext::detect().map_err(|error| anyhow!(error))?;
    let resolver_options = resolver_options_for_wrapper(args.strategy);
    let mut pipeline =
        WrapperPipeline::new(context, resolver_options).map_err(|error| anyhow!(error))?;
    let summary = pipeline.add(args).map_err(|error| anyhow!(error))?;

    print_wrapper_add_summary(&summary);
    Ok(())
}

fn handle_native_add_command(args: AddArgs) -> Result<()> {
    let resolver_options = resolver_options_from_strategy(args.strategy);

    let runtime = RuntimeBuilder::new_multi_thread()
        .enable_all()
        .build()
        .context("tokioランタイムの初期化に失敗しました")?;

    let client = Client::builder()
        .user_agent(format!("jvpm/{}", env!("CARGO_PKG_VERSION")))
        .timeout(Duration::from_secs(20))
        .build()
        .context("HTTPクライアントの初期化に失敗しました")?;

    let (mut manifest, manifest_path) = load_manifest_with_path()?;
    let project_root = manifest_path
        .parent()
        .ok_or_else(|| anyhow!("jv.toml の親ディレクトリを特定できませんでした"))?
        .to_path_buf();

    let mut manager = RepositoryManager::with_project_root(project_root.clone())
        .context("リポジトリマネージャーの初期化に失敗しました")?;
    manager.load_project_config(&manifest);

    let cache =
        Arc::new(DependencyCache::global().context("グローバルキャッシュの初期化に失敗しました")?);

    let interactive_allowed =
        !args.non_interactive && io::stdin().is_terminal() && io::stdout().is_terminal();

    let mut additions = Vec::new();

    for raw in &args.packages {
        let input = parse_dependency_input(raw)?;
        let (coordinate, version_hint) = match input {
            ParsedDependencyInput::Qualified {
                coordinate,
                requirement,
            } => (coordinate, requirement),
            ParsedDependencyInput::ArtifactOnly { query } => {
                let suggestions = search_package_candidates(&runtime, &client, &query, 10)
                    .with_context(|| format!("'{}' の依存候補検索に失敗しました", query))?;

                if suggestions.is_empty() {
                    return Err(anyhow!(
                        "依存候補が見つかりませんでした: '{}'. 完全な group:artifact を指定してください。",
                        query
                    ));
                }

                if suggestions.len() > 1 && (!interactive_allowed || args.non_interactive) {
                    print_non_interactive_suggestions(&suggestions);
                    std::process::exit(2);
                }

                let chosen = if suggestions.len() == 1 {
                    suggestions.into_iter().next().unwrap()
                } else {
                    println!("複数の候補が見つかりました。追加する依存関係を選択してください:");
                    print_suggestions(&suggestions);
                    let index = prompt_user_selection(suggestions.len())?;
                    suggestions.into_iter().nth(index).unwrap()
                };

                (chosen.coordinate, None)
            }
        };

        let version = resolve_version_for_coordinate(
            &runtime,
            &cache,
            &manager,
            &coordinate,
            version_hint.as_deref(),
        )
        .with_context(|| format!("{} のバージョン決定に失敗しました", coordinate.display()))?;

        manifest
            .package
            .dependencies
            .insert(coordinate.display(), version.clone());

        additions.push((coordinate, version));
    }

    finalize_project_state(
        &manifest,
        &manifest_path,
        &project_root,
        resolver_options.clone(),
    )
    .context("依存関係追加後のプロジェクト更新に失敗しました")?;

    for (coordinate, version) in &additions {
        println!(
            "依存関係 '{}' をバージョン {} で追加しました。",
            coordinate.display(),
            version
        );
    }
    println!("更新: jv.toml / jv.lock");

    Ok(())
}

fn handle_remove_command(args: RemoveArgs, mode: CliMode) -> Result<()> {
    if mode.is_wrapper() {
        return handle_wrapper_remove_command(&args);
    }
    handle_native_remove_command(args)
}

fn handle_wrapper_remove_command(args: &RemoveArgs) -> Result<()> {
    let context = WrapperContext::detect().map_err(|error| anyhow!(error))?;
    let resolver_options = resolver_options_for_wrapper(args.strategy);
    let mut pipeline =
        WrapperPipeline::new(context, resolver_options).map_err(|error| anyhow!(error))?;
    let summary = pipeline.remove(args).map_err(|error| anyhow!(error))?;

    print_wrapper_remove_summary(&summary);
    Ok(())
}

fn handle_native_remove_command(args: RemoveArgs) -> Result<()> {
    let resolver_options = resolver_options_from_strategy(args.strategy);

    let (mut manifest, manifest_path) = load_manifest_with_path()?;
    let project_root = manifest_path
        .parent()
        .ok_or_else(|| anyhow!("jv.toml の親ディレクトリを特定できませんでした"))?
        .to_path_buf();

    if manifest.package.dependencies.is_empty() {
        return Err(anyhow!("jv.toml に管理対象の依存関係がありません。"));
    }

    let interactive_allowed =
        !args.non_interactive && io::stdin().is_terminal() && io::stdout().is_terminal();

    let mut removed = Vec::new();

    for raw in &args.packages {
        let input = parse_dependency_input(raw)?;
        let target = match input {
            ParsedDependencyInput::Qualified { coordinate, .. } => {
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

        if let Some(requirement) = manifest.package.dependencies.remove(&target) {
            removed.push((target, requirement));
        } else {
            return Err(anyhow!("依存関係 '{}' は jv.toml に存在しません。", target));
        }
    }

    finalize_project_state(&manifest, &manifest_path, &project_root, resolver_options)
        .context("依存関係削除後のプロジェクト更新に失敗しました")?;

    for (name, _) in &removed {
        println!("依存関係 '{}' を削除しました。", name);
    }
    println!("更新: jv.toml / jv.lock");

    Ok(())
}

fn print_wrapper_add_summary(summary: &WrapperUpdateSummary) {
    if summary.added.is_empty() {
        println!("依存関係の追加に変更はありませんでした。");
    } else {
        for coordinate in &summary.added {
            println!("依存関係 '{}' を追加しました。", coordinate);
        }
    }
    print_wrapper_file_updates(summary);
}

fn print_wrapper_remove_summary(summary: &WrapperUpdateSummary) {
    if summary.removed.is_empty() {
        println!("依存関係の削除に変更はありませんでした。");
    } else {
        for coordinate in &summary.removed {
            println!("依存関係 '{}' を削除しました。", coordinate);
        }
    }
    print_wrapper_file_updates(summary);
}

fn print_wrapper_file_updates(summary: &WrapperUpdateSummary) {
    let mut updates = Vec::new();
    if summary.pom_updated {
        updates.push("pom.xml");
    }
    if summary.settings_updated {
        updates.push("settings.xml");
    }
    if summary.lockfile_updated {
        updates.push("jv.lock");
    }

    if updates.is_empty() {
        println!("更新: ファイルに変更はありませんでした。");
    } else {
        println!("更新: {}", updates.join(" / "));
    }
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

fn parse_dependency_input(raw: &str) -> Result<ParsedDependencyInput> {
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

#[derive(Debug, Clone)]
struct PackageSuggestion {
    coordinate: DependencyCoordinate,
    latest_version: Option<String>,
    packaging: Option<String>,
}

fn search_package_candidates(
    runtime: &tokio::runtime::Runtime,
    client: &Client,
    query: &str,
    limit: usize,
) -> Result<Vec<PackageSuggestion>> {
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

fn prompt_user_selection(count: usize) -> Result<usize> {
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
) -> Result<String> {
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

fn finalize_project_state(
    manifest: &Manifest,
    manifest_path: &Path,
    project_root: &Path,
    options: ResolverOptions,
) -> Result<()> {
    save_manifest(manifest, manifest_path)?;

    let dispatcher = ResolverDispatcher::with_default_strategies();
    let mut manager = RepositoryManager::with_project_root(project_root.to_path_buf())
        .context("リポジトリマネージャーの初期化に失敗しました")?;
    manager.load_project_config(manifest);

    let local_repository = ensure_local_repository(project_root)?;
    let repositories = collect_maven_repositories(&manager);
    let mirrors = collect_effective_mirrors(manifest)?;

    let ctx = MavenResolverContext {
        project_root: project_root.to_path_buf(),
        local_repository: local_repository.clone(),
        repositories: repositories.clone(),
        mirrors: mirrors.clone(),
    };
    let options_with_context = options.clone().with_maven_context(ctx);

    let resolved = if manifest.package.dependencies.is_empty() {
        let requested = options
            .strategy
            .as_deref()
            .unwrap_or_else(|| dispatcher.default_strategy());
        let (strategy_label, algorithm) = dispatcher
            .strategy_info(requested)
            .map(|info| (info.name.clone(), info.algorithm))
            .unwrap_or_else(|| {
                (
                    dispatcher.default_strategy().to_string(),
                    ResolverAlgorithmKind::PubGrub,
                )
            });

        ResolvedDependencies {
            strategy: strategy_label,
            algorithm,
            dependencies: Vec::new(),
            diagnostics: Vec::new(),
            stats: ResolutionStats {
                elapsed_ms: 0,
                total_dependencies: 0,
                decided_dependencies: 0,
            },
        }
    } else {
        dispatcher
            .resolve_manifest(manifest, options_with_context.clone())
            .context("依存関係の解決に失敗しました")?
    };

    let lockfile =
        LockfileService::generate(manifest, &resolved).context("jv.lock の生成に失敗しました")?;
    let lockfile_path = project_root.join("jv.lock");
    LockfileService::save(&lockfile_path, &lockfile)
        .with_context(|| format!("{} への書き込みに失敗しました", lockfile_path.display()))?;

    let sources_dir = resolve_sources_dir(project_root, manifest);
    let output_dir = resolve_output_dir(project_root);

    let request = ExportRequest {
        project_root,
        manifest,
        lockfile: &lockfile,
        sources_dir: sources_dir.as_path(),
        output_dir: output_dir.as_path(),
        local_repository: local_repository.as_path(),
        repositories: &repositories,
        mirrors: &mirrors,
        resolved: Some(&resolved),
    };

    match JavaProjectExporter::export(&request) {
        Ok(summary) => {
            println!(
                "Javaプロジェクトを {} にエクスポートしました (更新ファイル: {})",
                summary.output_dir.display(),
                summary.updated_files
            );
        }
        Err(ExportError::MissingSources(path)) => {
            println!(
                "警告: Javaソースディレクトリ {} が見つからないため、OUTPUT_DIR へのエクスポートをスキップしました。",
                path.display()
            );
        }
        Err(error) => return Err(error.into()),
    }

    Ok(())
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

fn collect_effective_mirrors(manifest: &Manifest) -> Result<Vec<MavenMirrorConfig>> {
    let mut combined = Vec::new();
    combined.extend(manifest.mirrors.iter().cloned());

    let global_state = load_global_config_state()?;
    combined.extend(global_state.data.mirrors.into_iter());

    let mirrors = combined
        .into_iter()
        .enumerate()
        .map(|(index, mirror)| {
            let id = mirror
                .name
                .clone()
                .unwrap_or_else(|| format!("mirror-{}", index));
            MavenMirrorConfig::new(id, mirror.mirror_of, mirror.url)
        })
        .collect();

    Ok(mirrors)
}

fn resolve_sources_dir(project_root: &Path, manifest: &Manifest) -> PathBuf {
    let output = &manifest.project.output.directory;
    let base = if Path::new(output).is_absolute() {
        PathBuf::from(output)
    } else {
        project_root.join(output)
    };
    base.join(format!("java{}", manifest.java_target().as_str()))
}

fn resolve_output_dir(project_root: &Path) -> PathBuf {
    project_root.join("target").join("java-project")
}

fn ensure_local_repository(project_root: &Path) -> Result<PathBuf> {
    let path = project_root.join(".jv").join("repository");
    fs::create_dir_all(&path)
        .with_context(|| format!("{} の作成に失敗しました", path.display()))?;
    Ok(path)
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
) -> Result<String> {
    let mut candidates = gather_dependency_candidates(manifest, query);
    if candidates.is_empty() {
        return Err(anyhow!(
            "依存関係 '{}' は jv.toml に存在しません。",
            display_query
        ));
    }

    if candidates.len() == 1 {
        return Ok(candidates.remove(0).name);
    }

    if non_interactive || !interactive_allowed {
        print_remove_candidates(&candidates);
        std::process::exit(2);
    }

    println!("複数の依存関係が該当します。削除する項目を選択してください:");
    print_remove_candidates(&candidates);
    let index = prompt_user_selection(candidates.len())?;
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

fn handle_resolver_command(command: ResolverCommand) -> Result<()> {
    let dispatcher = ResolverDispatcher::with_default_strategies();
    match command {
        ResolverCommand::List { json } => {
            let strategies = dispatcher.list_strategies();
            if json {
                let payload = serde_json::to_string_pretty(&strategies)
                    .context("failed to serialize resolver metadata")?;
                println!("{payload}");
                return Ok(());
            }

            let table = render_strategy_table(&strategies);
            println!("{table}");
            Ok(())
        }
        ResolverCommand::Info { name, json } => {
            let info = dispatcher
                .strategy_info(&name)
                .ok_or_else(|| unknown_strategy_error(&name))?;
            if json {
                let payload = serde_json::to_string_pretty(&info)
                    .context("failed to serialize resolver metadata")?;
                println!("{payload}");
                return Ok(());
            }

            let block = render_strategy_details(&info);
            println!("{block}");
            Ok(())
        }
    }
}

fn handle_repo_command(command: RepoCommand) -> Result<()> {
    match command {
        RepoCommand::List {
            include_global,
            json,
        } => repo_list(include_global, json),
        RepoCommand::Show {
            name,
            include_global,
            json,
        } => repo_show(&name, include_global, json),
        RepoCommand::Add {
            name,
            url,
            priority,
            scope,
            auth,
            username_env,
            password_env,
            token_env,
            include_groups,
            exclude_groups,
        } => repo_add(AddOptions {
            name,
            url,
            priority,
            scope,
            auth,
            username_env,
            password_env,
            token_env,
            include_groups,
            exclude_groups,
        }),
        RepoCommand::Remove { name, scope } => repo_remove(RemoveOptions { name, scope }),
        RepoCommand::Mirror {
            target,
            url,
            display_name,
            remove,
            json,
            scope,
        } => repo_mirror(MirrorOptions {
            target,
            url,
            display_name,
            remove,
            json,
            scope,
        }),
    }
}

fn repo_list(include_global: bool, json: bool) -> Result<()> {
    let context = load_repository_context(include_global)?;
    let entries = context
        .entries
        .iter()
        .map(|entry| RepositoryListEntry {
            name: entry.config.name.clone(),
            scope: entry.origin,
            priority: entry.config.priority,
            configured_url: entry.config.url.clone(),
            effective_url: entry.effective_url.clone(),
            mirrored: entry.effective_url != entry.config.url,
            auth: entry.config.auth.as_ref().and_then(summarise_auth),
            filter: entry.config.filter.as_ref().and_then(summarise_filter),
        })
        .collect::<Vec<_>>();

    if json {
        let payload = serde_json::to_string_pretty(&entries)
            .context("リポジトリ一覧のシリアライズに失敗しました")?;
        println!("{payload}");
    } else if entries.is_empty() {
        println!("登録されているリポジトリはありません。");
    } else {
        let table = render_repository_table(&entries);
        println!("{table}");
    }

    if include_global && !context.manifest.repositories.use_global {
        println!("(参考) --include-global 指定によりグローバル設定を含めて表示しました。");
    } else if !include_global && !context.manifest.repositories.use_global {
        println!("グローバルリポジトリは無効です (jv.toml の [repositories] use-global = false)。");
    }

    Ok(())
}

fn repo_show(name: &str, include_global: bool, json: bool) -> Result<()> {
    let context = load_repository_context(include_global)?;
    let entry = context
        .entries
        .iter()
        .find(|entry| entry.config.name == name)
        .ok_or_else(|| {
            anyhow!(
                "リポジトリ '{}' は見つかりません。--include-global を指定するとグローバル設定も検索します。",
                name
            )
        })?;

    let auth = entry.config.auth.as_ref().and_then(summarise_auth);
    let filter = entry.config.filter.as_ref().and_then(summarise_filter);
    let mirror_detail = if entry.effective_url != entry.config.url {
        find_mirror_for(
            &entry.config.name,
            &context.manifest.mirrors,
            &effective_global_mirrors(&context.global.data),
        )
    } else {
        None
    };

    if json {
        let detail = RepositoryDetail {
            name: entry.config.name.clone(),
            scope: entry.origin,
            priority: entry.config.priority,
            configured_url: entry.config.url.clone(),
            effective_url: entry.effective_url.clone(),
            mirrored: mirror_detail.clone(),
            auth,
            filter,
        };
        let payload = serde_json::to_string_pretty(&detail)
            .context("リポジトリ詳細のシリアライズに失敗しました")?;
        println!("{payload}");
        return Ok(());
    }

    println!("名称        : {}", entry.config.name);
    println!("スコープ    : {}", entry.origin.label());
    println!("優先度      : {}", entry.config.priority);
    println!("定義URL     : {}", entry.config.url);
    println!("実効URL     : {}", entry.effective_url);

    if let Some(mirror) = mirror_detail {
        println!(
            "ミラー      : {} (mirror-of='{}') => {}",
            mirror.scope.label(),
            mirror.mirror_of,
            mirror.url
        );
        if let Some(name) = mirror.name.as_deref() {
            println!("ミラー名称  : {name}");
        }
    } else {
        println!("ミラー      : なし");
    }

    if let Some(auth) = auth {
        println!("認証方式    : {}", auth_label(auth.mode));
        if let Some(user) = auth.username_env.as_deref() {
            println!("  ユーザー名環境変数 : {user}");
        }
        if let Some(pass) = auth.password_env.as_deref() {
            println!("  パスワード環境変数 : {pass}");
        }
        if let Some(token) = auth.token_env.as_deref() {
            println!("  トークン環境変数   : {token}");
        }
    } else {
        println!("認証方式    : なし");
    }

    if let Some(filter) = filter {
        if filter.include_groups.is_empty() && filter.exclude_groups.is_empty() {
            println!("グループフィルタ: 指定なし");
        } else {
            if !filter.include_groups.is_empty() {
                println!("  include-groups : {}", filter.include_groups.join(", "));
            }
            if !filter.exclude_groups.is_empty() {
                println!("  exclude-groups : {}", filter.exclude_groups.join(", "));
            }
        }
    } else {
        println!("グループフィルタ: 指定なし");
    }

    Ok(())
}

fn repo_add(options: AddOptions) -> Result<()> {
    match options.scope {
        RepoScope::Project => add_project_repository(options),
        RepoScope::Global => add_global_repository(options),
    }
}

fn add_project_repository(options: AddOptions) -> Result<()> {
    let (mut manifest, manifest_path) = load_manifest_with_path()?;
    let new_config = build_repository_config(&options)?;
    let name = new_config.name.clone();

    let mut replaced = false;
    if let Some(existing) = manifest
        .repositories
        .entries
        .iter_mut()
        .find(|repo| repo.name == new_config.name)
    {
        *existing = new_config;
        replaced = true;
    } else {
        manifest.repositories.entries.push(new_config);
    }

    sort_repositories(&mut manifest.repositories.entries);
    save_manifest(&manifest, &manifest_path)?;

    if replaced {
        println!("プロジェクトリポジトリ '{}' を更新しました。", name);
    } else {
        println!("プロジェクトリポジトリ '{}' を追加しました。", name);
    }

    Ok(())
}

fn add_global_repository(options: AddOptions) -> Result<()> {
    let mut global = load_global_config_state()?;
    if global.data.repositories.is_empty() {
        global.data.repositories = repository::builtin_global_repositories();
    }

    let new_config = build_repository_config(&options)?;
    let name = new_config.name.clone();

    let mut replaced = false;
    if let Some(existing) = global
        .data
        .repositories
        .iter_mut()
        .find(|repo| repo.name == new_config.name)
    {
        *existing = new_config;
        replaced = true;
    } else {
        global.data.repositories.push(new_config);
    }

    sort_repositories(&mut global.data.repositories);
    save_global_config_state(&global)?;

    if replaced {
        println!("グローバルリポジトリ '{}' を更新しました。", name);
    } else {
        println!("グローバルリポジトリ '{}' を追加しました。", name);
    }

    Ok(())
}

fn repo_remove(options: RemoveOptions) -> Result<()> {
    match options.scope {
        RepoScope::Project => remove_project_repository(options),
        RepoScope::Global => remove_global_repository(options),
    }
}

fn remove_project_repository(options: RemoveOptions) -> Result<()> {
    if options.name == "local" {
        return Err(anyhow!("ローカルリポジトリ 'local' は削除できません。"));
    }

    let (mut manifest, manifest_path) = load_manifest_with_path()?;
    let before = manifest.repositories.entries.len();
    manifest
        .repositories
        .entries
        .retain(|repo| repo.name != options.name);

    if manifest.repositories.entries.len() == before {
        return Err(anyhow!(
            "プロジェクトリポジトリ '{}' は存在しません。",
            options.name
        ));
    }

    save_manifest(&manifest, &manifest_path)?;
    println!("プロジェクトリポジトリ '{}' を削除しました。", options.name);
    Ok(())
}

fn remove_global_repository(options: RemoveOptions) -> Result<()> {
    let mut global = load_global_config_state()?;
    if global.data.repositories.is_empty() {
        global.data.repositories = repository::builtin_global_repositories();
    }

    let before = global.data.repositories.len();
    global
        .data
        .repositories
        .retain(|repo| repo.name != options.name);

    if global.data.repositories.len() == before {
        return Err(anyhow!(
            "グローバルリポジトリ '{}' は存在しません。",
            options.name
        ));
    }

    save_global_config_state(&global)?;
    println!("グローバルリポジトリ '{}' を削除しました。", options.name);
    Ok(())
}

fn repo_mirror(options: MirrorOptions) -> Result<()> {
    match options.scope {
        RepoScope::Project => {
            let (mut manifest, manifest_path) = load_manifest_with_path()?;
            let result = process_mirror_options(options, &mut manifest.mirrors)?;
            if matches!(result, MirrorCommandResult::Modified) {
                sort_mirrors(&mut manifest.mirrors);
                save_manifest(&manifest, &manifest_path)?;
            }
            Ok(())
        }
        RepoScope::Global => {
            let mut global = load_global_config_state()?;
            let result = process_mirror_options(options, &mut global.data.mirrors)?;
            if matches!(result, MirrorCommandResult::Modified) {
                sort_mirrors(&mut global.data.mirrors);
                save_global_config_state(&global)?;
            }
            Ok(())
        }
    }
}

fn process_mirror_options(
    options: MirrorOptions,
    mirrors: &mut Vec<MirrorConfig>,
) -> Result<MirrorCommandResult> {
    let MirrorOptions {
        target,
        url,
        display_name,
        remove,
        json,
        scope,
    } = options;

    if remove {
        let target = target.ok_or_else(|| {
            anyhow!("--remove を指定する場合は mirror-of の対象名を指定してください")
        })?;
        if url.is_some() {
            return Err(anyhow!("--remove とURL指定は同時に利用できません"));
        }

        let before = mirrors.len();
        mirrors.retain(|mirror| mirror.mirror_of != target);
        if mirrors.len() == before {
            return Err(anyhow!(format!(
                "{} スコープに mirror-of='{}' の設定は存在しません。",
                scope.label(),
                target
            )));
        }

        println!(
            "{} スコープのミラー '{}' を削除しました。",
            scope.label(),
            target
        );
        return Ok(MirrorCommandResult::Modified);
    }

    if let Some(url) = url {
        let target = target
            .ok_or_else(|| anyhow!("ミラーを設定するには mirror-of の対象名を指定してください"))?;

        let new_config = MirrorConfig {
            name: display_name,
            mirror_of: target.clone(),
            url,
        };

        let mut replaced = false;
        if let Some(existing) = mirrors.iter_mut().find(|mirror| mirror.mirror_of == target) {
            *existing = new_config;
            replaced = true;
        } else {
            mirrors.push(new_config);
        }

        if replaced {
            println!(
                "{} スコープのミラー '{}' を更新しました。",
                scope.label(),
                target
            );
        } else {
            println!(
                "{} スコープのミラー '{}' を追加しました。",
                scope.label(),
                target
            );
        }

        return Ok(MirrorCommandResult::Modified);
    }

    if let Some(target) = target {
        let detail = mirrors
            .iter()
            .find(|mirror| mirror.mirror_of == target)
            .map(|mirror| MirrorDetail {
                scope,
                mirror_of: mirror.mirror_of.clone(),
                url: mirror.url.clone(),
                name: mirror.name.clone(),
            })
            .ok_or_else(|| {
                anyhow!(
                    "{} スコープに mirror-of='{}' の設定は存在しません。",
                    scope.label(),
                    target
                )
            })?;

        if json {
            let payload = serde_json::to_string_pretty(&detail)
                .context("ミラー詳細のシリアライズに失敗しました")?;
            println!("{payload}");
        } else {
            println!("スコープ  : {}", detail.scope.label());
            println!("mirror-of : {}", detail.mirror_of);
            println!("URL       : {}", detail.url);
            println!("名称      : {}", detail.name.as_deref().unwrap_or("-"));
        }

        return Ok(MirrorCommandResult::Unchanged);
    }

    let list = mirrors
        .iter()
        .map(|mirror| MirrorDetail {
            scope,
            mirror_of: mirror.mirror_of.clone(),
            url: mirror.url.clone(),
            name: mirror.name.clone(),
        })
        .collect::<Vec<_>>();

    if json {
        let payload = serde_json::to_string_pretty(&list)
            .context("ミラー一覧のシリアライズに失敗しました")?;
        println!("{payload}");
    } else if list.is_empty() {
        println!("{} スコープのミラーは定義されていません。", scope.label());
    } else {
        let table = render_mirror_table(&list);
        println!("{table}");
    }

    Ok(MirrorCommandResult::Unchanged)
}

fn load_repository_context(include_global: bool) -> Result<RepositoryContext> {
    let (manifest, manifest_path) = load_manifest_with_path()?;
    let project_root = manifest_path
        .parent()
        .ok_or_else(|| anyhow!("jv.toml の親ディレクトリを特定できませんでした"))?
        .to_path_buf();

    let include_global_effective = include_global || manifest.repositories.use_global;
    let mut listing_manifest = manifest.clone();
    listing_manifest.repositories.use_global = include_global_effective;

    let mut manager = RepositoryManager::with_project_root(project_root.clone())
        .context("リポジトリマネージャーの初期化に失敗しました")?;
    manager.load_project_config(&listing_manifest);

    let global = load_global_config_state()?;
    let project_names = manifest
        .repositories
        .entries
        .iter()
        .map(|repo| repo.name.clone())
        .collect::<HashSet<_>>();
    let global_names = effective_global_repositories(&global.data)
        .into_iter()
        .map(|repo| repo.name)
        .collect::<HashSet<_>>();

    let entries = manager
        .list()
        .into_iter()
        .map(|handle| {
            let config = handle.config().clone();
            let origin = if config.name == "local" {
                RepoOrigin::Local
            } else if project_names.contains(&config.name) {
                RepoOrigin::Project
            } else if global_names.contains(&config.name) {
                RepoOrigin::Global
            } else if include_global_effective {
                RepoOrigin::Global
            } else {
                RepoOrigin::Project
            };

            RepositoryEntry {
                origin,
                config,
                effective_url: handle.url().to_string(),
            }
        })
        .collect::<Vec<_>>();

    Ok(RepositoryContext {
        manifest,
        global,
        entries,
    })
}

fn load_manifest_with_path() -> Result<(Manifest, PathBuf)> {
    let path = locate_manifest_file()?;
    let manifest = Manifest::load_from_path(&path)
        .with_context(|| format!("{} の読み込みに失敗しました", path.display()))?;
    Ok((manifest, path))
}

fn locate_manifest_file() -> Result<PathBuf> {
    let mut dir = env::current_dir().context("カレントディレクトリを取得できませんでした")?;
    loop {
        let candidate = dir.join("jv.toml");
        if candidate.exists() {
            return Ok(candidate);
        }

        if !dir.pop() {
            break;
        }
    }

    Err(anyhow!(
        "jv.toml が見つかりません。プロジェクトルートで実行してください。"
    ))
}

fn save_manifest(manifest: &Manifest, path: &Path) -> Result<()> {
    let toml = toml::to_string_pretty(manifest).context("jv.toml のシリアライズに失敗しました")?;
    fs::write(path, toml).with_context(|| format!("{} への書き込みに失敗しました", path.display()))
}

fn load_global_config_state() -> Result<GlobalConfigState> {
    let path = dirs::home_dir()
        .ok_or_else(|| anyhow!("ホームディレクトリを特定できませんでした"))?
        .join(".jv")
        .join("config.toml");

    if path.exists() {
        let content = fs::read_to_string(&path)
            .with_context(|| format!("{} の読み込みに失敗しました", path.display()))?;
        let data = toml::from_str(&content)
            .with_context(|| format!("{} の解析に失敗しました", path.display()))?;
        Ok(GlobalConfigState { path, data })
    } else {
        Ok(GlobalConfigState {
            path,
            data: StoredGlobalConfig::default(),
        })
    }
}

fn save_global_config_state(state: &GlobalConfigState) -> Result<()> {
    if let Some(parent) = state.path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("{} のディレクトリ作成に失敗しました", parent.display()))?;
    }

    let toml = toml::to_string_pretty(&state.data)
        .context("グローバル設定のシリアライズに失敗しました")?;
    fs::write(&state.path, toml)
        .with_context(|| format!("{} への書き込みに失敗しました", state.path.display()))
}

fn effective_global_repositories(config: &StoredGlobalConfig) -> Vec<RepositoryConfig> {
    if config.repositories.is_empty() {
        repository::builtin_global_repositories()
    } else {
        config.repositories.clone()
    }
}

fn effective_global_mirrors(config: &StoredGlobalConfig) -> Vec<MirrorConfig> {
    config.mirrors.clone()
}

fn build_repository_config(options: &AddOptions) -> Result<RepositoryConfig> {
    let priority = options.priority.unwrap_or(100);
    let auth = build_auth_config(
        options.auth,
        options.username_env.as_deref(),
        options.password_env.as_deref(),
        options.token_env.as_deref(),
    )?;
    let filter = build_filter_config(&options.include_groups, &options.exclude_groups);

    Ok(RepositoryConfig {
        name: options.name.clone(),
        url: options.url.clone(),
        priority,
        auth,
        filter,
    })
}

fn build_filter_config(
    include_groups: &[String],
    exclude_groups: &[String],
) -> Option<FilterConfig> {
    if include_groups.is_empty() && exclude_groups.is_empty() {
        return None;
    }

    let include = if include_groups.is_empty() {
        None
    } else {
        Some(include_groups.to_vec())
    };
    let exclude = if exclude_groups.is_empty() {
        None
    } else {
        Some(exclude_groups.to_vec())
    };

    Some(FilterConfig {
        include_groups: include,
        exclude_groups: exclude,
    })
}

fn build_auth_config(
    kind: RepoAuthKind,
    username_env: Option<&str>,
    password_env: Option<&str>,
    token_env: Option<&str>,
) -> Result<Option<AuthConfig>> {
    match kind {
        RepoAuthKind::None => {
            if username_env.is_some() || password_env.is_some() || token_env.is_some() {
                return Err(anyhow!(
                    "--auth none の場合は認証関連オプションを指定できません"
                ));
            }
            Ok(None)
        }
        RepoAuthKind::Basic => {
            if token_env.is_some() {
                return Err(anyhow!("--auth basic では --token-env を指定できません"));
            }
            let username = username_env.ok_or_else(|| {
                anyhow!("--auth basic を使用する場合は --username-env を指定してください")
            })?;
            let password = password_env.ok_or_else(|| {
                anyhow!("--auth basic を使用する場合は --password-env を指定してください")
            })?;
            Ok(Some(AuthConfig {
                auth_type: AuthType::Basic,
                username_env: Some(username.to_string()),
                password_env: Some(password.to_string()),
                token_env: None,
            }))
        }
        RepoAuthKind::Token => {
            if username_env.is_some() || password_env.is_some() {
                return Err(anyhow!(
                    "--auth token では --username-env / --password-env を指定できません"
                ));
            }
            let token = token_env.ok_or_else(|| {
                anyhow!("--auth token を使用する場合は --token-env を指定してください")
            })?;
            Ok(Some(AuthConfig {
                auth_type: AuthType::Token,
                username_env: None,
                password_env: None,
                token_env: Some(token.to_string()),
            }))
        }
    }
}

fn summarise_auth(auth: &AuthConfig) -> Option<AuthSummary> {
    if auth.is_none() {
        return None;
    }

    Some(AuthSummary {
        mode: RepoAuthKind::from(auth.auth_type),
        username_env: auth.username_env.clone(),
        password_env: auth.password_env.clone(),
        token_env: auth.token_env.clone(),
    })
}

fn summarise_filter(filter: &FilterConfig) -> Option<FilterSummary> {
    let mut include = filter.include_groups.clone().unwrap_or_default();
    let mut exclude = filter.exclude_groups.clone().unwrap_or_default();

    if include.is_empty() && exclude.is_empty() {
        return None;
    }

    include.sort();
    exclude.sort();

    Some(FilterSummary {
        include_groups: include,
        exclude_groups: exclude,
    })
}

fn sort_repositories(repos: &mut Vec<RepositoryConfig>) {
    repos.sort_by(|left, right| {
        left.priority
            .cmp(&right.priority)
            .then_with(|| left.name.cmp(&right.name))
    });
    repos.dedup_by(|left, right| left.name == right.name);
}

fn sort_mirrors(mirrors: &mut Vec<MirrorConfig>) {
    mirrors.sort_by(|left, right| {
        left.mirror_of
            .cmp(&right.mirror_of)
            .then_with(|| left.name.cmp(&right.name))
    });
    mirrors.dedup_by(|left, right| left.mirror_of == right.mirror_of);
}

fn render_repository_table(entries: &[RepositoryListEntry]) -> String {
    let headers = ["範囲", "名称", "優先度", "URL", "ミラー"];
    let mut widths = headers.iter().map(|h| h.len()).collect::<Vec<_>>();
    let rows = entries
        .iter()
        .map(|entry| {
            vec![
                entry.scope.label().to_string(),
                entry.name.clone(),
                entry.priority.to_string(),
                entry.effective_url.clone(),
                if entry.mirrored {
                    "あり".to_string()
                } else {
                    "なし".to_string()
                },
            ]
        })
        .collect::<Vec<_>>();

    for row in &rows {
        for (idx, cell) in row.iter().enumerate() {
            widths[idx] = widths[idx].max(cell.len());
        }
    }

    let mut buffer = String::new();
    append_row(&mut buffer, &headers, &widths);
    append_separator(&mut buffer, &widths);

    for row in rows {
        let refs = row.iter().map(|cell| cell.as_str()).collect::<Vec<_>>();
        append_row(&mut buffer, &refs, &widths);
    }

    buffer
}

fn render_mirror_table(entries: &[MirrorDetail]) -> String {
    let headers = ["スコープ", "mirror-of", "URL", "名称"];
    let mut widths = headers.iter().map(|h| h.len()).collect::<Vec<_>>();
    let rows = entries
        .iter()
        .map(|detail| {
            vec![
                detail.scope.label().to_string(),
                detail.mirror_of.clone(),
                detail.url.clone(),
                detail.name.clone().unwrap_or_else(|| "-".to_string()),
            ]
        })
        .collect::<Vec<_>>();

    for row in &rows {
        for (idx, cell) in row.iter().enumerate() {
            widths[idx] = widths[idx].max(cell.len());
        }
    }

    let mut buffer = String::new();
    append_row(&mut buffer, &headers, &widths);
    append_separator(&mut buffer, &widths);

    for row in rows {
        let refs = row.iter().map(|cell| cell.as_str()).collect::<Vec<_>>();
        append_row(&mut buffer, &refs, &widths);
    }

    buffer
}

fn find_mirror_for(
    repo_name: &str,
    project_mirrors: &[MirrorConfig],
    global_mirrors: &[MirrorConfig],
) -> Option<MirrorDetail> {
    for mirror in project_mirrors {
        if matches_pattern(repo_name, &mirror.mirror_of) {
            return Some(MirrorDetail {
                scope: RepoScope::Project,
                mirror_of: mirror.mirror_of.clone(),
                url: mirror.url.clone(),
                name: mirror.name.clone(),
            });
        }
    }

    for mirror in global_mirrors {
        if matches_pattern(repo_name, &mirror.mirror_of) {
            return Some(MirrorDetail {
                scope: RepoScope::Global,
                mirror_of: mirror.mirror_of.clone(),
                url: mirror.url.clone(),
                name: mirror.name.clone(),
            });
        }
    }

    None
}

fn matches_pattern(text: &str, pattern: &str) -> bool {
    if pattern == "*" {
        return true;
    }

    let mut remaining = text;
    let mut parts = pattern.split('*').peekable();

    if let Some(first) = parts.peek().copied() {
        if !pattern.starts_with('*') {
            if !remaining.starts_with(first) {
                return false;
            }
            remaining = &remaining[first.len()..];
            parts.next();
        }
    }

    while let Some(part) = parts.next() {
        if parts.peek().is_none() && !pattern.ends_with('*') {
            return remaining.ends_with(part);
        }

        if part.is_empty() {
            continue;
        }

        if let Some(index) = remaining.find(part) {
            remaining = &remaining[index + part.len()..];
        } else {
            return false;
        }
    }

    pattern.ends_with('*') || remaining.is_empty()
}

fn auth_label(kind: RepoAuthKind) -> &'static str {
    match kind {
        RepoAuthKind::None => "なし",
        RepoAuthKind::Basic => "BASIC",
        RepoAuthKind::Token => "トークン",
    }
}

fn unknown_strategy_error(requested: &str) -> anyhow::Error {
    let dispatcher = ResolverDispatcher::with_default_strategies();
    let available = dispatcher
        .list_strategies()
        .into_iter()
        .map(|info| info.name)
        .collect::<Vec<_>>()
        .join(", ");
    anyhow!(
        "Unknown resolver strategy '{requested}'. Available: {available}. Run `jv resolver list` for details."
    )
}

fn render_strategy_table(strategies: &[ResolverStrategyInfo]) -> String {
    let headers = [
        "strategy",
        "display",
        "algorithm",
        "stability",
        "default",
        "deterministic",
        "offline",
        "conflicts",
        "policy",
    ];
    let mut widths = headers.iter().map(|h| h.len()).collect::<Vec<_>>();
    let rows = strategies
        .iter()
        .map(|info| {
            vec![
                info.name.clone(),
                info.display_name.clone(),
                format_algorithm(info.algorithm).to_string(),
                format_stability(info.stability).to_string(),
                yes_no(info.is_default),
                yes_no(info.deterministic),
                yes_no(info.supports_offline),
                yes_no(info.emits_conflict_reasons),
                info.conflict_policy.clone(),
            ]
        })
        .collect::<Vec<_>>();

    for row in &rows {
        for (idx, cell) in row.iter().enumerate() {
            widths[idx] = widths[idx].max(cell.len());
        }
    }

    let mut buffer = String::new();
    append_row(&mut buffer, &headers, &widths);
    append_separator(&mut buffer, &widths);
    for row in rows {
        let refs = row.iter().map(|cell| cell.as_str()).collect::<Vec<_>>();
        append_row(&mut buffer, &refs, &widths);
    }

    buffer
}

fn render_strategy_details(info: &ResolverStrategyInfo) -> String {
    let mut buffer = String::new();
    writeln!(
        &mut buffer,
        "Strategy     : {} ({})",
        info.name, info.display_name
    )
    .unwrap();
    writeln!(
        &mut buffer,
        "Algorithm    : {}",
        format_algorithm(info.algorithm)
    )
    .unwrap();
    writeln!(
        &mut buffer,
        "Stability    : {}",
        format_stability(info.stability)
    )
    .unwrap();
    writeln!(&mut buffer, "Default      : {}", yes_no(info.is_default)).unwrap();
    writeln!(&mut buffer, "Deterministic: {}", yes_no(info.deterministic)).unwrap();
    writeln!(
        &mut buffer,
        "Offline      : {}",
        yes_no(info.supports_offline)
    )
    .unwrap();
    writeln!(
        &mut buffer,
        "Conflicts    : {}",
        yes_no(info.emits_conflict_reasons)
    )
    .unwrap();
    writeln!(&mut buffer, "Policy       : {}", info.conflict_policy).unwrap();

    if info.aliases.is_empty() {
        writeln!(&mut buffer, "Aliases      : -").unwrap();
    } else {
        writeln!(&mut buffer, "Aliases      : {}", info.aliases.join(", ")).unwrap();
    }

    buffer.push_str("Description:\n");
    for line in info.description.lines() {
        writeln!(&mut buffer, "  {line}").unwrap();
    }

    buffer
}

fn append_row(buffer: &mut String, cells: &[&str], widths: &[usize]) {
    for (idx, cell) in cells.iter().enumerate() {
        if idx > 0 {
            buffer.push_str("  ");
        }
        let width = widths[idx];
        let formatted = format!("{cell:<width$}");
        buffer.push_str(&formatted);
    }
    buffer.push('\n');
}

fn append_separator(buffer: &mut String, widths: &[usize]) {
    for (idx, width) in widths.iter().enumerate() {
        if idx > 0 {
            buffer.push_str("  ");
        }
        buffer.push_str(&"-".repeat(*width));
    }
    buffer.push('\n');
}

fn format_algorithm(kind: ResolverAlgorithmKind) -> &'static str {
    match kind {
        ResolverAlgorithmKind::PubGrub => "pubgrub",
        ResolverAlgorithmKind::BreadthFirst => "breadth-first",
        ResolverAlgorithmKind::MavenCompat => "maven-compat",
    }
}

fn format_stability(stability: StrategyStability) -> &'static str {
    match stability {
        StrategyStability::Experimental => "experimental",
        StrategyStability::Stable => "stable",
    }
}

fn yes_no(value: bool) -> String {
    if value {
        "yes".to_string()
    } else {
        "no".to_string()
    }
}

fn resolve_maven_binary() -> Result<PathBuf> {
    if let Some(explicit) = env::var_os("JVPM_MAVEN_BIN") {
        let candidate = PathBuf::from(&explicit);
        if is_executable(&candidate) {
            return Ok(candidate);
        }
        return Err(anyhow!(
            "環境変数 JVPM_MAVEN_BIN で指定された Maven バイナリ '{}' が見つかりません。",
            candidate.display()
        ));
    }

    if let Some(candidate) = resolve_maven_from_env_homes() {
        return Ok(candidate);
    }

    for start in candidate_roots() {
        if let Some(candidate) = locate_maven_in_toolchains(&start) {
            return Ok(candidate);
        }
    }

    if let Some(candidate) = locate_maven_in_path() {
        return Ok(candidate);
    }

    Err(anyhow!(
        "Mavenコマンド (mvn) を検出できませんでした。toolchains/maven/bin/mvn を利用可能にするか、JVPM_MAVEN_BIN / MVN_HOME を設定してください。"
    ))
}

fn resolve_maven_from_env_homes() -> Option<PathBuf> {
    for var in ["MVN_HOME", "MAVEN_HOME", "M2_HOME"] {
        if let Some(home) = env::var_os(var) {
            let base = PathBuf::from(&home).join("bin");
            for name in maven_candidates() {
                let candidate = base.join(name);
                if is_executable(&candidate) {
                    return Some(candidate);
                }
            }
        }
    }
    None
}

fn candidate_roots() -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Ok(dir) = env::current_dir() {
        roots.push(dir);
    }
    if let Ok(exe) = env::current_exe() {
        if let Some(parent) = exe.parent() {
            roots.push(parent.to_path_buf());
        }
    }
    roots.push(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
    roots
}

fn locate_maven_in_toolchains(start: &Path) -> Option<PathBuf> {
    for ancestor in start.ancestors() {
        let toolchains = ancestor.join("toolchains").join("maven").join("bin");
        for name in maven_candidates() {
            let candidate = toolchains.join(name);
            if is_executable(&candidate) {
                return Some(candidate);
            }
        }
    }
    None
}

fn locate_maven_in_path() -> Option<PathBuf> {
    let path_value = env::var_os("PATH")?;
    for entry in env::split_paths(&path_value) {
        for name in maven_candidates() {
            let candidate = entry.join(name);
            if is_executable(&candidate) {
                return Some(candidate);
            }
        }
    }
    None
}

fn maven_candidates() -> &'static [&'static str] {
    #[cfg(windows)]
    {
        &["mvn.cmd", "mvn.bat", "mvn.exe"]
    }
    #[cfg(not(windows))]
    {
        &["mvn"]
    }
}

fn is_executable(path: &Path) -> bool {
    path.is_file()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table_renders_all_rows() {
        let dispatcher = ResolverDispatcher::with_default_strategies();
        let strategies = dispatcher.list_strategies();
        let table = render_strategy_table(&strategies);
        for info in strategies {
            assert!(table.contains(&info.name));
            assert!(table.contains(&info.display_name));
        }
    }

    #[test]
    fn details_include_aliases_and_description() {
        let dispatcher = ResolverDispatcher::with_default_strategies();
        let info = dispatcher
            .strategy_info(dispatcher.default_strategy())
            .expect("default strategy should exist");
        let block = render_strategy_details(&info);
        assert!(block.contains("Strategy     :"));
        assert!(block.contains("Description:"));
    }
}
