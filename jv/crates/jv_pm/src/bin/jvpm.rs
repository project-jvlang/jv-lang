use std::collections::HashSet;
use std::env;
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow};
use clap::{Args, Parser, Subcommand, ValueEnum};
use jv_pm::{
    AuthConfig, AuthType, ExportRequest, FilterConfig, JavaProjectExporter, LockfileService,
    Manifest, MavenMirrorConfig, MavenRepositoryConfig, MirrorConfig, RepositoryConfig,
    RepositoryManager, ResolverAlgorithmKind, ResolverDispatcher, ResolverStrategyInfo,
    StrategyStability,
};
use serde::{Deserialize, Serialize};

#[derive(Parser, Debug)]
#[command(name = "jvpm")]
#[command(about = "jv package manager helper", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Inspect resolver strategies
    #[command(subcommand)]
    Resolver(ResolverCommand),
    /// Manage repository definitions and mirrors
    #[command(subcommand)]
    Repo(RepoCommand),
    /// 完全なJavaプロジェクトをエクスポートする
    Export(ExportArgs),
}

#[derive(Subcommand, Debug)]
enum ResolverCommand {
    /// List every registered resolver strategy
    List {
        /// Emit machine-readable JSON instead of a human table
        #[arg(long)]
        json: bool,
    },
    /// Show extended metadata for a single strategy
    Info {
        /// Strategy name or alias (see `jv resolver list`)
        name: String,
        /// Emit machine-readable JSON instead of a textual block
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand, Debug)]
enum RepoCommand {
    /// 現在有効なリポジトリ一覧を表示する
    List {
        /// グローバル設定に登録済みのリポジトリも含める
        #[arg(long = "include-global")]
        include_global: bool,
        /// JSON形式で出力する
        #[arg(long)]
        json: bool,
    },
    /// 指定したリポジトリの詳細を表示する
    Show {
        /// 対象となるリポジトリ名
        name: String,
        /// グローバル設定の一覧も検索対象に含める
        #[arg(long = "include-global")]
        include_global: bool,
        /// JSON形式で出力する
        #[arg(long)]
        json: bool,
    },
    /// 新しいリポジトリ定義を追加する
    Add {
        /// 追加するリポジトリ名
        name: String,
        /// 追加するリポジトリURL
        url: String,
        /// 優先度（小さいほど優先）
        #[arg(long)]
        priority: Option<u32>,
        /// 書き込み先のスコープ
        #[arg(long = "scope", value_enum, default_value_t = RepoScope::Project)]
        scope: RepoScope,
        /// 認証方式
        #[arg(long, value_enum, default_value_t = RepoAuthKind::None)]
        auth: RepoAuthKind,
        /// BASIC認証のユーザー名環境変数
        #[arg(long = "username-env")]
        username_env: Option<String>,
        /// BASIC認証のパスワード環境変数
        #[arg(long = "password-env")]
        password_env: Option<String>,
        /// トークン認証の環境変数
        #[arg(long = "token-env")]
        token_env: Option<String>,
        /// include-groups フィルタ
        #[arg(long = "include-group")]
        include_groups: Vec<String>,
        /// exclude-groups フィルタ
        #[arg(long = "exclude-group")]
        exclude_groups: Vec<String>,
    },
    /// 既存リポジトリ定義を削除する
    Remove {
        /// 削除するリポジトリ名
        name: String,
        /// 削除対象のスコープ
        #[arg(long = "scope", value_enum, default_value_t = RepoScope::Project)]
        scope: RepoScope,
    },
    /// ミラー設定を表示・編集する
    Mirror {
        /// mirror-of対象名
        target: Option<String>,
        /// ミラーURL（設定/更新時）
        url: Option<String>,
        /// 表示名（省略可）
        #[arg(long = "name")]
        display_name: Option<String>,
        /// 指定時は該当設定を削除する
        #[arg(long)]
        remove: bool,
        /// JSON形式で出力する
        #[arg(long)]
        json: bool,
        /// 操作対象スコープ
        #[arg(long = "scope", value_enum, default_value_t = RepoScope::Project)]
        scope: RepoScope,
    },
}

#[derive(Debug, Clone, Args)]
struct ExportArgs {
    /// エクスポート先ディレクトリ（未指定時は target/java-project）
    #[arg(long = "output-dir", value_name = "DIRECTORY")]
    output_dir: Option<PathBuf>,
    /// 生成済みJavaソースのディレクトリ（未指定時は manifest から推測）
    #[arg(long = "sources-dir", value_name = "DIRECTORY")]
    sources_dir: Option<PathBuf>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
enum RepoScope {
    Project,
    Global,
}

impl RepoScope {
    fn label(self) -> &'static str {
        match self {
            RepoScope::Project => "プロジェクト",
            RepoScope::Global => "グローバル",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
enum RepoAuthKind {
    None,
    Basic,
    Token,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RepoOrigin {
    Local,
    Project,
    Global,
}

impl RepoOrigin {
    fn label(self) -> &'static str {
        match self {
            RepoOrigin::Local => "ローカル",
            RepoOrigin::Project => "プロジェクト",
            RepoOrigin::Global => "グローバル",
        }
    }
}

impl Serialize for RepoOrigin {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(match self {
            RepoOrigin::Local => "local",
            RepoOrigin::Project => "project",
            RepoOrigin::Global => "global",
        })
    }
}

impl Serialize for RepoScope {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(match self {
            RepoScope::Project => "project",
            RepoScope::Global => "global",
        })
    }
}

impl Serialize for RepoAuthKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(match self {
            RepoAuthKind::None => "none",
            RepoAuthKind::Basic => "basic",
            RepoAuthKind::Token => "token",
        })
    }
}

impl From<AuthType> for RepoAuthKind {
    fn from(value: AuthType) -> Self {
        match value {
            AuthType::None => RepoAuthKind::None,
            AuthType::Basic => RepoAuthKind::Basic,
            AuthType::Token => RepoAuthKind::Token,
        }
    }
}

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
    let cli = Cli::parse();
    match cli.command {
        Commands::Resolver(command) => handle_resolver_command(command),
        Commands::Repo(command) => handle_repo_command(command),
        Commands::Export(command) => handle_export_command(command),
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
        global.data.repositories = default_global_repositories();
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
        global.data.repositories = default_global_repositories();
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

fn handle_export_command(args: ExportArgs) -> Result<()> {
    let (manifest, manifest_path) = load_manifest_with_path()?;
    let project_root = manifest_path
        .parent()
        .ok_or_else(|| anyhow!("jv.toml の親ディレクトリを特定できませんでした"))?
        .to_path_buf();

    let lockfile_path = project_root.join("jv.lock");
    let lockfile = LockfileService::load(&lockfile_path).with_context(|| {
        format!(
            "{} の読み込みに失敗しました。`jv lock` または `jv add` を実行してください。",
            lockfile_path.display()
        )
    })?;

    let output_dir = resolve_path(&project_root, args.output_dir.as_ref())
        .unwrap_or_else(|| project_root.join("target").join("java-project"));

    let sources_dir = resolve_path(&project_root, args.sources_dir.as_ref()).unwrap_or_else(|| {
        let base = project_root.join(&manifest.project.output.directory);
        let java_dir = format!("java{}", manifest.java_target().as_str());
        base.join(java_dir)
    });

    let local_repository = project_root.join(".jv").join("repository");

    let mut manager = RepositoryManager::with_project_root(project_root.clone())
        .context("リポジトリマネージャーの初期化に失敗しました")?;
    manager.load_project_config(&manifest);

    let repositories = manager
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
        .collect::<Vec<_>>();

    let global = load_global_config_state()?;
    let mut mirror_configs = Vec::new();
    mirror_configs.extend(manifest.mirrors.iter().cloned());
    mirror_configs.extend(global.data.mirrors.clone());
    let mirrors = mirror_configs
        .into_iter()
        .enumerate()
        .map(|(index, mirror)| {
            let id = mirror
                .name
                .clone()
                .unwrap_or_else(|| format!("mirror-{index}"));
            MavenMirrorConfig::new(id, mirror.mirror_of.clone(), mirror.url.clone())
        })
        .collect::<Vec<_>>();

    let request = ExportRequest {
        project_root: &project_root,
        manifest: &manifest,
        lockfile: &lockfile,
        sources_dir: sources_dir.as_path(),
        output_dir: output_dir.as_path(),
        local_repository: &local_repository,
        repositories: &repositories,
        mirrors: &mirrors,
        resolved: None,
    };

    let summary = JavaProjectExporter::export(&request)
        .with_context(|| format!("{} へのエクスポートに失敗しました", output_dir.display()))?;

    println!("Javaプロジェクトをエクスポートしました:");
    println!("  出力ディレクトリ : {}", summary.output_dir.display());
    println!(
        "  pom.xml         : {}",
        summary.output_dir.join("pom.xml").display()
    );
    println!(
        "  settings.xml    : {}",
        summary
            .output_dir
            .join(".jv")
            .join("settings.xml")
            .display()
    );
    println!(
        "  classpath.txt   : {}",
        summary
            .output_dir
            .join(".jv")
            .join("classpath.txt")
            .display()
    );
    println!(
        "  ソースディレクトリ : {}",
        summary.output_dir.join("src").display()
    );
    println!(
        "統計: 更新ファイル={} / リポジトリアーティファクト={} / ソース={} / classpath項目={}",
        summary.updated_files,
        summary.repository_artifacts,
        summary.source_files,
        summary.classpath_entries
    );

    Ok(())
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

fn resolve_path(base: &Path, candidate: Option<&PathBuf>) -> Option<PathBuf> {
    candidate.map(|path| {
        if path.is_absolute() {
            path.clone()
        } else {
            base.join(path)
        }
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
        default_global_repositories()
    } else {
        config.repositories.clone()
    }
}

fn effective_global_mirrors(config: &StoredGlobalConfig) -> Vec<MirrorConfig> {
    config.mirrors.clone()
}

fn default_global_repositories() -> Vec<RepositoryConfig> {
    vec![
        RepositoryConfig {
            name: "jv-registry".to_string(),
            url: "https://registry.jvlang.org".to_string(),
            priority: 50,
            auth: None,
            filter: None,
        },
        RepositoryConfig {
            name: "maven-central".to_string(),
            url: "https://repo.maven.apache.org/maven2".to_string(),
            priority: 100,
            auth: None,
            filter: None,
        },
    ]
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
