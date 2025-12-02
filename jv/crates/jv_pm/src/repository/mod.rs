pub mod config;
pub(crate) mod defaults;

use crate::Manifest;
use crate::repository::config::{AuthType, MirrorConfig, RepositoryConfig, RepositorySection};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use thiserror::Error;
use url::Url;

/// 認証情報キャッシュエントリ。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Credentials {
    Basic { username: String, password: String },
    Token(String),
}

/// リポジトリ管理エラー。
#[derive(Debug, Error)]
pub enum RepositoryError {
    #[error("ホームディレクトリを特定できませんでした")]
    HomeDirectoryUnavailable,
    #[error("設定ファイルの読み込みに失敗しました: {0}")]
    Io(#[from] std::io::Error),
    #[error("設定ファイルの解析に失敗しました: {0}")]
    Toml(#[from] toml::de::Error),
    #[error("認証設定が不完全です: {0}")]
    AuthConfigError(String),
}

/// 優先度付きリポジトリのビュー。
#[derive(Debug, Clone)]
pub struct RepositoryHandle<'a> {
    config: &'a RepositoryConfig,
    url: Cow<'a, str>,
}

impl<'a> RepositoryHandle<'a> {
    pub fn name(&self) -> &str {
        &self.config.name
    }

    pub fn url(&self) -> &str {
        &self.url
    }

    pub fn priority(&self) -> u32 {
        self.config.priority
    }

    pub fn config(&self) -> &RepositoryConfig {
        self.config
    }
}

/// リポジトリ統合マネージャー。
pub struct RepositoryManager {
    use_global_repositories: bool,
    local_repository: RepositoryConfig,
    project_repositories: Vec<RepositoryConfig>,
    global_repositories: Vec<RepositoryConfig>,
    project_mirrors: Vec<MirrorConfig>,
    global_mirrors: Vec<MirrorConfig>,
    auth_cache: HashMap<String, Credentials>,
}

impl RepositoryManager {
    /// カレントディレクトリをプロジェクトルートとして初期化する。
    pub fn new() -> Result<Self, RepositoryError> {
        let cwd = std::env::current_dir()?;
        Self::with_project_root(cwd)
    }

    /// プロジェクトルートを指定して初期化する。
    pub fn with_project_root(project_root: impl Into<PathBuf>) -> Result<Self, RepositoryError> {
        let project_root = project_root.into();
        let local_repo_path = project_root.join(".jv").join("repository");
        Self::build(local_repo_path, None)
    }

    /// カスタムローカルリポジトリパスを指定して初期化する（wrapper モード用）。
    pub fn with_local_repository(
        local_repository: impl Into<PathBuf>,
    ) -> Result<Self, RepositoryError> {
        Self::build(local_repository.into(), None)
    }

    fn build(
        local_repo_path: PathBuf,
        home_override: Option<PathBuf>,
    ) -> Result<Self, RepositoryError> {
        let global = load_global_config(home_override)?;
        let local_repository = local_repository_config_from_path(&local_repo_path);

        Ok(Self {
            use_global_repositories: true,
            local_repository,
            project_repositories: Vec::new(),
            global_repositories: sorted_unique(global.repositories),
            project_mirrors: Vec::new(),
            global_mirrors: global.mirrors,
            auth_cache: HashMap::new(),
        })
    }

    /// マニフェストの設定を読み込みプロジェクト依存のリポジトリ情報を反映する。
    pub fn load_project_config(&mut self, manifest: &Manifest) {
        let RepositorySection {
            use_global,
            entries,
        } = &manifest.repositories;

        self.use_global_repositories = *use_global;
        self.project_repositories = sorted_unique(entries.clone());
        self.project_mirrors = manifest.mirrors.clone();
        self.auth_cache.clear();
    }

    /// 全リポジトリ一覧を優先度順に返す。
    pub fn list(&self) -> Vec<RepositoryHandle<'_>> {
        self.prioritised_repositories()
            .into_iter()
            .map(|repo| self.handle_for(repo))
            .collect()
    }

    /// プロジェクト設定とグローバル設定を統合したミラー一覧を返す。
    pub fn effective_mirrors(&self) -> Vec<MirrorConfig> {
        let mut mirrors = self.project_mirrors.clone();
        if self.use_global_repositories {
            mirrors.extend(self.global_mirrors.clone());
        }
        mirrors
    }

    /// グループIDに合致する検索対象リポジトリを返す。
    pub fn get_repositories_for_dependency(&self, group_id: &str) -> Vec<RepositoryHandle<'_>> {
        self.prioritised_repositories()
            .into_iter()
            .filter(|repo| self.should_search_repo(repo, group_id))
            .map(|repo| self.handle_for(repo))
            .collect()
    }

    /// ミラー適用後のURLを取得する。
    pub fn get_repository_url(&self, repo: &RepositoryConfig) -> String {
        self.resolve_url(repo).into_owned()
    }

    /// 認証情報を取得する（必要なら環境変数参照）。
    pub fn get_credentials(
        &mut self,
        repo: &RepositoryConfig,
    ) -> Result<Option<Credentials>, RepositoryError> {
        if let Some(cached) = self.auth_cache.get(&repo.name) {
            return Ok(Some(cached.clone()));
        }

        let Some(auth) = repo.auth.as_ref() else {
            return Ok(None);
        };

        if auth.is_none() || matches!(auth.auth_type, AuthType::None) {
            return Ok(None);
        }

        let credentials = match auth.auth_type {
            AuthType::None => return Ok(None),
            AuthType::Basic => {
                let username = require_env(auth.username_env.as_deref(), "ユーザー名")?;
                let password = require_env(auth.password_env.as_deref(), "パスワード")?;
                Credentials::Basic { username, password }
            }
            AuthType::Token => {
                let token = require_env(auth.token_env.as_deref(), "トークン")?;
                Credentials::Token(token)
            }
        };

        self.auth_cache
            .insert(repo.name.clone(), credentials.clone());
        Ok(Some(credentials))
    }

    /// グローバル設定を使用するかどうか。
    pub fn uses_global(&self) -> bool {
        self.use_global_repositories
    }

    fn handle_for<'a>(&'a self, repo: &'a RepositoryConfig) -> RepositoryHandle<'a> {
        RepositoryHandle {
            config: repo,
            url: self.resolve_url(repo),
        }
    }

    fn resolve_url<'a>(&'a self, repo: &'a RepositoryConfig) -> Cow<'a, str> {
        if repo.name == self.local_repository.name {
            return Cow::Borrowed(&repo.url);
        }

        if let Some(mirror) = self
            .project_mirrors
            .iter()
            .find(|mirror| matches_pattern(&repo.name, &mirror.mirror_of))
        {
            return Cow::Owned(mirror.url.clone());
        }

        if self.use_global_repositories {
            if let Some(mirror) = self
                .global_mirrors
                .iter()
                .find(|mirror| matches_pattern(&repo.name, &mirror.mirror_of))
            {
                return Cow::Owned(mirror.url.clone());
            }
        }

        Cow::Borrowed(&repo.url)
    }

    fn prioritised_repositories(&self) -> Vec<&RepositoryConfig> {
        let mut combined: Vec<&RepositoryConfig> = Vec::new();
        combined.push(&self.local_repository);
        combined.extend(self.project_repositories.iter());
        if self.use_global_repositories {
            combined.extend(self.global_repositories.iter());
        }

        combined.sort_by(|left, right| {
            left.priority
                .cmp(&right.priority)
                .then_with(|| left.name.cmp(&right.name))
        });

        let mut seen = HashSet::new();
        combined
            .into_iter()
            .filter(|repo| seen.insert(repo.name.clone()))
            .collect()
    }

    fn should_search_repo(&self, repo: &RepositoryConfig, group_id: &str) -> bool {
        let Some(filter) = repo.filter.as_ref() else {
            return true;
        };

        if let Some(excludes) = &filter.exclude_groups {
            if excludes
                .iter()
                .any(|pattern| matches_pattern(group_id, pattern))
            {
                return false;
            }
        }

        if let Some(includes) = &filter.include_groups {
            return includes
                .iter()
                .any(|pattern| matches_pattern(group_id, pattern));
        }

        true
    }

    #[cfg(test)]
    fn with_overrides(project_root: PathBuf, home_dir: PathBuf) -> Result<Self, RepositoryError> {
        Self::build(project_root, Some(home_dir))
    }
}

fn require_env(var_name: Option<&str>, label: &str) -> Result<String, RepositoryError> {
    let Some(name) = var_name else {
        return Err(RepositoryError::AuthConfigError(format!(
            "{label} の環境変数名が設定されていません"
        )));
    };

    std::env::var(name)
        .map_err(|_| RepositoryError::AuthConfigError(format!("環境変数 {name} が見つかりません")))
}

fn matches_pattern(text: &str, pattern: &str) -> bool {
    if pattern == "*" {
        return true;
    }

    let mut remaining = text;
    let mut parts = pattern.split('*').peekable();

    if let Some(first) = parts.peek().copied() {
        if !pattern.starts_with('*') {
            let first = first;
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

fn local_repository_config_from_path(path: &Path) -> RepositoryConfig {
    let url = Url::from_file_path(path)
        .map(|url| url.to_string())
        .unwrap_or_else(|_| format!("file://{}", path.display()));

    RepositoryConfig {
        name: "local".to_string(),
        url,
        priority: 0,
        auth: None,
        filter: None,
    }
}

fn sorted_unique(mut repos: Vec<RepositoryConfig>) -> Vec<RepositoryConfig> {
    repos.sort_by(|left, right| {
        left.priority
            .cmp(&right.priority)
            .then_with(|| left.name.cmp(&right.name))
    });

    repos.dedup_by(|left, right| left.name == right.name);
    repos
}

struct GlobalConfig {
    repositories: Vec<RepositoryConfig>,
    mirrors: Vec<MirrorConfig>,
}

fn load_global_config(home_override: Option<PathBuf>) -> Result<GlobalConfig, RepositoryError> {
    let home = if let Some(home) = home_override {
        home
    } else {
        dirs::home_dir().ok_or(RepositoryError::HomeDirectoryUnavailable)?
    };

    let config_path = home.join(".jv").join("config.toml");
    if !config_path.exists() {
        return Ok(GlobalConfig {
            repositories: defaults::embedded_global_repositories(),
            mirrors: Vec::new(),
        });
    }

    let contents = fs::read_to_string(&config_path)?;
    let parsed: GlobalConfigFile = toml::from_str(&contents)?;

    let repositories = if parsed.repositories.is_empty() {
        defaults::embedded_global_repositories()
    } else {
        parsed.repositories
    };

    Ok(GlobalConfig {
        repositories,
        mirrors: parsed.mirrors,
    })
}

pub fn builtin_global_repositories() -> Vec<RepositoryConfig> {
    defaults::embedded_global_repositories()
}

#[derive(Debug, Clone, serde::Deserialize, Default)]
struct GlobalConfigFile {
    #[serde(default)]
    repositories: Vec<RepositoryConfig>,
    #[serde(default)]
    mirrors: Vec<MirrorConfig>,
    #[serde(flatten)]
    _extras: toml::value::Table,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::repository::config::{AuthConfig, FilterConfig, RepositorySection};
    use crate::{BuildInfo, LoggingConfig, Manifest, PackageInfo, ProjectSection};
    use std::collections::HashMap;
    use std::env;
    use tempfile::TempDir;

    struct EnvGuard {
        key: String,
        original: Option<String>,
    }

    impl EnvGuard {
        fn set(key: &str, value: &str) -> Self {
            let original = env::var(key).ok();
            unsafe {
                env::set_var(key, value);
            }
            Self {
                key: key.to_string(),
                original,
            }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            if let Some(value) = &self.original {
                unsafe {
                    env::set_var(&self.key, value);
                }
            } else {
                unsafe {
                    env::remove_var(&self.key);
                }
            }
        }
    }

    fn manifest_stub() -> Manifest {
        Manifest {
            package: PackageInfo {
                name: "demo".to_string(),
                version: "0.1.0".to_string(),
                description: None,
                dependencies: HashMap::new(),
            },
            project: ProjectSection::default(),
            repositories: RepositorySection::default(),
            mirrors: Vec::new(),
            build: Some(BuildInfo::default()),
            logging: LoggingConfig::default(),
            maven: Default::default(),
        }
    }

    #[test]
    fn loads_default_global_repositories_when_config_missing() {
        let home = TempDir::new().expect("temp home");
        let project = TempDir::new().expect("temp project");

        let mut manager = RepositoryManager::with_overrides(
            project.path().to_path_buf(),
            home.path().to_path_buf(),
        )
        .expect("manager");

        manager.load_project_config(&manifest_stub());
        let handles = manager.list();

        assert!(handles.iter().any(|h| h.name() == "local"));
        assert!(handles.iter().any(|h| h.name() == "maven-central"));
    }

    #[test]
    fn respects_project_priority_and_use_global_flag() {
        let home = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();

        let mut manager = RepositoryManager::with_overrides(
            project.path().to_path_buf(),
            home.path().to_path_buf(),
        )
        .unwrap();

        let mut manifest = manifest_stub();
        manifest.repositories = RepositorySection {
            use_global: false,
            entries: vec![
                RepositoryConfig {
                    name: "corp-private".to_string(),
                    url: "https://maven.corp/repository".to_string(),
                    priority: 10,
                    auth: None,
                    filter: None,
                },
                RepositoryConfig {
                    name: "jitpack".to_string(),
                    url: "https://jitpack.io".to_string(),
                    priority: 20,
                    auth: None,
                    filter: None,
                },
            ],
        };

        manager.load_project_config(&manifest);

        let list = manager.list();
        let names: Vec<_> = list.iter().map(|h| h.name().to_string()).collect();

        assert_eq!(names[0], "local");
        assert!(names.contains(&"corp-private".to_string()));
        assert!(names.contains(&"jitpack".to_string()));
        assert!(!names.contains(&"maven-central".to_string()));
    }

    #[test]
    fn applies_filters_and_mirrors() {
        let home = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();

        let mut manager = RepositoryManager::with_overrides(
            project.path().to_path_buf(),
            home.path().to_path_buf(),
        )
        .unwrap();

        let mut manifest = manifest_stub();
        manifest.repositories = RepositorySection {
            use_global: true,
            entries: vec![RepositoryConfig {
                name: "jitpack".to_string(),
                url: "https://jitpack.io".to_string(),
                priority: 5,
                auth: None,
                filter: Some(FilterConfig {
                    include_groups: Some(vec!["com.example.*".to_string()]),
                    exclude_groups: Some(vec!["com.example.internal".to_string()]),
                }),
            }],
        };
        manifest.mirrors = vec![MirrorConfig {
            name: Some("corp-mirror".to_string()),
            mirror_of: "maven-central".to_string(),
            url: "https://mirror.corp/maven".to_string(),
        }];

        manager.load_project_config(&manifest);

        let matching = manager.get_repositories_for_dependency("com.example.lib");
        assert!(matching.iter().any(|h| h.name() == "jitpack"));

        let excluded = manager.get_repositories_for_dependency("com.example.internal");
        assert!(!excluded.iter().any(|h| h.name() == "jitpack"));

        let global = manager
            .list()
            .into_iter()
            .find(|h| h.name() == "maven-central")
            .expect("global repo");
        assert_eq!(global.url(), "https://mirror.corp/maven");
    }

    #[test]
    fn fetches_and_caches_credentials() {
        let home = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();

        let mut manager = RepositoryManager::with_overrides(
            project.path().to_path_buf(),
            home.path().to_path_buf(),
        )
        .unwrap();

        let mut manifest = manifest_stub();
        manifest.repositories = RepositorySection {
            use_global: false,
            entries: vec![RepositoryConfig {
                name: "secure".to_string(),
                url: "https://secure.repo".to_string(),
                priority: 1,
                auth: Some(AuthConfig {
                    auth_type: AuthType::Basic,
                    username_env: Some("REPO_USER".to_string()),
                    password_env: Some("REPO_PASS".to_string()),
                    token_env: None,
                }),
                filter: None,
            }],
        };

        manager.load_project_config(&manifest);

        let user_guard = EnvGuard::set("REPO_USER", "alice");
        let pass_guard = EnvGuard::set("REPO_PASS", "secret");

        let handle = manager
            .list()
            .into_iter()
            .find(|h| h.name() == "secure")
            .unwrap();

        let repo_config = handle.config().clone();

        let first = manager
            .get_credentials(&repo_config)
            .expect("credentials")
            .expect("basic auth");

        match first {
            Credentials::Basic { username, password } => {
                assert_eq!(username, "alice");
                assert_eq!(password, "secret");
            }
            _ => panic!("unexpected credentials"),
        }

        drop(user_guard);
        drop(pass_guard);

        // 2回目はキャッシュ利用で成功する。
        assert!(
            manager
                .get_credentials(&repo_config)
                .expect("credentials")
                .is_some()
        );
    }

    #[test]
    fn fails_when_required_env_missing() {
        let home = TempDir::new().unwrap();
        let project = TempDir::new().unwrap();

        let mut manager = RepositoryManager::with_overrides(
            project.path().to_path_buf(),
            home.path().to_path_buf(),
        )
        .unwrap();

        let mut manifest = manifest_stub();
        manifest.repositories = RepositorySection {
            use_global: false,
            entries: vec![RepositoryConfig {
                name: "secure".to_string(),
                url: "https://secure.repo".to_string(),
                priority: 1,
                auth: Some(AuthConfig {
                    auth_type: AuthType::Token,
                    username_env: None,
                    password_env: None,
                    token_env: Some("REPO_TOKEN".to_string()),
                }),
                filter: None,
            }],
        };

        manager.load_project_config(&manifest);

        let handle = manager
            .list()
            .into_iter()
            .find(|h| h.name() == "secure")
            .unwrap();

        let repo_config = handle.config().clone();

        let err = manager
            .get_credentials(&repo_config)
            .expect_err("missing token should fail");

        match err {
            RepositoryError::AuthConfigError(message) => {
                assert!(message.contains("環境変数"));
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }
}
