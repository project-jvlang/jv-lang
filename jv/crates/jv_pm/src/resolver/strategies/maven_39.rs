use crate::DependencyCache;
use crate::maven::{MavenMirrorConfig, MavenRepositoryConfig};
use crate::registry::{
    ArtifactCoordinates, ArtifactResource, ChecksumAlgorithm, ChecksumVerifiedJar, MavenRegistry,
    RegistryError,
};
use crate::repository::defaults::maven_standard_plugins;
use crate::resolver::{
    DependencyProvider, DependencyScope, Manifest, MavenResolverContext, RequestedDependency,
    ResolutionDiagnostic, ResolutionSource, ResolutionStats, ResolvedDependencies,
    ResolvedDependency, ResolverAlgorithmKind, ResolverError, ResolverOptions, ResolverStrategy,
    ResolverStrategyInfo, StrategyStability, VersionDecision,
};
use pom_resolver::ClosureOptions;
use reqwest::StatusCode;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;
use tokio::runtime::Builder as RuntimeBuilder;
use tokio::task::JoinSet;

const MAX_PARALLEL_DOWNLOADS: usize = 8;
const MAX_JAR_DOWNLOAD_ATTEMPTS: usize = 3;

/// Dependency provider for Maven 3.9 compatibility mode.
pub struct MavenDependencyProvider3_9<'a> {
    project_root: &'a std::path::Path,
    repositories: Vec<MavenRepositoryConfig>,
    mirrors: Vec<MavenMirrorConfig>,
    _cache: Arc<DependencyCache>,
    base_dependencies: Vec<RequestedDependency>,
}

impl<'a> MavenDependencyProvider3_9<'a> {
    pub fn new(
        project_root: &'a std::path::Path,
        repositories: Vec<MavenRepositoryConfig>,
        mirrors: Vec<MavenMirrorConfig>,
        cache: Arc<DependencyCache>,
        base_dependencies: Vec<RequestedDependency>,
    ) -> Self {
        Self {
            project_root,
            repositories,
            mirrors,
            _cache: cache,
            base_dependencies,
        }
    }

    pub fn repositories(&self) -> &[MavenRepositoryConfig] {
        &self.repositories
    }

    pub fn mirrors(&self) -> &[MavenMirrorConfig] {
        &self.mirrors
    }
}

impl<'a> DependencyProvider for MavenDependencyProvider3_9<'a> {
    fn root_package(&self) -> &crate::PackageInfo {
        use once_cell::sync::OnceCell;
        static DUMMY: OnceCell<crate::PackageInfo> = OnceCell::new();
        DUMMY.get_or_init(|| crate::PackageInfo {
            name: "maven-wrapper".to_string(),
            version: "0.0.0".to_string(),
            description: None,
            dependencies: Default::default(),
        })
    }

    fn direct_dependencies(&self) -> Vec<RequestedDependency> {
        if !self.base_dependencies.is_empty() {
            return self.base_dependencies.clone();
        }

        let pom_path = self.project_root.join("pom.xml");
        let contents = match fs::read_to_string(&pom_path) {
            Ok(text) => text,
            Err(error) => {
                tracing::warn!(path = %pom_path.display(), error = %error, "pom.xml の読み込みに失敗しました");
                return Vec::new();
            }
        };

        match pom_resolver::parse_direct_dependencies(&contents) {
            Ok(deps) => deps,
            Err(error) => {
                tracing::warn!(path = %pom_path.display(), error = ?error, "pom.xml から直接依存を抽出できませんでした");
                Vec::new()
            }
        }
    }
}

fn repository_conflict(subject: impl Into<String>, details: impl Into<String>) -> ResolverError {
    ResolverError::Conflict {
        dependency: subject.into(),
        details: details.into(),
    }
}

fn registries_from_context(ctx: &MavenResolverContext) -> Vec<Arc<MavenRegistry>> {
    let mut registries = Vec::new();
    for repo in &ctx.repositories {
        match MavenRegistry::new(&repo.url) {
            Ok(registry) => registries.push(Arc::new(registry)),
            Err(error) => {
                tracing::warn!(
                    url = %repo.url,
                    error = ?error,
                    "リポジトリ初期化に失敗したためスキップします"
                );
            }
        }
    }
    registries
}

#[derive(Debug, Clone, Copy)]
enum JarVerification {
    Valid,
    NeedsDownload,
}

struct MavenJarDownloader {
    local_repository: PathBuf,
    registries: Vec<Arc<MavenRegistry>>,
    max_concurrent: usize,
}

impl MavenJarDownloader {
    fn new(local_repository: PathBuf, registries: Vec<Arc<MavenRegistry>>) -> Self {
        Self {
            local_repository,
            registries,
            max_concurrent: MAX_PARALLEL_DOWNLOADS,
        }
    }

    fn artifact_targets(
        &self,
        coords: &[ArtifactCoordinates],
    ) -> Vec<(ArtifactCoordinates, PathBuf)> {
        coords
            .iter()
            .cloned()
            .map(|coords| {
                let path = self.artifact_path(&coords);
                (coords, path)
            })
            .collect()
    }

    fn artifact_path(&self, coords: &ArtifactCoordinates) -> PathBuf {
        self.local_repository
            .join(coords.maven_coordinates().group_path())
            .join(&coords.artifact_id)
            .join(&coords.version)
            .join(coords.jar_file_name())
    }

    fn ensure_artifacts(
        &self,
        runtime: &tokio::runtime::Runtime,
        targets: &[(ArtifactCoordinates, PathBuf)],
    ) -> Result<(), ResolverError> {
        if targets.is_empty() {
            return Ok(());
        }

        let mut attempt = 0;
        loop {
            let (to_download, mut jar_paths) = self.build_download_plan(targets)?;
            eprintln!(
                "[maven-compat-debug] build_download_plan: targets={} to_download={}",
                targets.len(),
                to_download.len()
            );
            if to_download.is_empty() {
                if self.verify_repository_state(targets)? {
                    return Ok(());
                }
                continue;
            }

            attempt += 1;
            tracing::info!(
                "[maven-compat-download-plan] attempt {attempt}: {}/{} artifacts pending",
                to_download.len(),
                targets.len()
            );

            let downloads = runtime
                .block_on(self.download_all(to_download))
                .map_err(|error| {
                    repository_conflict(
                        "artifact-download",
                        format!("Jar のダウンロードに失敗しました: {error}"),
                    )
                })?;

            self.write_downloads(downloads, &mut jar_paths)?;

            if attempt >= MAX_JAR_DOWNLOAD_ATTEMPTS {
                let (remaining, _) = self.build_download_plan(targets)?;
                if remaining.is_empty() {
                    return Ok(());
                }
                return Err(repository_conflict(
                    "artifact-download",
                    format!(
                        "Jar のダウンロード確認に失敗しました (remaining: {})",
                        remaining
                            .iter()
                            .map(|coords| coords.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                ));
            }
        }
    }

    fn build_download_plan(
        &self,
        targets: &[(ArtifactCoordinates, PathBuf)],
    ) -> Result<
        (
            Vec<ArtifactCoordinates>,
            HashMap<ArtifactCoordinates, PathBuf>,
        ),
        ResolverError,
    > {
        let mut to_download = Vec::new();
        let mut jar_paths = HashMap::new();

        for (coords, path) in targets {
            if self.try_hydrate_from_maven_cache(coords, path)? {
                tracing::info!(target = %coords, "hydrated from MAVEN_CACHE_DIR");
                continue;
            }
            if self.validate_existing_jar(path)? {
                tracing::info!(target = %coords, path = %path.display(), "using existing jar");
                continue;
            }

            jar_paths.insert(coords.clone(), path.clone());
            to_download.push(coords.clone());
        }

        tracing::info!(
            "download_plan: targets={} to_download={} hydrated_or_existing={}",
            targets.len(),
            to_download.len(),
            targets.len().saturating_sub(to_download.len())
        );

        Ok((to_download, jar_paths))
    }

    /// フォールバックとして、MAVEN_CACHE_DIR（標準の Maven ローカルリポジトリ）に
    /// 既にダウンロード済みの Jar があればコピーして再利用する。
    fn try_hydrate_from_maven_cache(
        &self,
        coords: &ArtifactCoordinates,
        destination: &Path,
    ) -> Result<bool, ResolverError> {
        let Ok(cache_root) = std::env::var("MAVEN_CACHE_DIR") else {
            return Ok(false);
        };
        let cache_root = PathBuf::from(cache_root);
        let cache_path = cache_root
            .join(coords.maven_coordinates().group_path())
            .join(&coords.artifact_id)
            .join(&coords.version)
            .join(coords.jar_file_name());

        if !cache_path.exists() {
            return Ok(false);
        }

        let dest_parent = destination.parent().ok_or_else(|| {
            repository_conflict(
                destination.display().to_string(),
                "親ディレクトリなし".to_string(),
            )
        })?;
        fs::create_dir_all(dest_parent).map_err(|error| {
            repository_conflict(
                dest_parent.display().to_string(),
                format!("{} の作成に失敗しました: {error}", dest_parent.display()),
            )
        })?;

        fs::copy(&cache_path, destination).map_err(|error| {
            repository_conflict(
                destination.display().to_string(),
                format!(
                    "{} へのコピーに失敗しました (source: {}): {error}",
                    destination.display(),
                    cache_path.display()
                ),
            )
        })?;

        // 可能であればチェックサムもコピーして検証を通す
        for ext in ["sha256", "sha1", "md5"] {
            let source_checksum = cache_path.with_extension(format!("jar.{ext}"));
            if source_checksum.exists() {
                let dest_checksum = destination.with_extension(format!("jar.{ext}"));
                let _ = fs::copy(&source_checksum, &dest_checksum);
            }
        }

        Ok(true)
    }

    async fn download_all(
        &self,
        mut requests: Vec<ArtifactCoordinates>,
    ) -> Result<Vec<(ArtifactCoordinates, ChecksumVerifiedJar)>, RegistryError> {
        let mut results = Vec::new();
        let mut join_set = JoinSet::new();
        let mut queue: VecDeque<ArtifactCoordinates> = requests.drain(..).collect();

        while let Some(coords) = queue.pop_front() {
            let registries = self.registries.clone();
            join_set.spawn(async move { download_single(coords, registries).await });

            if join_set.len() >= self.max_concurrent {
                let joined = join_set.join_next().await;
                if let Some(result) = joined {
                    results.push(handle_join_result(result)?);
                }
            }
        }

        while let Some(joined) = join_set.join_next().await {
            results.push(handle_join_result(joined)?);
        }

        Ok(results)
    }

    fn write_downloads(
        &self,
        downloads: Vec<(ArtifactCoordinates, ChecksumVerifiedJar)>,
        jar_paths: &mut HashMap<ArtifactCoordinates, PathBuf>,
    ) -> Result<(), ResolverError> {
        let total = downloads.len();
        for (index, (coords, download)) in downloads.into_iter().enumerate() {
            tracing::info!("[maven-compat-download] {}/{} {}", index + 1, total, coords);
            let jar_path = jar_paths.remove(&coords).ok_or_else(|| {
                repository_conflict(
                    coords.to_string(),
                    "ダウンロード済み Jar のパスが見つかりません",
                )
            })?;
            self.write_downloaded_jar(&jar_path, download)?;
        }

        Ok(())
    }

    fn validate_existing_jar(&self, path: &Path) -> Result<bool, ResolverError> {
        if !path.exists() {
            return Ok(false);
        }

        if let Some((algorithm, expected)) = self.read_local_checksum(path)? {
            let bytes = fs::read(path).map_err(|error| {
                repository_conflict(
                    path.display().to_string(),
                    format!("{} の読み込みに失敗しました: {error}", path.display()),
                )
            })?;
            let actual = algorithm.compute(&bytes);
            if actual == expected {
                return Ok(true);
            }
        }

        self.remove_local_artifact(path)?;
        Ok(false)
    }

    fn read_local_checksum(
        &self,
        jar_path: &Path,
    ) -> Result<Option<(ChecksumAlgorithm, String)>, ResolverError> {
        for algorithm in [
            ChecksumAlgorithm::Sha256,
            ChecksumAlgorithm::Sha1,
            ChecksumAlgorithm::Md5,
        ] {
            let checksum_path = jar_path.with_extension(format!("jar.{}", algorithm.extension()));
            if !checksum_path.exists() {
                continue;
            }

            let content = fs::read_to_string(&checksum_path).map_err(|error| {
                repository_conflict(
                    checksum_path.display().to_string(),
                    format!(
                        "{} の読み込みに失敗しました: {error}",
                        checksum_path.display()
                    ),
                )
            })?;

            if let Some(value) = extract_local_checksum(&content) {
                return Ok(Some((algorithm, value)));
            }
        }

        Ok(None)
    }

    fn remove_local_artifact(&self, jar_path: &Path) -> Result<(), ResolverError> {
        if jar_path.exists() {
            fs::remove_file(jar_path).map_err(|error| {
                repository_conflict(
                    jar_path.display().to_string(),
                    format!("{} の削除に失敗しました: {error}", jar_path.display()),
                )
            })?;
        }

        for ext in &["sha256", "sha1", "md5"] {
            let checksum_path = jar_path.with_extension(format!("jar.{ext}"));
            if checksum_path.exists() {
                fs::remove_file(&checksum_path).map_err(|error| {
                    repository_conflict(
                        checksum_path.display().to_string(),
                        format!("{} の削除に失敗しました: {error}", checksum_path.display()),
                    )
                })?;
            }
        }

        Ok(())
    }

    fn verify_repository_state(
        &self,
        targets: &[(ArtifactCoordinates, PathBuf)],
    ) -> Result<bool, ResolverError> {
        if targets.is_empty() {
            return Ok(true);
        }

        tracing::info!(
            "[maven-compat-verify] {} artifacts scheduled for verification (repository: {})",
            targets.len(),
            self.local_repository.display()
        );

        let mut needs_download = false;
        let total = targets.len();
        for (index, (coords, jar_path)) in targets.iter().enumerate() {
            match self.verify_single_jar(coords, jar_path, index + 1, total)? {
                JarVerification::Valid => {}
                JarVerification::NeedsDownload => needs_download = true,
            }
        }

        if !needs_download {
            tracing::info!(
                "[maven-compat-verify] repository verification succeeded ({} artifacts)",
                targets.len()
            );
        } else {
            tracing::warn!(
                "[maven-compat-verify] integrity issues detected; invalid artifacts will be re-downloaded"
            );
        }

        Ok(!needs_download)
    }

    fn verify_single_jar(
        &self,
        coords: &ArtifactCoordinates,
        jar_path: &Path,
        index: usize,
        total: usize,
    ) -> Result<JarVerification, ResolverError> {
        if !jar_path.exists() {
            tracing::warn!(
                "[maven-compat-verify] {}/{} missing {}",
                index,
                total,
                coords
            );
            return Ok(JarVerification::NeedsDownload);
        }

        let Some((algorithm, expected)) = self.read_local_checksum(jar_path)? else {
            tracing::warn!(
                "[maven-compat-verify] {}/{} checksum file missing for {}",
                index,
                total,
                coords
            );
            self.remove_local_artifact(jar_path)?;
            return Ok(JarVerification::NeedsDownload);
        };

        let bytes = fs::read(jar_path).map_err(|error| {
            repository_conflict(
                jar_path.display().to_string(),
                format!("{} の読み込みに失敗しました: {error}", jar_path.display()),
            )
        })?;
        let actual = algorithm.compute(&bytes);

        if actual == expected {
            if let Some(relative) = self.relative_repository_path(jar_path) {
                tracing::info!(
                    "[maven-compat-verify] {}/{} OK {} ({})",
                    index,
                    total,
                    coords,
                    relative
                );
            } else {
                tracing::info!("[maven-compat-verify] {}/{} OK {}", index, total, coords);
            }
            return Ok(JarVerification::Valid);
        }

        tracing::warn!(
            "[maven-compat-verify] {}/{} checksum mismatch {} (expected {}, actual {})",
            index,
            total,
            coords,
            expected,
            actual
        );
        self.remove_local_artifact(jar_path)?;
        Ok(JarVerification::NeedsDownload)
    }

    fn relative_repository_path(&self, jar_path: &Path) -> Option<String> {
        jar_path
            .strip_prefix(&self.local_repository)
            .ok()
            .map(|path| path.to_string_lossy().replace('\\', "/"))
    }

    fn write_downloaded_jar(
        &self,
        jar_path: &Path,
        download: ChecksumVerifiedJar,
    ) -> Result<(), ResolverError> {
        if let Some(parent) = jar_path.parent() {
            fs::create_dir_all(parent).map_err(|error| {
                repository_conflict(
                    parent.display().to_string(),
                    format!("{} の作成に失敗しました: {error}", parent.display()),
                )
            })?;
        }

        fs::write(jar_path, &download.jar.bytes).map_err(|error| {
            repository_conflict(
                jar_path.display().to_string(),
                format!("{} への書き込みに失敗しました: {error}", jar_path.display()),
            )
        })?;

        let checksum_path =
            jar_path.with_extension(format!("jar.{}", download.algorithm.extension()));
        let content = format!("{}\n", download.expected_checksum);
        fs::write(&checksum_path, content).map_err(|error| {
            repository_conflict(
                checksum_path.display().to_string(),
                format!(
                    "{} への書き込みに失敗しました: {error}",
                    checksum_path.display()
                ),
            )
        })?;

        Ok(())
    }
}

fn handle_join_result(
    joined: Result<
        Result<(ArtifactCoordinates, ChecksumVerifiedJar), RegistryError>,
        tokio::task::JoinError,
    >,
) -> Result<(ArtifactCoordinates, ChecksumVerifiedJar), RegistryError> {
    match joined {
        Ok(Ok(value)) => Ok(value),
        Ok(Err(error)) => Err(error),
        Err(join_error) => Err(RegistryError::InvalidResponse {
            resource: "parallel-download".to_string(),
            message: join_error.to_string(),
        }),
    }
}

async fn download_single(
    coords: ArtifactCoordinates,
    registries: Vec<Arc<MavenRegistry>>,
) -> Result<(ArtifactCoordinates, ChecksumVerifiedJar), RegistryError> {
    let algorithms = [
        ChecksumAlgorithm::Sha256,
        ChecksumAlgorithm::Sha1,
        ChecksumAlgorithm::Md5,
    ];

    let mut last_error = None;
    tracing::info!("[maven-compat-download-start] {}", coords);

    for registry in registries {
        match registry
            .download_jar_with_algorithms(&coords, &algorithms)
            .await
        {
            Ok(download) => return Ok((coords.clone(), download)),
            Err(error) => match error {
                RegistryError::ArtifactNotFound { resource, .. }
                    if matches!(resource, ArtifactResource::Jar) =>
                {
                    last_error = Some(error);
                    continue;
                }
                RegistryError::PackageNotFound { .. } | RegistryError::ChecksumMissing { .. } => {
                    last_error = Some(error);
                    continue;
                }
                other => return Err(other),
            },
        }
    }

    Err(
        last_error.unwrap_or_else(|| RegistryError::ArtifactNotFound {
            coordinates: coords.clone(),
            resource: ArtifactResource::Jar,
            status: StatusCode::NOT_FOUND,
        }),
    )
}

fn extract_local_checksum(text: &str) -> Option<String> {
    text.lines()
        .map(str::trim)
        .find(|line| !line.is_empty())
        .map(|line| {
            line.split_whitespace()
                .next()
                .unwrap_or(line)
                .to_ascii_lowercase()
        })
}

#[derive(Debug, Default)]
pub struct MavenCompat39Strategy;

impl MavenCompat39Strategy {
    pub fn new() -> Self {
        Self
    }

    fn build_provider<'a>(
        &'a self,
        context: Option<&'a MavenResolverContext>,
    ) -> MavenDependencyProvider3_9<'a> {
        let ctx = context.expect("MavenResolverContext is required for MavenCompat39Strategy");
        MavenDependencyProvider3_9::new(
            ctx.project_root.as_path(),
            ctx.repositories.clone(),
            ctx.mirrors.clone(),
            Arc::new(DependencyCache::global().unwrap_or_else(|_| {
                DependencyCache::with_dir(ctx.project_root.join(".jv").join("cache"))
                    .unwrap_or_else(|_| DependencyCache::global().expect("cache init"))
            })),
            ctx.base_dependencies.clone(),
        )
    }
}

impl ResolverStrategy for MavenCompat39Strategy {
    fn metadata(&self) -> ResolverStrategyInfo {
        ResolverStrategyInfo {
            name: "maven".into(),
            display_name: "Maven 3.9-compatible".into(),
            description: "Maven 3.9 resolution semantics with artifact download hooks".into(),
            algorithm: ResolverAlgorithmKind::MavenCompat,
            stability: StrategyStability::Experimental,
            is_default: false,
            deterministic: true,
            supports_offline: false,
            emits_conflict_reasons: false,
            aliases: vec![
                "maven-compat".into(),
                "maven-compat-3.9".into(),
                "legacy".into(),
            ],
            conflict_policy: "first-wins".into(),
        }
    }

    fn resolve(
        &self,
        _manifest: &Manifest,
        _provider: &dyn DependencyProvider,
        options: &ResolverOptions,
    ) -> Result<ResolvedDependencies, ResolverError> {
        let info = self.metadata();
        let start = Instant::now();

        let ctx = match options.maven_context.as_ref() {
            Some(ctx) => ctx,
            None => {
                return Err(ResolverError::Conflict {
                    dependency: "maven-context".into(),
                    details: "MavenResolverContext が未設定です".into(),
                });
            }
        };

        fs::create_dir_all(&ctx.local_repository).map_err(|error| {
            repository_conflict(
                "local-repository",
                format!(
                    "{} の作成に失敗しました: {error}",
                    ctx.local_repository.display()
                ),
            )
        })?;

        let runtime = RuntimeBuilder::new_multi_thread()
            .enable_all()
            .build()
            .map_err(|error| ResolverError::Conflict {
                dependency: "runtime".into(),
                details: error.to_string(),
            })?;
        let cache =
            Arc::new(
                DependencyCache::global().map_err(|error| ResolverError::Conflict {
                    dependency: "cache".into(),
                    details: error.to_string(),
                })?,
            );

        let registries = registries_from_context(ctx);
        eprintln!(
            "[maven-compat-debug] registries={} local_repository={} strategy={}",
            registries.len(),
            ctx.local_repository.display(),
            info.name
        );

        let jar_downloader =
            MavenJarDownloader::new(ctx.local_repository.clone(), registries.clone());
        let provider = self.build_provider(options.maven_context.as_ref());
        // Wrapperモードでは base_dependencies を唯一のルートに固定し、追加のプラグイン由来ルート混入を防ぐ。
        let direct = if !ctx.base_dependencies.is_empty() {
            ctx.base_dependencies.clone()
        } else {
            provider.direct_dependencies()
        };
        eprintln!(
            "[maven-compat-debug] direct_dependencies={} (base_only={}) base_dep_names=[{}]",
            direct.len(),
            !ctx.base_dependencies.is_empty(),
            ctx.base_dependencies
                .iter()
                .map(|d| d.name.clone())
                .collect::<Vec<_>>()
                .join(",")
        );

        let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), registries.clone());

        let mut roots = Vec::new();
        let mut base_names: HashSet<String> = HashSet::new();
        let mut seen: HashSet<(String, String, Option<String>)> = HashSet::new();
        for dep in &direct {
            if let Some((group, artifact)) = dep.name.split_once(':') {
                let ver = dep.requirement.clone();
                let coords = ArtifactCoordinates::new(group.to_string(), artifact.to_string(), ver);
                base_names.insert(format!("{group}:{artifact}"));
                let key = (
                    coords.group_id.clone(),
                    coords.artifact_id.clone(),
                    coords.classifier.clone(),
                );
                if seen.insert(key) {
                    roots.push(coords);
                }
            }
        }
        let mut plugin_roots = Vec::new();
        for plugin in maven_standard_plugins() {
            let coords = ArtifactCoordinates::new(
                plugin.group_id.clone(),
                plugin.artifact_id.clone(),
                plugin.version.clone(),
            );
            let key = (
                coords.group_id.clone(),
                coords.artifact_id.clone(),
                coords.classifier.clone(),
            );
            if seen.insert(key) {
                plugin_roots.push(coords);
            }
        }
        if roots.len() != direct.len() {
            eprintln!(
                "[maven-compat-debug] roots_dedup_mismatch roots={} direct={} (duplicates dropped)",
                roots.len(),
                direct.len()
            );
        }
        let base_summary: Vec<String> = roots
            .iter()
            .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
            .collect();
        let plugin_summary: Vec<String> = plugin_roots
            .iter()
            .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
            .collect();
        eprintln!(
            "[maven-compat-debug] roots_list=[{}] plugin_roots=[{}]",
            base_summary.join(","),
            plugin_summary.join(",")
        );
        let base_closure = resolver
            .resolve_closure_with_options(&roots, ClosureOptions::base())
            .map_err(|error| ResolverError::Conflict {
                dependency: "resolver".into(),
                details: error.to_string(),
            })?;
        let plugin_closure = resolver
            .resolve_closure_with_options(&plugin_roots, ClosureOptions::plugin_download())
            .map_err(|error| ResolverError::Conflict {
                dependency: "resolver".into(),
                details: error.to_string(),
            })?;

        let mut download_plan: Vec<ArtifactCoordinates> = Vec::new();
        let mut seen_download = HashSet::new();
        for coords in base_closure.iter() {
            if seen_download.insert((
                coords.group_id.clone(),
                coords.artifact_id.clone(),
                coords.classifier.clone(),
                coords.version.clone(),
            )) {
                download_plan.push(coords.clone());
            }
        }
        for coords in plugin_closure.iter() {
            if seen_download.insert((
                coords.group_id.clone(),
                coords.artifact_id.clone(),
                coords.classifier.clone(),
                coords.version.clone(),
            )) {
                download_plan.push(coords.clone());
            }
        }
        let download_roots_summary = format!(
            "base_roots={} plugin_roots={} base_closure={} plugin_closure={}",
            roots.len(),
            plugin_roots.len(),
            base_closure.len(),
            plugin_closure.len()
        );

        eprintln!(
            "[maven-compat-debug] download_plan_size={} roots_info={}",
            download_plan.len(),
            download_roots_summary
        );
        // サンプルでダウンロード計画の最初と最後の数件を出力（多すぎる場合は一部のみ）
        if !download_plan.is_empty() {
            let head: Vec<String> = download_plan
                .iter()
                .take(5)
                .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
                .collect();
            let tail: Vec<String> = download_plan
                .iter()
                .rev()
                .take(5)
                .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
                .collect();
            eprintln!(
                "[maven-compat-debug] download_plan_head=[{}] download_plan_tail=[{}]",
                head.join(","),
                tail.into_iter().rev().collect::<Vec<_>>().join(",")
            );
        }

        let targets = jar_downloader.artifact_targets(&download_plan);
        eprintln!(
            "[maven-compat-debug] targets={} local_repo={}",
            targets.len(),
            ctx.local_repository.display()
        );
        jar_downloader.ensure_artifacts(&runtime, &targets)?;
        let present = targets.iter().filter(|(_, path)| path.exists()).count();
        eprintln!(
            "[maven-compat-debug] artifacts_present_after_download={}/{}",
            present,
            targets.len()
        );
        let jar_locations: HashMap<ArtifactCoordinates, PathBuf> =
            targets.iter().cloned().collect();

        let mut seen = HashSet::new();
        let mut dependencies: Vec<ResolvedDependency> = Vec::new();
        for coords in base_closure.into_iter() {
            let name = format!("{}:{}", coords.group_id, coords.artifact_id);
            if !seen.insert(name.clone()) {
                continue;
            }
            if !base_names.contains(&name) {
                continue;
            }
            let local = jar_locations
                .get(&coords)
                .map(|path| path.to_string_lossy().to_string());
            dependencies.push(ResolvedDependency {
                name,
                requested: coords.version.clone(),
                decision: VersionDecision::Exact(coords.version.clone()),
                scope: DependencyScope::Main,
                source: ResolutionSource::Registry,
                local_artifact: local,
            });
        }

        let stats = ResolutionStats::new(
            start.elapsed().as_millis(),
            dependencies.len(),
            dependencies.len(),
        );

        Ok(ResolvedDependencies {
            strategy: info.name.clone(),
            algorithm: info.algorithm,
            dependencies,
            diagnostics: vec![ResolutionDiagnostic::info(
                "MAVEN3900",
                "Resolved dependencies via Maven 3.9 compatibility strategy",
            )],
            stats,
        })
    }
}

pub fn dependency_coordinates_for_jar(
    dependency: &ResolvedDependency,
) -> Option<ArtifactCoordinates> {
    let version = match &dependency.decision {
        VersionDecision::Exact(value) => value.clone(),
        _ => return None,
    };

    let (group, artifact) = dependency.name.split_once(':')?;
    Some(ArtifactCoordinates::new(
        group.to_string(),
        artifact.to_string(),
        version,
    ))
}

pub fn hydrate_resolved_dependencies_with_jars(
    runtime: Option<&tokio::runtime::Runtime>,
    context: &MavenResolverContext,
    resolved: &mut ResolvedDependencies,
) -> Result<(), ResolverError> {
    if resolved.dependencies.is_empty() {
        return Ok(());
    }

    fs::create_dir_all(&context.local_repository).map_err(|error| {
        repository_conflict(
            "local-repository",
            format!(
                "{} の作成に失敗しました: {error}",
                context.local_repository.display()
            ),
        )
    })?;

    let mut coordinates = Vec::new();
    let mut dependency_indexes = Vec::new();
    for (index, dependency) in resolved.dependencies.iter().enumerate() {
        if let Some(coords) = dependency_coordinates_for_jar(dependency) {
            coordinates.push(coords);
            dependency_indexes.push(index);
        } else {
            tracing::debug!(
                dependency = %dependency.name,
                "Jar ダウンロードをスキップします（座標またはバージョン未確定）"
            );
        }
    }

    if coordinates.is_empty() {
        return Ok(());
    }

    let registries = registries_from_context(context);
    let downloader = MavenJarDownloader::new(context.local_repository.clone(), registries.clone());

    let runtime_guard = match runtime {
        Some(_) => None,
        None => Some(
            RuntimeBuilder::new_multi_thread()
                .enable_all()
                .build()
                .map_err(|error| ResolverError::Conflict {
                    dependency: "runtime".into(),
                    details: error.to_string(),
                })?,
        ),
    };
    let runtime_ref = runtime
        .or_else(|| runtime_guard.as_ref())
        .expect("runtime to exist");

    let targets = downloader.artifact_targets(&coordinates);
    downloader.ensure_artifacts(runtime_ref, &targets)?;
    let jar_locations: HashMap<ArtifactCoordinates, PathBuf> = targets.into_iter().collect();

    for (index, coords) in dependency_indexes.into_iter().zip(coordinates) {
        if let Some(path) = jar_locations.get(&coords) {
            resolved.dependencies[index].local_artifact = Some(path.to_string_lossy().to_string());
        }
    }

    Ok(())
}

// -----------------------------------------------------------------------------
// Maven 3.9 POM 解析ロジック（元: maven/dependency_graph.rs）
// -----------------------------------------------------------------------------

pub use pom_resolver::{MavenDependencyResolver, PomMetadata};

mod pom_resolver {
    use std::borrow::Cow;
    use std::collections::{HashMap, HashSet, VecDeque};
    use std::sync::Arc;

    use indexmap::IndexSet;
    use roxmltree::{Document, Node};

    use crate::cache::DependencyCache;
    use crate::registry::{ArtifactCoordinates, MavenRegistry, RegistryError};
    use crate::wrapper::error::WrapperError;
    use crate::{DependencyScope, RequestedDependency};

    /// Resolves the full Maven dependency graph (including transitives) for a set of
    /// root artifacts by reading and interpreting their POM files.
    pub struct MavenDependencyResolver<'a> {
        runtime: &'a tokio::runtime::Runtime,
        cache: Arc<DependencyCache>,
        registries: Vec<Arc<MavenRegistry>>,
    }

    pub struct PomMetadata {
        pub properties: HashMap<String, String>,
        pub plugin_management: HashMap<(String, String), String>,
    }

    /// Extract direct dependencies from a pom.xml (no transitive expansion).
    pub fn parse_direct_dependencies(
        contents: &str,
    ) -> Result<Vec<RequestedDependency>, WrapperError> {
        let model = PomModel::parse(contents)?;
        let mut deps = Vec::new();
        for dep in model.dependencies {
            let Some(group) = dep.group_id else { continue };
            let Some(artifact) = dep.artifact_id else {
                continue;
            };
            let Some(version) = dep.version else { continue };
            deps.push(RequestedDependency {
                name: format!("{group}:{artifact}"),
                requirement: version,
                scope: DependencyScope::Main,
            });
        }
        Ok(deps)
    }

    #[derive(Clone, Copy)]
    pub(super) struct ClosureOptions {
        include_optional: bool,
        /// When false, only compile/runtime scopes are traversed. When true, provided/test are also
        /// followed.
        allow_all_scopes: bool,
        /// When true, multiple versions of the same GA(+classifier) are kept (plugin/tooling paths
        /// can legitimately require different versions simultaneously).
        allow_multiple_versions: bool,
    }

    impl ClosureOptions {
        pub(super) fn base() -> Self {
            Self {
                include_optional: false,
                allow_all_scopes: false,
                allow_multiple_versions: false,
            }
        }

        /// Plugin/tooling 用のダウンロード展開。provided/test/optional を含め、複数バージョン共存を許容する。
        pub(super) fn plugin_download() -> Self {
            Self {
                include_optional: true,
                allow_all_scopes: true,
                allow_multiple_versions: true,
            }
        }
    }

    impl<'a> MavenDependencyResolver<'a> {
        pub fn new(
            runtime: &'a tokio::runtime::Runtime,
            cache: Arc<DependencyCache>,
            registries: Vec<Arc<MavenRegistry>>,
        ) -> Self {
            Self {
                runtime,
                cache,
                registries,
            }
        }

        /// Returns the closure of every dependency reachable from the provided root
        /// artifacts. The returned list preserves insertion order and de-duplicates
        /// coordinates.
        #[allow(dead_code)]
        pub(super) fn resolve_closure(
            &self,
            roots: &[ArtifactCoordinates],
        ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
            self.resolve_closure_with_options(roots, ClosureOptions::base())
        }

        /// Resolves the dependency closure with configurable optional/scope handling.
        pub(super) fn resolve_closure_with_options(
            &self,
            roots: &[ArtifactCoordinates],
            options: ClosureOptions,
        ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
            let include_optional = options.include_optional;
            let allow_all_scopes = options.allow_all_scopes;
            let allow_multiple_versions = options.allow_multiple_versions;
            let mut scope_counts: HashMap<String, usize> = HashMap::new();
            let mut edge_log_emitted = 0usize;
            let max_edge_log = 20usize;
            let mut pom_log_emitted = 0usize;
            let mut optional_seen = 0usize;
            let mut optional_enqueued = 0usize;
            let mut skipped_scope = 0usize;
            let mut optional_added_samples: Vec<String> = Vec::new();
            let mut ordered = IndexSet::new();
            let mut memo = HashMap::new();
            // Maven の nearest-wins: 最短経路（層の浅い依存）を優先するため BFS で展開する。
            // 依存管理（dependencyManagement）で指定された版は深さに関わらず優先する。
            let mut ga_versions: HashMap<(String, String, Option<String>), (String, usize)> =
                HashMap::new();
            let mut seen_versions: HashSet<(String, String, Option<String>, String)> =
                HashSet::new();
            let mut queue: VecDeque<(ArtifactCoordinates, Vec<(String, String)>, usize)> =
                VecDeque::new();
            for root in roots {
                let key = (
                    root.group_id.clone(),
                    root.artifact_id.clone(),
                    root.classifier.clone(),
                );
                if allow_multiple_versions {
                    ordered.insert(root.clone());
                    queue.push_back((root.clone(), Vec::new(), 0));
                } else {
                    if let Some((existing_version, existing_depth)) = ga_versions.get(&key) {
                        if existing_version == &root.version && *existing_depth <= 0 {
                            continue;
                        }
                    }
                    ga_versions.insert(key.clone(), (root.version.clone(), 0));
                    ordered.insert(root.clone());
                    queue.push_back((root.clone(), Vec::new(), 0));
                }
            }

            while let Some((coords, exclusions, depth)) = queue.pop_front() {
                let key = (
                    coords.group_id.clone(),
                    coords.artifact_id.clone(),
                    coords.classifier.clone(),
                );
                if allow_multiple_versions {
                    let version_key = (
                        key.0.clone(),
                        key.1.clone(),
                        key.2.clone(),
                        coords.version.clone(),
                    );
                    if !seen_versions.insert(version_key) {
                        continue;
                    }
                } else if let Some((best_version, best_depth)) = ga_versions.get(&key) {
                    if *best_version != coords.version || *best_depth < depth {
                        continue;
                    }
                }
                let effective =
                    self.load_effective_pom(coords.clone(), &mut memo, &mut HashSet::new())?;

                if pom_log_emitted < max_edge_log {
                    let samples: Vec<String> = effective
                        .dependencies
                        .iter()
                        .take(5)
                        .map(|dep| {
                            format!(
                                "{}:{}:{}@{:?}/opt={}",
                                dep.coordinates.group_id,
                                dep.coordinates.artifact_id,
                                dep.coordinates.version,
                                dep.scope,
                                dep.optional
                            )
                        })
                        .collect();
                    eprintln!(
                        "[maven-compat-debug] pom_deps coords={}:{}:{} depth={} count={} allow_all_scopes={} include_optional={} allow_multi={} samples=[{}]",
                        coords.group_id,
                        coords.artifact_id,
                        coords.version,
                        depth,
                        effective.dependencies.len(),
                        allow_all_scopes,
                        include_optional,
                        allow_multiple_versions,
                        samples.join(",")
                    );
                    pom_log_emitted += 1;
                }

                for dependency in &effective.dependencies {
                    let scope_label = dependency.scope.as_deref().unwrap_or("compile").to_string();
                    *scope_counts.entry(scope_label.clone()).or_insert(0) += 1;

                    if dependency.optional && !include_optional {
                        optional_seen += 1;
                        eprintln!(
                            "[maven-compat-debug] skip_optional coords={}:{}:{}",
                            dependency.coordinates.group_id,
                            dependency.coordinates.artifact_id,
                            dependency.coordinates.version
                        );
                        continue;
                    }
                    if !allow_all_scopes {
                        if !matches!(
                            dependency.scope.as_deref(),
                            None | Some("compile") | Some("runtime")
                        ) {
                            skipped_scope += 1;
                            eprintln!(
                                "[maven-compat-debug] skip_scope coords={}:{}:{} scope={:?}",
                                dependency.coordinates.group_id,
                                dependency.coordinates.artifact_id,
                                dependency.coordinates.version,
                                dependency.scope
                            );
                            continue;
                        }
                    }
                    if is_excluded(&dependency.coordinates, &exclusions) {
                        continue;
                    }

                    let mut next_exclusions = exclusions.clone();
                    next_exclusions.extend(
                        dependency
                            .exclusions
                            .iter()
                            .map(|ex| (ex.group_id.clone(), ex.artifact_id.clone())),
                    );
                    let mut coords = dependency.coordinates.clone();
                    let key = (
                        coords.group_id.clone(),
                        coords.artifact_id.clone(),
                        coords.classifier.clone(),
                    );

                    let managed_version = effective
                        .dependency_management
                        .get(&(coords.group_id.clone(), coords.artifact_id.clone()))
                        .map(|managed| managed.version.clone());
                    if let Some(version) = managed_version.as_ref() {
                        if coords.version != *version {
                            coords.version = version.clone();
                        }
                    }

                    if allow_multiple_versions {
                        ordered.insert(coords.clone());
                        queue.push_back((coords, next_exclusions, depth + 1));
                        if dependency.optional {
                            optional_enqueued += 1;
                            if optional_added_samples.len() < 10 {
                                optional_added_samples.push(format!(
                                    "{}:{}:{}",
                                    dependency.coordinates.group_id,
                                    dependency.coordinates.artifact_id,
                                    dependency.coordinates.version
                                ));
                            }
                        }
                        if edge_log_emitted < max_edge_log {
                            eprintln!(
                                "[maven-compat-debug] enqueue_edge parent={}:{}:{} -> child={}:{}:{} scope={:?} optional={}",
                                dependency.coordinates.group_id,
                                dependency.coordinates.artifact_id,
                                dependency.coordinates.version,
                                dependency.coordinates.group_id,
                                dependency.coordinates.artifact_id,
                                dependency.coordinates.version,
                                dependency.scope,
                                dependency.optional
                            );
                            edge_log_emitted += 1;
                        }
                    } else {
                        let mut should_replace = false;
                        match ga_versions.get(&key) {
                            None => {
                                should_replace = true;
                            }
                            Some((existing_version, existing_depth)) => {
                                // dependencyManagement が指定されている場合はバージョンを強制的に刷新する。
                                if let Some(managed) = managed_version.as_ref() {
                                    if managed != existing_version {
                                        should_replace = true;
                                        eprintln!(
                                            "[maven-compat-debug] managed_override ga={}:{:?} existing={} managed={} depth={}->{}",
                                            coords.group_id,
                                            coords.artifact_id,
                                            existing_version,
                                            managed,
                                            existing_depth,
                                            depth
                                        );
                                    } else if *existing_depth > depth {
                                        should_replace = true;
                                    }
                                } else if existing_version != &coords.version {
                                    // 異なるバージョンは深さに関わらず置換して差分脱落を防ぐ
                                    should_replace = true;
                                    eprintln!(
                                        "[maven-compat-debug] replace_version ga={}:{:?} existing={} new={} depth={}->{}",
                                        coords.group_id,
                                        coords.artifact_id,
                                        existing_version,
                                        coords.version,
                                        existing_depth,
                                        depth
                                    );
                                } else if *existing_depth > depth {
                                    // より浅い経路を優先
                                    should_replace = true;
                                } else if existing_version == &coords.version {
                                    // 同一バージョンで深さも優先されない場合はスキップ
                                    continue;
                                }
                            }
                        }

                        if should_replace {
                            // 既存の同一 GA (+classifier) を閉包から取り除いたうえで最新版を登録する。
                            ordered.retain(|item| {
                                !(item.group_id == coords.group_id
                                    && item.artifact_id == coords.artifact_id
                                    && item.classifier == coords.classifier)
                            });
                            ga_versions.insert(key.clone(), (coords.version.clone(), depth + 1));
                            ordered.insert(coords.clone());
                            queue.push_back((coords, next_exclusions, depth + 1));
                            if dependency.optional {
                                optional_enqueued += 1;
                                if optional_added_samples.len() < 10 {
                                    optional_added_samples.push(format!(
                                        "{}:{}:{}",
                                        dependency.coordinates.group_id,
                                        dependency.coordinates.artifact_id,
                                        dependency.coordinates.version
                                    ));
                                }
                            }
                            if edge_log_emitted < max_edge_log {
                                eprintln!(
                                    "[maven-compat-debug] enqueue_edge parent={}:{}:{} -> child={}:{}:{} scope={:?} optional={}",
                                    dependency.coordinates.group_id,
                                    dependency.coordinates.artifact_id,
                                    dependency.coordinates.version,
                                    dependency.coordinates.group_id,
                                    dependency.coordinates.artifact_id,
                                    dependency.coordinates.version,
                                    dependency.scope,
                                    dependency.optional
                                );
                                edge_log_emitted += 1;
                            }
                        } else {
                            eprintln!(
                                "[maven-compat-debug] retain_existing ga={}:{} version={} depth={}",
                                coords.group_id, coords.artifact_id, coords.version, depth
                            );
                        }
                    }
                }
            }

            eprintln!(
                "[maven-compat-debug] closure_finalize len={} optional_seen={} optional_enqueued={} skipped_scope={}",
                ordered.len(),
                optional_seen,
                optional_enqueued,
                skipped_scope
            );
            if !scope_counts.is_empty() {
                let mut parts: Vec<String> = scope_counts
                    .iter()
                    .map(|(k, v)| format!("{k}:{v}"))
                    .collect();
                parts.sort();
                eprintln!("[maven-compat-debug] scope_counts={}", parts.join(","));
            }
            if !optional_added_samples.is_empty() {
                eprintln!(
                    "[maven-compat-debug] optional_enqueued_samples=[{}]",
                    optional_added_samples.join(",")
                );
            }
            if allow_multiple_versions {
                let mut coords: Vec<String> = ordered
                    .iter()
                    .map(|c| format!("{}:{}:{}@{}", c.group_id, c.artifact_id, c.version, 0))
                    .collect();
                coords.sort();
                if coords.len() > 50 {
                    coords.truncate(50);
                }
                eprintln!(
                    "[maven-compat-debug] ga_versions size={} samples=[{}]",
                    ordered.len(),
                    coords.join(",")
                );
            } else if !ga_versions.is_empty() {
                let mut versions: Vec<String> = ga_versions
                    .iter()
                    .map(|((group, artifact, classifier), (version, depth))| {
                        let id = if let Some(classifier) = classifier {
                            format!("{group}:{artifact}:{classifier}")
                        } else {
                            format!("{group}:{artifact}")
                        };
                        format!("{id}={version}@{depth}")
                    })
                    .collect();
                versions.sort();
                if versions.len() > 50 {
                    versions.truncate(50);
                }
                eprintln!(
                    "[maven-compat-debug] ga_versions size={} samples=[{}]",
                    ga_versions.len(),
                    versions.join(",")
                );
            }

            Ok(ordered.into_iter().collect())
        }

        pub fn metadata_for(
            &self,
            coords: &ArtifactCoordinates,
        ) -> Result<PomMetadata, WrapperError> {
            let mut memo = HashMap::new();
            let mut stack = HashSet::new();
            let effective = self.load_effective_pom(coords.clone(), &mut memo, &mut stack)?;
            let plugin_management = effective
                .plugin_management
                .iter()
                .map(|((group, artifact), managed)| {
                    ((group.clone(), artifact.clone()), managed.version.clone())
                })
                .collect();
            Ok(PomMetadata {
                properties: effective.properties.clone(),
                plugin_management,
            })
        }

        fn load_effective_pom(
            &self,
            coords: ArtifactCoordinates,
            memo: &mut HashMap<ArtifactCoordinates, Arc<EffectivePom>>,
            parent_stack: &mut HashSet<ArtifactCoordinates>,
        ) -> Result<Arc<EffectivePom>, WrapperError> {
            if let Some(existing) = memo.get(&coords) {
                return Ok(existing.clone());
            }

            if !parent_stack.insert(coords.clone()) {
                return Err(WrapperError::OperationFailed(format!(
                    "親POMの解決中に循環が検出されました: {}",
                    coords
                )));
            }

            let pom_text = self.fetch_pom(&coords)?;
            let mut model = PomModel::parse(&pom_text)?;

            let parent_effective = if let Some(parent) = &model.parent {
                let parent_coords = ArtifactCoordinates::new(
                    parent.group_id.clone(),
                    parent.artifact_id.clone(),
                    parent.version.clone(),
                );
                Some(self.load_effective_pom(parent_coords, memo, parent_stack)?)
            } else {
                None
            };

            let mut property_context = parent_effective
                .as_ref()
                .map(|parent| parent.properties.clone())
                .unwrap_or_default();
            property_context.extend(model.properties.clone());
            property_context.insert("project.groupId".to_string(), coords.group_id.clone());
            property_context.insert("project.artifactId".to_string(), coords.artifact_id.clone());
            property_context.insert("project.version".to_string(), coords.version.clone());
            if let Some(parent_pom) = parent_effective.as_deref() {
                property_context.insert(
                    "project.parent.groupId".to_string(),
                    parent_pom.coordinates.group_id.clone(),
                );
                property_context.insert(
                    "project.parent.artifactId".to_string(),
                    parent_pom.coordinates.artifact_id.clone(),
                );
                property_context.insert(
                    "project.parent.version".to_string(),
                    parent_pom.coordinates.version.clone(),
                );
            }

            self.expand_dependency_management_imports(
                &mut model,
                &property_context,
                memo,
                parent_stack,
            )?;

            parent_stack.remove(&coords);

            let effective =
                EffectivePom::from_model(coords.clone(), model, parent_effective.as_deref())?;
            let shared = Arc::new(effective);
            memo.insert(coords, shared.clone());
            Ok(shared)
        }

        fn expand_dependency_management_imports(
            &self,
            model: &mut PomModel,
            properties: &HashMap<String, String>,
            memo: &mut HashMap<ArtifactCoordinates, Arc<EffectivePom>>,
            parent_stack: &mut HashSet<ArtifactCoordinates>,
        ) -> Result<(), WrapperError> {
            let mut retained = Vec::new();
            for entry in model.dependency_management.drain(..) {
                let is_import = matches!(
                    entry.dep_type.as_deref(),
                    Some(dep_type) if dep_type.eq_ignore_ascii_case("pom")
                ) && matches!(
                    entry.scope.as_deref(),
                    Some(scope) if scope.eq_ignore_ascii_case("import")
                );

                if !is_import {
                    retained.push(entry);
                    continue;
                }

                let group =
                    resolve_property(entry.group_id.as_deref(), properties).ok_or_else(|| {
                        WrapperError::OperationFailed(
                            "dependencyManagement import の groupId が未設定です".to_string(),
                        )
                    })?;
                let artifact = resolve_property(entry.artifact_id.as_deref(), properties)
                    .ok_or_else(|| {
                        WrapperError::OperationFailed(
                            "dependencyManagement import の artifactId が未設定です".to_string(),
                        )
                    })?;
                let version =
                    resolve_property(entry.version.as_deref(), properties).ok_or_else(|| {
                        WrapperError::OperationFailed(format!(
                            "{group}:{artifact} の dependencyManagement import にバージョンがありません"
                        ))
                    })?;

                let import_coords = ArtifactCoordinates::new(group, artifact, version);
                let imported = self.load_effective_pom(import_coords, memo, parent_stack)?;
                for ((managed_group, managed_artifact), managed) in
                    imported.dependency_management.iter()
                {
                    retained.push(PomDependency {
                        group_id: Some(managed_group.clone()),
                        artifact_id: Some(managed_artifact.clone()),
                        version: Some(managed.version.clone()),
                        scope: None,
                        optional: false,
                        exclusions: Vec::new(),
                        classifier: None,
                        dep_type: None,
                    });
                }
            }

            model.dependency_management = retained;
            Ok(())
        }

        fn fetch_pom(&self, coords: &ArtifactCoordinates) -> Result<String, WrapperError> {
            if let Some(cached) = self.cache.get_pom(coords).map_err(|error| {
                WrapperError::OperationFailed(format!(
                    "POMキャッシュの読み込みに失敗しました: {error}"
                ))
            })? {
                return Ok(cached.content);
            }

            let mut last_error: Option<RegistryError> = None;
            for registry in &self.registries {
                match self.runtime.block_on(registry.fetch_pom(coords)) {
                    Ok(text) => {
                        if let Err(error) = self.cache.store_pom(coords, &text) {
                            tracing::warn!(
                                artifact = %coords,
                                error = %error,
                                "POMをキャッシュへ保存できませんでした"
                            );
                        }
                        return Ok(text);
                    }
                    Err(error) => {
                        last_error = Some(error);
                    }
                }
            }

            Err(WrapperError::OperationFailed(match last_error {
                Some(error) => format!("{} の POM 取得に失敗しました: {error}", coords),
                None => format!("{} の POM を取得できるレジストリがありません", coords),
            }))
        }
    }

    fn is_excluded(coords: &ArtifactCoordinates, exclusions: &[(String, String)]) -> bool {
        exclusions
            .iter()
            .any(|(group, artifact)| group == &coords.group_id && artifact == &coords.artifact_id)
    }

    #[derive(Debug, Clone)]
    struct PomModel {
        _group_id: Option<String>,
        _artifact_id: Option<String>,
        _version: Option<String>,
        _packaging: Option<String>,
        parent: Option<PomParent>,
        properties: HashMap<String, String>,
        dependency_management: Vec<PomDependency>,
        dependencies: Vec<PomDependency>,
        plugin_management: Vec<PomPlugin>,
    }

    #[derive(Debug, Clone)]
    struct PomParent {
        group_id: String,
        artifact_id: String,
        version: String,
    }

    #[derive(Debug, Clone)]
    struct PomDependency {
        group_id: Option<String>,
        artifact_id: Option<String>,
        version: Option<String>,
        scope: Option<String>,
        optional: bool,
        exclusions: Vec<DependencyExclusion>,
        classifier: Option<String>,
        dep_type: Option<String>,
    }

    #[derive(Debug, Clone)]
    struct PomPlugin {
        group_id: Option<String>,
        artifact_id: Option<String>,
        version: Option<String>,
    }

    #[derive(Debug, Clone)]
    struct DependencyExclusion {
        group_id: String,
        artifact_id: String,
    }

    impl PomModel {
        fn parse(xml: &str) -> Result<Self, WrapperError> {
            let normalized = normalize_xml_entities(xml);
            let document = Document::parse(normalized.as_ref()).map_err(|error| {
                WrapperError::OperationFailed(format!("pom.xml の解析に失敗しました: {error}"))
            })?;
            let project = document
                .descendants()
                .find(|node| node.has_tag_name("project"))
                .ok_or_else(|| {
                    WrapperError::OperationFailed(
                        "pom.xml に <project> タグが存在しません".to_string(),
                    )
                })?;

            let group_id = node_text(&project, "groupId");
            let artifact_id = node_text(&project, "artifactId");
            let version = node_text(&project, "version");
            let packaging = node_text(&project, "packaging");
            let parent = project
                .children()
                .find(|node| node.is_element() && node.tag_name().name() == "parent")
                .map(parse_parent)
                .transpose()?;
            let properties = parse_properties(&project);
            let dependency_management = parse_dependency_group(&project, "dependencyManagement")?;
            let dependencies = parse_dependency_group(&project, "dependencies")?;
            let (plugin_management, _) = parse_plugin_sections(&project)?;

            Ok(Self {
                _group_id: group_id,
                _artifact_id: artifact_id,
                _version: version,
                _packaging: packaging,
                parent,
                properties,
                dependency_management,
                dependencies,
                plugin_management,
            })
        }
    }

    fn parse_parent(node: Node<'_, '_>) -> Result<PomParent, WrapperError> {
        let group_id = node_text(&node, "groupId").ok_or_else(|| {
            WrapperError::OperationFailed("parent.groupId が未指定です".to_string())
        })?;
        let artifact_id = node_text(&node, "artifactId").ok_or_else(|| {
            WrapperError::OperationFailed("parent.artifactId が未指定です".to_string())
        })?;
        let version = node_text(&node, "version").ok_or_else(|| {
            WrapperError::OperationFailed("parent.version が未指定です".to_string())
        })?;

        Ok(PomParent {
            group_id,
            artifact_id,
            version,
        })
    }

    fn parse_properties(node: &Node<'_, '_>) -> HashMap<String, String> {
        node.children()
            .find(|child| child.is_element() && child.tag_name().name() == "properties")
            .map(|props| {
                props
                    .children()
                    .filter(|child| child.is_element())
                    .filter_map(|prop| {
                        let key = prop.tag_name().name().to_string();
                        let value = prop.text().map(|text| text.trim().to_string())?;
                        Some((key, value))
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    fn parse_dependency_group(
        node: &Node<'_, '_>,
        group_tag: &str,
    ) -> Result<Vec<PomDependency>, WrapperError> {
        let group_node = node
            .children()
            .find(|child| child.is_element() && child.tag_name().name() == group_tag);

        if let Some(group) = group_node {
            let deps_parent = if group.tag_name().name() == "dependencies" {
                group
            } else {
                group
                    .children()
                    .find(|child| child.is_element() && child.tag_name().name() == "dependencies")
                    .ok_or_else(|| {
                        WrapperError::OperationFailed(format!(
                            "<{group_tag}> 内に <dependencies> タグが存在しません"
                        ))
                    })?
            };

            let mut deps = Vec::new();
            for dependency in deps_parent
                .children()
                .filter(|child| child.is_element() && child.tag_name().name() == "dependency")
            {
                deps.push(parse_dependency(dependency)?);
            }
            Ok(deps)
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_dependency(node: Node<'_, '_>) -> Result<PomDependency, WrapperError> {
        let exclusions = node
            .children()
            .find(|child| child.is_element() && child.tag_name().name() == "exclusions")
            .map(|exclusions_node| {
                exclusions_node
                    .children()
                    .filter(|child| child.is_element() && child.tag_name().name() == "exclusion")
                    .filter_map(|ex_node| {
                        let group = node_text(&ex_node, "groupId")?;
                        let artifact = node_text(&ex_node, "artifactId")?;
                        Some(DependencyExclusion {
                            group_id: group,
                            artifact_id: artifact,
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        Ok(PomDependency {
            group_id: node_text(&node, "groupId"),
            artifact_id: node_text(&node, "artifactId"),
            version: node_text(&node, "version"),
            scope: node_text(&node, "scope"),
            optional: node_text(&node, "optional")
                .map(|value| value.eq_ignore_ascii_case("true"))
                .unwrap_or(false),
            exclusions,
            classifier: node_text(&node, "classifier"),
            dep_type: node_text(&node, "type"),
        })
    }

    fn parse_plugin_sections(
        node: &Node<'_, '_>,
    ) -> Result<(Vec<PomPlugin>, Vec<PomPlugin>), WrapperError> {
        let mut management = Vec::new();
        let mut plugins = Vec::new();

        if let Some(build) = node
            .children()
            .find(|child| child.is_element() && child.tag_name().name() == "build")
        {
            management.extend(parse_plugin_group(&build, true)?);
            plugins.extend(parse_plugin_group(&build, false)?);
        }

        if let Some(reporting) = node
            .children()
            .find(|child| child.is_element() && child.tag_name().name() == "reporting")
        {
            plugins.extend(parse_plugin_group(&reporting, false)?);
        }

        Ok((management, plugins))
    }

    fn parse_plugin_group(
        node: &Node<'_, '_>,
        management: bool,
    ) -> Result<Vec<PomPlugin>, WrapperError> {
        let container = if management {
            node.children()
                .find(|child| child.is_element() && child.tag_name().name() == "pluginManagement")
                .and_then(|pm| {
                    pm.children()
                        .find(|child| child.is_element() && child.tag_name().name() == "plugins")
                })
        } else {
            node.children()
                .find(|child| child.is_element() && child.tag_name().name() == "plugins")
        };

        if let Some(plugins_node) = container {
            let mut plugins = Vec::new();
            for plugin in plugins_node
                .children()
                .filter(|child| child.is_element() && child.tag_name().name() == "plugin")
            {
                plugins.push(parse_plugin(plugin)?);
            }
            Ok(plugins)
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_plugin(node: Node<'_, '_>) -> Result<PomPlugin, WrapperError> {
        Ok(PomPlugin {
            group_id: node_text(&node, "groupId"),
            artifact_id: node_text(&node, "artifactId"),
            version: node_text(&node, "version"),
        })
    }

    fn node_text(node: &Node<'_, '_>, tag: &str) -> Option<String> {
        node.children()
            .find(|child| child.is_element() && child.tag_name().name() == tag)
            .and_then(|child| child.text())
            .map(|text| text.trim().to_string())
            .filter(|text| !text.is_empty())
    }

    fn normalize_xml_entities(input: &str) -> Cow<'_, str> {
        if !input.contains('&') {
            return Cow::Borrowed(input);
        }

        let mut output = String::with_capacity(input.len());
        let mut chars = input.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == '&' {
                let mut entity = String::new();
                while let Some(&next) = chars.peek() {
                    entity.push(next);
                    chars.next();
                    if next == ';' || entity.len() > 32 {
                        break;
                    }
                }

                if entity.ends_with(';') {
                    let name = &entity[..entity.len() - 1];
                    if name.eq_ignore_ascii_case("lt")
                        || name.eq_ignore_ascii_case("gt")
                        || name.eq_ignore_ascii_case("amp")
                        || name.eq_ignore_ascii_case("quot")
                        || name.eq_ignore_ascii_case("apos")
                        || name.starts_with('#')
                    {
                        output.push('&');
                        output.push_str(&entity);
                    } else {
                        output.push(' ');
                    }
                } else {
                    output.push('&');
                    output.push_str(&entity);
                }
            } else {
                output.push(ch);
            }
        }

        Cow::Owned(output)
    }

    #[derive(Debug, Clone)]
    struct EffectivePom {
        coordinates: ArtifactCoordinates,
        properties: HashMap<String, String>,
        dependency_management: HashMap<(String, String), ManagedDependency>,
        dependencies: Vec<ResolvedDependency>,
        plugin_management: HashMap<(String, String), ManagedPlugin>,
    }

    #[derive(Debug, Clone)]
    struct ManagedDependency {
        version: String,
        scope: Option<String>,
        optional: bool,
        classifier: Option<String>,
        _dep_type: Option<String>,
    }

    #[derive(Debug, Clone)]
    struct ManagedPlugin {
        version: String,
    }

    #[derive(Debug, Clone)]
    struct ResolvedDependency {
        coordinates: ArtifactCoordinates,
        scope: Option<String>,
        optional: bool,
        exclusions: Vec<DependencyExclusion>,
    }

    impl EffectivePom {
        fn from_model(
            coords: ArtifactCoordinates,
            model: PomModel,
            parent: Option<&EffectivePom>,
        ) -> Result<Self, WrapperError> {
            let mut properties = parent
                .map(|parent| parent.properties.clone())
                .unwrap_or_default();
            properties.extend(model.properties.into_iter());

            properties.insert("project.groupId".to_string(), coords.group_id.clone());
            properties.insert("project.artifactId".to_string(), coords.artifact_id.clone());
            properties.insert("project.version".to_string(), coords.version.clone());
            if let Some(parent_pom) = parent {
                properties.insert(
                    "project.parent.groupId".to_string(),
                    parent_pom.coordinates.group_id.clone(),
                );
                properties.insert(
                    "project.parent.artifactId".to_string(),
                    parent_pom.coordinates.artifact_id.clone(),
                );
                properties.insert(
                    "project.parent.version".to_string(),
                    parent_pom.coordinates.version.clone(),
                );
            }

            let mut dependency_management = parent
                .map(|parent| parent.dependency_management.clone())
                .unwrap_or_default();
            for entry in model.dependency_management {
                let group =
                    resolve_property(entry.group_id.as_deref(), &properties).ok_or_else(|| {
                        WrapperError::OperationFailed(
                            "dependencyManagement の groupId が未設定です".to_string(),
                        )
                    })?;
                let artifact = resolve_property(entry.artifact_id.as_deref(), &properties)
                    .ok_or_else(|| {
                        WrapperError::OperationFailed(
                            "dependencyManagement の artifactId が未設定です".to_string(),
                        )
                    })?;
                let Some(version) = resolve_property(entry.version.as_deref(), &properties) else {
                    tracing::warn!(
                        group = %group,
                        artifact = %artifact,
                        "dependencyManagement エントリに version がありません。スキップします。"
                    );
                    continue;
                };
                dependency_management.insert(
                    (group.clone(), artifact.clone()),
                    ManagedDependency {
                        version,
                        scope: entry.scope,
                        optional: entry.optional,
                        classifier: entry.classifier,
                        _dep_type: entry.dep_type,
                    },
                );
            }

            let mut resolved: Vec<ResolvedDependency> = Vec::new();
            for dependency in model.dependencies {
                let group = match resolve_property(dependency.group_id.as_deref(), &properties) {
                    Some(value) => value,
                    None => continue,
                };
                let artifact =
                    match resolve_property(dependency.artifact_id.as_deref(), &properties) {
                        Some(value) => value,
                        None => continue,
                    };
                let managed = dependency_management.get(&(group.clone(), artifact.clone()));
                let version = match resolve_property(dependency.version.as_deref(), &properties) {
                    Some(value) => value,
                    None => {
                        if let Some(managed) = managed {
                            managed.version.clone()
                        } else if matches!(
                            dependency.scope.as_deref(),
                            Some("compile") | Some("runtime") | None
                        ) {
                            return Err(WrapperError::OperationFailed(format!(
                                "{}:{} のバージョンを特定できません",
                                group, artifact
                            )));
                        } else {
                            continue;
                        }
                    }
                };

                let mut coords = ArtifactCoordinates::new(group, artifact, version);
                let classifier = dependency
                    .classifier
                    .clone()
                    .or_else(|| managed.and_then(|value| value.classifier.clone()));
                if let Some(classifier) = classifier {
                    coords = coords.with_classifier(classifier);
                }

                let scope = dependency
                    .scope
                    .clone()
                    .or_else(|| managed.and_then(|value| value.scope.clone()));
                let optional = if dependency.optional {
                    true
                } else {
                    managed.map(|value| value.optional).unwrap_or(false)
                };

                resolved.push(ResolvedDependency {
                    coordinates: coords,
                    scope,
                    optional,
                    exclusions: dependency.exclusions.clone(),
                });
            }

            let mut plugin_management = parent
                .map(|parent| parent.plugin_management.clone())
                .unwrap_or_default();
            for plugin in model.plugin_management {
                let group = resolve_plugin_group(plugin.group_id.as_deref(), &properties);
                let artifact = resolve_property(plugin.artifact_id.as_deref(), &properties)
                    .ok_or_else(|| {
                        WrapperError::OperationFailed(
                            "pluginManagement の artifactId が未設定です".to_string(),
                        )
                    })?;
                let Some(version) = resolve_property(plugin.version.as_deref(), &properties) else {
                    continue;
                };
                plugin_management
                    .insert((group.clone(), artifact.clone()), ManagedPlugin { version });
            }

            Ok(Self {
                coordinates: coords,
                properties,
                dependency_management,
                dependencies: resolved,
                plugin_management,
            })
        }
    }

    fn resolve_property(
        value: Option<&str>,
        properties: &HashMap<String, String>,
    ) -> Option<String> {
        let mut current = value?.trim().to_string();
        if current.is_empty() {
            return None;
        }

        if !current.contains("${") {
            return Some(current);
        }

        let mut attempts = 0;
        while current.contains("${") {
            attempts += 1;
            if attempts > 8 {
                return None;
            }
            current = resolve_placeholders(&current, properties).ok()?;
        }

        Some(current)
    }

    fn resolve_plugin_group(value: Option<&str>, properties: &HashMap<String, String>) -> String {
        resolve_property(value, properties)
            .filter(|group| !group.is_empty())
            .unwrap_or_else(|| "org.apache.maven.plugins".to_string())
    }

    fn resolve_placeholders(
        raw: &str,
        properties: &HashMap<String, String>,
    ) -> Result<String, WrapperError> {
        let mut result = String::new();
        let mut rest = raw;
        while let Some(start) = rest.find("${") {
            result.push_str(&rest[..start]);
            let suffix = &rest[start + 2..];
            let end = suffix.find('}').ok_or_else(|| {
                WrapperError::OperationFailed(format!("プロパティ参照が閉じていません: {raw}"))
            })?;
            let key = &suffix[..end];
            let replacement = properties.get(key).ok_or_else(|| {
                WrapperError::OperationFailed(format!("プロパティ '{key}' が未定義です"))
            })?;
            result.push_str(replacement);
            rest = &suffix[end + 1..];
        }
        result.push_str(rest);
        Ok(result)
    }

    // -------------------------------------------------------------------------
    // Tests
    // -------------------------------------------------------------------------
    #[cfg(test)]
    mod tests;
}
