use crate::DependencyCache;
use crate::maven::{MavenMirrorConfig, MavenRepositoryConfig};
use crate::registry::{
    ArtifactCoordinates, ArtifactResource, ChecksumAlgorithm, ChecksumVerifiedJar, MavenRegistry,
    RegistryError,
};
use crate::resolver::{
    DependencyProvider, DependencyScope, Manifest, MavenResolverContext, RequestedDependency,
    ResolutionDiagnostic, ResolutionSource, ResolutionStats, ResolvedDependencies,
    ResolvedDependency, ResolverAlgorithmKind, ResolverError, ResolverOptions, ResolverStrategy,
    ResolverStrategyInfo, StrategyStability, VersionDecision,
};
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

fn is_lifecycle_bound_plugin(coords: &ArtifactCoordinates) -> bool {
    crate::repository::defaults::maven_lifecycle_bound_plugins()
        .iter()
        .any(|plugin| plugin.artifact_id == coords.artifact_id)
}

pub(crate) fn resolve_plugin_closure(
    resolver: &pom_resolver::MavenDependencyResolver,
    plugin_roots: &[ArtifactCoordinates],
) -> Result<Vec<ArtifactCoordinates>, ResolverError> {
    // Maven downloads transitive deps for lifecycle-bound plugins, but only roots for standalone plugins.
    // Split plugins into lifecycle-bound (full resolution) and standalone (roots only).
    let (lifecycle_plugins, standalone_plugins): (Vec<_>, Vec<_>) = plugin_roots
        .iter()
        .cloned()
        .partition(is_lifecycle_bound_plugin);

    let mut result = Vec::new();
    let mut seen = std::collections::HashSet::new();

    // Resolve full transitive deps for lifecycle-bound plugins
    if !lifecycle_plugins.is_empty() {
        let lifecycle_closure = pom_resolver::resolve_union_per_root(
            resolver,
            &lifecycle_plugins,
            pom_resolver::ClosureOptions::plugin_download(),
        )
        .map_err(|error| ResolverError::Conflict {
            dependency: "lifecycle-plugin-resolver".into(),
            details: error.to_string(),
        })?;
        pom_resolver::append_artifacts(&mut result, &lifecycle_closure, &mut seen);
    }

    // Only include roots for standalone plugins (no transitive deps)
    pom_resolver::append_artifacts(&mut result, &standalone_plugins, &mut seen);

    Ok(result)
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
        // Wrapperモードでは base_dependencies を最優先で seeds にする。空なら pom.xml から取得。
        let base_only = !ctx.base_dependencies.is_empty();
        let mut direct = if base_only {
            let mut deps = ctx.base_dependencies.clone();
            // wrapper-default でも pom.xml 由来の直接依存を seeds に含める（Maven dependency:resolve に揃える）
            let mut pom_direct = provider.direct_dependencies();
            if !pom_direct.is_empty() {
                deps.append(&mut pom_direct);
            }
            deps
        } else {
            provider.direct_dependencies()
        };

        let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), registries.clone());

        let mut roots = Vec::new();
        let mut base_names: HashSet<String> = HashSet::new();
        let mut seen: HashSet<(String, String, Option<String>)> = HashSet::new();
        fn collect_roots(
            deps: &[RequestedDependency],
            roots: &mut Vec<ArtifactCoordinates>,
            base_names: &mut HashSet<String>,
            seen: &mut HashSet<(String, String, Option<String>)>,
        ) {
            for dep in deps {
                if let Some((group, artifact)) = dep.name.split_once(':') {
                    let ver = dep.requirement.clone();
                    let coords =
                        ArtifactCoordinates::new(group.to_string(), artifact.to_string(), ver);
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
        }
        collect_roots(&direct, &mut roots, &mut base_names, &mut seen);

        if roots.is_empty() {
            let pom_direct = provider.direct_dependencies();
            if !pom_direct.is_empty() {
                eprintln!(
                    "[maven-compat-debug] roots empty; falling back to pom.xml direct_dependencies ({} items)",
                    pom_direct.len()
                );
                direct = pom_direct;
                base_names.clear();
                seen.clear();
                roots.clear();
                collect_roots(&direct, &mut roots, &mut base_names, &mut seen);
            }
        }

        eprintln!(
            "[maven-compat-debug] direct_dependencies={} (base_only={}) base_dep_names=[{}]",
            direct.len(),
            base_only,
            direct
                .iter()
                .map(|d| d.name.clone())
                .collect::<Vec<_>>()
                .join(",")
        );
        // Maven baseline に含まれる標準プラグインを seeds として投入し、依存も展開する。
        let plugin_roots: Vec<ArtifactCoordinates> = crate::wrapper::plugins::standard_plugins()
            .iter()
            .map(|p| p.to_artifact())
            .collect();
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
        eprintln!(
            "[maven-compat-debug] roots_list=[{}] plugin_roots=[{}]",
            base_summary.join(","),
            plugin_roots
                .iter()
                .map(|c| format!("{}:{}:{}", c.group_id, c.artifact_id, c.version))
                .collect::<Vec<_>>()
                .join(",")
        );
        // Maven dependency:resolve 実績に合わせ、wrapper seeds では provided/test を辿り、同一GAの複数版も保持する。
        let base_closure = pom_resolver::resolve_union_per_root(&resolver, &roots, ClosureOptions::base())
        .map_err(|error| ResolverError::Conflict {
            dependency: "resolver".into(),
            details: error.to_string(),
        })?;
        let plugin_closure = resolve_plugin_closure(&resolver, &plugin_roots)?;

        let mut download_plan: Vec<ArtifactCoordinates> = Vec::new();
        let mut seen_download = HashSet::new();
        // managed_artifacts (dependencyManagement相当) を download_plan に強制的に含める
        let managed: Vec<ArtifactCoordinates> = crate::wrapper::plugins::managed_artifacts()
            .iter()
            .map(|coord| coord.clone())
            .collect();

        // Deduplicate by full GAV - Maven keeps different versions from different contexts
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
        // plugin transitive deps を追加
        // Maven は plugin deps を lazy に取得するが、wrapper mode では pre-cache する
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
        pom_resolver::append_artifacts(&mut download_plan, &plugin_roots, &mut seen_download);
        pom_resolver::append_managed_artifacts(&mut download_plan, &managed, &mut seen_download);
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
        for coords in base_closure.into_iter().chain(plugin_closure.into_iter()) {
            let name = format!("{}:{}", coords.group_id, coords.artifact_id);
            if !seen.insert(name.clone()) {
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

pub use pom_resolver::{
    ClosureOptions, ConflictStrategy, MavenDependencyResolver, PomMetadata, compare_maven_versions,
    version_satisfies_range,
};

mod pom_resolver {
    use std::borrow::Cow;
    use std::cmp::Ordering;
    use std::collections::{HashMap, HashSet, VecDeque};
    use std::sync::Arc;

    use indexmap::IndexSet;
    use roxmltree::{Document, Node};

    use crate::cache::DependencyCache;
    use crate::registry::{ArtifactCoordinates, MavenRegistry, RegistryError};
    use crate::wrapper::error::WrapperError;
    use crate::{DependencyScope, RequestedDependency};
    use semver::Version;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum VersionToken {
        Num(u64),
        Qualifier(String),
    }

    fn normalize_qualifier(raw: &str) -> String {
        match raw.to_ascii_lowercase().as_str() {
            "a" => "alpha".to_string(),
            "b" => "beta".to_string(),
            "m" => "milestone".to_string(),
            "cr" => "rc".to_string(),
            "ga" | "final" | "release" => "".to_string(),
            other => other.to_string(),
        }
    }

    fn qualifier_rank(q: &str) -> (i32, String) {
        let normalized = normalize_qualifier(q);
        let rank = match normalized.as_str() {
            "alpha" => 1,
            "beta" => 2,
            "milestone" => 3,
            "rc" => 4,
            "snapshot" => 5,
            "" => 6,
            "sp" => 7,
            _ => 8,
        };
        (rank, normalized)
    }

    fn tokenize_version(input: &str) -> Vec<VersionToken> {
        let mut tokens = Vec::new();
        let mut current = String::new();
        let mut current_is_digit: Option<bool> = None;

        for ch in input.chars() {
            if ch == '.' || ch == '-' {
                if !current.is_empty() {
                    tokens.push(if current_is_digit.unwrap_or(false) {
                        VersionToken::Num(current.parse().unwrap_or(0))
                    } else {
                        VersionToken::Qualifier(current.to_ascii_lowercase())
                    });
                    current.clear();
                }
                current_is_digit = None;
                continue;
            }

            let is_digit = ch.is_ascii_digit();
            match current_is_digit {
                None => {
                    current_is_digit = Some(is_digit);
                    current.push(ch);
                }
                Some(flag) if flag == is_digit => current.push(ch),
                Some(_) => {
                    if !current.is_empty() {
                        tokens.push(if current_is_digit.unwrap_or(false) {
                            VersionToken::Num(current.parse().unwrap_or(0))
                        } else {
                            VersionToken::Qualifier(current.to_ascii_lowercase())
                        });
                        current.clear();
                    }
                    current_is_digit = Some(is_digit);
                    current.push(ch);
                }
            }
        }

        if !current.is_empty() {
            tokens.push(if current_is_digit.unwrap_or(false) {
                VersionToken::Num(current.parse().unwrap_or(0))
            } else {
                VersionToken::Qualifier(current.to_ascii_lowercase())
            });
        }

        tokens
    }

    /// ComparableVersion 互換のバージョン比較。主要なプリリリース修飾子の優先度を Maven に揃える。
    pub fn compare_maven_versions(lhs: &str, rhs: &str) -> Ordering {
        match (Version::parse(lhs), Version::parse(rhs)) {
            (Ok(a), Ok(b)) => a.cmp(&b),
            _ => {
                let left = tokenize_version(lhs);
                let right = tokenize_version(rhs);
                let max_len = left.len().max(right.len());

                for idx in 0..max_len {
                    let l = left.get(idx).cloned().unwrap_or(VersionToken::Num(0));
                    let r = right.get(idx).cloned().unwrap_or(VersionToken::Num(0));
                    match (l, r) {
                        (VersionToken::Num(a), VersionToken::Num(b)) => {
                            let cmp = a.cmp(&b);
                            if cmp != Ordering::Equal {
                                return cmp;
                            }
                        }
                        (VersionToken::Num(_), VersionToken::Qualifier(_)) => {
                            return Ordering::Greater;
                        }
                        (VersionToken::Qualifier(_), VersionToken::Num(_)) => {
                            return Ordering::Less;
                        }
                        (VersionToken::Qualifier(a), VersionToken::Qualifier(b)) => {
                            let (rank_a, name_a) = qualifier_rank(&a);
                            let (rank_b, name_b) = qualifier_rank(&b);
                            let cmp = rank_a.cmp(&rank_b);
                            if cmp != Ordering::Equal {
                                return cmp;
                            }
                            let cmp_name = name_a.cmp(&name_b);
                            if cmp_name != Ordering::Equal {
                                return cmp_name;
                            }
                        }
                    }
                }

                Ordering::Equal
            }
        }
    }

    fn split_range_intervals(range: &str) -> Vec<String> {
        let mut parts = Vec::new();
        let mut start = 0usize;
        let bytes = range.as_bytes();
        let len = bytes.len();
        let mut i = 0usize;
        while i < len.saturating_sub(2) {
            let current = bytes[i];
            let next = bytes[i + 1];
            let after = bytes[i + 2];
            let is_separator = (current == b']' || current == b')')
                && next == b','
                && (after == b'[' || after == b'(');
            if is_separator {
                parts.push(range[start..=i].trim().to_string());
                start = i + 1;
            }
            i += 1;
        }
        if start < len {
            parts.push(range[start..].trim().to_string());
        }
        parts
    }

    /// Maven の括弧記法に近い範囲評価。`[1.0,2.0)`、`(,1.0],[1.2,)` などをサポートする。
    pub fn version_satisfies_range(version: &str, range: &str) -> bool {
        let intervals = split_range_intervals(range);
        if intervals.is_empty() {
            return false;
        }

        for interval in intervals {
            let trimmed = interval.trim();
            if trimmed.is_empty() {
                continue;
            }

            // 単一バージョン指定
            if !trimmed.starts_with(['[', '(']) || !trimmed.ends_with([']', ')']) {
                if compare_maven_versions(version, trimmed) == Ordering::Equal {
                    return true;
                }
                continue;
            }

            if trimmed.len() < 2 {
                continue;
            }

            let lower_inclusive = trimmed.starts_with('[');
            let upper_inclusive = trimmed.ends_with(']');
            let body = &trimmed[1..trimmed.len() - 1];
            let (lower_raw, upper_raw) = match body.split_once(',') {
                Some((lhs, rhs)) => (lhs.trim(), rhs.trim()),
                None => (body.trim(), ""),
            };

            let lower = if lower_raw.is_empty() {
                None
            } else {
                Some(lower_raw)
            };
            let upper = if upper_raw.is_empty() {
                None
            } else {
                Some(upper_raw)
            };

            if let Some(bound) = lower {
                let cmp = compare_maven_versions(version, bound);
                if cmp == Ordering::Less || (cmp == Ordering::Equal && !lower_inclusive) {
                    continue;
                }
            }

            if let Some(bound) = upper {
                let cmp = compare_maven_versions(version, bound);
                if cmp == Ordering::Greater || (cmp == Ordering::Equal && !upper_inclusive) {
                    continue;
                }
            }

            return true;
        }

        false
    }

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

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum ConflictStrategy {
        /// Maven のデフォルト: 最短経路（浅い深さ）を優先し、同一深さでは最初に遭遇したものを採用。
        Nearest,
        /// 最深経路を優先（FarthestConflictResolver 相当）。
        Farthest,
        /// バージョンの新しさを優先（NewestConflictResolver 相当）。
        Newest,
        /// バージョンの古さを優先（OldestConflictResolver 相当）。
        Oldest,
        /// 競合解決せず全バージョンを保持（プラグインなどツール系ダウンロード用）。
        KeepAll,
    }

    #[derive(Clone, Copy)]
    pub struct ClosureOptions {
        include_optional: bool,
        include_provided: bool,
        include_test: bool,
        /// When true, multiple versions of the same GA(+classifier) are kept (plugin/tooling paths can legitimately require different versions simultaneously).
        allow_multiple_versions: bool,
        /// Optional depth制限（0 = roots のみ、1 = 1段目までなど）。
        max_depth: Option<usize>,
        /// When true, only the roots themselves are included; transitive dependencies are skipped.
        skip_transitive: bool,
        conflict_strategy: ConflictStrategy,
    }

    /// Adds managed (dependencyManagement-equivalent) artifacts into the download plan.
    /// Maven's dependencyManagement overrides transitive dependency versions,
    /// so this function removes any existing versions of the same GA before adding the managed version.
    pub(crate) fn append_managed_artifacts(
        download_plan: &mut Vec<ArtifactCoordinates>,
        managed: &[ArtifactCoordinates],
        seen_download: &mut HashSet<(String, String, Option<String>, String)>,
    ) {
        for coords in managed {
            // Remove any existing versions of the same GA from download_plan
            // (Maven's dependencyManagement overrides transitive versions)
            download_plan.retain(|existing| {
                if existing.group_id == coords.group_id
                    && existing.artifact_id == coords.artifact_id
                    && existing.classifier == coords.classifier
                    && existing.version != coords.version
                {
                    // Remove from seen_download too
                    seen_download.remove(&(
                        existing.group_id.clone(),
                        existing.artifact_id.clone(),
                        existing.classifier.clone(),
                        existing.version.clone(),
                    ));
                    false // Remove this entry
                } else {
                    true // Keep this entry
                }
            });

            // Add the managed version
            if seen_download.insert((
                coords.group_id.clone(),
                coords.artifact_id.clone(),
                coords.classifier.clone(),
                coords.version.clone(),
            )) {
                download_plan.push(coords.clone());
            }
        }
    }

    /// Appends arbitrary artifacts into the download plan while deduplicating by GA+classifier+version.
    /// Maven keeps different versions from different contexts (project deps vs plugin deps),
    /// so we use full GAV deduplication to match Maven's behavior.
    pub(crate) fn append_artifacts(
        download_plan: &mut Vec<ArtifactCoordinates>,
        artifacts: &[ArtifactCoordinates],
        seen_download: &mut HashSet<(String, String, Option<String>, String)>,
    ) {
        for coords in artifacts {
            if seen_download.insert((
                coords.group_id.clone(),
                coords.artifact_id.clone(),
                coords.classifier.clone(),
                coords.version.clone(),
            )) {
                download_plan.push(coords.clone());
            }
        }
    }

    /// Resolves each root independently with the given options and returns the union (deduped).
    pub(crate) fn resolve_union_per_root(
        resolver: &MavenDependencyResolver,
        roots: &[ArtifactCoordinates],
        options: ClosureOptions,
    ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
        let mut aggregated = Vec::new();
        let mut seen = HashSet::new();
        for root in roots {
            let closure = resolver.resolve_closure_with_options(&[root.clone()], options)?;
            append_artifacts(&mut aggregated, &closure, &mut seen);
        }
        Ok(aggregated)
    }
    impl ClosureOptions {
        pub fn base() -> Self {
            Self {
                include_optional: false,
                // Maven dependency:resolve は provided/test/optional を除外。
                // Maven NearestVersionSelector により同一 GA は最近接バージョン 1 つに収束。
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false, // Maven: 同一 GA は 1 バージョンのみ
                max_depth: None,
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Nearest, // Maven: 最近接勝ち
            }
        }

        /// Plugin 用ダウンロード展開（PluginDependenciesResolver 相当: provided/test/optional を除外）
        /// Maven NearestVersionSelector により同一 GA は最近接バージョン 1 つに収束。
        pub fn plugin_download() -> Self {
            Self {
                include_optional: false,
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false, // Maven: 同一 GA は 1 バージョンのみ
                max_depth: None,
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Nearest, // Maven: 最近接勝ち
            }
        }

        /// プラグイン seed を最低限ダウンロードするためのオプション。root のみ取得し依存は展開しない。
        pub fn plugin_seed() -> Self {
            Self {
                include_optional: false,
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false,
                max_depth: Some(0),
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Nearest,
            }
        }

        /// Maven 本家と同じく「root 単位で最近接勝ち」を基準にし、GA 重複は 1 つに収束させる。
        pub fn plugin_download_nearest() -> Self {
            Self {
                include_optional: false,
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false, // Maven は Nearest で同一 GA を 1 バージョンに収束
                max_depth: None,
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Nearest,
            }
        }

        /// プラグイン実行時に Maven 本体が提供する provided 依存を除外した解決オプション。
        pub fn plugin_runtime_nearest() -> Self {
            Self {
                include_optional: false,
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false,
                max_depth: Some(1),
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Nearest,
            }
        }

        /// 実際に実行されるプラグイン用にフルの推移解決を行う。
        pub fn plugin_execution_full() -> Self {
            Self {
                include_optional: false,
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false,
                max_depth: None,
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Nearest,
            }
        }

        pub fn newest_conflict() -> Self {
            Self {
                include_optional: false,
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false,
                max_depth: None,
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Newest,
            }
        }

        pub fn oldest_conflict() -> Self {
            Self {
                include_optional: false,
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false,
                max_depth: None,
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Oldest,
            }
        }

        pub fn farthest_conflict() -> Self {
            Self {
                include_optional: false,
                include_provided: false,
                include_test: false,
                allow_multiple_versions: false,
                max_depth: None,
                skip_transitive: false,
                conflict_strategy: ConflictStrategy::Farthest,
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
        pub fn resolve_closure(
            &self,
            roots: &[ArtifactCoordinates],
        ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
            self.resolve_closure_with_options(roots, ClosureOptions::base())
        }

        /// Resolves the dependency closure with configurable optional/scope handling.
        pub fn resolve_closure_with_options(
            &self,
            roots: &[ArtifactCoordinates],
            options: ClosureOptions,
        ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
            let include_optional = options.include_optional;
            let include_provided = options.include_provided;
            let include_test = options.include_test;
            let allow_multiple_versions = options.allow_multiple_versions;
            let conflict_strategy = options.conflict_strategy;
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
                let current_depth = depth;
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
                    if *best_version != coords.version || *best_depth != current_depth {
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
                        "[maven-compat-debug] pom_deps coords={}:{}:{} depth={} count={} allow_provided={} allow_test={} include_optional={} allow_multi={} samples=[{}]",
                        coords.group_id,
                        coords.artifact_id,
                        coords.version,
                        depth,
                        effective.dependencies.len(),
                        include_provided,
                        include_test,
                        include_optional,
                        allow_multiple_versions,
                        samples.join(",")
                    );
                    pom_log_emitted += 1;
                }

                if let Some(limit) = options.max_depth {
                    if current_depth >= limit {
                        continue;
                    }
                }
                if options.skip_transitive && current_depth > 0 {
                    continue;
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
                    let scope = dependency.scope.as_deref();
                    let allowed_scope = match scope {
                        None | Some("compile") | Some("runtime") => true,
                        Some("provided") => include_provided,
                        Some("system") => include_provided,
                        Some("test") => include_test,
                        Some(_) => include_test,
                    };
                    if !allowed_scope {
                        skipped_scope += 1;
                        eprintln!(
                            "[maven-compat-debug] skip_scope coords={}:{}:{} scope={:?} include_provided={} include_test={}",
                            dependency.coordinates.group_id,
                            dependency.coordinates.artifact_id,
                            dependency.coordinates.version,
                            dependency.scope,
                            include_provided,
                            include_test
                        );
                        continue;
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
                    let candidate_depth = current_depth + 1;

                    let managed_version = effective
                        .dependency_management
                        .get(&(coords.group_id.clone(), coords.artifact_id.clone()))
                        .map(|managed| managed.version.clone());
                    if let Some(version) = managed_version.as_ref() {
                        if coords.version != *version {
                            coords.version = version.clone();
                        }
                    }

                    if allow_multiple_versions
                        || matches!(conflict_strategy, ConflictStrategy::KeepAll)
                    {
                        ordered.insert(coords.clone());
                        queue.push_back((coords, next_exclusions, candidate_depth));
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
                        let mut should_replace = matches!(ga_versions.get(&key), None);
                        if let Some((existing_version, existing_depth)) = ga_versions.get(&key) {
                            let managed_override = managed_version
                                .as_ref()
                                .map(|managed| managed != existing_version)
                                .unwrap_or(false);
                            match conflict_strategy {
                                ConflictStrategy::Nearest => {
                                    if managed_override && candidate_depth <= *existing_depth {
                                        should_replace = true;
                                    } else if candidate_depth < *existing_depth {
                                        should_replace = true;
                                    } else if candidate_depth == *existing_depth {
                                        if managed_override {
                                            should_replace = true;
                                        } else if existing_version != &coords.version {
                                            continue;
                                        }
                                    } else {
                                        continue;
                                    }
                                }
                                ConflictStrategy::Farthest => {
                                    if managed_override && candidate_depth >= *existing_depth {
                                        should_replace = true;
                                    } else if candidate_depth > *existing_depth {
                                        should_replace = true;
                                    } else if candidate_depth == *existing_depth {
                                        if managed_override {
                                            should_replace = true;
                                        } else if existing_version != &coords.version {
                                            continue;
                                        }
                                    } else {
                                        continue;
                                    }
                                }
                                ConflictStrategy::Newest => {
                                    let cmp =
                                        compare_maven_versions(&coords.version, existing_version);
                                    if managed_override {
                                        should_replace = true;
                                    } else if cmp.is_gt() {
                                        should_replace = true;
                                    } else if cmp.is_eq() && candidate_depth < *existing_depth {
                                        should_replace = true;
                                    } else {
                                        continue;
                                    }
                                }
                                ConflictStrategy::Oldest => {
                                    let cmp =
                                        compare_maven_versions(&coords.version, existing_version);
                                    if managed_override {
                                        should_replace = true;
                                    } else if cmp.is_lt() {
                                        should_replace = true;
                                    } else if cmp.is_eq() && candidate_depth < *existing_depth {
                                        should_replace = true;
                                    } else {
                                        continue;
                                    }
                                }
                                ConflictStrategy::KeepAll => {
                                    should_replace = true;
                                }
                            }
                        }

                        if should_replace {
                            ordered.retain(|item| {
                                !(item.group_id == coords.group_id
                                    && item.artifact_id == coords.artifact_id
                                    && item.classifier == coords.classifier)
                            });
                            ga_versions
                                .insert(key.clone(), (coords.version.clone(), candidate_depth));
                            ordered.insert(coords.clone());
                            queue.push_back((coords, next_exclusions, candidate_depth));
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
                                coords.group_id, coords.artifact_id, coords.version, current_depth
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
