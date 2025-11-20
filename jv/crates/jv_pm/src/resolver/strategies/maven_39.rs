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
use std::collections::{HashMap, VecDeque};
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
}

impl<'a> MavenDependencyProvider3_9<'a> {
    pub fn new(
        project_root: &'a std::path::Path,
        repositories: Vec<MavenRepositoryConfig>,
        mirrors: Vec<MavenMirrorConfig>,
        cache: Arc<DependencyCache>,
    ) -> Self {
        Self {
            project_root,
            repositories,
            mirrors,
            _cache: cache,
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
            if to_download.is_empty() {
                if self.verify_repository_state(targets)? {
                    return Ok(());
                }
                continue;
            }

            attempt += 1;
            println!(
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
                    "Jar のダウンロード確認に失敗しました",
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
            if self.validate_existing_jar(path)? {
                continue;
            }

            jar_paths.insert(coords.clone(), path.clone());
            to_download.push(coords.clone());
        }

        Ok((to_download, jar_paths))
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
            println!("[maven-compat-download] {}/{} {}", index + 1, total, coords);
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

        println!(
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
            println!(
                "[maven-compat-verify] repository verification succeeded ({} artifacts)",
                targets.len()
            );
        } else {
            println!(
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
            println!(
                "[maven-compat-verify] {}/{} missing {}",
                index, total, coords
            );
            return Ok(JarVerification::NeedsDownload);
        }

        let Some((algorithm, expected)) = self.read_local_checksum(jar_path)? else {
            println!(
                "[maven-compat-verify] {}/{} checksum file missing for {}",
                index, total, coords
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
                println!(
                    "[maven-compat-verify] {}/{} OK {} ({})",
                    index, total, coords, relative
                );
            } else {
                println!("[maven-compat-verify] {}/{} OK {}", index, total, coords);
            }
            return Ok(JarVerification::Valid);
        }

        println!(
            "[maven-compat-verify] {}/{} checksum mismatch {} (expected {}, actual {})",
            index, total, coords, expected, actual
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
    println!("[maven-compat-download-start] {}", coords);

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

        let mut registries = Vec::new();
        for repo in &ctx.repositories {
            match MavenRegistry::new(&repo.url) {
                Ok(registry) => registries.push(Arc::new(registry)),
                Err(error) => {
                    tracing::warn!(url = %repo.url, error = ?error, "リポジトリ初期化に失敗したためスキップします");
                }
            }
        }

        let jar_downloader =
            MavenJarDownloader::new(ctx.local_repository.clone(), registries.clone());
        let provider = self.build_provider(options.maven_context.as_ref());
        let direct = provider.direct_dependencies();

        let resolver = MavenDependencyResolver::new(&runtime, cache.clone(), registries.clone());

        let mut roots = Vec::new();
        for dep in &direct {
            if let Some((group, artifact)) = dep.name.split_once(':') {
                let ver = dep.requirement.clone();
                roots.push(ArtifactCoordinates::new(
                    group.to_string(),
                    artifact.to_string(),
                    ver,
                ));
            }
        }

        let closure =
            resolver
                .resolve_closure(&roots)
                .map_err(|error| ResolverError::Conflict {
                    dependency: "resolver".into(),
                    details: error.to_string(),
                })?;

        let targets = jar_downloader.artifact_targets(&closure);
        jar_downloader.ensure_artifacts(&runtime, &targets)?;
        let jar_locations: HashMap<ArtifactCoordinates, PathBuf> =
            targets.iter().cloned().collect();

        let dependencies: Vec<ResolvedDependency> = closure
            .into_iter()
            .map(|coords| {
                let local = jar_locations
                    .get(&coords)
                    .map(|path| path.to_string_lossy().to_string());
                ResolvedDependency {
                    name: format!("{}:{}", coords.group_id, coords.artifact_id),
                    requested: coords.version.clone(),
                    decision: VersionDecision::Exact(coords.version.clone()),
                    scope: DependencyScope::Main,
                    source: ResolutionSource::Registry,
                    local_artifact: local,
                }
            })
            .collect();

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

// -----------------------------------------------------------------------------
// Maven 3.9 POM 解析ロジック（元: maven/dependency_graph.rs）
// -----------------------------------------------------------------------------

pub use pom_resolver::{MavenDependencyResolver, PomMetadata};

mod pom_resolver {
    use std::borrow::Cow;
    use std::collections::{HashMap, HashSet};
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
        pub fn resolve_closure(
            &self,
            roots: &[ArtifactCoordinates],
        ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
            self.resolve_closure_with_optional(roots, false)
        }

        /// Resolves the dependency closure, optionally keeping dependencies marked as optional.
        pub fn resolve_closure_with_optional(
            &self,
            roots: &[ArtifactCoordinates],
            include_optional: bool,
        ) -> Result<Vec<ArtifactCoordinates>, WrapperError> {
            let mut ordered = IndexSet::new();
            let mut memo = HashMap::new();
            // Maven の nearest-wins 調停に合わせ、最初に現れた groupId/artifactId(+classifier) のバージョンを固定する。
            let mut seen_versions: HashMap<(String, String, Option<String>), String> =
                HashMap::new();
            for root in roots {
                self.expand(
                    root.clone(),
                    &mut ordered,
                    &mut memo,
                    &[],
                    include_optional,
                    &mut HashSet::new(),
                    &mut seen_versions,
                )?;
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

        fn expand(
            &self,
            coords: ArtifactCoordinates,
            ordered: &mut IndexSet<ArtifactCoordinates>,
            memo: &mut HashMap<ArtifactCoordinates, Arc<EffectivePom>>,
            inherited_exclusions: &[(String, String)],
            include_optional: bool,
            stack: &mut HashSet<ArtifactCoordinates>,
            seen_versions: &mut HashMap<(String, String, Option<String>), String>,
        ) -> Result<(), WrapperError> {
            if ordered.contains(&coords) {
                return Ok(());
            }

            let key = (
                coords.group_id.clone(),
                coords.artifact_id.clone(),
                coords.classifier.clone(),
            );

            if let Some(existing) = seen_versions.get(&key) {
                if existing != &coords.version {
                    // nearest-wins: 先に確定したバージョンを優先し、後続の異なるバージョンは展開しない。
                    return Ok(());
                }
            } else {
                seen_versions.insert(key, coords.version.clone());
            }

            if !stack.insert(coords.clone()) {
                return Err(WrapperError::OperationFailed(format!(
                    "依存グラフに循環が検出されました: {}",
                    coords
                )));
            }

            ordered.insert(coords.clone());
            let effective = self.load_effective_pom(coords.clone(), memo, &mut HashSet::new())?;

            for dependency in &effective.dependencies {
                if dependency.optional && !include_optional {
                    continue;
                }
                if !matches!(
                    dependency.scope.as_deref(),
                    None | Some("compile") | Some("runtime") | Some("provided")
                ) {
                    continue;
                }
                if is_excluded(&dependency.coordinates, inherited_exclusions) {
                    continue;
                }

                let mut next_exclusions = inherited_exclusions.to_vec();
                next_exclusions.extend(
                    dependency
                        .exclusions
                        .iter()
                        .map(|ex| (ex.group_id.clone(), ex.artifact_id.clone())),
                );
                self.expand(
                    dependency.coordinates.clone(),
                    ordered,
                    memo,
                    &next_exclusions,
                    include_optional,
                    stack,
                    seen_versions,
                )?;
            }

            stack.remove(&coords);
            Ok(())
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
        _scope: Option<String>,
        _optional: bool,
        _classifier: Option<String>,
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
                let version =
                    resolve_property(entry.version.as_deref(), &properties).ok_or_else(|| {
                        WrapperError::OperationFailed(format!(
                            "{group}:{artifact} の dependencyManagement にバージョンがありません"
                        ))
                    })?;
                dependency_management.insert(
                    (group.clone(), artifact.clone()),
                    ManagedDependency {
                        version,
                        _scope: entry.scope,
                        _optional: entry.optional,
                        _classifier: entry.classifier,
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
                let version = match resolve_property(dependency.version.as_deref(), &properties) {
                    Some(value) => value,
                    None => {
                        if let Some(managed) =
                            dependency_management.get(&(group.clone(), artifact.clone()))
                        {
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
                if let Some(classifier) = dependency.classifier {
                    coords = coords.with_classifier(classifier);
                }

                resolved.push(ResolvedDependency {
                    coordinates: coords,
                    scope: dependency.scope.clone(),
                    optional: dependency.optional,
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
}
