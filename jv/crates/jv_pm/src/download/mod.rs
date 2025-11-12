use crate::BuildInfo;
use crate::cache::{CacheError, CachedArtifact, DependencyCache};
use crate::registry::{ArtifactCoordinates, DownloadedJar, MavenRegistry, RegistryError};
use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use tokio::task::JoinSet;
use tracing::warn;

use thiserror::Error;

#[derive(Debug, Clone)]
pub struct DownloadSettings {
    pub default_max_concurrent: usize,
    pub warning_threshold: usize,
    pub hard_limit: usize,
}

impl Default for DownloadSettings {
    fn default() -> Self {
        Self {
            default_max_concurrent: 8,
            warning_threshold: 16,
            hard_limit: 32,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArtifactDownloadRequest {
    coordinates: ArtifactCoordinates,
    checksum: Option<String>,
}

impl ArtifactDownloadRequest {
    pub fn new(coordinates: ArtifactCoordinates) -> Self {
        Self {
            coordinates,
            checksum: None,
        }
    }

    pub fn with_checksum(mut self, checksum: impl Into<String>) -> Self {
        self.checksum = Some(checksum.into());
        self
    }

    pub fn coordinates(&self) -> &ArtifactCoordinates {
        &self.coordinates
    }

    pub fn checksum(&self) -> Option<&str> {
        self.checksum.as_deref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DownloadSource {
    Cache,
    Registry,
}

#[derive(Debug, Clone)]
pub struct DownloadSuccess {
    pub(crate) order: usize,
    pub request: ArtifactDownloadRequest,
    pub artifact: CachedArtifact,
    pub source: DownloadSource,
}

impl DownloadSuccess {
    fn from_cache(
        order: usize,
        request: ArtifactDownloadRequest,
        artifact: CachedArtifact,
    ) -> Self {
        Self {
            order,
            request,
            artifact,
            source: DownloadSource::Cache,
        }
    }

    fn from_registry(
        order: usize,
        request: ArtifactDownloadRequest,
        artifact: CachedArtifact,
    ) -> Self {
        Self {
            order,
            request,
            artifact,
            source: DownloadSource::Registry,
        }
    }

    pub fn coordinates(&self) -> &ArtifactCoordinates {
        self.request.coordinates()
    }

    pub fn source(&self) -> DownloadSource {
        self.source
    }
}

#[derive(Debug)]
pub struct DownloadFailure {
    pub(crate) order: Option<usize>,
    pub request: Option<ArtifactDownloadRequest>,
    pub error: DownloadError,
}

impl DownloadFailure {
    fn with_request(order: usize, request: ArtifactDownloadRequest, error: DownloadError) -> Self {
        Self {
            order: Some(order),
            request: Some(request),
            error,
        }
    }

    fn without_request(error: DownloadError) -> Self {
        Self {
            order: None,
            request: None,
            error,
        }
    }

    pub fn request(&self) -> Option<&ArtifactDownloadRequest> {
        self.request.as_ref()
    }
}

#[derive(Debug, Default)]
pub struct DownloadReport {
    pub successes: Vec<DownloadSuccess>,
    pub failures: Vec<DownloadFailure>,
    pub warnings: Vec<String>,
}

impl DownloadReport {
    pub fn is_success(&self) -> bool {
        self.failures.is_empty()
    }

    fn merge(&mut self, mut other: DownloadReport) {
        self.successes.append(&mut other.successes);
        self.failures.append(&mut other.failures);
        self.warnings.append(&mut other.warnings);
    }

    fn sort_entries(&mut self) {
        self.successes.sort_by_key(|success| success.order);
        self.failures
            .sort_by_key(|failure| failure.order.unwrap_or(usize::MAX));
    }
}

#[derive(Debug, Error)]
pub enum DownloadError {
    #[error(transparent)]
    Registry(#[from] RegistryError),
    #[error(transparent)]
    Cache(#[from] CacheError),
    #[error("ダウンロードタスクが異常終了しました: {0}")]
    Join(#[from] tokio::task::JoinError),
}

pub trait JarFetcher: Send + Sync {
    fn download_jar<'a>(
        &'a self,
        coords: &'a ArtifactCoordinates,
    ) -> Pin<Box<dyn Future<Output = Result<DownloadedJar, RegistryError>> + Send + 'a>>;
}

impl JarFetcher for MavenRegistry {
    fn download_jar<'a>(
        &'a self,
        coords: &'a ArtifactCoordinates,
    ) -> Pin<Box<dyn Future<Output = Result<DownloadedJar, RegistryError>> + Send + 'a>> {
        Box::pin(MavenRegistry::download_jar(self, coords))
    }
}

struct IndexedRequest {
    order: usize,
    request: ArtifactDownloadRequest,
}

pub struct ParallelDownloader<R = MavenRegistry>
where
    R: JarFetcher + Send + Sync + 'static,
{
    registry: Arc<R>,
    cache: Arc<DependencyCache>,
    max_concurrent: usize,
}

impl<R> ParallelDownloader<R>
where
    R: JarFetcher + Send + Sync + 'static,
{
    pub fn new(registry: Arc<R>, cache: Arc<DependencyCache>, max_concurrent: usize) -> Self {
        let enforced = max_concurrent.max(1);
        Self {
            registry,
            cache,
            max_concurrent: enforced,
        }
    }

    pub fn max_concurrent(&self) -> usize {
        self.max_concurrent
    }

    async fn download(&self, requests: Vec<IndexedRequest>) -> DownloadReport {
        let mut report = DownloadReport::default();
        if requests.is_empty() {
            return report;
        }

        let mut join_set: JoinSet<(IndexedRequest, Result<CachedArtifact, DownloadError>)> =
            JoinSet::new();
        let mut queue: VecDeque<IndexedRequest> = requests.into();
        let limit = self.max_concurrent;

        while let Some(task) = queue.pop_front() {
            self.spawn_download(&mut join_set, task);

            if join_set.len() >= limit {
                self.consume_next(&mut join_set, &mut report).await;
            }
        }

        while !join_set.is_empty() {
            self.consume_next(&mut join_set, &mut report).await;
        }

        report
    }

    fn spawn_download(
        &self,
        join_set: &mut JoinSet<(IndexedRequest, Result<CachedArtifact, DownloadError>)>,
        task: IndexedRequest,
    ) {
        let registry = Arc::clone(&self.registry);
        let cache = Arc::clone(&self.cache);
        join_set.spawn(async move {
            let IndexedRequest { order, request } = task;
            let coords = request.coordinates().clone();

            let result = async {
                let jar = registry.download_jar(&coords).await?;
                let cached = cache.store_artifact(&coords, jar.bytes.as_slice(), &jar.checksum)?;
                Ok::<CachedArtifact, DownloadError>(cached)
            }
            .await;

            (IndexedRequest { order, request }, result)
        });
    }

    async fn consume_next(
        &self,
        join_set: &mut JoinSet<(IndexedRequest, Result<CachedArtifact, DownloadError>)>,
        report: &mut DownloadReport,
    ) {
        if let Some(joined) = join_set.join_next().await {
            match joined {
                Ok((IndexedRequest { order, request }, Ok(artifact))) => {
                    report
                        .successes
                        .push(DownloadSuccess::from_registry(order, request, artifact));
                }
                Ok((IndexedRequest { order, request }, Err(error))) => {
                    report
                        .failures
                        .push(DownloadFailure::with_request(order, request, error));
                }
                Err(join_error) => {
                    report
                        .failures
                        .push(DownloadFailure::without_request(DownloadError::from(
                            join_error,
                        )));
                }
            }
        }
    }
}

pub struct DownloadManager<R = MavenRegistry>
where
    R: JarFetcher + Send + Sync + 'static,
{
    registry: Arc<R>,
    cache: Arc<DependencyCache>,
    settings: DownloadSettings,
    manifest_max_concurrent: Option<usize>,
    manifest_warning_threshold: Option<usize>,
    cli_max_concurrent: Option<usize>,
    cli_warning_threshold: Option<usize>,
}

impl<R> DownloadManager<R>
where
    R: JarFetcher + Send + Sync + 'static,
{
    pub fn new(registry: Arc<R>, cache: Arc<DependencyCache>) -> Self {
        Self {
            registry,
            cache,
            settings: DownloadSettings::default(),
            manifest_max_concurrent: None,
            manifest_warning_threshold: None,
            cli_max_concurrent: None,
            cli_warning_threshold: None,
        }
    }

    pub fn with_settings(mut self, settings: DownloadSettings) -> Self {
        self.settings = settings;
        self
    }

    pub fn apply_manifest(mut self, build: Option<&BuildInfo>) -> Self {
        self.set_manifest_options(build);
        self
    }

    pub fn with_cli_max_concurrent(mut self, value: Option<usize>) -> Self {
        self.cli_max_concurrent = value;
        self
    }

    pub fn with_cli_warning_threshold(mut self, value: Option<usize>) -> Self {
        self.cli_warning_threshold = value;
        self
    }

    pub fn set_manifest_options(&mut self, build: Option<&BuildInfo>) {
        if let Some(build) = build {
            self.manifest_max_concurrent = build.max_concurrent_downloads;
            self.manifest_warning_threshold = build.max_concurrent_warning;
        } else {
            self.manifest_max_concurrent = None;
            self.manifest_warning_threshold = None;
        }
    }

    pub fn set_cli_max_concurrent(&mut self, value: Option<usize>) {
        self.cli_max_concurrent = value;
    }

    pub fn set_cli_warning_threshold(&mut self, value: Option<usize>) {
        self.cli_warning_threshold = value;
    }

    pub fn effective_concurrency(&self) -> usize {
        self.compute_effective_concurrency().applied
    }

    pub async fn download_artifacts<I>(&self, requests: I) -> DownloadReport
    where
        I: IntoIterator<Item = ArtifactDownloadRequest>,
    {
        let mut report = DownloadReport::default();
        let mut network_queue = Vec::new();

        for (order, request) in requests.into_iter().enumerate() {
            let coords = request.coordinates().clone();
            let checksum_hint = request.checksum().map(|value| value.to_string());
            match self.cache.get_artifact(&coords, checksum_hint.as_deref()) {
                Ok(Some(artifact)) => {
                    report
                        .successes
                        .push(DownloadSuccess::from_cache(order, request, artifact));
                }
                Ok(None) => {
                    network_queue.push(IndexedRequest { order, request });
                }
                Err(error) => {
                    report.failures.push(DownloadFailure::with_request(
                        order,
                        request,
                        DownloadError::from(error),
                    ));
                }
            }
        }

        if !network_queue.is_empty() {
            let concurrency = self.compute_effective_concurrency();
            report.warnings.extend(concurrency.warnings.clone());
            let downloader = ParallelDownloader::new(
                Arc::clone(&self.registry),
                Arc::clone(&self.cache),
                concurrency.applied,
            );
            let network_report = downloader.download(network_queue).await;
            report.merge(network_report);
        }

        report.sort_entries();
        report
    }

    fn compute_effective_concurrency(&self) -> EffectiveConcurrency {
        let mut warnings = Vec::new();
        let requested = self
            .cli_max_concurrent
            .or(self.manifest_max_concurrent)
            .unwrap_or(self.settings.default_max_concurrent);

        let mut applied = requested.max(1);

        let warning_threshold = self
            .cli_warning_threshold
            .or(self.manifest_warning_threshold)
            .unwrap_or(self.settings.warning_threshold)
            .max(1);

        let hard_limit = self.settings.hard_limit.max(1);

        if requested == 0 {
            let message = "並列ダウンロード数が0のため1に補正しました".to_string();
            warn!(requested = requested, "{message}", message = message);
            warnings.push(message);
        }

        if applied > hard_limit {
            let original_requested = applied;
            let message = format!(
                "要求された並列ダウンロード数 {original_requested} は許可上限 {hard_limit} を超えているため {hard_limit} に丸めました",
            );
            warn!(
                requested = original_requested,
                hard_limit = hard_limit,
                applied = hard_limit,
                "{message}",
                message = message
            );
            warnings.push(message);
            applied = hard_limit;
        }

        if applied > warning_threshold {
            let message = format!(
                "並列ダウンロード数 {applied} が警告閾値 {warning_threshold} を超えています"
            );
            warn!(
                applied = applied,
                warning_threshold = warning_threshold,
                "{message}",
                message = message
            );
            warnings.push(message);
        }

        EffectiveConcurrency { applied, warnings }
    }
}

struct EffectiveConcurrency {
    applied: usize,
    warnings: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::ArtifactResource;
    use sha2::{Digest, Sha256};
    use std::collections::HashMap;
    use std::sync::Mutex;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use tempfile::tempdir;
    use tokio::time::{Duration, sleep};

    #[derive(Clone)]
    struct MockRegistry {
        responses: Arc<Mutex<HashMap<String, MockResponse>>>,
        in_flight: Arc<AtomicUsize>,
        max_in_flight: Arc<AtomicUsize>,
        call_count: Arc<AtomicUsize>,
    }

    #[derive(Clone)]
    enum MockResponse {
        Success {
            bytes: Arc<Vec<u8>>,
            checksum: String,
            delay: Duration,
        },
        NotFound {
            coordinates: ArtifactCoordinates,
        },
    }

    impl MockRegistry {
        fn new() -> Self {
            Self {
                responses: Arc::new(Mutex::new(HashMap::new())),
                in_flight: Arc::new(AtomicUsize::new(0)),
                max_in_flight: Arc::new(AtomicUsize::new(0)),
                call_count: Arc::new(AtomicUsize::new(0)),
            }
        }

        fn insert_success(&self, coords: &ArtifactCoordinates, bytes: Vec<u8>, delay: Duration) {
            let checksum = format!("{:x}", Sha256::digest(bytes.as_slice()));
            let mut guard = self.responses.lock().unwrap();
            guard.insert(
                coords.to_string(),
                MockResponse::Success {
                    bytes: Arc::new(bytes),
                    checksum,
                    delay,
                },
            );
        }

        fn insert_not_found(&self, coords: &ArtifactCoordinates) {
            let mut guard = self.responses.lock().unwrap();
            guard.insert(
                coords.to_string(),
                MockResponse::NotFound {
                    coordinates: coords.clone(),
                },
            );
        }

        fn max_in_flight(&self) -> usize {
            self.max_in_flight.load(Ordering::SeqCst)
        }

        fn call_count(&self) -> usize {
            self.call_count.load(Ordering::SeqCst)
        }

        fn reset_counters(&self) {
            self.in_flight.store(0, Ordering::SeqCst);
            self.max_in_flight.store(0, Ordering::SeqCst);
            self.call_count.store(0, Ordering::SeqCst);
        }

        fn record_begin(&self) {
            let current = self.in_flight.fetch_add(1, Ordering::SeqCst) + 1;
            self.call_count.fetch_add(1, Ordering::SeqCst);
            loop {
                let observed = self.max_in_flight.load(Ordering::SeqCst);
                if current <= observed {
                    break;
                }
                if self
                    .max_in_flight
                    .compare_exchange(observed, current, Ordering::SeqCst, Ordering::SeqCst)
                    .is_ok()
                {
                    break;
                }
            }
        }

        fn record_end(&self) {
            self.in_flight.fetch_sub(1, Ordering::SeqCst);
        }
    }

    impl JarFetcher for MockRegistry {
        fn download_jar<'a>(
            &'a self,
            coords: &'a ArtifactCoordinates,
        ) -> Pin<Box<dyn Future<Output = Result<DownloadedJar, RegistryError>> + Send + 'a>>
        {
            let response = {
                let guard = self.responses.lock().unwrap();
                guard
                    .get(&coords.to_string())
                    .cloned()
                    .expect("未設定のモックレスポンス")
            };
            let registry = self.clone();
            Box::pin(async move {
                registry.record_begin();
                let result = match response {
                    MockResponse::Success {
                        bytes,
                        checksum,
                        delay,
                    } => {
                        if !delay.is_zero() {
                            sleep(delay).await;
                        }
                        Ok(DownloadedJar {
                            bytes: bytes.as_ref().clone(),
                            checksum,
                        })
                    }
                    MockResponse::NotFound { coordinates } => {
                        Err(RegistryError::ArtifactNotFound {
                            coordinates,
                            resource: ArtifactResource::Jar,
                            status: reqwest::StatusCode::NOT_FOUND,
                        })
                    }
                };
                registry.record_end();
                result
            })
        }
    }

    fn sample_coords(index: usize) -> ArtifactCoordinates {
        ArtifactCoordinates::new("org.example", format!("demo{index:02}"), "1.0.0")
    }

    #[tokio::test]
    async fn caps_concurrency_to_safe_limit() {
        let registry = Arc::new(MockRegistry::new());
        let temp = tempdir().expect("一時ディレクトリ作成に失敗");
        let cache = Arc::new(
            DependencyCache::with_dir(temp.path().join("cache")).expect("キャッシュ初期化に失敗"),
        );

        for index in 0..40 {
            let coords = sample_coords(index);
            registry.insert_success(&coords, vec![index as u8; 8], Duration::from_millis(25));
        }

        let requests = (0..40)
            .map(|index| ArtifactDownloadRequest::new(sample_coords(index)))
            .collect::<Vec<_>>();

        let manager = DownloadManager::new(Arc::clone(&registry), Arc::clone(&cache))
            .with_cli_max_concurrent(Some(100));

        let report = manager.download_artifacts(requests).await;

        assert!(report.failures.is_empty());
        assert!(
            report
                .warnings
                .iter()
                .any(|message| message.contains("許可上限"))
        );
        assert!(registry.max_in_flight() <= DownloadSettings::default().hard_limit);
    }

    #[tokio::test]
    async fn warns_when_warning_threshold_exceeded() {
        let registry = Arc::new(MockRegistry::new());
        let temp = tempdir().expect("一時ディレクトリ作成に失敗");
        let cache = Arc::new(
            DependencyCache::with_dir(temp.path().join("cache")).expect("キャッシュ初期化に失敗"),
        );

        for index in 0..24 {
            let coords = sample_coords(index);
            registry.insert_success(&coords, vec![index as u8; 4], Duration::from_millis(30));
        }

        let requests = (0..24)
            .map(|index| ArtifactDownloadRequest::new(sample_coords(index)))
            .collect::<Vec<_>>();

        let mut build = BuildInfo::default();
        build.max_concurrent_downloads = Some(20);
        build.max_concurrent_warning = Some(16);

        let manager = DownloadManager::new(Arc::clone(&registry), Arc::clone(&cache))
            .apply_manifest(Some(&build));

        let report = manager.download_artifacts(requests).await;

        assert!(report.failures.is_empty());
        assert!(
            report
                .warnings
                .iter()
                .any(|message| message.contains("警告閾値"))
        );
        assert!(registry.max_in_flight() <= 20);
        assert!(registry.max_in_flight() > 0);
    }

    #[tokio::test]
    async fn aggregates_failures_from_registry() {
        let registry = Arc::new(MockRegistry::new());
        let temp = tempdir().expect("一時ディレクトリ作成に失敗");
        let cache = Arc::new(
            DependencyCache::with_dir(temp.path().join("cache")).expect("キャッシュ初期化に失敗"),
        );

        let ok_coords = sample_coords(0);
        let ng_coords = sample_coords(1);
        registry.insert_success(&ok_coords, vec![1, 2, 3], Duration::from_millis(5));
        registry.insert_not_found(&ng_coords);

        let requests = vec![
            ArtifactDownloadRequest::new(ok_coords.clone()),
            ArtifactDownloadRequest::new(ng_coords.clone()),
        ];

        let manager = DownloadManager::new(Arc::clone(&registry), Arc::clone(&cache));
        let report = manager.download_artifacts(requests).await;

        assert_eq!(report.successes.len(), 1);
        assert_eq!(report.failures.len(), 1);
        assert!(matches!(
            report.failures[0].error,
            DownloadError::Registry(RegistryError::ArtifactNotFound { .. })
        ));
        assert!(
            report.failures[0]
                .request()
                .is_some_and(|request| request.coordinates() == &ng_coords)
        );
    }

    #[tokio::test]
    async fn cache_hits_skip_network_requests() {
        let registry = Arc::new(MockRegistry::new());
        let temp = tempdir().expect("一時ディレクトリ作成に失敗");
        let cache = Arc::new(
            DependencyCache::with_dir(temp.path().join("cache")).expect("キャッシュ初期化に失敗"),
        );

        let coords = sample_coords(0);
        registry.insert_success(&coords, vec![42; 16], Duration::from_millis(5));

        let request = ArtifactDownloadRequest::new(coords.clone());
        let manager = DownloadManager::new(Arc::clone(&registry), Arc::clone(&cache));

        let first = manager.download_artifacts(vec![request.clone()]).await;
        assert!(first.failures.is_empty());
        assert_eq!(registry.call_count(), 1);

        registry.reset_counters();

        let second = manager.download_artifacts(vec![request]).await;
        assert!(second.failures.is_empty());
        assert_eq!(registry.call_count(), 0);
    }
}
