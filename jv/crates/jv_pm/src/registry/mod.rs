use md5;
use sha1::Sha1;
use std::error::Error as StdError;
use std::fmt;
use std::time::Duration;

use bytes::Bytes;
use reqwest::{Client, StatusCode, Url};
use sha2::{Digest, Sha256};
use thiserror::Error;
use tokio::time::sleep;
use tracing::{debug, warn};
use url::ParseError;

mod metadata;

pub use metadata::{MavenMetadata, MetadataParseError};

const DEFAULT_TIMEOUT: Duration = Duration::from_secs(30);

/// Mavenレジストリ内のグループID+アーティファクトIDの組み合わせ。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MavenCoordinates {
    pub group_id: String,
    pub artifact_id: String,
}

impl MavenCoordinates {
    pub fn new(group_id: impl Into<String>, artifact_id: impl Into<String>) -> Self {
        Self {
            group_id: group_id.into(),
            artifact_id: artifact_id.into(),
        }
    }

    pub fn group_path(&self) -> String {
        self.group_id.replace('.', "/")
    }

    pub fn path(&self) -> String {
        format!("{}/{}", self.group_path(), self.artifact_id)
    }
}

impl fmt::Display for MavenCoordinates {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.group_id, self.artifact_id)
    }
}

/// バージョンや分類子を含む完全なアーティファクト座標。
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArtifactCoordinates {
    pub group_id: String,
    pub artifact_id: String,
    pub version: String,
    pub classifier: Option<String>,
}

impl ArtifactCoordinates {
    pub fn new(
        group_id: impl Into<String>,
        artifact_id: impl Into<String>,
        version: impl Into<String>,
    ) -> Self {
        Self {
            group_id: group_id.into(),
            artifact_id: artifact_id.into(),
            version: version.into(),
            classifier: None,
        }
    }

    pub fn with_classifier(mut self, classifier: impl Into<String>) -> Self {
        self.classifier = Some(classifier.into());
        self
    }

    pub fn without_classifier(mut self) -> Self {
        self.classifier = None;
        self
    }

    pub fn classifier(&self) -> Option<&str> {
        self.classifier.as_deref()
    }

    pub fn maven_coordinates(&self) -> MavenCoordinates {
        MavenCoordinates::new(self.group_id.clone(), self.artifact_id.clone())
    }

    fn group_path(&self) -> String {
        self.group_id.replace('.', "/")
    }

    fn version_path(&self) -> String {
        format!(
            "{}/{}/{}",
            self.group_path(),
            self.artifact_id,
            self.version
        )
    }

    fn jar_basename(&self) -> String {
        match self.classifier() {
            Some(classifier) => format!("{}-{}-{}", self.artifact_id, self.version, classifier),
            None => format!("{}-{}", self.artifact_id, self.version),
        }
    }

    pub fn jar_file_name(&self) -> String {
        format!("{}.jar", self.jar_basename())
    }

    fn pom_basename(&self) -> String {
        format!("{}-{}", self.artifact_id, self.version)
    }

    fn jar_path(&self) -> String {
        format!("{}/{}.jar", self.version_path(), self.jar_basename())
    }

    fn pom_path(&self) -> String {
        format!("{}/{}.pom", self.version_path(), self.pom_basename())
    }

    pub fn checksum_path(&self) -> String {
        self.checksum_path_with_extension("sha256")
    }

    fn checksum_path_with_extension(&self, extension: &str) -> String {
        format!(
            "{}/{}.jar.{}",
            self.version_path(),
            self.jar_basename(),
            extension
        )
    }
}

impl fmt::Display for ArtifactCoordinates {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(classifier) = &self.classifier {
            write!(
                f,
                "{}:{}:{}:{}",
                self.group_id, self.artifact_id, self.version, classifier
            )
        } else {
            write!(f, "{}:{}:{}", self.group_id, self.artifact_id, self.version)
        }
    }
}

impl From<&ArtifactCoordinates> for MavenCoordinates {
    fn from(value: &ArtifactCoordinates) -> Self {
        value.maven_coordinates()
    }
}

/// 取得対象となるアーティファクトの種別。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArtifactResource {
    Pom,
    Jar,
    Checksum,
}

impl fmt::Display for ArtifactResource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArtifactResource::Pom => f.write_str("pom"),
            ArtifactResource::Jar => f.write_str("jar"),
            ArtifactResource::Checksum => f.write_str("checksum"),
        }
    }
}

/// リトライ挙動を制御する設定。
#[derive(Debug, Clone, Copy)]
pub struct RetryConfig {
    pub base_delay: Duration,
    pub max_delay: Duration,
    pub max_attempts: u32,
}

impl RetryConfig {
    pub fn new(base_delay: Duration, max_delay: Duration, max_attempts: u32) -> Self {
        let mut config = Self {
            base_delay,
            max_delay,
            max_attempts,
        };
        config.normalize();
        config
    }

    fn normalize(&mut self) {
        if self.max_attempts == 0 {
            self.max_attempts = 1;
        }
        if self.max_delay < self.base_delay {
            self.max_delay = self.base_delay;
        }
    }

    fn delay_for_attempt(&self, attempt: u32) -> Duration {
        let exp = attempt.saturating_sub(1).min(20);
        let base_ms = self.base_delay.as_millis() as u128;
        let multiplier = 1u128 << exp;
        let delay_ms = base_ms.saturating_mul(multiplier);
        let capped_ms = delay_ms.min(self.max_delay.as_millis() as u128);
        Duration::from_millis(capped_ms as u64)
    }
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self::new(Duration::from_millis(500), Duration::from_secs(5), 5)
    }
}

/// チェックサム検証済みのJARダウンロード結果。
#[derive(Debug, Clone)]
pub struct DownloadedJar {
    pub bytes: Vec<u8>,
    pub checksum: String,
}

impl DownloadedJar {
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }
}

/// チェックサムアルゴリズムの選択肢。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChecksumAlgorithm {
    Sha256,
    Sha1,
    Md5,
}

impl ChecksumAlgorithm {
    pub fn extension(&self) -> &'static str {
        match self {
            ChecksumAlgorithm::Sha256 => "sha256",
            ChecksumAlgorithm::Sha1 => "sha1",
            ChecksumAlgorithm::Md5 => "md5",
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            ChecksumAlgorithm::Sha256 => "SHA-256",
            ChecksumAlgorithm::Sha1 => "SHA-1",
            ChecksumAlgorithm::Md5 => "MD5",
        }
    }

    pub fn compute(&self, bytes: &[u8]) -> String {
        match self {
            ChecksumAlgorithm::Sha256 => format!("{:x}", Sha256::digest(bytes)),
            ChecksumAlgorithm::Sha1 => {
                let mut hasher = Sha1::new();
                hasher.update(bytes);
                format!("{:x}", hasher.finalize())
            }
            ChecksumAlgorithm::Md5 => format!("{:x}", md5::compute(bytes)),
        }
    }
}

impl fmt::Display for ChecksumAlgorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.description())
    }
}

/// チェックサム付きJar。
#[derive(Debug, Clone)]
pub struct ChecksumVerifiedJar {
    pub jar: DownloadedJar,
    pub algorithm: ChecksumAlgorithm,
    pub expected_checksum: String,
}

/// Mavenレジストリに関するエラー種別。
#[derive(Debug, Error)]
pub enum RegistryError {
    #[error("レジストリURL '{url}' が不正です: {source}")]
    InvalidUrl { url: String, source: ParseError },
    #[error("レジストリURLの構築に失敗しました (base: {base}, path: {path}): {source}")]
    UrlJoin {
        base: String,
        path: String,
        source: ParseError,
    },
    #[error("HTTPクライアントの初期化に失敗しました: {source}")]
    ClientBuild { source: reqwest::Error },
    #[error("パッケージ {coordinates} が見つかりません (HTTP {status})")]
    PackageNotFound {
        coordinates: MavenCoordinates,
        status: StatusCode,
    },
    #[error("アーティファクト {resource} {coordinates} が見つかりません (HTTP {status})")]
    ArtifactNotFound {
        coordinates: ArtifactCoordinates,
        resource: ArtifactResource,
        status: StatusCode,
    },
    #[error("HTTP {status} が返されました: {resource}")]
    HttpStatus {
        resource: String,
        status: StatusCode,
    },
    #[error("レジストリ通信に失敗しました: {source}")]
    Network { source: reqwest::Error },
    #[error("{resource} の内容が不正です: {message}")]
    InvalidResponse { resource: String, message: String },
    #[error("チェックサム不一致: {coordinates} (expected={expected}, actual={actual})")]
    ChecksumMismatch {
        coordinates: ArtifactCoordinates,
        expected: String,
        actual: String,
    },
    #[error("チェックサムファイルが見つかりません ({algorithms:?}): {coordinates}")]
    ChecksumMissing {
        coordinates: ArtifactCoordinates,
        algorithms: Vec<String>,
    },
    #[error("メタデータの解析に失敗しました: {source}")]
    MetadataParse {
        coordinates: MavenCoordinates,
        source: MetadataParseError,
    },
}

/// Maven Central などのHTTPレジストリクライアント。
pub struct MavenRegistry {
    client: Client,
    base_url: Url,
    retry: RetryConfig,
}

impl MavenRegistry {
    /// デフォルト設定でクライアントを生成する。
    pub fn new(base_url: impl AsRef<str>) -> Result<Self, RegistryError> {
        Self::with_retry_config(base_url, RetryConfig::default())
    }

    /// リトライ設定を指定してクライアントを生成する。
    pub fn with_retry_config(
        base_url: impl AsRef<str>,
        retry: RetryConfig,
    ) -> Result<Self, RegistryError> {
        let client = Client::builder()
            .timeout(DEFAULT_TIMEOUT)
            // Avoid HTTP/2 oddities against Maven Central; prefer HTTP/1.1.
            .http1_only()
            .user_agent(format!("jv-pm/{}", env!("CARGO_PKG_VERSION")))
            .build()
            .map_err(|source| RegistryError::ClientBuild { source })?;
        Self::with_client(base_url, client, retry)
    }

    /// 既存のHTTPクライアントを利用してインスタンス化する。
    pub fn with_client(
        base_url: impl AsRef<str>,
        client: Client,
        retry: RetryConfig,
    ) -> Result<Self, RegistryError> {
        let mut base_url =
            Url::parse(base_url.as_ref()).map_err(|source| RegistryError::InvalidUrl {
                url: base_url.as_ref().to_string(),
                source,
            })?;
        ensure_trailing_slash(&mut base_url);

        Ok(Self {
            client,
            base_url,
            retry,
        })
    }

    pub fn base_url(&self) -> &Url {
        &self.base_url
    }

    pub fn retry_config(&self) -> RetryConfig {
        self.retry
    }

    /// `maven-metadata.xml` を取得して構造体へ変換する。
    pub async fn fetch_metadata(
        &self,
        coords: &MavenCoordinates,
    ) -> Result<MavenMetadata, RegistryError> {
        let url = self.join_path(&format!("{}/maven-metadata.xml", coords.path()))?;
        let bytes = self
            .request_bytes(url, ResourceKind::Metadata(coords))
            .await?;
        metadata::parse_metadata(bytes.as_ref()).map_err(|source| RegistryError::MetadataParse {
            coordinates: coords.clone(),
            source,
        })
    }

    /// POMファイルを取得し、UTF-8文字列として返す。
    pub async fn fetch_pom(&self, coords: &ArtifactCoordinates) -> Result<String, RegistryError> {
        let url = self.join_path(&coords.pom_path())?;
        let bytes = self.request_bytes(url, ResourceKind::Pom(coords)).await?;
        std::str::from_utf8(bytes.as_ref())
            .map(|text| text.to_owned())
            .map_err(|error| RegistryError::InvalidResponse {
                resource: format!("pom {}", coords),
                message: error.to_string(),
            })
    }

    /// JARをダウンロードし、チェックサムで完全性を検証する。
    pub async fn download_jar(
        &self,
        coords: &ArtifactCoordinates,
    ) -> Result<DownloadedJar, RegistryError> {
        let result = self
            .download_jar_with_algorithms(coords, &[ChecksumAlgorithm::Sha256])
            .await?;
        Ok(result.jar)
    }

    /// 指定アルゴリズムのチェックサムファイルを取得し、正規化したハッシュ文字列を返す。
    pub async fn download_checksum_with_algorithm(
        &self,
        coords: &ArtifactCoordinates,
        algorithm: ChecksumAlgorithm,
    ) -> Result<String, RegistryError> {
        let url = self.join_path(&coords.checksum_path_with_extension(algorithm.extension()))?;
        let bytes = self
            .request_bytes(url, ResourceKind::Checksum(coords))
            .await?;
        let text = std::str::from_utf8(bytes.as_ref()).map_err(|error| {
            RegistryError::InvalidResponse {
                resource: format!("checksum {}", coords),
                message: error.to_string(),
            }
        })?;
        extract_checksum(text).ok_or_else(|| RegistryError::InvalidResponse {
            resource: format!("checksum {} ({})", coords, algorithm),
            message: format!("{}チェックサムが含まれていません", algorithm.description()),
        })
    }

    pub async fn download_checksum(
        &self,
        coords: &ArtifactCoordinates,
    ) -> Result<String, RegistryError> {
        self.download_checksum_with_algorithm(coords, ChecksumAlgorithm::Sha256)
            .await
    }

    pub async fn download_jar_with_algorithms(
        &self,
        coords: &ArtifactCoordinates,
        algorithms: &[ChecksumAlgorithm],
    ) -> Result<ChecksumVerifiedJar, RegistryError> {
        let url = self.join_path(&coords.jar_path())?;
        let jar_bytes = self.request_bytes(url, ResourceKind::Jar(coords)).await?;

        for algorithm in algorithms {
            match self
                .download_checksum_with_algorithm(coords, *algorithm)
                .await
            {
                Ok(expected) => {
                    let actual = algorithm.compute(jar_bytes.as_ref());
                    if actual != expected {
                        warn!(
                            artifact = %coords,
                            algorithm = %algorithm,
                            expected = %expected,
                            actual = %actual,
                            "ダウンロードしたアーティファクトのチェックサムが一致しません"
                        );
                        return Err(RegistryError::ChecksumMismatch {
                            coordinates: coords.clone(),
                            expected,
                            actual,
                        });
                    }

                    return Ok(ChecksumVerifiedJar {
                        jar: DownloadedJar {
                            bytes: jar_bytes.to_vec(),
                            checksum: actual,
                        },
                        algorithm: *algorithm,
                        expected_checksum: expected,
                    });
                }
                Err(error) => match error {
                    RegistryError::ArtifactNotFound {
                        resource: ArtifactResource::Checksum,
                        ..
                    } => continue,
                    other => return Err(other),
                },
            }
        }

        Err(RegistryError::ChecksumMissing {
            coordinates: coords.clone(),
            algorithms: algorithms
                .iter()
                .map(|algorithm| algorithm.description().to_string())
                .collect(),
        })
    }

    fn join_path(&self, relative: &str) -> Result<Url, RegistryError> {
        self.base_url
            .join(relative)
            .map_err(|source| RegistryError::UrlJoin {
                base: self.base_url.to_string(),
                path: relative.to_string(),
                source,
            })
    }

    async fn request_bytes(
        &self,
        url: Url,
        resource: ResourceKind<'_>,
    ) -> Result<Bytes, RegistryError> {
        let max_attempts = self.retry.max_attempts.max(1);
        for attempt in 1..=max_attempts {
            let request = self.client.get(url.clone());
            debug!(
                attempt,
                max_attempts,
                target = %resource,
                url = %url,
                "レジストリリクエストを送信"
            );

            match request.send().await {
                Ok(response) => {
                    let status = response.status();
                    if status.is_success() {
                        return response
                            .bytes()
                            .await
                            .map_err(|source| RegistryError::Network { source });
                    }

                    if status == StatusCode::NOT_FOUND || status == StatusCode::GONE {
                        warn!(
                            status = %status,
                            target = %resource,
                            url = %url,
                            "HTTP {} を受信。再試行せず失敗 (policy: 4xx no-retry)",
                            status
                        );
                        return Err(resource.not_found_error(status));
                    }

                    if status.is_client_error() {
                        warn!(
                            status = %status,
                            target = %resource,
                            url = %url,
                            "HTTP {} を受信。再試行しません (policy: 4xx no-retry)",
                            status
                        );
                        return Err(RegistryError::HttpStatus {
                            resource: resource.description(),
                            status,
                        });
                    }

                    if status.is_server_error() {
                        warn!(
                            attempt,
                            max_attempts,
                            status = %status,
                            target = %resource,
                            url = %url,
                            "HTTP {} を受信。再試行 (policy: 5xx only)",
                            status
                        );
                        if attempt == max_attempts {
                            return Err(RegistryError::HttpStatus {
                                resource: resource.description(),
                                status,
                            });
                        }
                        let delay = self.retry.delay_for_attempt(attempt);
                        sleep(delay).await;
                        continue;
                    }

                    warn!(
                        attempt,
                        max_attempts,
                        status = %status,
                        target = %resource,
                        url = %url,
                        "想定外のHTTP応答。再試行を継続"
                    );
                    if attempt == max_attempts {
                        return Err(RegistryError::HttpStatus {
                            resource: resource.description(),
                            status,
                        });
                    }
                    let delay = self.retry.delay_for_attempt(attempt);
                    sleep(delay).await;
                }
                Err(error) => {
                    // Emit full error chain to aid diagnosing transport failures.
                    let mut chain = format!("{error:?}");
                    let mut curr = error.source();
                    while let Some(src) = curr {
                        chain.push_str(&format!(" | caused by: {src}"));
                        curr = src.source();
                    }
                    let retryable = error.is_timeout() || error.is_connect();
                    warn!(
                        attempt,
                        max_attempts,
                        target = %resource,
                        url = %url,
                        retryable,
                        error = %chain,
                        "HTTPリクエスト失敗"
                    );
                    if retryable && attempt < max_attempts {
                        let delay = self.retry.delay_for_attempt(attempt);
                        sleep(delay).await;
                        continue;
                    }
                    return Err(RegistryError::Network { source: error });
                }
            }
        }

        Err(RegistryError::HttpStatus {
            resource: resource.description(),
            status: StatusCode::INTERNAL_SERVER_ERROR,
        })
    }
}

fn ensure_trailing_slash(url: &mut Url) {
    if !url.path().ends_with('/') {
        let mut path = url.path().to_string();
        if !path.ends_with('/') {
            path.push('/');
        }
        url.set_path(&path);
    }
}

fn extract_checksum(text: &str) -> Option<String> {
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

#[derive(Clone, Copy)]
enum ResourceKind<'a> {
    Metadata(&'a MavenCoordinates),
    Pom(&'a ArtifactCoordinates),
    Jar(&'a ArtifactCoordinates),
    Checksum(&'a ArtifactCoordinates),
}

impl fmt::Display for ResourceKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.description())
    }
}

impl<'a> ResourceKind<'a> {
    fn description(&self) -> String {
        match self {
            ResourceKind::Metadata(coords) => format!("metadata {}", coords),
            ResourceKind::Pom(coords) => format!("pom {}", coords),
            ResourceKind::Jar(coords) => format!("jar {}", coords),
            ResourceKind::Checksum(coords) => format!("checksum {}", coords),
        }
    }

    fn not_found_error(&self, status: StatusCode) -> RegistryError {
        match self {
            ResourceKind::Metadata(coords) => RegistryError::PackageNotFound {
                coordinates: (*coords).clone(),
                status,
            },
            ResourceKind::Pom(coords) => RegistryError::ArtifactNotFound {
                coordinates: (*coords).clone(),
                resource: ArtifactResource::Pom,
                status,
            },
            ResourceKind::Jar(coords) => RegistryError::ArtifactNotFound {
                coordinates: (*coords).clone(),
                resource: ArtifactResource::Jar,
                status,
            },
            ResourceKind::Checksum(coords) => RegistryError::ArtifactNotFound {
                coordinates: (*coords).clone(),
                resource: ArtifactResource::Checksum,
                status,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn artifact_coordinate_paths_include_classifier() {
        let base = ArtifactCoordinates::new("org.example", "demo", "1.0.0");
        assert_eq!(base.jar_path(), "org/example/demo/1.0.0/demo-1.0.0.jar");
        assert_eq!(
            base.checksum_path(),
            "org/example/demo/1.0.0/demo-1.0.0.jar.sha256"
        );

        let sources = base.clone().with_classifier("sources");
        assert_eq!(
            sources.jar_path(),
            "org/example/demo/1.0.0/demo-1.0.0-sources.jar"
        );
        assert_eq!(
            sources.checksum_path(),
            "org/example/demo/1.0.0/demo-1.0.0-sources.jar.sha256"
        );
    }

    #[test]
    fn extract_checksum_returns_lowercase_first_token() {
        let text = " \n 5F366B some-file.jar\n";
        assert_eq!(extract_checksum(text), Some("5f366b".to_string()));
    }
}
