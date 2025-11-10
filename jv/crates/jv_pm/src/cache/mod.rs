use crate::registry::{ArtifactCoordinates, MavenCoordinates, MavenMetadata};
use sha2::{Digest, Sha256};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use thiserror::Error;
use tracing::{debug, warn};

#[derive(Debug, Error)]
pub enum CacheError {
    #[error("ホームディレクトリを特定できませんでした")]
    HomeDirNotFound,
    #[error("キャッシュパス {path:?} の親ディレクトリを解決できませんでした")]
    MissingParent { path: PathBuf },
    #[error("IOエラー: {0}")]
    Io(#[from] std::io::Error),
    #[error("JSONシリアライズ/デシリアライズに失敗しました: {0}")]
    Json(#[from] serde_json::Error),
    #[error("キャッシュ整合性エラー: {message} ({path:?})")]
    Integrity { path: PathBuf, message: String },
    #[error("チェックサムが一致しません: expected {expected}, actual {actual} ({path:?})")]
    ChecksumMismatch {
        path: PathBuf,
        expected: String,
        actual: String,
    },
}

#[derive(Debug, Clone)]
pub struct CachedArtifact {
    pub path: PathBuf,
    pub size: u64,
    pub checksum: Option<String>,
}

#[derive(Debug, Clone)]
pub struct CachedPom {
    pub path: PathBuf,
    pub content: String,
}

#[derive(Debug, Clone)]
pub struct CachedMetadata {
    pub path: PathBuf,
    pub metadata: MavenMetadata,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CacheStatsSnapshot {
    pub artifact_hits: u64,
    pub artifact_misses: u64,
    pub pom_hits: u64,
    pub pom_misses: u64,
    pub metadata_hits: u64,
    pub metadata_misses: u64,
}

#[derive(Debug, Default)]
struct CacheStats {
    artifact_hits: AtomicU64,
    artifact_misses: AtomicU64,
    pom_hits: AtomicU64,
    pom_misses: AtomicU64,
    metadata_hits: AtomicU64,
    metadata_misses: AtomicU64,
}

impl CacheStats {
    fn record_artifact_hit(&self) {
        self.artifact_hits.fetch_add(1, Ordering::Relaxed);
    }

    fn record_artifact_miss(&self) {
        self.artifact_misses.fetch_add(1, Ordering::Relaxed);
    }

    fn record_pom_hit(&self) {
        self.pom_hits.fetch_add(1, Ordering::Relaxed);
    }

    fn record_pom_miss(&self) {
        self.pom_misses.fetch_add(1, Ordering::Relaxed);
    }

    fn record_metadata_hit(&self) {
        self.metadata_hits.fetch_add(1, Ordering::Relaxed);
    }

    fn record_metadata_miss(&self) {
        self.metadata_misses.fetch_add(1, Ordering::Relaxed);
    }

    fn snapshot(&self) -> CacheStatsSnapshot {
        CacheStatsSnapshot {
            artifact_hits: self.artifact_hits.load(Ordering::Relaxed),
            artifact_misses: self.artifact_misses.load(Ordering::Relaxed),
            pom_hits: self.pom_hits.load(Ordering::Relaxed),
            pom_misses: self.pom_misses.load(Ordering::Relaxed),
            metadata_hits: self.metadata_hits.load(Ordering::Relaxed),
            metadata_misses: self.metadata_misses.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug)]
pub struct DependencyCache {
    root: PathBuf,
    stats: CacheStats,
}

impl DependencyCache {
    /// グローバルキャッシュ (`~/.jv/cache`) を初期化する。
    pub fn global() -> Result<Self, CacheError> {
        let home = dirs::home_dir().ok_or(CacheError::HomeDirNotFound)?;
        let base = home.join(".jv");
        ensure_directory(&base)?;
        let cache_dir = base.join("cache");
        Self::with_dir(cache_dir)
    }

    /// 指定ディレクトリでキャッシュを初期化する（主にテスト用）。
    pub fn with_dir(cache_dir: PathBuf) -> Result<Self, CacheError> {
        ensure_directory(&cache_dir)?;
        let cache = Self {
            root: cache_dir,
            stats: CacheStats::default(),
        };
        cache.ensure_layout()?;
        Ok(cache)
    }

    /// キャッシュルートディレクトリを返す。
    pub fn base_dir(&self) -> &Path {
        &self.root
    }

    /// JARを保存・取得するサブディレクトリ。
    fn jars_dir(&self) -> PathBuf {
        self.root.join("jars")
    }

    /// POMを保存・取得するサブディレクトリ。
    fn poms_dir(&self) -> PathBuf {
        self.root.join("poms")
    }

    /// Mavenメタデータを保存・取得するサブディレクトリ。
    fn metadata_dir(&self) -> PathBuf {
        self.root.join("metadata")
    }

    fn ensure_layout(&self) -> Result<(), CacheError> {
        ensure_directory(&self.jars_dir())?;
        ensure_directory(&self.poms_dir())?;
        ensure_directory(&self.metadata_dir())?;
        Ok(())
    }

    /// JARのキャッシュパスを求める。
    pub fn artifact_path(&self, coords: &ArtifactCoordinates) -> PathBuf {
        let dir = self.version_dir(self.jars_dir(), coords);
        dir.join(jar_file_name(coords))
    }

    /// POMのキャッシュパスを求める。
    pub fn pom_path(&self, coords: &ArtifactCoordinates) -> PathBuf {
        let dir = self.version_dir(self.poms_dir(), coords);
        dir.join(pom_file_name(coords))
    }

    /// メタデータのキャッシュパスを求める。
    pub fn metadata_path(&self, coords: &MavenCoordinates) -> PathBuf {
        let dir = self.group_artifact_dir(self.metadata_dir(), coords);
        dir.join("maven-metadata.json")
    }

    fn group_artifact_dir(&self, mut base: PathBuf, coords: &MavenCoordinates) -> PathBuf {
        for segment in coords.group_id.split('.') {
            base.push(segment);
        }
        base.push(&coords.artifact_id);
        base
    }

    fn version_dir(&self, base: PathBuf, coords: &ArtifactCoordinates) -> PathBuf {
        let mut dir = self.group_artifact_dir(base, &coords.maven_coordinates());
        dir.push(&coords.version);
        dir
    }

    /// キャッシュ済みJARを取得する。`expected_checksum`が指定されていれば整合性を検証する。
    pub fn get_artifact(
        &self,
        coords: &ArtifactCoordinates,
        expected_checksum: Option<&str>,
    ) -> Result<Option<CachedArtifact>, CacheError> {
        let path = self.artifact_path(coords);
        if !path.exists() {
            debug!(artifact = %coords, "JARキャッシュミス");
            self.stats.record_artifact_miss();
            return Ok(None);
        }

        let stored_checksum_path = path.with_extension("jar.sha256");
        let stored_checksum = if stored_checksum_path.exists() {
            Some(
                fs::read_to_string(&stored_checksum_path)?
                    .trim()
                    .to_string(),
            )
        } else {
            None
        };

        let checksum_to_verify = expected_checksum
            .map(str::to_owned)
            .or(stored_checksum.clone());
        let mut validated_checksum = stored_checksum.clone();

        if let Some(expected) = checksum_to_verify.as_deref() {
            let actual = compute_file_sha256(&path)?;
            if actual != expected {
                self.stats.record_artifact_miss();
                warn!(
                    artifact = %coords,
                    path = %path.display(),
                    %expected,
                    %actual,
                    "JARキャッシュのチェックサムが一致しません"
                );
                return Err(CacheError::ChecksumMismatch {
                    path,
                    expected: expected.to_string(),
                    actual,
                });
            }
            validated_checksum = Some(actual);
        }

        let size = fs::metadata(&path)?.len();
        self.stats.record_artifact_hit();
        Ok(Some(CachedArtifact {
            path,
            size,
            checksum: validated_checksum.or(stored_checksum),
        }))
    }

    /// JARをキャッシュへ保存する。保存前にチェックサムを検証する。
    pub fn store_artifact(
        &self,
        coords: &ArtifactCoordinates,
        bytes: &[u8],
        expected_checksum: &str,
    ) -> Result<CachedArtifact, CacheError> {
        let path = self.artifact_path(coords);
        let actual = sha256_hex(bytes);
        if actual != expected_checksum {
            warn!(
                artifact = %coords,
                expected = expected_checksum,
                %actual,
                "保存対象JARのチェックサムが一致しないためキャッシュへ保存しません"
            );
            return Err(CacheError::ChecksumMismatch {
                path,
                expected: expected_checksum.to_string(),
                actual,
            });
        }

        write_bytes(&path, bytes)?;
        let checksum_path = path.with_extension("jar.sha256");
        write_text(&checksum_path, expected_checksum)?;

        let size = bytes.len() as u64;
        Ok(CachedArtifact {
            path,
            size,
            checksum: Some(actual),
        })
    }

    /// POMを取得する。
    pub fn get_pom(&self, coords: &ArtifactCoordinates) -> Result<Option<CachedPom>, CacheError> {
        let path = self.pom_path(coords);
        if !path.exists() {
            debug!(artifact = %coords, "POMキャッシュミス");
            self.stats.record_pom_miss();
            return Ok(None);
        }

        let content = fs::read_to_string(&path)?;
        self.stats.record_pom_hit();
        Ok(Some(CachedPom { path, content }))
    }

    /// POMを保存する。
    pub fn store_pom(
        &self,
        coords: &ArtifactCoordinates,
        pom_xml: &str,
    ) -> Result<CachedPom, CacheError> {
        let path = self.pom_path(coords);
        write_text(&path, pom_xml)?;
        Ok(CachedPom {
            path,
            content: pom_xml.to_string(),
        })
    }

    /// Mavenメタデータを取得する（グループ単位）。
    pub fn get_metadata(
        &self,
        coords: &MavenCoordinates,
    ) -> Result<Option<CachedMetadata>, CacheError> {
        let path = self.metadata_path(coords);
        if !path.exists() {
            debug!(coordinates = %coords, "メタデータキャッシュミス");
            self.stats.record_metadata_miss();
            return Ok(None);
        }

        let content = fs::read_to_string(&path)?;
        let metadata: MavenMetadata = serde_json::from_str(&content)?;
        if metadata.group_id != coords.group_id || metadata.artifact_id != coords.artifact_id {
            self.stats.record_metadata_miss();
            warn!(
                coordinates = %coords,
                path = %path.display(),
                "メタデータキャッシュの整合性が失われています"
            );
            return Err(CacheError::Integrity {
                path,
                message: "グループIDまたはアーティファクトIDが一致しません".to_string(),
            });
        }

        self.stats.record_metadata_hit();
        Ok(Some(CachedMetadata { path, metadata }))
    }

    /// Mavenメタデータを保存する。
    pub fn store_metadata(
        &self,
        coords: &MavenCoordinates,
        metadata: &MavenMetadata,
    ) -> Result<CachedMetadata, CacheError> {
        let path = self.metadata_path(coords);
        if metadata.group_id != coords.group_id || metadata.artifact_id != coords.artifact_id {
            return Err(CacheError::Integrity {
                path,
                message: "座標とメタデータのIDが一致しません".to_string(),
            });
        }

        let serialized = serde_json::to_string_pretty(metadata)?;
        write_text(&path, &serialized)?;
        Ok(CachedMetadata {
            path,
            metadata: metadata.clone(),
        })
    }

    /// キャッシュ統計情報をスナップショットとして返す。
    pub fn stats(&self) -> CacheStatsSnapshot {
        self.stats.snapshot()
    }
}

fn write_bytes(path: &Path, bytes: &[u8]) -> Result<(), CacheError> {
    let parent = path.parent().ok_or_else(|| CacheError::MissingParent {
        path: path.to_path_buf(),
    })?;
    ensure_directory(parent)?;
    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path)?;
    file.write_all(bytes)?;
    file.sync_all()?;
    set_file_permissions(path)?;
    Ok(())
}

fn write_text(path: &Path, text: &str) -> Result<(), CacheError> {
    write_bytes(path, text.as_bytes())
}

fn ensure_directory(path: &Path) -> Result<(), CacheError> {
    fs::create_dir_all(path)?;
    set_dir_permissions(path)?;
    Ok(())
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

fn compute_file_sha256(path: &Path) -> Result<String, CacheError> {
    let mut file = File::open(path)?;
    let mut hasher = Sha256::new();
    let mut buffer = [0u8; 8192];
    loop {
        let read = file.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    Ok(format!("{:x}", hasher.finalize()))
}

fn jar_file_name(coords: &ArtifactCoordinates) -> String {
    match coords.classifier() {
        Some(classifier) => format!(
            "{}-{}-{}.jar",
            coords.artifact_id, coords.version, classifier
        ),
        None => format!("{}-{}.jar", coords.artifact_id, coords.version),
    }
}

fn pom_file_name(coords: &ArtifactCoordinates) -> String {
    format!("{}-{}.pom", coords.artifact_id, coords.version)
}

#[cfg(unix)]
fn set_dir_permissions(path: &Path) -> Result<(), CacheError> {
    use std::os::unix::fs::PermissionsExt;

    let permissions = fs::Permissions::from_mode(0o755);
    fs::set_permissions(path, permissions)?;
    Ok(())
}

#[cfg(unix)]
fn set_file_permissions(path: &Path) -> Result<(), CacheError> {
    use std::os::unix::fs::PermissionsExt;

    let permissions = fs::Permissions::from_mode(0o644);
    fs::set_permissions(path, permissions)?;
    Ok(())
}

#[cfg(not(unix))]
fn set_dir_permissions(_path: &Path) -> Result<(), CacheError> {
    Ok(())
}

#[cfg(not(unix))]
fn set_file_permissions(_path: &Path) -> Result<(), CacheError> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::MavenMetadata;
    use tempfile::tempdir;

    #[test]
    fn initialize_layout_creates_directories() -> Result<(), CacheError> {
        let temp = tempdir().expect("tempdir");
        let cache_root = temp.path().join("cache-root");
        DependencyCache::with_dir(cache_root.clone())?;

        assert!(cache_root.join("jars").exists());
        assert!(cache_root.join("poms").exists());
        assert!(cache_root.join("metadata").exists());

        Ok(())
    }

    #[test]
    fn store_and_load_artifact() -> Result<(), CacheError> {
        let temp = tempdir().expect("tempdir");
        let cache = DependencyCache::with_dir(temp.path().join("cache"))?;
        let coords = ArtifactCoordinates::new("org.example", "demo", "1.0.0");
        let bytes = b"hello world";
        let expected = sha256_hex(bytes);

        cache.store_artifact(&coords, bytes, &expected)?;

        let cached = cache
            .get_artifact(&coords, Some(&expected))?
            .expect("artifact cached");
        assert_eq!(cached.size, bytes.len() as u64);
        assert_eq!(cached.checksum.as_deref(), Some(expected.as_str()));

        let stats = cache.stats();
        assert_eq!(stats.artifact_hits, 1);
        assert_eq!(stats.artifact_misses, 0);

        Ok(())
    }

    #[test]
    fn artifact_checksum_mismatch_returns_error() -> Result<(), CacheError> {
        let temp = tempdir().expect("tempdir");
        let cache = DependencyCache::with_dir(temp.path().join("cache"))?;
        let coords = ArtifactCoordinates::new("org.example", "demo", "1.0.0");
        let bytes = b"hello world";
        let expected = sha256_hex(bytes);
        cache.store_artifact(&coords, bytes, &expected)?;

        let result = cache.get_artifact(&coords, Some("deadbeef"));
        assert!(matches!(result, Err(CacheError::ChecksumMismatch { .. })));

        Ok(())
    }

    #[test]
    fn store_and_load_pom() -> Result<(), CacheError> {
        let temp = tempdir().expect("tempdir");
        let cache = DependencyCache::with_dir(temp.path().join("cache"))?;
        let coords = ArtifactCoordinates::new("org.example", "demo", "1.0.0");
        let pom = "<project></project>";

        cache.store_pom(&coords, pom)?;
        let loaded = cache.get_pom(&coords)?.expect("pom cached");
        assert_eq!(loaded.content, pom);

        let stats = cache.stats();
        assert_eq!(stats.pom_hits, 1);
        assert_eq!(stats.pom_misses, 0);

        Ok(())
    }

    #[test]
    fn store_and_load_metadata() -> Result<(), CacheError> {
        let temp = tempdir().expect("tempdir");
        let cache = DependencyCache::with_dir(temp.path().join("cache"))?;
        let coords = MavenCoordinates::new("org.example", "demo");
        let metadata = MavenMetadata {
            group_id: "org.example".into(),
            artifact_id: "demo".into(),
            versioning: None,
        };

        cache.store_metadata(&coords, &metadata)?;
        let loaded = cache.get_metadata(&coords)?.expect("metadata cached");
        assert_eq!(loaded.metadata, metadata);

        let stats = cache.stats();
        assert_eq!(stats.metadata_hits, 1);
        assert_eq!(stats.metadata_misses, 0);

        Ok(())
    }

    #[test]
    fn cache_miss_updates_stats() -> Result<(), CacheError> {
        let temp = tempdir().expect("tempdir");
        let cache = DependencyCache::with_dir(temp.path().join("cache"))?;
        let coords = ArtifactCoordinates::new("org.example", "demo", "1.0.0");
        assert!(cache.get_pom(&coords)?.is_none());
        let mcoords = MavenCoordinates::new("org.example", "demo");
        assert!(cache.get_metadata(&mcoords)?.is_none());

        let stats = cache.stats();
        assert_eq!(stats.pom_misses, 1);
        assert_eq!(stats.metadata_misses, 1);

        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn file_permissions_are_enforced() -> Result<(), CacheError> {
        use std::os::unix::fs::PermissionsExt;

        let temp = tempdir().expect("tempdir");
        let cache_root = temp.path().join("cache-root");
        let cache = DependencyCache::with_dir(cache_root.clone())?;
        let coords = ArtifactCoordinates::new("org.example", "demo", "1.0.0");
        let bytes = b"hello";
        let checksum = sha256_hex(bytes);
        cache.store_artifact(&coords, bytes, &checksum)?;
        cache.store_pom(&coords, "<project />")?;

        let jar_path = cache.artifact_path(&coords);
        let pom_path = cache.pom_path(&coords);

        assert_eq!(fs::metadata(&jar_path)?.permissions().mode() & 0o777, 0o644);
        assert_eq!(fs::metadata(&pom_path)?.permissions().mode() & 0o777, 0o644);
        assert_eq!(
            fs::metadata(cache_root.join("jars"))?.permissions().mode() & 0o777,
            0o755
        );

        Ok(())
    }
}
