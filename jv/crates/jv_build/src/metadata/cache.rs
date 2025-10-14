use super::builder::BuildContext;
use super::index::SymbolIndex;
use blake3::Hasher;
use serde::{Deserialize, Serialize};
use std::cmp;
use std::fs::{self, File};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use thiserror::Error;

const CACHE_VERSION: u32 = 1;
const FINGERPRINT_VERSION: u32 = 1;

/// Error type for cache load/store operations.
#[derive(Debug, Error)]
pub enum CacheError {
    #[error("cache IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("cache encoding error: {0}")]
    Encode(#[from] bincode::Error),
    #[error("cache version mismatch (expected {expected}, found {found})")]
    VersionMismatch { expected: u32, found: u32 },
    #[error("cache fingerprint mismatch")]
    FingerprintMismatch,
}

/// Metrics captured when the cache entry was produced.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CacheMetrics {
    pub build_ms: u64,
    pub artifact_count: u32,
    pub recorded_at_ms: u64,
    pub memory_bytes: Option<u64>,
}

/// Statistics collected when persisting a fresh index to the cache.
#[derive(Debug, Clone)]
pub struct CacheStoreStats {
    pub build_duration: Duration,
    pub artifact_count: usize,
    pub memory_bytes: Option<u64>,
}

#[derive(Debug, Serialize, Deserialize)]
struct CacheFile {
    version: u32,
    fingerprint: CacheFingerprint,
    metrics: CacheMetrics,
    index: SymbolIndex,
}

/// Artifact fingerprint describing inputs that produced the cache entry.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CacheFingerprint {
    version: u32,
    target: String,
    java_home: Option<String>,
    module_artifacts: Vec<ArtifactFingerprint>,
    classpath: Vec<ArtifactFingerprint>,
}

impl CacheFingerprint {
    pub(crate) fn capture(
        context: &BuildContext,
        module_artifacts: &[PathBuf],
    ) -> Result<Self, CacheError> {
        let module_fps = module_artifacts
            .iter()
            .map(|path| ArtifactFingerprint::capture(path, ArtifactSource::ModulePath))
            .collect::<Result<Vec<_>, _>>()?;

        let classpath_fps = context
            .classpath
            .iter()
            .map(|path| ArtifactFingerprint::capture(path, ArtifactSource::Classpath))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            version: FINGERPRINT_VERSION,
            target: context.target.as_str().to_string(),
            java_home: context
                .java_home
                .as_ref()
                .map(|path| path.to_string_lossy().into_owned()),
            module_artifacts: module_fps,
            classpath: classpath_fps,
        })
    }

    fn digest_hex(&self) -> Result<String, CacheError> {
        let encoded = bincode::serialize(self)?;
        let mut hasher = Hasher::new();
        hasher.update(&encoded);
        Ok(hasher.finalize().to_hex().to_string())
    }

    pub(crate) fn artifact_count(&self) -> usize {
        self.module_artifacts.len() + self.classpath.len()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
enum ArtifactSource {
    ModulePath,
    Classpath,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
struct ArtifactFingerprint {
    path: String,
    source: ArtifactSource,
    kind: ArtifactKind,
}

impl ArtifactFingerprint {
    fn capture(path: &Path, source: ArtifactSource) -> Result<Self, CacheError> {
        let kind = artifact_kind(path)?;
        Ok(Self {
            path: path.to_string_lossy().into_owned(),
            source,
            kind,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
enum ArtifactKind {
    Missing,
    File {
        len: u64,
        modified_secs: Option<u64>,
    },
    Directory {
        total_entries: u64,
        total_size: u64,
        latest_modified_secs: Option<u64>,
    },
}

fn artifact_kind(path: &Path) -> Result<ArtifactKind, CacheError> {
    if !path.exists() {
        return Ok(ArtifactKind::Missing);
    }

    let metadata = fs::metadata(path)?;
    if metadata.is_dir() {
        aggregate_directory(path, &metadata)
    } else {
        Ok(ArtifactKind::File {
            len: metadata.len(),
            modified_secs: metadata.modified().ok().and_then(system_time_to_secs),
        })
    }
}

fn aggregate_directory(path: &Path, metadata: &fs::Metadata) -> Result<ArtifactKind, CacheError> {
    let mut stack = vec![path.to_path_buf()];
    let mut total_entries = 0u64;
    let mut total_size = 0u64;
    let mut latest_modified = metadata.modified().ok().and_then(system_time_to_secs);

    while let Some(dir) = stack.pop() {
        for entry in fs::read_dir(&dir)? {
            let entry = entry?;
            let meta = entry.metadata()?;
            if meta.is_dir() {
                stack.push(entry.path());
                latest_modified = cmp::max(
                    latest_modified,
                    meta.modified().ok().and_then(system_time_to_secs),
                );
            } else {
                total_entries += 1;
                total_size += meta.len();
                latest_modified = cmp::max(
                    latest_modified,
                    meta.modified().ok().and_then(system_time_to_secs),
                );
            }
        }
    }

    Ok(ArtifactKind::Directory {
        total_entries,
        total_size,
        latest_modified_secs: latest_modified,
    })
}

fn system_time_to_secs(time: SystemTime) -> Option<u64> {
    time.duration_since(UNIX_EPOCH)
        .ok()
        .map(|duration| duration.as_secs())
}

/// Cached symbol index alongside the metrics captured when storing it.
#[derive(Debug)]
pub struct CacheEntry {
    pub index: SymbolIndex,
    pub metrics: CacheMetrics,
}

/// Persistent cache manager for symbol indices.
#[derive(Debug, Clone)]
pub struct SymbolIndexCache {
    root: PathBuf,
}

impl SymbolIndexCache {
    /// Construct a cache rooted under the provided directory.
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    /// Construct a cache using the default location inside the build target directory.
    pub fn with_default_location() -> Self {
        Self {
            root: default_cache_dir(),
        }
    }

    /// Compute the cache key (hex digest) for the supplied fingerprint.
    pub fn cache_key(&self, fingerprint: &CacheFingerprint) -> Result<String, CacheError> {
        fingerprint.digest_hex()
    }

    /// Build the environmental fingerprint describing the inputs for the symbol index.
    pub fn fingerprint(
        &self,
        context: &BuildContext,
        module_artifacts: &[PathBuf],
    ) -> Result<CacheFingerprint, CacheError> {
        CacheFingerprint::capture(context, module_artifacts)
    }

    /// Attempt to load a cached symbol index.
    pub fn load(&self, fingerprint: &CacheFingerprint) -> Result<Option<CacheEntry>, CacheError> {
        let key = fingerprint.digest_hex()?;
        let path = self.root.join(format!("symbol-index-{}.bin", key));

        if !path.exists() {
            return Ok(None);
        }

        let file = File::open(&path)?;
        let mut reader = BufReader::new(file);
        let entry: CacheFile = bincode::deserialize_from(&mut reader)?;

        if entry.version != CACHE_VERSION {
            return Err(CacheError::VersionMismatch {
                expected: CACHE_VERSION,
                found: entry.version,
            });
        }

        if entry.fingerprint != *fingerprint {
            return Err(CacheError::FingerprintMismatch);
        }

        Ok(Some(CacheEntry {
            index: entry.index,
            metrics: entry.metrics,
        }))
    }

    /// Persist a freshly built index to disk.
    pub fn store(
        &self,
        fingerprint: &CacheFingerprint,
        index: &SymbolIndex,
        stats: CacheStoreStats,
    ) -> Result<(), CacheError> {
        let key = fingerprint.digest_hex()?;
        fs::create_dir_all(&self.root)?;
        let path = self.root.join(format!("symbol-index-{}.bin", key));

        let metrics = CacheMetrics {
            build_ms: stats.build_duration.as_millis().min(u128::from(u64::MAX)) as u64,
            artifact_count: stats.artifact_count.min(u32::MAX as usize) as u32,
            recorded_at_ms: current_epoch_millis(),
            memory_bytes: stats.memory_bytes,
        };

        let file = File::create(path)?;
        let mut writer = BufWriter::new(file);
        let entry = CacheFile {
            version: CACHE_VERSION,
            fingerprint: fingerprint.clone(),
            metrics,
            index: index.clone(),
        };

        bincode::serialize_into(&mut writer, &entry)?;
        Ok(())
    }
}

fn current_epoch_millis() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_millis().min(u128::from(u64::MAX)) as u64)
        .unwrap_or(0)
}

fn default_cache_dir() -> PathBuf {
    if let Ok(dir) = std::env::var("JV_CACHE_DIR") {
        PathBuf::from(dir)
    } else if let Ok(dir) = std::env::var("CARGO_TARGET_DIR") {
        PathBuf::from(dir).join("jv").join("symbol-index")
    } else {
        PathBuf::from("target").join("jv").join("symbol-index")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::metadata::ModuleEntry;
    use jv_pm::JavaTarget;
    use std::fs;
    use std::time::Duration;

    fn build_context() -> BuildContext {
        BuildContext {
            target: JavaTarget::Java25,
            java_home: None,
            classpath: Vec::new(),
            module_path: Vec::new(),
        }
    }

    fn unique_temp_dir(label: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        path.push(format!("jv-cache-test-{}-{}", label, std::process::id()));
        fs::create_dir_all(&path).expect("create temp dir");
        path
    }

    #[test]
    fn cache_round_trip() {
        let dir = unique_temp_dir("round-trip");
        let cache = SymbolIndexCache::new(dir.clone());
        let context = build_context();
        let fingerprint = cache
            .fingerprint(&context, &[])
            .expect("fingerprint generation");

        let mut index = SymbolIndex::new(Some(25));
        index.insert_module(ModuleEntry::new("java.base".to_string()));

        cache
            .store(
                &fingerprint,
                &index,
                CacheStoreStats {
                    build_duration: Duration::from_millis(42),
                    artifact_count: 0,
                    memory_bytes: Some(1024),
                },
            )
            .expect("store cache");

        let loaded = cache
            .load(&fingerprint)
            .expect("load cache")
            .expect("cache entry expected");

        assert_eq!(loaded.index.modules.len(), 1);
        assert_eq!(loaded.metrics.build_ms, 42);
        assert_eq!(loaded.metrics.memory_bytes, Some(1024));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn cache_miss_on_changed_classpath() {
        let dir = unique_temp_dir("miss");
        let cache = SymbolIndexCache::new(dir.clone());
        let mut context = build_context();
        let fingerprint = cache
            .fingerprint(&context, &[])
            .expect("fingerprint generation");

        cache
            .store(
                &fingerprint,
                &SymbolIndex::new(Some(25)),
                CacheStoreStats {
                    build_duration: Duration::from_millis(10),
                    artifact_count: 0,
                    memory_bytes: None,
                },
            )
            .expect("store cache");

        let fake_classpath = dir.join("lib.jar");
        fs::write(&fake_classpath, b"binary").expect("write classpath entry");
        context.classpath.push(fake_classpath);

        let fingerprint_changed = cache
            .fingerprint(&context, &[])
            .expect("fingerprint generation");
        let loaded = cache.load(&fingerprint_changed).expect("load cache");
        assert!(loaded.is_none(), "fingerprint change should miss");

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn cache_corruption_reports_error() {
        let dir = unique_temp_dir("corrupt");
        let cache = SymbolIndexCache::new(dir.clone());
        let context = build_context();
        let fingerprint = cache
            .fingerprint(&context, &[])
            .expect("fingerprint generation");

        cache
            .store(
                &fingerprint,
                &SymbolIndex::new(Some(25)),
                CacheStoreStats {
                    build_duration: Duration::from_millis(10),
                    artifact_count: 0,
                    memory_bytes: None,
                },
            )
            .expect("store cache");

        let key = cache.cache_key(&fingerprint).expect("cache key");
        let cache_path = dir.join(format!("symbol-index-{}.bin", key));
        fs::write(&cache_path, b"corrupted").expect("corrupt cache file");

        let error = cache
            .load(&fingerprint)
            .expect_err("corruption should error");
        assert!(matches!(error, CacheError::Encode(_)));

        let _ = fs::remove_dir_all(dir);
    }
}
