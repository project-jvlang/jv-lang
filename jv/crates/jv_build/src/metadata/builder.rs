use super::cache::{CacheStoreStats, SymbolIndexCache};
use super::classfile::{ClassParseError, ModuleInfo, parse_class, parse_module_info};
use super::index::{ModuleEntry, SymbolIndex, TypeEntry};
use crate::{config::BuildConfig, jdk};
use jv_pm::JavaTarget;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::{self, BufReader, Read};
use std::path::{Path, PathBuf};
use std::time::Instant;
use thiserror::Error;
use tracing::{debug, info, warn};
use zip::ZipArchive;
use zip::result::ZipError;

const LOG_TARGET: &str = "jv::build::symbol_index";
const MAX_CLASS_BYTES: u64 = 5 * 1024 * 1024;

#[derive(Debug, Clone)]
pub struct BuildContext {
    pub target: JavaTarget,
    pub java_home: Option<PathBuf>,
    pub classpath: Vec<PathBuf>,
    pub module_path: Vec<PathBuf>,
}

impl BuildContext {
    pub fn from_config(config: &BuildConfig) -> Self {
        Self {
            target: config.target,
            java_home: detect_java_home(),
            classpath: expand_classpath(&config.classpath),
            module_path: Vec::new(),
        }
    }

    pub fn with_java_home(mut self, java_home: impl Into<PathBuf>) -> Self {
        self.java_home = Some(java_home.into());
        self
    }

    pub fn with_module_path<I>(mut self, module_path: I) -> Self
    where
        I: IntoIterator<Item = PathBuf>,
    {
        self.module_path = module_path.into_iter().collect();
        self
    }

    pub fn add_classpath<I>(&mut self, entries: I)
    where
        I: IntoIterator<Item = PathBuf>,
    {
        self.classpath.extend(entries);
    }
}

#[derive(Debug, Error)]
pub enum IndexError {
    #[error("IO error while scanning {path}: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
    #[error("ZIP error while scanning {path}: {source}")]
    Zip {
        path: PathBuf,
        #[source]
        source: ZipError,
    },
    #[error("Class parse error in {path}: {source}")]
    ClassFile {
        path: PathBuf,
        #[source]
        source: ClassParseError,
    },
}

pub struct SymbolIndexBuilder<'a> {
    context: &'a BuildContext,
}

impl<'a> SymbolIndexBuilder<'a> {
    pub fn new(context: &'a BuildContext) -> Self {
        Self { context }
    }

    pub fn from_build_context(context: &'a BuildContext) -> Result<SymbolIndex, IndexError> {
        Self::new(context).build()
    }

    pub fn build(&self) -> Result<SymbolIndex, IndexError> {
        let module_artifacts = self.resolve_module_artifacts()?;
        self.build_with_artifacts(&module_artifacts)
    }

    pub fn build_with_cache(&self, cache: &SymbolIndexCache) -> Result<SymbolIndex, IndexError> {
        let module_artifacts = self.resolve_module_artifacts()?;

        let fingerprint = match cache.fingerprint(self.context, &module_artifacts) {
            Ok(fp) => fp,
            Err(error) => {
                warn!(target: LOG_TARGET, %error, "Failed to compute symbol index fingerprint; rebuilding without cache");
                return self.build_with_artifacts(&module_artifacts);
            }
        };

        let cache_key = match cache.cache_key(&fingerprint) {
            Ok(key) => key,
            Err(error) => {
                warn!(target: LOG_TARGET, %error, "Failed to derive cache key; rebuilding without cache");
                return self.build_with_artifacts(&module_artifacts);
            }
        };

        let load_started = Instant::now();
        match cache.load(&fingerprint) {
            Ok(Some(entry)) => {
                let load_elapsed = load_started.elapsed();
                info!(
                    target: LOG_TARGET,
                    action = "cache-hit",
                    cache_key = cache_key.as_str(),
                    load_ms = load_elapsed.as_millis() as u64,
                    recorded_build_ms = entry.metrics.build_ms,
                    recorded_artifact_count = entry.metrics.artifact_count,
                    recorded_memory_bytes = entry.metrics.memory_bytes.unwrap_or(0),
                    "Reusing cached SymbolIndex"
                );
                return Ok(entry.index);
            }
            Ok(None) => {
                debug!(
                    target: LOG_TARGET,
                    action = "cache-miss",
                    cache_key = cache_key.as_str(),
                    "SymbolIndex cache miss"
                );
            }
            Err(error) => {
                warn!(
                    target: LOG_TARGET,
                    action = "cache-error",
                    cache_key = cache_key.as_str(),
                    %error,
                    "Failed to load SymbolIndex cache; rebuilding"
                );
            }
        }

        let build_started = Instant::now();
        let memory_before = memory_usage_bytes();
        let result = self.build_with_artifacts(&module_artifacts);
        let build_elapsed = build_started.elapsed();
        let memory_after = memory_usage_bytes();

        match result {
            Ok(index) => {
                info!(
                    target: LOG_TARGET,
                    action = "cold-build",
                    cache_key = cache_key.as_str(),
                    duration_ms = build_elapsed.as_millis() as u64,
                    memory_before_bytes = memory_before.unwrap_or(0),
                    memory_after_bytes = memory_after.unwrap_or(0),
                    artifact_inputs = fingerprint.artifact_count() as u64,
                    "Indexed symbols from scratch"
                );

                let store_stats = CacheStoreStats {
                    build_duration: build_elapsed,
                    artifact_count: fingerprint.artifact_count(),
                    memory_bytes: memory_after,
                };

                if let Err(error) = cache.store(&fingerprint, &index, store_stats) {
                    warn!(
                        target: LOG_TARGET,
                        action = "cache-store-error",
                        cache_key = cache_key.as_str(),
                        %error,
                        "Failed to persist SymbolIndex cache"
                    );
                } else {
                    debug!(
                        target: LOG_TARGET,
                        action = "cache-store",
                        cache_key = cache_key.as_str(),
                        "Stored SymbolIndex cache entry"
                    );
                }

                Ok(index)
            }
            Err(error) => Err(error),
        }
    }

    fn build_with_artifacts(
        &self,
        module_artifacts: &[PathBuf],
    ) -> Result<SymbolIndex, IndexError> {
        let jdk_release = self.context.target.as_str().parse::<u16>().ok();
        let mut index = SymbolIndex::new(jdk_release);

        // Scan module path first (JDK modules, explicit module path entries).
        for path in module_artifacts {
            self.scan_artifact(path, ArtifactKind::Module, &mut index)?;
        }

        // Then scan classpath entries (project dependencies, compiled output directories, etc.).
        for path in &self.context.classpath {
            self.scan_artifact(path, ArtifactKind::Classpath, &mut index)?;
        }

        Ok(index)
    }

    fn resolve_module_artifacts(&self) -> Result<Vec<PathBuf>, IndexError> {
        let mut artifacts = Vec::new();

        if !self.context.module_path.is_empty() {
            for entry in &self.context.module_path {
                collect_artifact(entry, &mut artifacts)?;
            }
        } else if let Some(java_home) = &self.context.java_home {
            let jmods_dir = java_home.join("jmods");
            if jmods_dir.exists() {
                collect_artifact(&jmods_dir, &mut artifacts)?;
            }
        }

        Ok(artifacts)
    }

    fn scan_artifact(
        &self,
        path: &Path,
        kind: ArtifactKind,
        index: &mut SymbolIndex,
    ) -> Result<(), IndexError> {
        if !path.exists() {
            return Ok(());
        }

        if path.is_dir() {
            self.scan_directory(path, kind, index)?;
            return Ok(());
        }

        if is_archive(path) {
            self.scan_archive(path, kind, index)?;
            return Ok(());
        }

        if path.extension().and_then(OsStr::to_str) == Some("class") {
            self.scan_class_file(path, None, index)?;
        }

        Ok(())
    }

    fn scan_directory(
        &self,
        root: &Path,
        kind: ArtifactKind,
        index: &mut SymbolIndex,
    ) -> Result<(), IndexError> {
        let mut module_info: Option<ModuleInfo> = if matches!(kind, ArtifactKind::Module) {
            let module_path = root.join("module-info.class");
            if module_path.exists() {
                Some(read_module_info(&module_path)?)
            } else {
                None
            }
        } else {
            None
        };

        if let Some(info) = &module_info {
            register_module(index, info);
        }

        let mut dirs = vec![root.to_path_buf()];
        while let Some(dir) = dirs.pop() {
            let entries = fs::read_dir(&dir).map_err(|source| IndexError::Io {
                path: dir.clone(),
                source,
            })?;
            for entry in entries {
                let entry = entry.map_err(|source| IndexError::Io {
                    path: dir.clone(),
                    source,
                })?;
                let path = entry.path();
                let metadata = entry.metadata().map_err(|source| IndexError::Io {
                    path: path.clone(),
                    source,
                })?;
                if metadata.is_dir() {
                    dirs.push(path);
                    continue;
                }

                if is_archive(&path) {
                    self.scan_archive(&path, kind, index)?;
                    continue;
                }

                if path
                    .extension()
                    .and_then(OsStr::to_str)
                    .map(|ext| ext.eq_ignore_ascii_case("class"))
                    .unwrap_or(false)
                {
                    // On module paths where module-info.class may live under subdirectories
                    // (e.g., classes/module-info.class in exploded directories), detect it on-demand.
                    if path
                        .file_name()
                        .and_then(OsStr::to_str)
                        .map(|name| name == "module-info.class")
                        .unwrap_or(false)
                    {
                        let info = read_module_info(&path)?;
                        register_module(index, &info);
                        module_info = Some(info);
                        continue;
                    }

                    self.scan_class_file(&path, module_info.as_ref(), index)?;
                }
            }
        }

        Ok(())
    }

    fn scan_archive(
        &self,
        path: &Path,
        kind: ArtifactKind,
        index: &mut SymbolIndex,
    ) -> Result<(), IndexError> {
        let module_info = match kind {
            ArtifactKind::Module => extract_module_info(path)?,
            ArtifactKind::Classpath => extract_module_info(path)?,
        };

        if let Some(info) = &module_info {
            register_module(index, info);
        }

        let file = File::open(path).map_err(|source| IndexError::Io {
            path: path.to_path_buf(),
            source,
        })?;
        let mut archive =
            ZipArchive::new(BufReader::new(file)).map_err(|source| IndexError::Zip {
                path: path.to_path_buf(),
                source,
            })?;

        let mut buffer = Vec::new();

        for idx in 0..archive.len() {
            let mut entry = archive.by_index(idx).map_err(|source| IndexError::Zip {
                path: path.to_path_buf(),
                source,
            })?;

            if !entry.is_file() {
                continue;
            }

            let name = entry.name().to_string();
            if should_skip_entry(&name) {
                continue;
            }

            if name.ends_with("/module-info.class") || name == "module-info.class" {
                // Already processed via extract_module_info
                continue;
            }

            if !name.ends_with(".class") {
                continue;
            }

            let entry_size = entry.size();
            if entry_size > MAX_CLASS_BYTES {
                warn!(
                    target: LOG_TARGET,
                    %name,
                    size = entry_size,
                    limit = MAX_CLASS_BYTES,
                    "クラスエントリのサイズが上限を超えたためスキップしました"
                );
                continue;
            }

            buffer.clear();
            if let Ok(expected) = usize::try_from(entry_size) {
                if buffer.capacity() < expected {
                    buffer.reserve(expected - buffer.capacity());
                }
            }
            entry
                .read_to_end(&mut buffer)
                .map_err(|source| IndexError::Io {
                    path: path.to_path_buf(),
                    source,
                })?;

            let module = module_info.as_ref();
            self.index_class_bytes(&buffer, module, index, archive_entry_path(path, &name))?;
        }

        Ok(())
    }

    fn scan_class_file(
        &self,
        path: &Path,
        module_info: Option<&ModuleInfo>,
        index: &mut SymbolIndex,
    ) -> Result<(), IndexError> {
        let mut file = File::open(path).map_err(|source| IndexError::Io {
            path: path.to_path_buf(),
            source,
        })?;
        let metadata = file.metadata().map_err(|source| IndexError::Io {
            path: path.to_path_buf(),
            source,
        })?;
        if metadata.len() > MAX_CLASS_BYTES {
            warn!(
                target: LOG_TARGET,
                path = %path.display(),
                size = metadata.len(),
                limit = MAX_CLASS_BYTES,
                "単体クラスファイルのサイズが上限を超えたためスキップしました"
            );
            return Ok(());
        }
        let mut buffer = Vec::new();
        let expected = usize::try_from(metadata.len()).unwrap_or(0);
        if expected > 0 {
            buffer.reserve(expected);
        }
        file.read_to_end(&mut buffer)
            .map_err(|source| IndexError::Io {
                path: path.to_path_buf(),
                source,
            })?;
        self.index_class_bytes(&buffer, module_info, index, path.to_path_buf())
    }

    fn index_class_bytes(
        &self,
        bytes: &[u8],
        module_info: Option<&ModuleInfo>,
        index: &mut SymbolIndex,
        virtual_path: PathBuf,
    ) -> Result<(), IndexError> {
        let parsed = parse_class(bytes).map_err(|source| IndexError::ClassFile {
            path: virtual_path,
            source,
        })?;

        let mut entry = TypeEntry::new(
            parsed.fqcn.clone(),
            parsed.package.clone(),
            module_info.map(|info| info.name.clone()),
        );

        for (name, ty) in parsed.static_fields {
            entry.add_static_field(name, ty);
        }

        for (name, signature) in parsed.static_methods {
            entry.add_static_method(name, signature);
        }

        for name in parsed.instance_fields {
            entry.add_instance_field(name);
        }

        for (name, signature) in parsed.instance_methods {
            entry.add_instance_method(name, signature);
        }

        index.insert_type(entry);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArtifactKind {
    Module,
    Classpath,
}

fn detect_java_home() -> Option<PathBuf> {
    jdk::discover_jdk().map(|info| info.java_home).ok()
}

fn expand_classpath(raw: &[String]) -> Vec<PathBuf> {
    let mut result = Vec::new();
    let separator = if cfg!(windows) { ';' } else { ':' };
    for entry in raw {
        if entry.trim().is_empty() {
            continue;
        }
        if entry.contains(separator) {
            for part in entry.split(separator) {
                if part.trim().is_empty() {
                    continue;
                }
                result.push(PathBuf::from(part));
            }
        } else {
            result.push(PathBuf::from(entry));
        }
    }
    result
}

fn collect_artifact(path: &Path, out: &mut Vec<PathBuf>) -> Result<(), IndexError> {
    if !path.exists() {
        return Ok(());
    }

    if path.is_file() {
        out.push(path.to_path_buf());
        return Ok(());
    }

    let entries = fs::read_dir(path).map_err(|source| IndexError::Io {
        path: path.to_path_buf(),
        source,
    })?;

    for entry in entries {
        let entry = entry.map_err(|source| IndexError::Io {
            path: path.to_path_buf(),
            source,
        })?;
        let entry_path = entry.path();
        if entry_path.is_file() {
            out.push(entry_path);
        }
    }

    Ok(())
}

fn is_archive(path: &Path) -> bool {
    path.extension()
        .and_then(OsStr::to_str)
        .map(|ext| matches_ignore_case(ext, &["jar", "zip", "jmod"]))
        .unwrap_or(false)
}

fn matches_ignore_case(candidate: &str, values: &[&str]) -> bool {
    values
        .iter()
        .any(|value| candidate.eq_ignore_ascii_case(value))
}

fn should_skip_entry(name: &str) -> bool {
    if name.starts_with("META-INF/") {
        return true;
    }
    if let Some(stripped) = name.strip_prefix("classes/") {
        return should_skip_entry(stripped);
    }
    false
}

fn extract_module_info(path: &Path) -> Result<Option<ModuleInfo>, IndexError> {
    let file = File::open(path).map_err(|source| IndexError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    let mut archive = ZipArchive::new(BufReader::new(file)).map_err(|source| IndexError::Zip {
        path: path.to_path_buf(),
        source,
    })?;

    for idx in 0..archive.len() {
        let mut entry = archive.by_index(idx).map_err(|source| IndexError::Zip {
            path: path.to_path_buf(),
            source,
        })?;
        if !entry.is_file() {
            continue;
        }
        let name = entry.name().to_string();
        if name == "module-info.class" || name.ends_with("/module-info.class") {
            let mut buffer = Vec::new();
            entry
                .read_to_end(&mut buffer)
                .map_err(|source| IndexError::Io {
                    path: path.to_path_buf(),
                    source,
                })?;
            let info = parse_module_info(&buffer).map_err(|source| IndexError::ClassFile {
                path: path.to_path_buf(),
                source,
            })?;
            return Ok(Some(info));
        }
    }

    Ok(None)
}

fn read_module_info(path: &Path) -> Result<ModuleInfo, IndexError> {
    let mut file = File::open(path).map_err(|source| IndexError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .map_err(|source| IndexError::Io {
            path: path.to_path_buf(),
            source,
        })?;
    parse_module_info(&buffer).map_err(|source| IndexError::ClassFile {
        path: path.to_path_buf(),
        source,
    })
}

fn register_module(index: &mut SymbolIndex, info: &ModuleInfo) {
    let mut entry = ModuleEntry::new(info.name.clone());
    entry.exports.extend(info.exports.iter().cloned());
    index.insert_module(entry);
}

fn archive_entry_path(archive: &Path, entry: &str) -> PathBuf {
    let mut display = archive.display().to_string();
    display.push('!');
    display.push('/');
    display.push_str(entry);
    PathBuf::from(display)
}

fn memory_usage_bytes() -> Option<u64> {
    use sysinfo::{ProcessExt, System, SystemExt, get_current_pid};

    let pid = get_current_pid().ok()?;
    let mut system = System::new();
    system.refresh_process(pid);
    system.process(pid).map(|process| process.memory() * 1024)
}
