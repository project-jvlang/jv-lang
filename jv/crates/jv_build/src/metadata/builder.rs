use super::classfile::{parse_class, parse_module_info, ClassParseError, ModuleInfo};
use super::index::{ModuleEntry, SymbolIndex, TypeEntry};
use crate::config::BuildConfig;
use jv_pm::JavaTarget;
use std::env;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::{self, BufReader, Read};
use std::path::{Path, PathBuf};
use thiserror::Error;
use zip::result::ZipError;
use zip::ZipArchive;

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
        let jdk_release = self.context.target.as_str().parse::<u16>().ok();
        let mut index = SymbolIndex::new(jdk_release);

        // Scan module path first (JDK modules, explicit module path entries).
        for path in self.resolve_module_artifacts()? {
            self.scan_artifact(&path, ArtifactKind::Module, &mut index)?;
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

            buffer.clear();
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
        let mut buffer = Vec::new();
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
    if let Ok(path) = env::var("JAVA_HOME") {
        let candidate = PathBuf::from(path);
        if candidate.exists() {
            return Some(candidate);
        }
    }

    if let Ok(java_path) = which::which("java") {
        if let Some(bin_dir) = java_path.parent() {
            if let Some(home) = bin_dir.parent() {
                return Some(home.to_path_buf());
            }
        }
    }

    None
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
