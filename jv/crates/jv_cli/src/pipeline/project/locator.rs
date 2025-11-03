use std::fs;
use std::path::{Component, Path, PathBuf};

use jv_checker::diagnostics::{DiagnosticSeverity, DiagnosticStrategy, EnhancedDiagnostic};

const MANIFEST_FILE: &str = "jv.toml";
const JV1001_CODE: &str = "JV1001";
const JV1001_TITLE: &str = "プロジェクトルートを検出できません";
const JV1001_HELP: &str =
    "jv.toml をプロジェクトルートに配置するか `jv init` で初期化してから再実行してください。";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectRoot {
    root_dir: PathBuf,
    manifest_path: PathBuf,
}

impl ProjectRoot {
    pub fn new(root_dir: PathBuf, manifest_path: PathBuf) -> Self {
        Self {
            root_dir,
            manifest_path,
        }
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub fn manifest_path(&self) -> &Path {
        &self.manifest_path
    }

    pub fn join(&self, relative: impl AsRef<Path>) -> PathBuf {
        self.root_dir.join(relative)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ProjectLocator {
    manifest_name: &'static str,
}

impl ProjectLocator {
    pub fn new() -> Self {
        Self {
            manifest_name: MANIFEST_FILE,
        }
    }

    pub fn with_manifest(mut self, manifest_name: &'static str) -> Self {
        self.manifest_name = manifest_name;
        self
    }

    pub fn locate(&self, start: impl AsRef<Path>) -> Result<ProjectRoot, EnhancedDiagnostic> {
        let start_path = start.as_ref();
        let search_origin = self.normalize_start(start_path)?;
        let mut current = search_origin.clone();

        let mut visited = Vec::new();
        visited.push(current.clone());

        loop {
            let manifest_path = current.join(self.manifest_name);
            if manifest_path.is_file() {
                let manifest_path = match fs::canonicalize(&manifest_path) {
                    Ok(canonical) => canonical,
                    Err(_) => manifest_path,
                };

                let root_dir = match fs::canonicalize(&current) {
                    Ok(canonical) => canonical,
                    Err(_) => current.clone(),
                };

                if !self.is_ancestor(&root_dir, &search_origin) {
                    return Err(self.outside_workspace_diagnostic(&search_origin));
                }

                return Ok(ProjectRoot::new(root_dir, manifest_path));
            }

            if !current.pop() {
                break;
            }

            if visited.iter().any(|seen| seen == &current) {
                break;
            }
            visited.push(current.clone());
        }

        Err(self.not_found_diagnostic(&search_origin))
    }

    fn normalize_start(&self, start: &Path) -> Result<PathBuf, EnhancedDiagnostic> {
        let cwd = std::env::current_dir().map_err(|_| self.not_found_diagnostic(start))?;
        let cwd_canonical = fs::canonicalize(&cwd).map_err(|_| self.not_found_diagnostic(&cwd))?;

        let enforce_workspace = !start.is_absolute();
        let absolute = if start.is_absolute() {
            start.to_path_buf()
        } else {
            cwd.join(start)
        };

        let metadata = fs::metadata(&absolute).map_err(|_| self.not_found_diagnostic(start))?;
        let base_dir = if metadata.is_dir() {
            absolute
        } else {
            absolute
                .parent()
                .map(Path::to_path_buf)
                .ok_or_else(|| self.not_found_diagnostic(start))?
        };

        let canonical = fs::canonicalize(&base_dir).map_err(|_| self.not_found_diagnostic(start))?;
        if enforce_workspace && !canonical.starts_with(&cwd_canonical) {
            return Err(self.outside_workspace_diagnostic(start));
        }
        Ok(strip_trailing_dot_components(&canonical))
    }

    fn not_found_diagnostic(&self, start: &Path) -> EnhancedDiagnostic {
        EnhancedDiagnostic {
            code: JV1001_CODE,
            title: JV1001_TITLE,
            message: format!(
                "{} から {} を探索しましたが検出できませんでした。",
                start.display(),
                self.manifest_name
            ),
            help: JV1001_HELP,
            severity: DiagnosticSeverity::Error,
            strategy: DiagnosticStrategy::Immediate,
            span: None,
            related_locations: Vec::new(),
            suggestions: Vec::new(),
            learning_hints: None,
            categories: Vec::new(),
        }
    }

    fn outside_workspace_diagnostic(&self, start: &Path) -> EnhancedDiagnostic {
        EnhancedDiagnostic {
            code: JV1001_CODE,
            title: JV1001_TITLE,
            message: format!(
                "探索開始パス {} がワークスペース外を参照しているため、安全に {} を検出できません。",
                start.display(),
                self.manifest_name
            ),
            help: JV1001_HELP,
            severity: DiagnosticSeverity::Error,
            strategy: DiagnosticStrategy::Immediate,
            span: None,
            related_locations: Vec::new(),
            suggestions: Vec::new(),
            learning_hints: None,
            categories: Vec::new(),
        }
    }

    fn is_ancestor(&self, root: &Path, candidate: &Path) -> bool {
        let candidate = match fs::canonicalize(candidate) {
            Ok(path) => path,
            Err(_) => candidate.to_path_buf(),
        };

        let root = match fs::canonicalize(root) {
            Ok(path) => path,
            Err(_) => root.to_path_buf(),
        };

        candidate.starts_with(root)
    }
}

fn strip_trailing_dot_components(path: &Path) -> PathBuf {
    let mut components = path.components().peekable();
    let mut result = PathBuf::new();

    while let Some(component) = components.next() {
        match component {
            Component::CurDir => {
                // Skip redundant ./ segments
                continue;
            }
            _ => result.push(component.as_os_str()),
        }
    }

    result
}
