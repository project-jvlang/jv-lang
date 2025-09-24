use std::ffi::OsStr;
use std::fs;
use std::path::{Component, Path, PathBuf};

use jv_checker::diagnostics::ToolingDiagnostic;

use super::locator::ProjectRoot;
use super::manifest::ProjectSettings;

const JV1002_CODE: &str = "JV1002";
const JV1002_TITLE: &str = "プロジェクトソースを解決できません";
const JV1002_HELP: &str =
    "jv.toml の include/exclude 設定と .jv ファイルの配置を確認してください。";

#[derive(Debug, Clone)]
pub struct ProjectLayout {
    root: ProjectRoot,
    sources: Vec<PathBuf>,
    entrypoint: PathBuf,
}

impl ProjectLayout {
    pub fn from_settings(
        root: &ProjectRoot,
        settings: &ProjectSettings,
    ) -> Result<Self, ToolingDiagnostic> {
        let include_patterns = compile_patterns(&settings.sources.include);
        let exclude_patterns = compile_patterns(&settings.sources.exclude);

        let mut candidates = enumerate_sources(root.root_dir())?;
        let mut matched: Vec<(String, PathBuf)> = Vec::new();
        for candidate in candidates.drain(..) {
            let relative = candidate
                .strip_prefix(root.root_dir())
                .map_err(|_| io_diagnostic(root.root_dir(), &candidate, None))?;
            let relative_string = normalise_relative_path(relative);
            let segments = path_segments(relative);

            if !include_patterns.iter().any(|pattern| pattern.matches(&segments)) {
                continue;
            }

            if exclude_patterns
                .iter()
                .any(|pattern| pattern.matches(&segments))
            {
                continue;
            }

            let absolute = root.root_dir().join(relative);
            matched.push((relative_string, absolute));
        }

        matched.sort_by(|a, b| a.0.cmp(&b.0));
        matched.dedup_by(|a, b| a.0 == b.0);

        if matched.is_empty() {
            return Err(no_sources_diagnostic(
                root.root_dir(),
                &settings.sources.include,
                &settings.sources.exclude,
            ));
        }

        let sources: Vec<PathBuf> = matched.into_iter().map(|(_, path)| path).collect();

        let entrypoint = resolve_entrypoint(root, settings, &sources)?;

        Ok(Self {
            root: root.clone(),
            sources,
            entrypoint,
        })
    }

    pub fn root(&self) -> &ProjectRoot {
        &self.root
    }

    pub fn sources(&self) -> &[PathBuf] {
        &self.sources
    }

    pub fn entrypoint(&self) -> &Path {
        &self.entrypoint
    }
}

fn resolve_entrypoint(
    root: &ProjectRoot,
    settings: &ProjectSettings,
    sources: &[PathBuf],
) -> Result<PathBuf, ToolingDiagnostic> {
    if let Some(explicit) = &settings.entrypoint {
        let absolute = root.join(explicit);
        if sources.iter().any(|candidate| same_file(candidate, &absolute)) {
            return Ok(absolute);
        }
        return Err(entrypoint_not_found_diagnostic(explicit));
    }

    if let Some(candidate) = sources.iter().find(|path| {
        matches_relative(path, root.root_dir(), "src/main.jv")
    }) {
        return Ok(candidate.clone());
    }

    if let Some(candidate) = sources
        .iter()
        .find(|path| path.file_name() == Some(OsStr::new("main.jv")))
    {
        return Ok(candidate.clone());
    }

    Err(entrypoint_inference_failed())
}

fn enumerate_sources(root: &Path) -> Result<Vec<PathBuf>, ToolingDiagnostic> {
    let mut results = Vec::new();
    visit_directory(root, root, &mut results)?;
    Ok(results)
}

fn visit_directory(
    root: &Path,
    directory: &Path,
    results: &mut Vec<PathBuf>,
) -> Result<(), ToolingDiagnostic> {
    let entries = fs::read_dir(directory)
        .map_err(|error| io_diagnostic(root, directory, Some(error)))?;

    for entry in entries {
        let entry = entry
            .map_err(|error| io_diagnostic(root, directory, Some(error)))?;
        let path = entry.path();
        let file_type = entry
            .file_type()
            .map_err(|error| io_diagnostic(root, &path, Some(error)))?;

        if file_type.is_dir() {
            visit_directory(root, &path, results)?;
        } else if file_type.is_file() {
            if path.extension() == Some(OsStr::new("jv")) {
                results.push(path);
            }
        }
    }

    Ok(())
}

fn compile_patterns(patterns: &[String]) -> Vec<GlobPattern> {
    patterns.iter().map(|value| GlobPattern::new(value)).collect()
}

fn path_segments(path: &Path) -> Vec<String> {
    path.components()
        .filter_map(|component| match component {
            Component::CurDir => None,
            Component::Normal(segment) => Some(segment.to_string_lossy().to_string()),
            _ => None,
        })
        .collect()
}

fn normalise_relative_path(path: &Path) -> String {
    let segments = path_segments(path);
    segments.join("/")
}

fn same_file(lhs: &Path, rhs: &Path) -> bool {
    match (fs::canonicalize(lhs), fs::canonicalize(rhs)) {
        (Ok(left), Ok(right)) => left == right,
        _ => lhs == rhs,
    }
}

fn matches_relative(path: &Path, root: &Path, relative: &str) -> bool {
    if let Ok(stripped) = path.strip_prefix(root) {
        return normalise_relative_path(stripped) == relative.replace('\\', "/");
    }
    false
}

fn no_sources_diagnostic(
    root: &Path,
    include: &[String],
    exclude: &[String],
) -> ToolingDiagnostic {
    ToolingDiagnostic {
        code: JV1002_CODE,
        title: JV1002_TITLE,
        message: format!(
            "{} 配下で include={:?}, exclude={:?} を適用しましたが .jv ソースを検出できませんでした。",
            root.display(), include, exclude
        ),
        help: JV1002_HELP,
        span: None,
    }
}

fn entrypoint_not_found_diagnostic(entrypoint: &Path) -> ToolingDiagnostic {
    ToolingDiagnostic {
        code: JV1002_CODE,
        title: JV1002_TITLE,
        message: format!(
            "指定されたエントリポイント {} がプロジェクト内で検出できません。",
            entrypoint.display()
        ),
        help: JV1002_HELP,
        span: None,
    }
}

fn entrypoint_inference_failed() -> ToolingDiagnostic {
    ToolingDiagnostic {
        code: JV1002_CODE,
        title: JV1002_TITLE,
        message: "エントリポイントを推論できませんでした。project.entrypoint を設定してください。"
            .to_string(),
        help: JV1002_HELP,
        span: None,
    }
}

fn io_diagnostic(root: &Path, path: &Path, error: Option<std::io::Error>) -> ToolingDiagnostic {
    let detail = error
        .map(|err| err.to_string())
        .unwrap_or_else(|| "不明なエラー".to_string());

    ToolingDiagnostic {
        code: JV1002_CODE,
        title: JV1002_TITLE,
        message: format!(
            "{} 配下の {} を列挙中にエラーが発生しました: {}",
            root.display(),
            path.display(),
            detail
        ),
        help: JV1002_HELP,
        span: None,
    }
}

#[derive(Debug, Clone)]
struct GlobPattern {
    segments: Vec<Segment>,
}

impl GlobPattern {
    fn new(value: &str) -> Self {
        let normalised = value.replace('\\', "/");
        let segments = normalised
            .split('/')
            .filter(|segment| !segment.is_empty())
            .map(|segment| {
                if segment == "**" {
                    Segment::Recursive
                } else {
                    Segment::Component(segment.to_string())
                }
            })
            .collect();
        Self { segments }
    }

    fn matches(&self, input: &[String]) -> bool {
        matches_segments(&self.segments, input)
    }
}

#[derive(Debug, Clone)]
enum Segment {
    Recursive,
    Component(String),
}

fn matches_segments(pattern: &[Segment], input: &[String]) -> bool {
    if pattern.is_empty() {
        return input.is_empty();
    }

    match &pattern[0] {
        Segment::Recursive => {
            if matches_segments(&pattern[1..], input) {
                return true;
            }
            if let Some((_, rest)) = input.split_first() {
                return matches_segments(pattern, rest);
            }
            false
        }
        Segment::Component(component) => match input.split_first() {
            Some((head, tail)) if matches_component(component, head) => {
                matches_segments(&pattern[1..], tail)
            }
            _ => false,
        },
    }
}

fn matches_component(pattern: &str, component: &str) -> bool {
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let text_chars: Vec<char> = component.chars().collect();

    let mut p_idx = 0;
    let mut t_idx = 0;
    let mut star_idx: Option<usize> = None;
    let mut match_idx = 0usize;

    while t_idx < text_chars.len() {
        if p_idx < pattern_chars.len()
            && (pattern_chars[p_idx] == '?' || pattern_chars[p_idx] == text_chars[t_idx])
        {
            p_idx += 1;
            t_idx += 1;
        } else if p_idx < pattern_chars.len() && pattern_chars[p_idx] == '*' {
            star_idx = Some(p_idx);
            p_idx += 1;
            match_idx = t_idx;
        } else if let Some(star) = star_idx {
            p_idx = star + 1;
            match_idx += 1;
            t_idx = match_idx;
        } else {
            return false;
        }
    }

    while p_idx < pattern_chars.len() && pattern_chars[p_idx] == '*' {
        p_idx += 1;
    }

    p_idx == pattern_chars.len()
}

