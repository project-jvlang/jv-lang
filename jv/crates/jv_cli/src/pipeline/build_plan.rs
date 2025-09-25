use std::fs;
use std::path::{Path, PathBuf};

use jv_build::{BuildConfig, JavaTarget};
use jv_checker::diagnostics::ToolingDiagnostic;

use super::project::{
    layout::ProjectLayout,
    locator::ProjectRoot,
    manifest::{OutputConfig, ProjectSettings},
};

const JV1001_CODE: &str = "JV1001";
const JV1001_TITLE: &str = "jv.toml の設定を読み込めません";
const JV1001_HELP: &str =
    "manifest か CLI フラグで指定したパスを確認し、プロジェクトルート配下の安全な場所を指定してください。";

const JV1002_CODE: &str = "JV1002";
const JV1002_TITLE: &str = "プロジェクトソースを解決できません";
const JV1002_HELP: &str =
    "manifest の include/exclude 設定と CLI で指定したエントリポイントを確認してください。";

#[derive(Debug, Clone, Default)]
pub struct CliOverrides {
    pub entrypoint: Option<PathBuf>,
    pub output: Option<PathBuf>,
    pub java_only: bool,
    pub check: bool,
    pub format: bool,
    pub target: Option<JavaTarget>,
}

#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub entrypoint: PathBuf,
    pub output_dir: PathBuf,
    pub java_only: bool,
    pub check: bool,
    pub format: bool,
}

#[derive(Debug, Clone)]
pub struct BuildPlan {
    pub root: ProjectRoot,
    pub settings: ProjectSettings,
    pub layout: ProjectLayout,
    pub options: BuildOptions,
    pub build_config: BuildConfig,
}

impl BuildPlan {
    pub fn entrypoint(&self) -> &Path {
        self.options.entrypoint.as_path()
    }

    pub fn output_dir(&self) -> &Path {
        self.options.output_dir.as_path()
    }

    pub fn with_output_dir(&self, output_dir: PathBuf) -> Self {
        let mut clone = self.clone();
        clone.options.output_dir = output_dir.clone();
        clone.build_config.output_dir = stringify_path(&output_dir);
        clone
    }
}

pub struct BuildOptionsFactory;

impl BuildOptionsFactory {
    pub fn compose(
        root: ProjectRoot,
        settings: ProjectSettings,
        layout: ProjectLayout,
        overrides: CliOverrides,
    ) -> Result<BuildPlan, ToolingDiagnostic> {
        let entrypoint = resolve_entrypoint(&root, &layout, overrides.entrypoint)?;
        let output_dir = resolve_output_dir(&root, &settings.output, overrides.output)?;
        let target = overrides
            .target
            .unwrap_or_else(|| settings.manifest.java_target());

        let mut build_config = BuildConfig::with_target(target);
        build_config.output_dir = stringify_path(&output_dir);

        let options = BuildOptions {
            entrypoint,
            output_dir,
            java_only: overrides.java_only,
            check: overrides.check,
            format: overrides.format,
        };

        Ok(BuildPlan {
            root,
            settings,
            layout,
            options,
            build_config,
        })
    }
}

fn resolve_entrypoint(
    root: &ProjectRoot,
    layout: &ProjectLayout,
    override_path: Option<PathBuf>,
) -> Result<PathBuf, ToolingDiagnostic> {
    match override_path {
        Some(path) => {
            let absolute = absolutize(&path)?;
            guard_within_root(root.root_dir(), &absolute)?;
            guard_exists(&absolute)?;
            guard_extension(&absolute, "jv")?;
            guard_listed_in_layout(layout, &absolute)?;
            Ok(absolute)
        }
        None => Ok(layout.entrypoint().to_path_buf()),
    }
}

fn resolve_output_dir(
    root: &ProjectRoot,
    output: &OutputConfig,
    override_path: Option<PathBuf>,
) -> Result<PathBuf, ToolingDiagnostic> {
    let candidate = match override_path {
        Some(path) => absolutize(&path)?,
        None => root.join(&output.directory),
    };

    guard_within_root(root.root_dir(), &candidate)?;
    Ok(candidate)
}

fn guard_within_root(root: &Path, candidate: &Path) -> Result<(), ToolingDiagnostic> {
    if !path_within(root, candidate) {
        return Err(ToolingDiagnostic {
            code: JV1001_CODE,
            title: JV1001_TITLE,
            message: format!(
                "パス '{}' はプロジェクトルート '{}' の外部を指しています。",
                candidate.display(),
                root.display()
            ),
            help: JV1001_HELP,
            span: None,
        });
    }
    Ok(())
}

fn guard_exists(path: &Path) -> Result<(), ToolingDiagnostic> {
    if path.is_file() {
        return Ok(());
    }

    Err(ToolingDiagnostic {
        code: JV1002_CODE,
        title: JV1002_TITLE,
        message: format!("エントリポイント '{}' が見つかりません。", path.display()),
        help: JV1002_HELP,
        span: None,
    })
}

fn guard_extension(path: &Path, expected: &str) -> Result<(), ToolingDiagnostic> {
    if path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.eq_ignore_ascii_case(expected))
        .unwrap_or(false)
    {
        return Ok(());
    }

    Err(ToolingDiagnostic {
        code: JV1002_CODE,
        title: JV1002_TITLE,
        message: format!(
            "エントリポイント '{}' は .{} ファイルではありません。",
            path.display(), expected
        ),
        help: JV1002_HELP,
        span: None,
    })
}

fn guard_listed_in_layout(
    layout: &ProjectLayout,
    entrypoint: &Path,
) -> Result<(), ToolingDiagnostic> {
    if layout
        .sources()
        .iter()
        .any(|source| same_file(source, entrypoint))
        || same_file(layout.entrypoint(), entrypoint)
    {
        return Ok(());
    }

    Err(ToolingDiagnostic {
        code: JV1002_CODE,
        title: JV1002_TITLE,
        message: format!(
            "エントリポイント '{}' は manifest の include/exclude 設定に含まれていません。",
            entrypoint.display()
        ),
        help: JV1002_HELP,
        span: None,
    })
}

fn absolutize(path: &Path) -> Result<PathBuf, ToolingDiagnostic> {
    if path.is_absolute() {
        Ok(path.to_path_buf())
    } else {
        let cwd = std::env::current_dir().map_err(|error| ToolingDiagnostic {
            code: JV1001_CODE,
            title: JV1001_TITLE,
            message: format!(
                "カレントディレクトリを取得できません: {}",
                error.to_string()
            ),
            help: JV1001_HELP,
            span: None,
        })?;
        Ok(cwd.join(path))
    }
}

fn same_file(lhs: &Path, rhs: &Path) -> bool {
    match (fs::canonicalize(lhs), fs::canonicalize(rhs)) {
        (Ok(left), Ok(right)) => left == right,
        _ => lhs == rhs,
    }
}

fn path_within(root: &Path, candidate: &Path) -> bool {
    let Ok(root_canonical) = fs::canonicalize(root) else {
        return false;
    };

    match fs::canonicalize(candidate) {
        Ok(candidate) => candidate.starts_with(&root_canonical),
        Err(_) => candidate.starts_with(root),
    }
}

fn stringify_path(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}
