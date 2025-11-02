use std::fs;
use std::path::{Path, PathBuf};

use jv_build::{BuildConfig, JavaTarget};
use jv_checker::{
    diagnostics::{DiagnosticSeverity, DiagnosticStrategy, EnhancedDiagnostic},
    ParallelInferenceConfig,
};
use jv_pm::{LoggingConfig, LoggingConfigLayer};

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
    pub clean: bool,
    pub perf: bool,
    pub emit_types: bool,
    pub verbose: bool,
    pub emit_telemetry: bool,
    pub parallel_inference: bool,
    pub inference_workers: Option<usize>,
    pub constraint_batch: Option<usize>,
    // APT options
    pub apt_enabled: bool,
    pub apt_processors: Option<String>,
    pub apt_processorpath: Option<String>,
    pub apt_options: Vec<String>,
    // ロギング関連の上書き層
    pub logging_cli: LoggingConfigLayer,
    pub logging_env: LoggingConfigLayer,
}

#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub entrypoint: PathBuf,
    pub output_dir: PathBuf,
    pub java_only: bool,
    pub check: bool,
    pub format: bool,
    pub clean: bool,
    pub perf: bool,
    pub emit_types: bool,
    pub verbose: bool,
    pub parallel_config: ParallelInferenceConfig,
    pub emit_telemetry: bool,
}

#[derive(Debug, Clone)]
pub struct BuildPlan {
    pub root: ProjectRoot,
    pub settings: ProjectSettings,
    pub layout: ProjectLayout,
    pub options: BuildOptions,
    pub logging_config: LoggingConfig,
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
    ) -> Result<BuildPlan, EnhancedDiagnostic> {
        let CliOverrides {
            entrypoint: entry_override,
            output: output_override,
            java_only,
            check: check_flag,
            format: format_flag,
            target,
            clean: clean_flag,
            perf,
            emit_types: emit_types_flag,
            verbose,
            emit_telemetry: emit_telemetry_flag,
            parallel_inference,
            inference_workers,
            constraint_batch,
            apt_enabled,
            apt_processors,
            apt_processorpath,
            apt_options,
            logging_cli,
            logging_env,
        } = overrides;

        let entrypoint = resolve_entrypoint(&root, &layout, entry_override)?;
        let output_dir = resolve_output_dir(&root, &settings.output, output_override)?;
        let target = target.unwrap_or_else(|| settings.manifest.java_target());

        let mut build_config = BuildConfig::with_target(target);
        build_config.output_dir = stringify_path(&output_dir);

        let emit_types = emit_types_flag;
        let emit_telemetry = emit_telemetry_flag;
        let mut parallel_config = ParallelInferenceConfig::default();
        if parallel_inference {
            parallel_config.module_parallelism = true;
        }
        if let Some(workers) = inference_workers {
            parallel_config.worker_threads = workers;
        }
        if let Some(batch) = constraint_batch {
            parallel_config.constraint_batching = batch;
        }
        parallel_config = parallel_config.sanitized();
        let options = BuildOptions {
            entrypoint,
            output_dir,
            java_only,
            check: check_flag || emit_types,
            format: format_flag,
            clean: clean_flag || settings.output.clean,
            perf,
            emit_types,
            verbose,
            parallel_config,
            emit_telemetry,
        };

        let mut logging_layers = Vec::new();
        if !logging_env.is_empty() {
            logging_layers.push(logging_env.clone());
        }
        if !logging_cli.is_empty() {
            logging_layers.push(logging_cli.clone());
        }
        let manifest_logging = settings.manifest.logging.clone();
        let logging_config =
            LoggingConfig::from_manifest(Some(manifest_logging)).with_layers(&logging_layers);

        Ok(BuildPlan {
            root,
            settings,
            layout,
            options,
            logging_config,
            build_config: {
                // Apply APT overrides
                let mut cfg = build_config;
                if apt_enabled
                    || apt_processors.is_some()
                    || apt_processorpath.is_some()
                    || !apt_options.is_empty()
                {
                    cfg.enable_apt();
                }
                if let Some(list) = apt_processors.as_ref() {
                    let processors = list
                        .split(',')
                        .map(|s| s.trim().to_string())
                        .filter(|s| !s.is_empty())
                        .collect::<Vec<_>>();
                    if !processors.is_empty() {
                        cfg.set_apt_processors(processors);
                    }
                }
                if let Some(path) = apt_processorpath.as_ref() {
                    // Do not try to be smart here; allow user to pass joined path
                    let entries = path
                        .split(if cfg!(windows) { ';' } else { ':' })
                        .map(|s| s.trim().to_string())
                        .filter(|s| !s.is_empty())
                        .collect::<Vec<_>>();
                    if !entries.is_empty() {
                        cfg.set_apt_processorpath(entries);
                    }
                }
                for opt in apt_options {
                    let trimmed = opt.trim();
                    if !trimmed.is_empty() {
                        cfg.add_apt_option(opt);
                    }
                }
                cfg
            },
        })
    }
}

fn resolve_entrypoint(
    root: &ProjectRoot,
    layout: &ProjectLayout,
    override_path: Option<PathBuf>,
) -> Result<PathBuf, EnhancedDiagnostic> {
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
) -> Result<PathBuf, EnhancedDiagnostic> {
    let candidate = match override_path {
        Some(path) => absolutize(&path)?,
        None => root.join(&output.directory),
    };

    guard_within_root(root.root_dir(), &candidate)?;
    Ok(candidate)
}

fn guard_within_root(root: &Path, candidate: &Path) -> Result<(), EnhancedDiagnostic> {
    if !path_within(root, candidate) {
        return Err(root_diagnostic(format!(
            "パス '{}' はプロジェクトルート '{}' の外部を指しています。",
            candidate.display(),
            root.display()
        )));
    }
    Ok(())
}

fn guard_exists(path: &Path) -> Result<(), EnhancedDiagnostic> {
    if path.is_file() {
        return Ok(());
    }

    Err(source_diagnostic(format!(
        "エントリポイント '{}' が見つかりません。",
        path.display()
    )))
}

fn guard_extension(path: &Path, expected: &str) -> Result<(), EnhancedDiagnostic> {
    if path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.eq_ignore_ascii_case(expected))
        .unwrap_or(false)
    {
        return Ok(());
    }

    Err(source_diagnostic(format!(
        "エントリポイント '{}' は .{} ファイルではありません。",
        path.display(),
        expected
    )))
}

fn guard_listed_in_layout(
    layout: &ProjectLayout,
    entrypoint: &Path,
) -> Result<(), EnhancedDiagnostic> {
    if layout
        .sources()
        .iter()
        .any(|source| same_file(source, entrypoint))
        || same_file(layout.entrypoint(), entrypoint)
    {
        return Ok(());
    }

    Err(source_diagnostic(format!(
        "エントリポイント '{}' は manifest の include/exclude 設定に含まれていません。",
        entrypoint.display()
    )))
}

fn absolutize(path: &Path) -> Result<PathBuf, EnhancedDiagnostic> {
    if path.is_absolute() {
        Ok(path.to_path_buf())
    } else {
        let cwd = std::env::current_dir().map_err(|error| {
            root_diagnostic(format!("カレントディレクトリを取得できません: {}", error))
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

fn plan_diagnostic_with(
    code: &'static str,
    title: &'static str,
    message: impl Into<String>,
    help: &'static str,
) -> EnhancedDiagnostic {
    EnhancedDiagnostic {
        code,
        title,
        message: message.into(),
        help,
        severity: DiagnosticSeverity::Error,
        strategy: DiagnosticStrategy::Immediate,
        span: None,
        related_locations: Vec::new(),
        suggestions: Vec::new(),
        learning_hints: None,
    }
}

fn root_diagnostic(message: impl Into<String>) -> EnhancedDiagnostic {
    plan_diagnostic_with(JV1001_CODE, JV1001_TITLE, message, JV1001_HELP)
}

fn source_diagnostic(message: impl Into<String>) -> EnhancedDiagnostic {
    plan_diagnostic_with(JV1002_CODE, JV1002_TITLE, message, JV1002_HELP)
}
