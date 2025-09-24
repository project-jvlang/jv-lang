use std::path::{Path, PathBuf};

use jv_checker::diagnostics::ToolingDiagnostic;
use jv_pm::{Manifest, OutputSection, PackageError, SourceSection};

const JV1001_CODE: &str = "JV1001";
const JV1001_TITLE: &str = "jv.toml の設定を読み込めません";
const JV1001_HELP: &str =
    "jv.toml の include/exclude/output/entrypoint セクションを見直してください。";

#[derive(Debug, Clone)]
pub struct ProjectSettings {
    pub manifest: Manifest,
    pub sources: SourceConfig,
    pub output: OutputConfig,
    pub entrypoint: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct SourceConfig {
    pub include: Vec<String>,
    pub exclude: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct OutputConfig {
    pub directory: PathBuf,
    pub clean: bool,
}

pub struct ManifestLoader;

impl ManifestLoader {
    pub fn load(manifest_path: impl AsRef<Path>) -> Result<ProjectSettings, ToolingDiagnostic> {
        let manifest_path = manifest_path.as_ref();
        let manifest = Manifest::load_from_path(manifest_path)
            .map_err(|error| Self::map_package_error(manifest_path, error))?;

        Self::build_settings(manifest, manifest_path)
    }

    fn build_settings(
        manifest: Manifest,
        manifest_path: &Path,
    ) -> Result<ProjectSettings, ToolingDiagnostic> {
        let project = manifest.project.clone();

        Self::validate_sources(manifest_path, &project.sources)?;
        let entrypoint = project
            .entrypoint
            .as_ref()
            .map(|value| Self::validate_entrypoint(manifest_path, value))
            .transpose()?;
        let output_dir = Self::validate_output(manifest_path, &project.output)?;

        Ok(ProjectSettings {
            manifest,
            sources: SourceConfig {
                include: project.sources.include,
                exclude: project.sources.exclude,
            },
            output: OutputConfig {
                directory: output_dir,
                clean: project.output.clean,
            },
            entrypoint,
        })
    }

    fn validate_sources(
        manifest_path: &Path,
        sources: &SourceSection,
    ) -> Result<(), ToolingDiagnostic> {
        Self::validate_patterns(manifest_path, "project.sources.include", &sources.include)?;
        Self::validate_patterns(manifest_path, "project.sources.exclude", &sources.exclude)?;
        Ok(())
    }

    fn validate_patterns(
        manifest_path: &Path,
        field: &str,
        patterns: &[String],
    ) -> Result<(), ToolingDiagnostic> {
        for pattern in patterns {
            let trimmed = pattern.trim();
            if trimmed.is_empty() {
                return Err(Self::invalid_field(
                    manifest_path,
                    field,
                    "空のパターンは指定できません",
                ));
            }

            if starts_with_separator(trimmed) {
                return Err(Self::invalid_field(
                    manifest_path,
                    field,
                    format!("絶対パターン '{pattern}' はサポートされていません"),
                ));
            }

            if contains_parent_reference(trimmed) {
                return Err(Self::invalid_field(
                    manifest_path,
                    field,
                    format!("上位ディレクトリ参照 '..' は使用できません (pattern: '{pattern}')"),
                ));
            }
        }

        Ok(())
    }

    fn validate_entrypoint(
        manifest_path: &Path,
        value: &str,
    ) -> Result<PathBuf, ToolingDiagnostic> {
        let trimmed = value.trim();
        if trimmed.is_empty() {
            return Err(Self::invalid_field(
                manifest_path,
                "project.entrypoint",
                "entrypoint には .jv ファイルへの相対パスを指定してください",
            ));
        }

        if starts_with_separator(trimmed) {
            return Err(Self::invalid_field(
                manifest_path,
                "project.entrypoint",
                "絶対パスは指定できません",
            ));
        }

        if contains_parent_reference(trimmed) {
            return Err(Self::invalid_field(
                manifest_path,
                "project.entrypoint",
                "上位ディレクトリ参照 '..' は使用できません",
            ));
        }

        if !trimmed.ends_with(".jv") {
            return Err(Self::invalid_field(
                manifest_path,
                "project.entrypoint",
                format!("'{value}' は .jv ファイルではありません"),
            ));
        }

        Ok(PathBuf::from(trimmed))
    }

    fn validate_output(
        manifest_path: &Path,
        output: &OutputSection,
    ) -> Result<PathBuf, ToolingDiagnostic> {
        let trimmed = output.directory.trim();
        if trimmed.is_empty() {
            return Err(Self::invalid_field(
                manifest_path,
                "project.output.directory",
                "空のパスは指定できません",
            ));
        }

        if starts_with_separator(trimmed) {
            return Err(Self::invalid_field(
                manifest_path,
                "project.output.directory",
                "絶対パスは指定できません",
            ));
        }

        if contains_parent_reference(trimmed) {
            return Err(Self::invalid_field(
                manifest_path,
                "project.output.directory",
                "上位ディレクトリ参照 '..' は使用できません",
            ));
        }

        Ok(PathBuf::from(trimmed))
    }

    fn map_package_error(manifest_path: &Path, error: PackageError) -> ToolingDiagnostic {
        let detail = match error {
            PackageError::InvalidManifest(message) => message,
            PackageError::IoError(err) => err.to_string(),
            other => other.to_string(),
        };

        Self::diagnostic(format!(
            "{} の読み込み中にエラーが発生しました: {}",
            manifest_path.display(), detail
        ))
    }

    fn invalid_field(
        manifest_path: &Path,
        field: &str,
        detail: impl Into<String>,
    ) -> ToolingDiagnostic {
        Self::diagnostic(format!(
            "{} の {} は無効です: {}",
            manifest_path.display(),
            field,
            detail.into()
        ))
    }

    fn diagnostic(message: String) -> ToolingDiagnostic {
        ToolingDiagnostic {
            code: JV1001_CODE,
            title: JV1001_TITLE,
            message,
            help: JV1001_HELP,
            span: None,
        }
    }
}

fn starts_with_separator(value: &str) -> bool {
    value.starts_with('/') || value.starts_with('\\')
}

fn contains_parent_reference(value: &str) -> bool {
    value
        .split(|c| c == '/' || c == '\\')
        .any(|segment| segment == "..")
}
