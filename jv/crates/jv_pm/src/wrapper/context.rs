use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use crate::maven::settings::{SettingsGenerationRequest, generate_settings_xml};
use crate::{
    LoggingConfig, Manifest, MavenMirrorConfig, MavenProjectMetadata, MavenRepositoryConfig,
    PackageInfo, PomGenerator, ProjectSection, RepositorySection, ResolutionStats,
    ResolvedDependencies, ResolverAlgorithmKind,
};

use super::error::WrapperError;

/// Execution context for `jvpm` wrapper mode.
#[derive(Debug)]
pub struct WrapperContext {
    pub project_root: PathBuf,
    pub pom_path: PathBuf,
    pub settings_path: PathBuf,
    pub template_generated: bool,
}

impl WrapperContext {
    /// Detects the project layout and prepares the workspace for wrapper mode.
    pub fn detect() -> Result<Self, WrapperError> {
        let project_root = env::current_dir().map_err(|error| {
            WrapperError::OperationFailed(format!(
                "プロジェクトルートの特定に失敗しました: {error}"
            ))
        })?;
        Self::detect_in(project_root)
    }

    fn detect_in(project_root: PathBuf) -> Result<Self, WrapperError> {
        let manifest_path = project_root.join("jv.toml");
        let pom_path = project_root.join("pom.xml");
        let settings_path = project_root.join("settings.xml");

        let manifest_exists = manifest_path.exists();
        let pom_exists = pom_path.exists();

        match (manifest_exists, pom_exists) {
            (true, false) => Err(WrapperError::NativeProjectDetected),
            (true, true) => Err(WrapperError::MixedProjectConfiguration),
            (false, true) => {
                Self::ensure_settings(&project_root, &settings_path)?;
                Ok(Self {
                    project_root,
                    pom_path,
                    settings_path,
                    template_generated: false,
                })
            }
            (false, false) => Self::generate_templates(project_root, pom_path, settings_path),
        }
    }

    fn generate_templates(
        project_root: PathBuf,
        pom_path: PathBuf,
        settings_path: PathBuf,
    ) -> Result<Self, WrapperError> {
        let manifest = Self::template_manifest(&project_root);
        let resolved = Self::template_resolved();

        let pom_xml = PomGenerator::new(&manifest, &resolved)
            .generate()
            .map_err(|error| {
                WrapperError::OperationFailed(format!("pom.xml の生成に失敗しました: {error}"))
            })?;
        Self::write_text(&pom_path, &pom_xml)?;

        Self::ensure_settings(&project_root, &settings_path)?;

        Ok(Self {
            project_root,
            pom_path,
            settings_path,
            template_generated: true,
        })
    }

    fn ensure_settings(project_root: &Path, settings_path: &Path) -> Result<(), WrapperError> {
        if settings_path.exists() {
            return Ok(());
        }

        let local_repository = project_root.join(".jv").join("repository");
        fs::create_dir_all(&local_repository).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "{} の作成に失敗しました: {error}",
                local_repository.display()
            ))
        })?;

        let request = SettingsGenerationRequest {
            local_repository: &local_repository,
            repositories: &[] as &[MavenRepositoryConfig],
            mirrors: &[] as &[MavenMirrorConfig],
        };

        let settings_xml = generate_settings_xml(&request).map_err(|error| {
            WrapperError::OperationFailed(format!("settings.xml の生成に失敗しました: {error}"))
        })?;

        Self::write_text(settings_path, &settings_xml)
    }

    fn write_text(path: &Path, contents: &str) -> Result<(), WrapperError> {
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                fs::create_dir_all(parent).map_err(|error| {
                    WrapperError::OperationFailed(format!(
                        "{} の作成に失敗しました: {error}",
                        parent.display()
                    ))
                })?;
            }
        }

        fs::write(path, contents).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "{} の書き込みに失敗しました: {error}",
                path.display()
            ))
        })
    }

    fn template_manifest(project_root: &Path) -> Manifest {
        let package_name = project_root
            .file_name()
            .and_then(|os| os.to_str())
            .map(str::to_string)
            .filter(|value| !value.trim().is_empty())
            .unwrap_or_else(|| "wrapper-template".to_string());

        let group_id = Self::suggest_group_id(&package_name);

        Manifest {
            package: PackageInfo {
                name: package_name.clone(),
                version: "0.1.0".to_string(),
                description: Some("Generated Maven wrapper template".to_string()),
                dependencies: HashMap::new(),
            },
            project: ProjectSection::default(),
            repositories: RepositorySection::default(),
            mirrors: Vec::new(),
            build: None,
            logging: LoggingConfig::default(),
            maven: MavenProjectMetadata {
                group_id,
                artifact_id: Some(package_name),
                packaging: Some("jar".to_string()),
                description: Some("Generated Maven wrapper template".to_string()),
                url: None,
            },
        }
    }

    fn template_resolved() -> ResolvedDependencies {
        ResolvedDependencies {
            strategy: "wrapper-template".to_string(),
            algorithm: ResolverAlgorithmKind::PubGrub,
            dependencies: Vec::new(),
            diagnostics: Vec::new(),
            stats: ResolutionStats {
                elapsed_ms: 0,
                total_dependencies: 0,
                decided_dependencies: 0,
            },
        }
    }

    fn suggest_group_id(project_name: &str) -> String {
        let cleaned = project_name
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '.' })
            .collect::<String>();

        let collapsed = cleaned
            .split('.')
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>()
            .join(".");

        if collapsed.is_empty() {
            "com.example".to_string()
        } else if collapsed.contains('.') {
            format!("com.{collapsed}")
        } else {
            format!("com.{collapsed}")
        }
    }
}
