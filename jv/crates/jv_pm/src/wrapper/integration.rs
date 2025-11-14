use std::path::PathBuf;

use crate::maven::{
    MavenIntegrationConfig, MavenIntegrationError, MavenIntegrationFiles, MavenIntegrationStrategy,
    PomGenerator,
    settings::{SettingsGenerationRequest, generate_settings_xml},
};

/// Generates pom.xml/settings.xml for wrapper mode.
pub struct WrapperIntegrationStrategy;

impl MavenIntegrationStrategy for WrapperIntegrationStrategy {
    fn name(&self) -> &str {
        "wrapper-default"
    }

    fn description(&self) -> &str {
        "Mavenラッパーモード向けの pom.xml と settings.xml を生成します"
    }

    fn maven_version(&self) -> &str {
        "wrapper"
    }

    fn generate(
        &self,
        config: &MavenIntegrationConfig<'_>,
    ) -> Result<MavenIntegrationFiles, MavenIntegrationError> {
        let manifest = config
            .manifest
            .ok_or(MavenIntegrationError::MissingManifest)?;

        let pom = PomGenerator::new(manifest, config.resolved)
            .with_lockfile(config.lockfile)
            .with_repositories(config.repositories)
            .generate()?;

        let settings_request = SettingsGenerationRequest {
            local_repository: config.local_repository,
            repositories: config.repositories,
            mirrors: config.mirrors,
        };
        let settings = generate_settings_xml(&settings_request)?;

        let mut files = MavenIntegrationFiles::new();
        files.push(PathBuf::from("pom.xml"), pom);
        files.push(PathBuf::from("settings.xml"), settings);
        Ok(files)
    }
}
