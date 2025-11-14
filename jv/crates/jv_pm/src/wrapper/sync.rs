use std::fs;
use std::path::Path;

use crate::MavenCoordinates;
use crate::maven::MavenIntegrationFiles;

use super::error::WrapperError;

/// Summary of what changed during wrapper synchronization runs.
#[derive(Debug, Clone)]
pub struct WrapperUpdateSummary {
    pub added: Vec<MavenCoordinates>,
    pub removed: Vec<MavenCoordinates>,
    pub pom_updated: bool,
    pub settings_updated: bool,
    pub lockfile_updated: bool,
}

pub fn write_lockfile(path: &Path, contents: &[u8]) -> Result<bool, WrapperError> {
    write_bytes_if_changed(path, contents)
}

pub fn sync_maven_artifacts(
    project_root: &Path,
    files: &MavenIntegrationFiles,
) -> Result<(bool, bool), WrapperError> {
    let mut pom_updated = false;
    let mut settings_updated = false;

    for (relative, contents) in &files.files {
        let target = project_root.join(relative);
        let updated = write_bytes_if_changed(&target, contents.as_bytes())?;

        if let Some(name) = relative.file_name().and_then(|os| os.to_str()) {
            if name == "pom.xml" {
                pom_updated |= updated;
            } else if name == "settings.xml" {
                settings_updated |= updated;
            }
        }
    }

    Ok((pom_updated, settings_updated))
}

fn write_bytes_if_changed(path: &Path, contents: &[u8]) -> Result<bool, WrapperError> {
    if path.exists() {
        let existing = fs::read(path).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "{} の読み込みに失敗しました: {error}",
                path.display()
            ))
        })?;
        if existing.as_slice() == contents {
            return Ok(false);
        }
    }

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "{} の作成に失敗しました: {error}",
                parent.display()
            ))
        })?;
    }

    fs::write(path, contents).map_err(|error| {
        WrapperError::OperationFailed(format!(
            "{} への書き込みに失敗しました: {error}",
            path.display()
        ))
    })?;

    Ok(true)
}
