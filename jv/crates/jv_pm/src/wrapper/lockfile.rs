use std::path::Path;

use crate::{LockfileService, ResolvedDependencies};

use super::error::WrapperError;

pub struct WrapperLockfileWriter;

impl WrapperLockfileWriter {
    pub fn write_lockfile(
        project_root: impl AsRef<Path>,
        resolved: &ResolvedDependencies,
    ) -> Result<(), WrapperError> {
        let lockfile = LockfileService::generate_from_resolved(resolved).map_err(|error| {
            WrapperError::OperationFailed(format!("jv.lock の生成に失敗しました: {error}"))
        })?;

        let lockfile_path = project_root.as_ref().join("jv.lock");
        LockfileService::save(&lockfile_path, &lockfile).map_err(|error| {
            WrapperError::OperationFailed(format!(
                "{} への書き込みに失敗しました: {error}",
                lockfile_path.display()
            ))
        })
    }
}
