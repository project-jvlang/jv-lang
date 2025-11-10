use std::env;
use std::ffi::{OsStr, OsString};
use std::io;
use std::path::PathBuf;
use std::process::{Command, ExitStatus};

use anyhow::{Context, Result, anyhow};

pub fn spawn_jvpm(args: &[OsString]) -> Result<ExitStatus> {
    if let Some(explicit) = env::var_os("JVPM_BIN") {
        let explicit_path = PathBuf::from(explicit);
        return Command::new(&explicit_path)
            .args(args)
            .status()
            .with_context(|| format!("{} の実行に失敗しました", explicit_path.display()));
    }

    match Command::new(binary_name()).args(args).status() {
        Ok(status) => return Ok(status),
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
    }

    if let Some(candidate) = sibling_jvpm_candidate() {
        return Command::new(&candidate)
            .args(args)
            .status()
            .with_context(|| format!("{} の実行に失敗しました", candidate.display()));
    }

    let cargo = cargo_command();
    Command::new(&cargo)
        .arg("run")
        .arg("-p")
        .arg("jv_pm")
        .arg("--bin")
        .arg("jvpm")
        .arg("--")
        .args(args)
        .status()
        .with_context(|| {
            format!(
                "jvpm 実行のため `{}` の呼び出しに失敗しました",
                cargo.to_string_lossy()
            )
        })
}

pub fn propagate_status(status: ExitStatus) -> Result<()> {
    if status.success() {
        return Ok(());
    }

    if let Some(code) = status.code() {
        std::process::exit(code);
    }

    Err(anyhow!("jvpm がシグナルで終了しました"))
}

fn sibling_jvpm_candidate() -> Option<PathBuf> {
    let exe = env::current_exe().ok()?;
    let dir = exe.parent()?;
    let candidate = dir.join(binary_name());
    if candidate.exists() {
        Some(candidate)
    } else {
        None
    }
}

fn cargo_command() -> OsString {
    env::var_os("CARGO").unwrap_or_else(|| OsString::from("cargo"))
}

fn binary_name() -> &'static OsStr {
    #[cfg(windows)]
    {
        OsStr::new("jvpm.exe")
    }

    #[cfg(not(windows))]
    {
        OsStr::new("jvpm")
    }
}
