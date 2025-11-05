use std::env;
use std::ffi::{OsStr, OsString};
use std::io;
use std::path::PathBuf;
use std::process::{Command, ExitStatus};

use anyhow::{Context, Result, anyhow};
use clap::{Args, Subcommand};

#[derive(Debug, Clone, Args)]
pub struct ResolverArgs {
    #[command(subcommand)]
    pub command: ResolverCommand,
}

#[derive(Debug, Clone, Subcommand)]
pub enum ResolverCommand {
    /// List resolver strategies known to jvpm
    List {
        /// Emit JSON instead of a formatted table
        #[arg(long)]
        json: bool,
    },
    /// Show detailed information about a resolver strategy
    Info {
        /// Strategy name or alias (see `jv resolver list`).
        name: String,
        /// Emit JSON instead of a formatted block
        #[arg(long)]
        json: bool,
    },
}

pub fn run(args: &ResolverArgs) -> Result<()> {
    let arg_list = build_jvpm_args(args);
    let status = spawn_jvpm(&arg_list).context("failed to execute jvpm helper")?;
    propagate_status(status)
}

fn build_jvpm_args(args: &ResolverArgs) -> Vec<OsString> {
    let mut result = vec![OsString::from("resolver")];
    match &args.command {
        ResolverCommand::List { json } => {
            result.push(OsString::from("list"));
            if *json {
                result.push(OsString::from("--json"));
            }
        }
        ResolverCommand::Info { name, json } => {
            result.push(OsString::from("info"));
            result.push(OsString::from(name));
            if *json {
                result.push(OsString::from("--json"));
            }
        }
    }
    result
}

fn spawn_jvpm(args: &[OsString]) -> Result<ExitStatus> {
    if let Some(explicit) = env::var_os("JVPM_BIN") {
        let explicit_path = PathBuf::from(explicit);
        return Command::new(&explicit_path)
            .args(args)
            .status()
            .with_context(|| format!("failed to execute {}", explicit_path.display()));
    }

    // First try the normal PATH lookup.
    match Command::new(binary_name()).args(args).status() {
        Ok(status) => return Ok(status),
        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
        Err(error) => return Err(error.into()),
    }

    // Fall back to a sibling binary in the same directory as the current executable.
    if let Some(candidate) = sibling_jvpm_candidate() {
        return Command::new(&candidate)
            .args(args)
            .status()
            .with_context(|| format!("failed to execute {}", candidate.display()));
    }
    // Developer fallback: invoke cargo to run the helper directly.
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
                "failed to execute `{}` while attempting to run jvpm",
                cargo.to_string_lossy()
            )
        })
}

fn propagate_status(status: ExitStatus) -> Result<()> {
    if status.success() {
        return Ok(());
    }

    if let Some(code) = status.code() {
        std::process::exit(code);
    }

    Err(anyhow!("jvpm terminated by signal"))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_args_for_list() {
        let args = ResolverArgs {
            command: ResolverCommand::List { json: true },
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![
                OsString::from("resolver"),
                OsString::from("list"),
                OsString::from("--json"),
            ]
        );
    }

    #[test]
    fn build_args_for_info() {
        let args = ResolverArgs {
            command: ResolverCommand::Info {
                name: "pubgrub".into(),
                json: false,
            },
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![
                OsString::from("resolver"),
                OsString::from("info"),
                OsString::from("pubgrub"),
            ]
        );
    }
}
