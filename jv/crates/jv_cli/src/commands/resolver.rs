use std::ffi::OsString;

use super::jvpm_bridge::{propagate_status, spawn_jvpm};
use anyhow::{Context, Result};
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
