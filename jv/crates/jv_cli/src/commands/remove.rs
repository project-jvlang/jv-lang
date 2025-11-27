use std::ffi::OsString;

use anyhow::{Context, Result};
use clap::Args;
use jv_pm::cli::ResolverAlgorithm;

use super::jvpm_bridge::{propagate_status, spawn_jvpm};

#[derive(Debug, Clone, Args)]
pub struct RemoveArgs {
    /// 削除する依存関係（名前または group:artifact 形式）
    #[arg(value_name = "package", required = true)]
    pub packages: Vec<String>,

    /// 非対話モード（候補が複数ある場合は一覧表示のみを行って終了）
    #[arg(long = "non-interactive")]
    pub non_interactive: bool,

    /// 使用する依存解決アルゴリズム
    #[arg(long = "strategy", value_enum)]
    pub strategy: Option<ResolverAlgorithm>,
}

pub fn run(args: &RemoveArgs) -> Result<()> {
    let argv = build_jvpm_args(args);
    let status = spawn_jvpm(&argv).context("jvpmの呼び出しに失敗しました")?;
    propagate_status(status)
}

fn build_jvpm_args(args: &RemoveArgs) -> Vec<OsString> {
    let mut argv = Vec::with_capacity(2 + args.packages.len());
    argv.push(OsString::from("remove"));
    if args.non_interactive {
        argv.push(OsString::from("--non-interactive"));
    }
    if let Some(strategy) = args.strategy {
        argv.push(OsString::from("--strategy"));
        argv.push(OsString::from(strategy.strategy_name()));
    }
    argv.extend(args.packages.iter().map(OsString::from));
    argv
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_args_for_single_package() {
        let args = RemoveArgs {
            packages: vec!["com.example:demo".into()],
            non_interactive: false,
            strategy: None,
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![OsString::from("remove"), OsString::from("com.example:demo")]
        );
    }

    #[test]
    fn build_args_with_non_interactive_flag() {
        let args = RemoveArgs {
            packages: vec!["foo".into()],
            non_interactive: true,
            strategy: None,
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![
                OsString::from("remove"),
                OsString::from("--non-interactive"),
                OsString::from("foo"),
            ]
        );
    }
}
