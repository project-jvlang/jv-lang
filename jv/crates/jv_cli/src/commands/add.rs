use std::ffi::OsString;

use anyhow::{Context, Result};
use clap::Args;

use super::jvpm_bridge::{propagate_status, spawn_jvpm};

#[derive(Debug, Clone, Args)]
pub struct AddArgs {
    /// 追加する依存関係（group:artifact[:version] または group:artifact@version 形式）
    #[arg(value_name = "package", required = true)]
    pub packages: Vec<String>,

    /// 非対話モード（候補が複数ある場合は一覧表示のみを行って終了）
    #[arg(long = "non-interactive")]
    pub non_interactive: bool,
}

pub fn run(args: &AddArgs) -> Result<()> {
    let argv = build_jvpm_args(args);
    let status = spawn_jvpm(&argv).context("jvpmの呼び出しに失敗しました")?;
    propagate_status(status)
}

fn build_jvpm_args(args: &AddArgs) -> Vec<OsString> {
    let mut argv = Vec::with_capacity(2 + args.packages.len());
    argv.push(OsString::from("add"));
    if args.non_interactive {
        argv.push(OsString::from("--non-interactive"));
    }
    argv.extend(args.packages.iter().map(OsString::from));
    argv
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_args_with_single_package() {
        let args = AddArgs {
            packages: vec!["com.example:demo".into()],
            non_interactive: false,
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![OsString::from("add"), OsString::from("com.example:demo")]
        );
    }

    #[test]
    fn build_args_with_non_interactive_and_multiple_packages() {
        let args = AddArgs {
            packages: vec!["foo".into(), "bar".into()],
            non_interactive: true,
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![
                OsString::from("add"),
                OsString::from("--non-interactive"),
                OsString::from("foo"),
                OsString::from("bar"),
            ]
        );
    }
}
