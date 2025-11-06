use std::ffi::OsString;

use anyhow::{Context, Result};
use clap::{Args, Subcommand};

use super::jvpm_bridge::{propagate_status, spawn_jvpm};

#[derive(Debug, Clone, Args)]
pub struct RepoArgs {
    #[command(subcommand)]
    pub command: RepoCommand,
}

#[derive(Debug, Clone, Subcommand)]
pub enum RepoCommand {
    /// リポジトリ設定を一覧表示する
    List {
        /// JSON形式で出力する
        #[arg(long)]
        json: bool,
        /// プロジェクトローカルだけでなくグローバルも含める
        #[arg(long)]
        global: bool,
    },
    /// 指定したリポジトリの詳細を表示する
    Show {
        /// リポジトリ名
        name: String,
        /// JSON形式で出力する
        #[arg(long)]
        json: bool,
        /// グローバル設定から検索する
        #[arg(long)]
        global: bool,
    },
    /// 新しいリポジトリを追加する
    Add {
        /// 追加するリポジトリ名
        name: String,
        /// 追加するURL
        url: String,
        /// 優先度（小さいほど高優先度）
        #[arg(long)]
        priority: Option<u32>,
        /// グローバル設定へ追加する
        #[arg(long)]
        global: bool,
        /// 認証方式 (none/basic/token)
        #[arg(long, value_name = "mode", value_parser = ["none", "basic", "token"])]
        auth: Option<String>,
        /// BASIC認証時のユーザー名環境変数名
        #[arg(long = "username-env")]
        username_env: Option<String>,
        /// BASIC認証時のパスワード環境変数名
        #[arg(long = "password-env")]
        password_env: Option<String>,
        /// トークン認証時の環境変数名
        #[arg(long = "token-env")]
        token_env: Option<String>,
        /// include-groups フィルタを複数指定
        #[arg(long = "include-group")]
        include_groups: Vec<String>,
        /// exclude-groups フィルタを複数指定
        #[arg(long = "exclude-group")]
        exclude_groups: Vec<String>,
    },
    /// リポジトリ設定を削除する
    Remove {
        /// 削除対象のリポジトリ名
        name: String,
        /// グローバル設定から削除する
        #[arg(long)]
        global: bool,
    },
    /// ミラー設定を閲覧・更新する
    Mirror {
        /// mirror-of対象名 (指定時のみ)
        target: Option<String>,
        /// ミラーURL (設定/更新時のみ)
        url: Option<String>,
        /// ミラー表示名
        #[arg(long = "name")]
        display_name: Option<String>,
        /// 設定を削除する
        #[arg(long)]
        remove: bool,
        /// JSON形式で出力する
        #[arg(long)]
        json: bool,
        /// グローバル設定を対象にする
        #[arg(long)]
        global: bool,
    },
}

pub fn run(args: &RepoArgs) -> Result<()> {
    let arg_list = build_jvpm_args(args);
    let status = spawn_jvpm(&arg_list).context("jvpmの呼び出しに失敗しました")?;
    propagate_status(status)
}

fn build_jvpm_args(args: &RepoArgs) -> Vec<OsString> {
    match &args.command {
        RepoCommand::List { json, global } => {
            let mut result = vec![OsString::from("repo"), OsString::from("list")];
            if *global {
                result.push(OsString::from("--include-global"));
            }
            if *json {
                result.push(OsString::from("--json"));
            }
            result
        }
        RepoCommand::Show { name, json, global } => {
            let mut result = vec![
                OsString::from("repo"),
                OsString::from("show"),
                OsString::from(name),
            ];
            if *global {
                result.push(OsString::from("--include-global"));
            }
            if *json {
                result.push(OsString::from("--json"));
            }
            result
        }
        RepoCommand::Add {
            name,
            url,
            priority,
            global,
            auth,
            username_env,
            password_env,
            token_env,
            include_groups,
            exclude_groups,
        } => {
            let mut result = vec![
                OsString::from("repo"),
                OsString::from("add"),
                OsString::from(name),
                OsString::from(url),
            ];
            if let Some(value) = priority {
                result.push(OsString::from("--priority"));
                result.push(OsString::from(value.to_string()));
            }
            if *global {
                result.push(OsString::from("--scope"));
                result.push(OsString::from("global"));
            }
            if let Some(mode) = auth {
                result.push(OsString::from("--auth"));
                result.push(OsString::from(mode));
            }
            if let Some(value) = username_env {
                result.push(OsString::from("--username-env"));
                result.push(OsString::from(value));
            }
            if let Some(value) = password_env {
                result.push(OsString::from("--password-env"));
                result.push(OsString::from(value));
            }
            if let Some(value) = token_env {
                result.push(OsString::from("--token-env"));
                result.push(OsString::from(value));
            }
            for value in include_groups {
                result.push(OsString::from("--include-group"));
                result.push(OsString::from(value));
            }
            for value in exclude_groups {
                result.push(OsString::from("--exclude-group"));
                result.push(OsString::from(value));
            }
            result
        }
        RepoCommand::Remove { name, global } => {
            let mut result = vec![
                OsString::from("repo"),
                OsString::from("remove"),
                OsString::from(name),
            ];
            if *global {
                result.push(OsString::from("--scope"));
                result.push(OsString::from("global"));
            }
            result
        }
        RepoCommand::Mirror {
            target,
            url,
            display_name,
            remove,
            json,
            global,
        } => {
            let mut result = vec![OsString::from("repo"), OsString::from("mirror")];
            if *global {
                result.push(OsString::from("--scope"));
                result.push(OsString::from("global"));
            }
            if *remove {
                result.push(OsString::from("--remove"));
            }
            if *json {
                result.push(OsString::from("--json"));
            }
            if let Some(name) = display_name {
                result.push(OsString::from("--name"));
                result.push(OsString::from(name));
            }
            if let Some(value) = target {
                result.push(OsString::from(value));
            }
            if let Some(value) = url {
                result.push(OsString::from(value));
            }
            result
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_args_for_list_includes_flags() {
        let args = RepoArgs {
            command: RepoCommand::List {
                json: true,
                global: true,
            },
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![
                OsString::from("repo"),
                OsString::from("list"),
                OsString::from("--include-global"),
                OsString::from("--json"),
            ]
        );
    }

    #[test]
    fn build_args_for_add_basic() {
        let args = RepoArgs {
            command: RepoCommand::Add {
                name: "example".into(),
                url: "https://example.com".into(),
                priority: Some(5),
                global: false,
                auth: Some("basic".into()),
                username_env: Some("USERNAME".into()),
                password_env: Some("PASSWORD".into()),
                token_env: None,
                include_groups: vec!["com.example.*".into()],
                exclude_groups: vec![],
            },
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![
                OsString::from("repo"),
                OsString::from("add"),
                OsString::from("example"),
                OsString::from("https://example.com"),
                OsString::from("--priority"),
                OsString::from("5"),
                OsString::from("--auth"),
                OsString::from("basic"),
                OsString::from("--username-env"),
                OsString::from("USERNAME"),
                OsString::from("--password-env"),
                OsString::from("PASSWORD"),
                OsString::from("--include-group"),
                OsString::from("com.example.*"),
            ]
        );
    }

    #[test]
    fn build_args_for_remove_global() {
        let args = RepoArgs {
            command: RepoCommand::Remove {
                name: "example".into(),
                global: true,
            },
        };
        let built = build_jvpm_args(&args);
        assert_eq!(
            built,
            vec![
                OsString::from("repo"),
                OsString::from("remove"),
                OsString::from("example"),
                OsString::from("--scope"),
                OsString::from("global"),
            ]
        );
    }
}
