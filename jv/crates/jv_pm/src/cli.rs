use clap::{Args, Parser, Subcommand, ValueEnum};
use serde::{Deserialize, Serialize};
use std::ffi::OsString;

use crate::repository::config::AuthType;

#[derive(Parser, Debug)]
#[command(name = "jvpm")]
#[command(about = "jv package manager helper", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// 依存関係を追加する
    Add(AddArgs),
    /// 依存関係を削除する
    Remove(RemoveArgs),
    /// Inspect resolver strategies
    #[command(subcommand)]
    Resolver(ResolverCommand),
    /// Manage repository definitions and mirrors
    #[command(subcommand)]
    Repo(RepoCommand),
    /// 未定義コマンドはMavenへフォワード
    #[command(external_subcommand)]
    Maven(Vec<OsString>),
}

#[derive(Debug, Clone, Args)]
pub struct AddArgs {
    /// 追加する依存関係（group:artifact[:version] または group:artifact@version 形式）
    #[arg(value_name = "package", required = true)]
    pub packages: Vec<String>,
    /// 非対話モード（候補のみ表示して終了）
    #[arg(long = "non-interactive")]
    pub non_interactive: bool,
}

#[derive(Debug, Clone, Args)]
pub struct RemoveArgs {
    /// 削除する依存関係（名前または group:artifact 形式）
    #[arg(value_name = "package", required = true)]
    pub packages: Vec<String>,
    /// 非対話モード（候補のみ表示して終了）
    #[arg(long = "non-interactive")]
    pub non_interactive: bool,
}

#[derive(Subcommand, Debug)]
pub enum ResolverCommand {
    /// List every registered resolver strategy
    List {
        /// Emit machine-readable JSON instead of a human table
        #[arg(long)]
        pub json: bool,
    },
    /// Show extended metadata for a single strategy
    Info {
        /// Strategy name or alias (see `jv resolver list`)
        pub name: String,
        /// Emit machine-readable JSON instead of a textual block
        #[arg(long)]
        pub json: bool,
    },
}

#[derive(Subcommand, Debug)]
pub enum RepoCommand {
    /// 現在有効なリポジトリ一覧を表示する
    List {
        /// グローバル設定に登録済みのリポジトリも含める
        #[arg(long = "include-global")]
        pub include_global: bool,
        /// JSON形式で出力する
        #[arg(long)]
        pub json: bool,
    },
    /// 指定したリポジトリの詳細を表示する
    Show {
        /// 対象となるリポジトリ名
        pub name: String,
        /// グローバル設定の一覧も検索対象に含める
        #[arg(long = "include-global")]
        pub include_global: bool,
        /// JSON形式で出力する
        #[arg(long)]
        pub json: bool,
    },
    /// 新しいリポジトリ定義を追加する
    Add {
        /// 追加するリポジトリ名
        pub name: String,
        /// 追加するリポジトリURL
        pub url: String,
        /// 優先度（小さいほど優先）
        #[arg(long)]
        pub priority: Option<u32>,
        /// 書き込み先のスコープ
        #[arg(long = "scope", value_enum, default_value_t = RepoScope::Project)]
        pub scope: RepoScope,
        /// 認証方式
        #[arg(long, value_enum, default_value_t = RepoAuthKind::None)]
        pub auth: RepoAuthKind,
        /// BASIC認証のユーザー名環境変数
        #[arg(long = "username-env")]
        pub username_env: Option<String>,
        /// BASIC認証のパスワード環境変数
        #[arg(long = "password-env")]
        pub password_env: Option<String>,
        /// トークン認証の環境変数
        #[arg(long = "token-env")]
        pub token_env: Option<String>,
        /// include-groups フィルタ
        #[arg(long = "include-group")]
        pub include_groups: Vec<String>,
        /// exclude-groups フィルタ
        #[arg(long = "exclude-group")]
        pub exclude_groups: Vec<String>,
    },
    /// 既存リポジトリ定義を削除する
    Remove {
        /// 削除するリポジトリ名
        pub name: String,
        /// 削除対象のスコープ
        #[arg(long = "scope", value_enum, default_value_t = RepoScope::Project)]
        pub scope: RepoScope,
    },
    /// ミラー設定を表示・編集する
    Mirror {
        /// mirror-of対象名
        pub target: Option<String>,
        /// ミラーURL（設定/更新時）
        pub url: Option<String>,
        /// 表示名（省略可）
        #[arg(long = "name")]
        pub display_name: Option<String>,
        /// 指定時は該当設定を削除する
        #[arg(long)]
        pub remove: bool,
        /// JSON形式で出力する
        #[arg(long)]
        pub json: bool,
        /// 操作対象スコープ
        #[arg(long = "scope", value_enum, default_value_t = RepoScope::Project)]
        pub scope: RepoScope,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub enum RepoScope {
    Project,
    Global,
}

impl RepoScope {
    pub fn label(self) -> &'static str {
        match self {
            RepoScope::Project => "プロジェクト",
            RepoScope::Global => "グローバル",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub enum RepoAuthKind {
    None,
    Basic,
    Token,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RepoOrigin {
    Local,
    Project,
    Global,
}

impl RepoOrigin {
    pub fn label(self) -> &'static str {
        match self {
            RepoOrigin::Local => "ローカル",
            RepoOrigin::Project => "プロジェクト",
            RepoOrigin::Global => "グローバル",
        }
    }
}

impl Serialize for RepoOrigin {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(match self {
            RepoOrigin::Local => "local",
            RepoOrigin::Project => "project",
            RepoOrigin::Global => "global",
        })
    }
}

impl Serialize for RepoScope {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(match self {
            RepoScope::Project => "project",
            RepoScope::Global => "global",
        })
    }
}

impl Serialize for RepoAuthKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(match self {
            RepoAuthKind::None => "none",
            RepoAuthKind::Basic => "basic",
            RepoAuthKind::Token => "token",
        })
    }
}

impl From<AuthType> for RepoAuthKind {
    fn from(value: AuthType) -> Self {
        match value {
            AuthType::None => RepoAuthKind::None,
            AuthType::Basic => RepoAuthKind::Basic,
            AuthType::Token => RepoAuthKind::Token,
        }
    }
}
