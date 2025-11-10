use anyhow::Result;
use clap::{Args, ValueEnum};

#[derive(Debug, Clone, Args)]
pub struct HelpArgs {
    /// 表示するヘルプトピック
    #[arg(value_enum, default_value_t = HelpTopic::Overview)]
    pub topic: HelpTopic,

    /// 利用可能なトピック一覧のみを表示する
    #[arg(long = "list-topics")]
    pub list_topics: bool,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum HelpTopic {
    #[value(alias = "pm")]
    Overview,
    Resolver,
    Repo,
    Cache,
    Export,
}

impl Default for HelpTopic {
    fn default() -> Self {
        HelpTopic::Overview
    }
}

impl HelpTopic {
    fn possible_name(&self) -> &'static str {
        match self {
            HelpTopic::Overview => "overview",
            HelpTopic::Resolver => "resolver",
            HelpTopic::Repo => "repo",
            HelpTopic::Cache => "cache",
            HelpTopic::Export => "export",
        }
    }

    fn body(&self) -> &'static str {
        match self {
            HelpTopic::Overview => OVERVIEW_TEXT,
            HelpTopic::Resolver => RESOLVER_TEXT,
            HelpTopic::Repo => REPO_TEXT,
            HelpTopic::Cache => CACHE_TEXT,
            HelpTopic::Export => EXPORT_TEXT,
        }
    }
}

const OVERVIEW_TEXT: &str = r#"jv_pm Phase 2 パッケージマネージャ概要

基本フロー:
- プロジェクトを作成: `jv init demo-app`
- 依存関係を追加: `jv add org.example:demo:1.0.0`
- 依存解決戦略を確認: `jv resolver list`
- リポジトリ設定を調整: `jv repo list --global`
- Javaエクスポート: `jv build --java-only src/main.jv` を実行すると `target/java-project` に Maven 互換ファイルが自動生成されます。

補足:
- `jv remove <name>` で不要な依存関係を削除できます。
- `jv help resolver` / `jv help repo` / `jv help cache` / `jv help export` で詳細ヘルプを表示できます。
- サンプルプロジェクト: `examples/jv_pm/` をコピーすると最小構成で確認できます。

詳細ドキュメント: `docs/jv_pm.md` を参照してください。
"#;

const RESOLVER_TEXT: &str = r#"依存解決戦略ヘルプ

`jv resolver list` は登録済み戦略をアルファベット順に整形テーブルで表示します。出力例:

```
strategy       display            algorithm      stability   default  deterministic  offline  conflicts  policy
---------------------------------------------------------------------------------------------------------------
breadth-first  Breadth-first      breadth-first  experimental no       yes            yes      yes        queue-order
maven          Maven-compatible   maven-compat   experimental no       yes            no       no         first-wins
pubgrub        PubGrub            pubgrub        stable       yes      yes            no       yes        learned-clauses
```

主な列の意味:
- strategy: CLIで指定する識別子。`--resolver` や `jv resolver info` の引数に使います。
- algorithm: 実装アルゴリズム (`pubgrub` / `breadth-first` / `maven-compat`)。
- stability: 安定度 (`stable` または `experimental`)。
- default: デフォルト戦略かどうか。
- conflicts: 競合理由を人間可読で提示できるか。
- policy: バージョン選択ポリシーのキーワード。

詳細は `jv resolver info <strategy>` で確認できます。例:

```
Strategy     : pubgrub (PubGrub)
Algorithm    : pubgrub
Stability    : stable
Default      : yes
Deterministic: yes
Offline      : no
Conflicts    : yes
Policy       : learned-clauses
Aliases      : pg, default
Description:
  競合駆動節学習を備えた決定論的なPubGrubベースの解決戦略
```

`--json` を付与すると同じ情報をJSON形式で取得できます。CIやスクリプトでは `jv resolver list --json | jq` のように利用してください。
"#;

const REPO_TEXT: &str = r#"リポジトリ管理ヘルプ

`jv repo` サブコマンドで Maven リポジトリとミラーを操作できます:
- 一覧表示: `jv repo list`（`--global` を付けるとグローバル設定も含めます）
- 詳細表示: `jv repo show central --global`
- 追加: `jv repo add internal https://maven.example.com --priority 50 --auth token --token-env COMPANY_TOKEN`
- 削除: `jv repo remove internal`
- ミラー設定: `jv repo mirror central https://cache.example.com --name corp-cache`

典型的な `jv repo show central` の出力:

```
名称        : central
スコープ    : グローバル
優先度      : 10
定義URL     : https://repo1.maven.org/maven2
実効URL     : https://repo1.maven.org/maven2
ミラー      : なし
認証方式    : なし
グループフィルタ: 指定なし
```

ポイント:
- `--global` を付けると `~/.jv/config.toml`（グローバル設定）を更新します。省略時はプロジェクトローカルの `jv.toml` を更新します。
- include/exclude フィルタは複数指定可能です (`--include-group org.example` のように繰り返し指定)。
- `--json` で構造化出力が得られます。自動化時に便利です。
"#;

const CACHE_TEXT: &str = r#"キャッシュと並列ダウンロード

グローバルキャッシュ:
- 既定パスは `~/.jv/cache`。`jars/`, `poms/`, `metadata/` に分類保存されます。
- キャッシュにヒットするとネットワークをスキップし、ハッシュ不一致時は自動削除して再取得します。

並列ダウンロード設定:
- 既定値: 最大8並列、警告閾値16、ハード上限32。
- `jv.toml` の `[build]` で上書きできます:

```toml
[build]
java_version = "25"
max_concurrent_downloads = 24   # 要求上限
max_concurrent_warning = 12     # この値を超えると警告を表示
```

挙動メモ:
- 0以下を指定すると1に補正され警告が記録されます。
- 要求値がハード上限32を超えると `[warn]` ログとともに32へ丸められます。
- 警告閾値を超える設定では「並列ダウンロード数 {N} が警告閾値 {T} を超えています」というメッセージが表示されます。

`jv build` / `jv add` / `jv remove` から共通の `DownloadManager` が使われるため、設定は全コマンドで共有されます。
"#;

const EXPORT_TEXT: &str = r#"OUTPUT_DIRエクスポート

- `jv build --java-only <entry>` を実行すると、`project.output.directory`（既定: `target/java-project`）に Maven 互換プロジェクトが自動生成されます。
- 生成物には `pom.xml`, `settings.xml`, `.jv/classpath.txt`, エクスポートされた `.jv/repository/`、Javaソース (`src/`) が含まれます。
- 生成物を再生成したい場合は再度 `jv build` を実行してください。`OUTPUT_DIR` や Javaターゲットを変更する場合は `jv.toml` か CLI フラグで上書きします。
- ソースディレクトリが存在しない場合はエクスポートがスキップされ、その旨が警告として表示されます。
- `.jv/repository` にはローカルキャッシュ済みの全JARがコピーされるため、CI/CDでの完全再現ビルドに直接利用できます。
"#;

pub fn run(args: &HelpArgs) -> Result<()> {
    if args.list_topics {
        println!("利用可能なトピック:");
        for variant in HelpTopic::value_variants() {
            println!("  - {}", variant.possible_name());
        }
        return Ok(());
    }

    println!("{}", args.topic.body().trim_end());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::Parser;

    #[derive(Parser)]
    #[command(disable_help_subcommand = true)]
    struct Wrapper {
        #[command(subcommand)]
        command: Commands,
    }

    #[derive(clap::Subcommand)]
    enum Commands {
        Help(HelpArgs),
    }

    #[test]
    fn parses_default_topic() {
        let wrapper = Wrapper::parse_from(["jv", "help"]);
        match wrapper.command {
            Commands::Help(args) => assert!(matches!(args.topic, HelpTopic::Overview)),
        }
    }

    #[test]
    fn parses_specific_topic() {
        let wrapper = Wrapper::parse_from(["jv", "help", "resolver"]);
        match wrapper.command {
            Commands::Help(args) => assert!(matches!(args.topic, HelpTopic::Resolver)),
        }
    }

    #[test]
    fn list_topics_flag_is_respected() {
        let wrapper = Wrapper::parse_from(["jv", "help", "--list-topics"]);
        match wrapper.command {
            Commands::Help(args) => {
                assert!(args.list_topics);
                assert!(matches!(args.topic, HelpTopic::Overview));
            }
        }
    }
}
