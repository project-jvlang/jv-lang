# jv_pm Phase 2 利用ガイド

## 1. 概要

jv_pm Phase 2 は、`jv` 言語の依存管理と Maven 互換エクスポートを統合するパッケージマネージャーです。PubGrub ベースの解決器、グローバルキャッシュ、並列ダウンロード、リポジトリ/ミラー管理、Maven プロジェクト出力 (`pom.xml`, `settings.xml`, `.jv/repository`) を提供し、Java エコシステムとの往来を滑らかにします。

本書では、CLI コマンドの使い方・設定ファイルの記述方法・サンプルプロジェクトを通じて Phase 2 のワークフローを解説します。

## 2. セットアップとプロジェクト構成

### 2.1 サンプルプロジェクト

最小構成を手早く試したい場合は、リポジトリに含まれる `examples/jv_pm/` をコピーします。

```bash
cp -r examples/jv_pm my-jvpm-demo
cd my-jvpm-demo
```

ディレクトリ構成は以下のとおりです。

```
my-jvpm-demo/
├── jv.toml
├── README.md
└── src/
    └── main.jv
```

### 2.2 マニフェストの要点

`jv.toml` の主要項目:

```toml
[package]
name = "jvpm-demo"
version = "0.1.0"
description = "jv_pm Phase 2 のチュートリアル用サンプル"

[package.dependencies]
"org.example:demo" = "1.0.0"

[build]
java_version = "25"
max_concurrent_downloads = 12
max_concurrent_warning = 8

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
exclude = []

[project.output]
directory = "target"
clean = false

[repositories]
use-global = true

[[repositories.entries]]
name = "central"
url = "https://repo1.maven.org/maven2"
priority = 10
```

- `[package.dependencies]` は `group:artifact` をキー、バージョン条件を値に取るマップです。
- `[build]` の `max_concurrent_downloads` / `max_concurrent_warning` は ParallelDownloader の挙動を制御します（詳細は §6）。
- `[repositories]` でプロジェクトローカルのレジストリを定義しつつ `use-global = true` でグローバル設定を併用できます。

## 3. 依存関係の追加と削除

依存を追加するには `jv add` を使用します。

```bash
jv add org.example:demo:1.0.0
```

実行例:

```
依存関係 'org.example:demo' をバージョン 1.0.0 で追加しました。
更新: jv.toml / jv.lock
```

- `group:artifact` だけ指定した場合は最新安定版が探索されます。
- 候補が複数ある場合はインタラクティブに選択できます。CI などでプロンプトを避けたい場合は `--non-interactive` を指定してください。

削除は `jv remove` で行います。

```bash
jv remove org.example:demo
```

出力:

```
依存関係 'org.example:demo' を削除しました。
更新: jv.toml / jv.lock
```

候補が複数ある場合は同様に選択プロンプトが表示されます。`--non-interactive` で一覧表示のみを行って終了させることも可能です。

## 4. 依存解決戦略の確認 (`jv resolver`)

Phase 2 では PubGrub / 幅優先 / Maven 互換の 3 戦略が登録されています。`jv resolver list` は戦略一覧をテーブルで表示します。

```
strategy       display            algorithm      stability   default  deterministic  offline  conflicts  policy
---------------------------------------------------------------------------------------------------------------
breadth-first  Breadth-first      breadth-first  experimental no       yes            yes      yes        queue-order
maven          Maven-compatible   maven-compat   experimental no       yes            no       no         first-wins
pubgrub        PubGrub            pubgrub        stable       yes      yes            no       yes        learned-clauses
```

- `strategy`: CLI で指定する識別子 (`--resolver` / `jv resolver info` に利用)。
- `algorithm`: 実装アルゴリズム (`pubgrub`, `breadth-first`, `maven-compat`)。
- `stability`: 安定度。`stable` は本番推奨、`experimental` は新機能評価向けを意味します。
- `conflicts`: 競合理由を人間可読で提示できるかどうか。
- `policy`: バージョン選択ポリシーのキーワード。詳細は設計書を参照してください。

詳細は `jv resolver info <strategy>` で確認できます。

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

`--json` オプションを付けると同じ内容を JSON で取得でき、CI での検証や自動ドキュメント生成に活用できます。

## 5. リポジトリ操作 (`jv repo`)

`jv repo` サブコマンドはプロジェクト/グローバル双方のレジストリ設定を管理します。

- 一覧: `jv repo list` （`--global` でグローバル設定も含める）
- 詳細: `jv repo show central --global`
- 追加: `jv repo add internal https://maven.example.com --priority 50 --auth token --token-env COMPANY_TOKEN`
- 削除: `jv repo remove internal`
- ミラー: `jv repo mirror central https://cache.example.com --name corp-cache`

`jv repo show central` の典型的な出力:

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

メモ:

- `--global` を付与すると `~/.jv/config.toml`（グローバル設定）を更新します。省略時はプロジェクトの `jv.toml` が更新されます。
- `--include-group` / `--exclude-group` は複数回指定でき、グループベースでリポジトリを制限できます。
- すべてのサブコマンドに `--json` があり、構造化出力を取得できます。自動化スクリプトでは `jq` との組み合わせが推奨です。

## 6. グローバルキャッシュと ParallelDownloader

### 6.1 キャッシュの挙動

- グローバルキャッシュは `~/.jv/cache` に作成され、`jars/`, `poms/`, `metadata/` に分割保存されます。
- 取得済みファイルは SHA-256 を再検証した上で再利用されます。不一致が発生した場合は破棄して再取得します。
- キャッシュが破損した場合は明示的な `CacheError` が発生し、再構築手順を案内します。

### 6.2 並列ダウンロード設定

`DownloadManager` は [build] セクションの設定を読み込み、並列取得数を調整します。

```toml
[build]
max_concurrent_downloads = 24   # 要求上限 (既定値 8)
max_concurrent_warning = 12     # 警告閾値 (既定値 16)
```

- 0 以下を指定すると 1 に補正され、「並列ダウンロード数が0のため1に補正しました」と警告します。
- ハード上限は 32。超過すると「要求された並列ダウンロード数 N は許可上限 32 を超えているため 32 に丸めました」と表示されます。
- 警告閾値を超えると「並列ダウンロード数 N が警告閾値 T を超えています」と通知されます。
- これらの設定は `jv add`, `jv remove`, `jv build` のいずれから呼び出されても共有されます。

## 7. OUTPUT_DIR エクスポートと Maven 統合

`jv build --java-only` または `jv build` 成功時に、Phase 2 で導入された Exporter が自動で `target/java-project` を更新します。

```
出力ディレクトリ: .../target/java-project (Java25)
クリーンビルド: 実行しました / Clean build: applied
Javaプロジェクトを ./target/java-project にエクスポートしました (更新ファイル: 5)
```

生成物:

- `pom.xml`: プロジェクトの座標と依存を書き出した Maven プロジェクトファイル。
- `.jv/settings.xml`: リポジトリ/ミラー設定と認証情報への参照。
- `.jv/classpath.txt`: 依存 JAR のクラスパス一覧。
- `.jv/repository/`: ローカルリポジトリに存在するアーティファクトを完全ミラーリング。
- `src/`: `project.output.directory/java{target}` からコピーされた Java ソース。

生成物を再生成したい場合は再度 `jv build`（または `jv build --java-only`）を実行してください。`project.output.directory` を変更すると出力先が変わります。

ソースディレクトリが見つからない場合はエクスポートをスキップし、警告メッセージを表示します。

## 8. CLI ヘルプ (`jv help`)

Phase 2 では専用のヘルプエントリを追加しました。

```bash
jv help                 # Phase 2 全体の概要
jv help resolver        # 依存解決戦略の詳細
jv help repo            # リポジトリ管理コマンドの早見表
jv help cache           # キャッシュと並列ダウンロードの設定
jv help export          # OUTPUT_DIR エクスポート手順
jv help --list-topics   # 利用可能なトピック一覧を表示
```

`docs/jv_pm.md`（本書）と `examples/jv_pm/` を併用することで、Phase 2 の新機能を短時間で試せます。

## 9. トラブルシューティングのヒント

- 依存が見つからない: `jv repo list --global` でリポジトリ設定を確認し、`mirror` の有無をチェックしてください。
- チェックサム不一致: キャッシュが破損している可能性があります。`~/.jv/cache` をバックアップ後に削除し、再実行してください。
- 並列ダウンロード警告: `[build]` の `max_concurrent_downloads` と `max_concurrent_warning` を見直し、物理コア数やレジストリレート制限に合わせて調整してください。
- エクスポートされない: `jv build --java-only` 実行後に `target/java25` が生成されているか確認し、`jv build` を再実行してください。
