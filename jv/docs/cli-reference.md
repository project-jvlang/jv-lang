# jv CLIリファレンス

**日本語** | [English](cli-reference-en.md)

jvコマンドラインインターフェースの完全なリファレンスです。

## 概要

`jv`コマンドは、jvプロジェクトの操作における主要なインターフェースです。プロジェクト管理、コンパイル、開発のためのツールを提供します。

```
jv [COMMAND] [OPTIONS] [ARGUMENTS]
```

注意事項：
- 引数なしで`jv`を実行すると、インタラクティブREPLが開始されます。
- `jv repl`で明示的に開始することもできます。
- ファイルやコードスニペットの素早い実行用に、別の`jvx`バイナリが利用可能です。

## グローバルオプション

- `-h, --help`: ヘルプ情報を表示
- `-V, --version`: バージョン情報を表示
- `--verbose`: 詳細出力を有効化
- `--quiet`: 必須でない出力を抑制

## コマンド

### `jvx`

jvファイルやスニペット用のスタンドアロン高速実行ツール。

```bash
# ファイルを実行
jvx src/main.jv -- arg1 arg2

# インラインスニペットを実行
jvx 'fun main(){ println("Hi") }'

# 標準入力から読み込み
echo 'fun main(){ println("stdin") }' | jvx -
```

注意事項：
- `jv x`/`jv exec`の動作をミラーしますが、別のバイナリとして提供されます。
- shebangスタイルやパイプラインワークフローに便利です。

<!-- クイック実行は別の`jvx`バイナリで提供され、`jv exec`ではありません。 -->

### `jv repl`

インタラクティブRead–Eval–Print Loopを開始します。

```bash
jv repl
# または単純に
jv
```

機能：
- jvスニペットをインタラクティブに解析し、パースエラーを報告します。
- メタコマンド: `:help`、`:quit`
- 将来: Javaプレビューと実行フック

### `jv init`

現在のディレクトリに新しいjvプロジェクトを初期化します。

```bash
jv init [OPTIONS] [PROJECT_NAME]
```

**オプション:**
- `--template <TEMPLATE>`: 特定のプロジェクトテンプレートを使用
  - `basic` (デフォルト): シンプルなコンソールアプリケーション
  - `library`: ライブラリプロジェクト構造
  - `web`: Webアプリケーションテンプレート
- `--package <PACKAGE>`: パッケージ名を設定
- `--jdk <VERSION>`: ターゲットJDKバージョンを指定 (デフォルト: 25)

**例:**
```bash
# 現在のディレクトリに基本プロジェクトを初期化
jv init

# プロジェクト名を指定して初期化
jv init my-project

# ライブラリプロジェクトを初期化
jv init --template library my-lib

# 特定のパッケージで初期化
jv init --package com.example.myapp
```

**生成される構造:**
```
project/
├── jv.toml          # プロジェクト設定
├── src/
│   └── main.jv      # メインソースファイル
├── test/
│   └── main_test.jv # テストファイル
└── README.md        # プロジェクトドキュメント
```

### `jv build`

jvソースコードをJavaにコンパイルし、バイトコードを生成します。

```bash
jv build [OPTIONS] [FILES...]
```

**オプション:**
- `--check`: 出力を生成せずにコードをチェック
- `--format`: 生成されたJavaコードをフォーマット
- `--preview`: コンパイルせずに生成されたJavaを表示
- `--output <DIR>`: 出力ディレクトリ (デフォルト: `out/`)
- `--target <DIR>`: コンパイル済みクラスのターゲットディレクトリ
- `--classpath <PATH>`: 追加のクラスパスエントリ
- `--jdk <VERSION>`: ターゲットJDKバージョン
- `--optimization <LEVEL>`: 最適化レベル (0-3)
- `--debug`: デバッグ情報を含める
- `--perf`: AST→IR性能メトリクスを計測し `target/perf-reports/ast-ir-phase1.json` に保存

#### パフォーマンスモード (`--perf`)

`--perf` を付けると AST→IR ローアリングの性能を収集し、パース時間・ローアリング時間・メモリアロケータの再利用率・ピークRSS（対応プラットフォームのみ）を JSON レポートに保存します。閾値は 3,000ms / 100MB /再利用率 0.90 で、失敗すると CLI がリターンコードで通知します。

- 出力先: `target/perf-reports/ast-ir-phase1.json`
- レポート形式: `jv_support::perf::report::PerfReport`
- 利用フェーズ: ローカル検証、CI ワークフロー `perf_phase1`

結果の読み方・ベースライン更新手順・トリアージフローは [AST→IR性能ベースラインガイド](perf-baselines.md) に集約しています。失敗時は `checks` セクションの失敗項目を確認し、ベースライン鮮度や `target/perf-reports` の内容を照合してください。

```bash
# AST→IR 性能を計測しつつプレビューする
jv build --perf --preview
```

Sample/@Sampleオプション:
- `--sample-mode=embed|load`: デフォルトの`@Sample`モードを上書き (デフォルト: embed)
- `--sample-network=allow|deny`: サンプル取得時のネットワークアクセスを許可 (デフォルト: deny)
- `--sample-embed-max-bytes=<N>`: コードに埋め込む最大バイト数 (デフォルト: 1048576)
- `--sample-embed-fallback=load|fail`: 埋め込みが制限を超えた場合の動作 (デフォルト: fail)
- `--sample-cache=on|off`: ランタイムロード用のローカルキャッシュを有効化 (デフォルト: on)

バイナリパッケージングオプション:
- `--binary jar|native`: コンパイル後に単一ファイルアーティファクトを作成
  - `jar`: `<output>/app.jar` (または`--bin-name`)を作成。`jar`ツールが必要。
  - `native`: GraalVMの`native-image`を試行して`<output>`にネイティブバイナリを作成。インストールされていない場合はエラー。
- `--bin-name <NAME>`: アーティファクトの出力ベース名 (デフォルト: `app`)

**例:**
```bash
# プロジェクト全体をビルド
jv build

# 特定のファイルをビルド
jv build src/main.jv src/utils.jv

# フォーマットとプレビューでビルド
jv build --format --preview

# デバッグ情報付きでビルド
jv build --debug

# ビルドとチェックのみ（出力なし）
jv build --check

# ビルドして単一JARにパッケージ
jv build src/main.jv --binary jar --bin-name myapp

# ビルドしてネイティブイメージを試行（GraalVM native-imageが必要）
jv build src/main.jv --binary native --bin-name myapp
```

**ビルドプロセス:**
1. **字句解析**: jvソースファイルをトークン化
2. **構文解析**: 抽象構文木（AST）を生成
3. **型チェック**: 型の検証と推論を実行
4. **IR生成**: 中間表現に変換
5. **Java生成**: 読みやすいJava 25ソースコードを生成
6. **Javaコンパイル**: `javac`でバイトコードにコンパイル

### `jv run`

コンパイル済みjvプログラムを実行します。

```bash
jv run [OPTIONS] [MAIN_CLASS] [PROGRAM_ARGS...]
```

**オプション:**
- `--classpath <PATH>`: 追加のクラスパスエントリ
- `--jvm-args <ARGS>`: JVMに渡す引数
- `--main <CLASS>`: 実行するメインクラスを指定
- `--args <ARGS>`: プログラムに渡す引数

**例:**
```bash
# メインクラスを実行
jv run

# JVM引数付きで実行
jv run --jvm-args "-Xmx1g -Xms512m"

# 特定のクラスを実行
jv run --main com.example.MyApp

# プログラム引数付きで実行
jv run --args "arg1 arg2 arg3"

# 複合例
jv run --main MyApp --jvm-args "-Xmx2g" --args "input.txt output.txt"
```

### `jv debug --stage ir --emit ast`

デシュガリング済み IR から AST を再構築し、警告と統計を含むデバッグレポートを生成します。

```bash
jv debug --stage ir --emit ast \\
  --input build/fixtures/sample.ir.json \\
  --output - \\
  --format json
```

主なオプション:
- `--stage ir`: 対象ステージを指定（現状は IR のみ対応）。
- `--emit ast`: 出力アーティファクトを AST に設定。
- `--input <PATH>`: IR JSON の入力元。`-` または未指定で標準入力を利用。
- `--output <PATH>`: 出力先。`-` で標準出力、ファイルパス指定で保存。
- `--format json|pretty`: JSON か整形テキストを選択（デフォルト: `json`）。
- `--no-stats`: サマリーフッター出力を抑制。
- `--fail-on-timeout <ms>`: 実行時間が閾値を超えた場合に非ゼロ終了。

JSON 出力例は `jv/crates/jv_cli/tests/fixtures/debug_ir_expected.json` のゴールデンスナップショットを参照してください。`--format pretty` を指定すると統計値・警告一覧・AST を段階的に表示し、デフォルトでは標準エラーへサマリーを出力します。

回帰テストは `jv/crates/jv_cli/tests/debug_ir.rs` に配置されており、`cargo test -p jv_cli` で実行できます。

### `jv fmt`

標準スタイルガイドラインに従ってjvソースコードをフォーマットします。

```bash
jv fmt [OPTIONS] [FILES...]
```

**オプション:**
- `--check`: ファイルを変更せずにフォーマットされているかチェック
- `--diff`: 行われる変更の差分を表示
- `--write`: ファイルに変更を書き込み（デフォルトの動作）
- `--indent <SIZE>`: インデントサイズ (デフォルト: 4)
- `--line-width <WIDTH>`: 最大行幅 (デフォルト: 100)
- `--trailing-comma`: 常に末尾カンマを使用

**例:**
```bash
# プロジェクト内のすべてのjvファイルをフォーマット
jv fmt

# 特定のファイルをフォーマット
jv fmt src/main.jv src/utils.jv

# 変更せずにフォーマットをチェック
jv fmt --check

# フォーマットの差分を表示
jv fmt --diff

# カスタム行幅でフォーマット
jv fmt --line-width 80
```

**フォーマットルール:**
- **インデント**: 4スペース（設定可能）
- **行長**: 100文字（設定可能）
- **ブレース**: K&Rスタイル（開きブレースは同一行）
- **スペース**: 演算子周りの一貫したスペース
- **インポート整理**: 自動インポートソートと重複除去

### `jv check`

jvソースコードに対して静的解析と型チェックを実行します。

```bash
jv check [OPTIONS] [FILES...]
```

**オプション:**
- `--strict`: 厳密チェックモードを有効化
- `--null-safety`: 拡張null安全性チェックを有効化
- `--unused`: 未使用変数とインポートをチェック
- `--performance`: パフォーマンスアンチパターンをチェック
- `--security`: セキュリティ脆弱性チェックを有効化
- `--json`: 結果をJSON形式で出力
- `--fix`: 可能な場合は自動的に問題を修正

**例:**
```bash
# 基本的な型チェック
jv check

# すべての警告を含む厳密チェック
jv check --strict --unused --performance

# 特定のファイルをチェック
jv check src/main.jv src/models/

# セキュリティ重視のチェック
jv check --security --strict

# 機械可読な出力を取得
jv check --json > check-results.json
```

**チェックカテゴリ:**
- **型安全性**: 型の不一致、null安全性違反
- **コード品質**: 未使用変数、到達不可能コード、複雑性
- **パフォーマンス**: 非効率なパターン、メモリリーク
- **セキュリティ**: SQLインジェクション、XSS脆弱性、安全でないパターン
- **スタイル**: 命名規則、コード構成

### `jv version`

jvとそのコンポーネントのバージョン情報を表示します。

```bash
jv version [OPTIONS]
```

**オプション:**
- `--verbose`: 詳細なバージョン情報を表示
- `--json`: バージョン情報をJSON形式で出力

**例:**
```bash
# 基本バージョンを表示
jv version
# 出力: jv 0.1.0 - Javaシンタックスシュガーコンパイラ

# 詳細なバージョン情報を表示
jv version --verbose
# 出力:
# jv 0.1.0 - Javaシンタックスシュガーコンパイラ
# Built: 2024-09-05
# Rust: 1.70.0
# Target: Java 25
# Features: null-safety, async, pattern-matching

# JSON出力
jv version --json
```

## 設定

### プロジェクト設定 (`jv.toml`)

```toml
[project]
name = "my-project"
version = "1.0.0"
authors = ["Your Name <you@example.com>"]
description = "jvプロジェクト"
license = "MIT"

[build]
jdk_version = "25"
optimization_level = 2
debug = false
target_dir = "out"

[dependencies]
# jvレジストリ依存関係
math-utils = "1.2.0"

# Maven依存関係
[dependencies.maven]
"org.apache.commons:commons-lang3" = "3.12.0"
"com.fasterxml.jackson.core:jackson-core" = "2.15.0"

[dev-dependencies]
junit = "5.9.0"

[toolchain]
jdk = "temurin-25"
prefer_system = false

[format]
line_width = 100
indent_size = 4
trailing_comma = true

[check]
strict = true
null_safety = true
unused_warnings = true
performance_warnings = true
```

### グローバル設定

グローバルjv設定は以下に保存されます：
- **Linux/macOS**: `~/.config/jv/config.toml`
- **Windows**: `%APPDATA%\jv\config.toml`

```toml
[defaults]
jdk_version = "25"
editor = "code"
format_on_save = true

[toolchain]
auto_install = true
preferred_vendor = "temurin"

[registry]
default = "https://registry.jv-lang.org"
mirrors = [
    "https://mirror1.jv-lang.org",
    "https://mirror2.jv-lang.org"
]

[check]
default_strict = true
auto_fix = false
```

## 環境変数

- `JV_HOME`: jvインストールディレクトリ
- `JV_REGISTRY`: デフォルトレジストリURLを上書き
- `JV_JDK_HOME`: JDK検出を上書き
- `JV_LOG_LEVEL`: ログレベルを設定 (debug, info, warn, error)
- `JV_NO_COLOR`: カラー出力を無効化
- `JV_OFFLINE`: オフラインモードで動作（ネットワーク要求なし）

## 終了コード

- `0`: 成功
- `1`: 一般エラー
- `2`: コンパイルエラー
- `3`: ランタイムエラー
- `4`: 設定エラー
- `5`: ネットワークエラー
- `101`: 内部エラー（バグとして報告してください）

## シェル補完

お使いのシェル用の補完を生成：

```bash
# Bash
jv completion bash > ~/.local/share/bash-completion/completions/jv

# Zsh
jv completion zsh > ~/.local/share/zsh/site-functions/_jv

# Fish
jv completion fish > ~/.config/fish/completions/jv.fish

# PowerShell
jv completion powershell > $PROFILE
```

## IDEとの統合

### VS Code

VS Code用jv拡張機能をインストール：
- 構文ハイライト
- エラー診断
- コードフォーマット
- IntelliSenseサポート

### IntelliJ IDEA

jv言語サーバーが提供：
- コード補完
- エラーハイライト
- リファクタリングサポート
- デバッグ統合

### Vim/Neovim

`jv-vim`プラグインを使用するか、`nvim-lspconfig`などのLSPクライアントで設定。

## トラブルシューティング

### よくある問題

**"jv: command not found"**
- jvがPATHに含まれていることを確認
- フルパスで実行を試行: `./target/release/jv`

**"Java 25 required but Java X found"**
- Java 25をインストールするか`JAVA_HOME`を設定
- JDKバージョン管理に`jv toolchain install`を使用

**クラスパスエラーでビルドが失敗**
- `jv.toml`の依存関係を確認
- Maven依存関係が利用可能であることを確認

**パフォーマンスの問題**
- リリースビルドには`--optimization 3`を使用
- `jv check --performance`でパフォーマンス警告をチェック

### デバッグモード

トラブルシューティング用の詳細ログを有効化：

```bash
JV_LOG_LEVEL=debug jv build --verbose
```

### ヘルプの取得

- [GitHub Issues](https://github.com/project-jvlang/jv-lang/issues)をチェック
- コミュニティ[Discussions](https://github.com/project-jvlang/jv-lang/discussions)に参加
