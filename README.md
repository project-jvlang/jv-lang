# jv [/jawa/] - シンタックスシュガー

[English](README-en.md) | **日本語**

**jv** 言語（**やわ-らんぐ**）は、読みやすいJava 25ソースコードにコンパイルされるJavaシンタックスシュガーです。Kotlinスタイルのシンタックスシュガーを提供しながら、ランタイム依存関係ゼロで完全なJVM互換性を維持します。

## 概要

jvは、モダンで簡潔な構文を純粋なJava 25ソースコードにトランスパイルし、両方の世界の最良の部分を提供します：Kotlinライクな機能による開発者の生産性と、シームレスなJavaエコシステム統合。

- **ターゲット**: Java 25 LTS
- **出力**: 純粋なJavaソースコード（追加ランタイムなし）
- **実装**: Rustベースのコンパイラツールチェーン
- **哲学**: ランタイムオーバーヘッドゼロ、最大限の互換性

## リリース状況

`ir-java-codegen`マイルストーンを完了し、0.1.0リリースに向けた機能が揃いました。変換・コード生成・ソースマップ・CLI統合の全タスクを出荷し、`cargo fmt --all` と `cargo test -p jv_ir -p jv_codegen_java -p jv_mapper -p jv_cli` を通過しています（その他のクレートは引き続き開発中です）。反映された内容は [CHANGELOG.md](CHANGELOG.md) と [release notes](jv/docs/release-notes-ir-java-codegen-en.md) を参照してください。

## 機能

### 言語機能
- 型推論付き`val/var`宣言
- null安全演算子: `?`, `?.`, `?:`
- `when`式 → Javaのswitch/パターンマッチング
- `data class` → record（不変）またはclass（可変）
- 拡張関数 → 静的ユーティリティメソッド
- 文字列補間: `"Hello, ${name}"`
- 仮想スレッド: `spawn {}`
- 非同期/await: `async {}.await()` → CompletableFuture
- リソース管理: `use {}` → try-with-resources
- デフォルト引数と名前付き引数 → メソッドオーバーロード
- トップレベル関数 → ユーティリティクラス

### ツールチェーン機能
- 読みやすいJava 25への**高速コンパイル**
- デバッグサポート用**ソースマップ**
- jvレジストリ + Mavenブリッジ付き**パッケージマネージャー**
- 自動インストール付き**JDK管理**
- Language Server Protocol経由の**IDE統合**
- javac統合付き**ビルドシステム**
- **コードフォーマッター**と静的解析

## クイックスタート

### インストール

```bash
# GitHubリリースからインストール
curl -L https://github.com/project-jvlang/jv-lang/releases/latest/download/install.sh | sh

# またはcargoを使用
cargo install jv-cli

# またはソースからビルド
git clone https://github.com/project-jvlang/jv-lang.git
cd jv-lang
cargo build --release
```

### Hello World

新しいプロジェクトを作成：
```bash
jv init hello-world
cd hello-world
```

jvコードを書く（`src/main.jv`）：
```kotlin
fun main() {
    val name = "World"
    println("Hello, ${name}!")

    val numbers = listOf(1, 2, 3, 4, 5)
    val doubled = numbers.map { it * 2 }
    println("Doubled: ${doubled}")
}
```

ビルドして実行：
```bash
jv build
jv run
```

生成されるJavaコードはクリーンで読みやすい：
```java
public class Main {
    public static void main(String[] args) {
        final var name = "World";
        System.out.println("Hello, " + name + "!");

        final var numbers = List.of(1, 2, 3, 4, 5);
        final var doubled = numbers.stream()
            .map(it -> it * 2)
            .toList();
        System.out.println("Doubled: " + doubled);
    }
}
```

## プロジェクト構造

このリポジトリには複数のクレートを含むRustワークスペースが含まれています：

```
crates/
├── jv_lexer         # 字句解析
├── jv_parser        # 構文解析（chumskyを使用）
├── jv_ast           # 抽象構文木定義
├── jv_ir            # 脱糖用中間表現
├── jv_codegen_java  # Java 25コード生成
├── jv_mapper        # ソースマップ（.jv ↔ .java）
├── jv_checker       # 静的解析（null安全、禁止構文）
├── jv_fmt           # コードフォーマッター
├── jv_pm            # パッケージマネージャー
├── jv_build         # ビルドシステムとjavac統合
├── jv_lsp           # Language Server Protocol実装
└── jv_cli           # CLIエントリポイント（jv/jvlangコマンド）
```

`jv_parser` の延期構文状況は `jv/crates/jv_parser/docs/deferred-syntax.md` を参照してください。

## 開発

### 前提条件
- Rust 1.75+
- Java 21+（生成されたコードのテスト用）

### ビルド
```bash
# ワークスペース全体をビルド
cargo build

# CLIを実行
cargo run --bin jv_cli

# テストを実行
cargo test

# ビルドせずにチェック
cargo check
```

### AST→IR性能ハーネス

- ローカルでの回帰検知: `cargo test --package jv_ir --lib` と `cargo test --package jv_ir -- --ignored perf_phase1`
- CLI経由のメトリクス採取: `jv build --perf`（または `cargo run --bin jv_cli -- build --perf`）
- 生成レポート: `target/perf-reports/ast-ir-phase1.json` に `summary` / `checks` が保存され、3,000ms / 100MB / 再利用率0.90が閾値
- 運用フローとベースライン更新は [AST→IR性能ベースラインガイド](jv/docs/perf-baselines.md) を参照

### 言語ツアーの品質検証

新しい統合テストスイート `crates/jv_cli/tests/tour_tests.rs` は、`jv tour` のメインフロー・環境チェック・ポートフォリオ生成を通して学習体験を検証します。以下のコマンドで再現可能です。

```bash
cargo test -p jv_cli --test tour_tests
```

模擬JDKプローブを用いるためクロスプラットフォームで安全に実行でき、生成される成果物の整合性も自動確認されます。継続的テストやドキュメント更新時の回帰チェックに活用してください。

### CLIコマンド（実装ターゲット）

```bash
jv init                # 新しいプロジェクトを作成
jv add <pkg>           # 依存関係を追加
jv build [--preview]   # Javaにビルド
jv run                 # 実行
jv test                # テストを実行
jv fmt                 # ソースをフォーマット
jv lint                # 静的解析
jv toolchain install   # JDKバージョンをインストール
jv use <jdk>           # プロジェクトJDKを設定
jv publish             # レジストリに公開
jv doctor              # 環境診断
```

## 言語ガイド

### 変数と型推論
```kotlin
val immutable = "変更不可"
var mutable = 42
val inferred = listOf(1, 2, 3)  // List<Integer>
```

### null安全
```kotlin
val nullable: String? = getName()
val length = nullable?.length ?: 0
val safe = nullable ?: "default"
```

### データクラス
```kotlin
data class Person(val name: String, val age: Int)

val person = Person("Alice", 30)
val older = person.copy(age = 31)
```

### when式
```kotlin
val result = when (value) {
    is String -> "text: ${value}"
    is Int -> "number: ${value}"
    else -> "unknown"
}
```

### 拡張関数
```kotlin
fun String.isPalindrome(): Boolean {
    return this == this.reversed()
}

"racecar".isPalindrome()  // true
```

### 非同期プログラミング
```kotlin
async {
    val data = fetchData().await()
    processData(data)
}.await()
```

## パッケージ管理

### jv.toml
```toml
[package]
name = "my-app"
version = "1.0.0"
jdk = "21"

[dependencies]
commons-lang = "3.14.0"  # Maven依存関係
jv-json = "1.0"          # jvレジストリ
```

### コマンド
```bash
jv add commons-lang      # Maven依存関係を追加
jv add jv-json           # jv依存関係を追加
jv audit                 # セキュリティ監査
```

## IDE サポート

jvにはIDE統合用のLanguage Server Protocol実装が含まれています：

- **構文ハイライト**
- **エラー診断**
- **自動補完**
- **定義へのジャンプ**
- **リファクタリングサポート**
- **デバッグ**（ソースマップ経由）

### VS Code
マーケットプレイスからjv拡張機能をインストール。

### IntelliJ IDEA
IDEA UltimateとCommunity用プラグインが利用可能。

## Javaとの比較

| 機能 | Java 25 | jv |
|------|---------|-----|
| 変数宣言 | `final var name = "value";` | `val name = "value"` |
| null安全 | 手動nullチェック | `?.`, `?:`演算子 |
| パターンマッチング | `switch`式 | `when`式 |
| データクラス | Record + ボイラープレート | `data class` |
| コレクション | 冗長なstream API | 拡張関数 |
| 文字列テンプレート | テキストブロック + 連結 | `"Hello ${name}"` |
| 非同期プログラミング | CompletableFuture | `async/await` |

## コントリビューション

開発セットアップとガイドラインについては[CONTRIBUTING.md](jv/docs/contributing.md)を参照してください。

## ライセンス

以下のいずれかでライセンスされています：
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT License ([LICENSE-MIT](LICENSE-MIT))

お好みの方をお選びください。

## ロードマップ

- [ ] **フェーズ1**: コアコンパイラ（字句解析器、構文解析器、基本コード生成）
- [ ] **フェーズ2**: 高度な機能（null安全、パターンマッチング）
- [ ] **フェーズ3**: ツール（LSP、フォーマッター、パッケージマネージャー）
- [ ] **フェーズ4**: エコシステム（レジストリ、IDEプラグイン、ドキュメント）

## サポート

このプロジェクトが役に立ったら、開発を支援してください：

<a href="https://buymeacoffee.com/asopitechia"><img src="assets/yellow-button.png" alt="Buy Me A Coffee" width="150"></a>

## リンク

- [言語仕様](jv/docs/language-spec.md)
- [はじめに](jv/docs/getting-started.md)
- [アーキテクチャ概要](jv/docs/architecture.md)
- [CLIリファレンス](jv/docs/cli-reference.md)
