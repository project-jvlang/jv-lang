# jvドキュメント

**日本語** | [English](README-en.md)

jv（Javaシンタックスシュガー）のドキュメントへようこそ。

## 目次

### ユーザーガイド
- [はじめに](getting-started.md) - インストールと最初のステップ
- [言語ガイド](language-guide.md) - jv言語リファレンス
- [CLIリファレンス](cli-reference.md) - コマンドラインの使い方
- [プロジェクト構成](project-structure.md) - jvプロジェクトの整理方法
- [パターンマッチングガイド](pattern-matching.md) - when式とパターンマッチング
- [JSONリテラルとPOJO生成](language/json.md) - JSONネイティブサポート

### 言語機能詳細
- [空白区切り配列と引数](whitespace-arrays.md) - 文脈考慮パースと同種シーケンス
- [テストフレームワーク統合](testing-frameworks.md) - JUnit、Testcontainers、Playwright等の統合（準備中）
- [ロギングフレームワーク](logging-frameworks.md) - SLF4J、Log4j2等の統合（準備中）
- [DSL埋め込みシステム](dsl-embedding.md) - カスタムDSLの統合方法（準備中）
- [単位系システム](unit-system.md) - 物理量の型安全な扱い（準備中）
- [数値・数学システム](math-system.md) - 複素数、有理数、行列演算（準備中）
- [正規表現サポート](regex-support.md) - 正規表現リテラルと検証（準備中）

### 開発者ガイド
- [アーキテクチャ](architecture.md) - コンパイラの概要
- [コントリビュートガイド](contributing.md) - jvへの貢献方法
- [AST→IR性能ベースラインガイド](perf-baselines.md) - パフォーマンスハーネス運用とベースライン管理
- [Android SDK非サポートポリシー](android-policy.md) - Android対応しない理由と代替手段
- [ir-java-codegenリリースノート](release-notes-ir-java-codegen.md) - 0.1.0マイルストーン
- ビルドシステムドキュメント（準備中）

### リファレンス
- [言語仕様](language-spec.md) - jvの形式的仕様
- [Java連携](java-interop.md) - Javaライブラリとの連携方法
- [Javaターゲットマトリクス](java-target-matrix.md) - Java 21/25の機能対応表
- [アノテーション仕様](sample-annotation.md) - @Sampleアノテーション
- エラーリファレンス（準備中）
- 移行ガイド（準備中）

### サンプル
- [サンプル集](../examples/) - jvプログラム例
- 応用サンプル（準備中）
- ベストプラクティス（準備中）

## jvについて

**jv** 言語（**やわ-らんぐ**）は、読みやすいJava 25ソースコードにコンパイルされるJavaシンタックスシュガーです。Kotlinスタイルのシンタックスシュガーを提供しながら、ランタイム依存関係ゼロで完全なJVM互換性を維持します。

jvは、モダンで簡潔な構文を純粋なJava 25ソースコードにトランスパイルし、Kotlinライクな機能による開発者の生産性と、シームレスなJavaエコシステム統合という両方の利点を提供します。

### 概要
- **ターゲット**: Java 25 LTS
- **出力**: 純粋なJavaソースコード（追加ランタイムなし）
- **実装**: Rust製コンパイラツールチェーン
- **哲学**: ランタイムオーバーヘッドゼロ、最大限の互換性

### 主な機能
- **ゼロランタイム**: 出力は純粋なJava 25ソースコード（Java 21互換出力も対応）
- **静的型付け**: ダイナミックディスパッチなしで完全静的
- **Java 25最適化**: レコード、パターンマッチング、仮想スレッドを活用
- **統合環境**: 依存関係、JDK、JREを一括管理
- **統一された制御フロー**: when式による条件分岐統一、forループによる反復統一
- **JSONネイティブサポート**: コメント付きJSONリテラルからのPOJO自動生成
- **テストフレームワーク統合**: JUnit、Testcontainers、Playwright等をシームレス統合
- **DSL埋め込み**: SQL、YAML、Drools等の外部DSLを型安全に統合
- **単位系システム**: 物理量の次元解析とコンパイル時検証

## クイックリンク

- **GitHubリポジトリ**: [project-jvlang/jv-lang](https://github.com/project-jvlang/jv-lang)
- **課題トラッカー**: [GitHub Issues](https://github.com/project-jvlang/jv-lang/issues)
- **コミュニティ**: [Discussions](https://github.com/project-jvlang/jv-lang/discussions)

詳細な背景情報はプロジェクトルートの[README](../../README.md)を参照してください。
