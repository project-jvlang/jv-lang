# jvドキュメント

**日本語** | [English](README-en.md)

jv（Javaシンタックスシュガー）のドキュメントへようこそ。

## 目次

### ユーザーガイド
- [はじめに](getting-started.md) - インストールと最初のステップ
- [言語ガイド](language-guide.md) - jv言語リファレンス
- [Raw文字列ガイド](language/raw-strings.md) - テキストブロックとRawリテラル
- [正規表現診断ガイド](language/regex-diagnostics.md) - CLI/LSPでのエラー確認
- [CLIリファレンス](cli-reference.md) - コマンドラインの使い方
- [プロジェクト構成](project-structure.md) - jvプロジェクトの整理方法

### 開発者ガイド
- [アーキテクチャ](architecture.md) - コンパイラの概要
- [コントリビュートガイド](contributing.md) - jvへの貢献方法
- [AST→IR性能ベースラインガイド](perf-baselines.md) - パフォーマンスハーネス運用とベースライン管理
- ビルドシステムドキュメント（準備中）
- テストガイド（準備中）

### リファレンス
- [言語仕様](language-spec.md) - jvの形式的仕様
- [Java連携](java-interop.md) - Javaライブラリとの連携方法
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
- **ゼロランタイム**: 出力は純粋なJava 25ソースコード
- **静的型付け**: ダイナミックディスパッチなしで完全静的
- **Java 25最適化**: レコード、パターンマッチング、仮想スレッドを活用
- **統合環境**: 依存関係、JDK、JREを一括管理

## クイックリンク

- **GitHubリポジトリ**: [project-jvlang/jv-lang](https://github.com/project-jvlang/jv-lang)
- **課題トラッカー**: [GitHub Issues](https://github.com/project-jvlang/jv-lang/issues)
- **コミュニティ**: [Discussions](https://github.com/project-jvlang/jv-lang/discussions)

詳細な背景情報はプロジェクトルートの[README](../../README.md)を参照してください。
