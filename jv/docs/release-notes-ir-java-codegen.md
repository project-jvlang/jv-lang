# ir-java-codegenリリースノート (0.1.0)

**日本語** | [English](release-notes-ir-java-codegen-en.md)

## 概要

ir-java-codegenマイルストーンは、jv ASTからJava 25ソースコードへの脱糖パイプラインを完成させます。変換コンテキスト管理、診断エラーレポート、Java出力、ソースマッピング、およびCLIオーケストレーションが本番環境対応になりました。

## ハイライト

- すべての変換モジュールで再利用可能なAPIを持つスコープ対応`TransformContext`
- アクション可能なガイダンスと正確な`Span`メタデータを持つ充実した`TransformError`診断
- レコード、パターンマッチング、仮想スレッド、非同期フロー、およびリソース管理構文のためのIR定義とJavaコード生成の確定
- IDEツール向けに`jv_mapper`を通じて提供されるソースマッピングサポート
- レキサー → パーサー → IR → コード生成 → javac検証に配線されたエンドツーエンドCLIワークフロー（`jv_cli`）
- `jv_ir`、`jv_codegen_java`、`jv_mapper`、`jv_cli`での単体テスト、統合テスト、スナップショットテストの拡充

## 品質ゲート

- `cargo fmt --all`
- `cargo test -p jv_ir -p jv_codegen_java -p jv_mapper -p jv_cli`

## アップグレードガイダンス

- バージョン`0.1.0`に更新して、完成したすべてのIRおよびJavaコード生成機能を取得してください
- ビルド失敗のトリアージ時に、新しい`TransformError`バリアントからの充実した診断を確認してください
- カスタムIRノードを持つダウンストリームフォークを維持している場合は、更新後にスナップショットを再生成してください
