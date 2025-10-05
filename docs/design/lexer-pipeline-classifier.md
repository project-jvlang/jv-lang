# lexer-pipeline-2025-10: Classifier 実装メモ

## 実装概要
- `pipeline/stages/classifier/` 配下に責務別モジュール（keyword, number_literal, operator, string_interpolation, json_detection, layout_analysis, comment）を作成し、`Classifier` が順序固定で適用する構成にした。
- `layout_analysis.rs` では PreMetadata 内の `TokenMetadata::LayoutComma` を検出して `TokenType::LayoutComma` を確定し、下流がレイアウト駆動コンマを扱えるようにした。
- `comment.rs` で RawTokenKind::CommentCandidate を行/ブロック/JavaDoc コメントに分類し、既存トークン型の互換性を確保した。
- `ClassificationState` を導入して、各モジュールが確定した TokenType・仮メタデータ・診断を共有できるようにした。
- `TokenPluginManager::with_default_plugins()` を追加し、`plugins/mod.rs` から静的プラグインを読み込む初期化フローを整備した。
- `LegacyLoopPlugin` を実装して、`while`/`do` の診断 (`TokenDiagnostic::LegacyLoop`) をプラグイン段階で付与するようにした。

## テスト
- `pipeline::stages::classifier::tests` にキーワード・数値・演算子に加えてレイアウト検出とコメント分類のテストを追加。
- `plugins::tests::legacy_loop_plugin_adds_diagnostic` でプラグインが診断を付与することを確認。
- `make test-lowmem-crate CRATE=jv_lexer` を実行し、76 件のユニットテストがすべて成功することを確認。

## 今後の TODO
- string_interpolation / layout_analysis / comment モジュールは枠組みのみ。Emitter 実装フェーズで詳細処理を埋める。
- JSON 判定ロジックは旧実装を簡略化して移植済みだが、巨大入力向けのベンチと回帰テストを Task 6 で拡充する。
