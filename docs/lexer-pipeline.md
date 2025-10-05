# レキサーパイプライン運用ガイド

## ステージ構成と役割
- **CharScanner**: ソース文字列からロー・トークン候補を抽出し、チェックポイント／戻し処理と最大1KB先読みリングバッファを管理します。行・列情報およびトリビアの収集もここで行います。
- **Normalizer**: 文字列・数値の正規化、JSON/レイアウト検出、トリビアメタデータの付与を担当します。Unicode正規化とグルーピング記号の整形もこの段階で完了します。
- **Classifier**: 正規化済みトークンを `TokenType` へ確定し、診断・メタデータ・文字列補間計画を決定します。キーワード・演算子・コメント・数値・レイアウトの各モジュールが順番に適用されます。
- **Emitter**: `ClassifiedToken` を最終的な `Token` に変換し、トリビア再注入や文字列補間の分割出力を実行します。コメント引き継ぎや診断転写もここで最終化されます。

## テストと品質保証
- ユニットテスト: `cargo test -p jv_lexer` でステージ単体テストと統合テストを実行します。`jv/crates/jv_lexer/src/tests.rs` に CharScanner/Normalizer/Classifier/Emitter 各段のテストを追加済みです。
- 回帰テスト: プロジェクト全体の省メモリテストは `make test-lowmem-crate CRATE=jv_lexer` を利用してください。
- ベンチマーク: `cargo bench -p jv_lexer --bench pipeline` でパイプライン性能を計測します。`pipeline_baseline` と `pipeline_new` の2系統を比較し、±2%以内の差分確認を目標にしています。

## トレース機構（trace-stages フィーチャ）
- `jv_lexer` には任意機能 `trace-stages` を追加しました。`cargo test -p jv_lexer --features trace-stages` や `cargo bench ... --features trace-stages` のように有効化すると、`pipeline::trace::enable()` / `disable()` を通じて `tracing` ログにステージイベントが出力されます。
- ログにはステージ名・イベント種別・位置情報・トークンプレビューが含まれ、既存の `tracing-subscriber` 設定（例: `RUST_LOG=jv_lexer::pipeline=info`）で観測できます。

## 運用・移行メモ
- 旧レガシー走査ループは完全にパイプラインへ移行済みです。ダウンストリームでトークン列比較テストを行っている場合は、`pipeline_token_sequence_matches_expected_assignment` などの統合テストを参考に期待値を更新してください。
- プラグイン（例: LegacyLoop 診断）は `TokenPluginManager::with_default_plugins()` に集約されています。追加診断を行う場合はプラグインとして登録し、ベンチマークでオーバーヘッドを監視してください。
- 大規模入力向けに CharScanner のチェックポイント/リングバッファ挙動を確認するテストを維持しています。新しいステージを追加する際は同様の単体テスト＋ベンチ指標を揃えることを推奨します。
