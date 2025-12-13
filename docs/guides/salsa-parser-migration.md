# Salsa Parser Migration Guide

Salsa パイプラインを既定として運用するための手順を示す。

## 前提
- Rust toolchain がセットアップ済み。
- `cargo test -p jv_parser_salsa` がローカルで通ること。

## 手順
1. **依存関係更新**: `jv_parser_salsa` をワークスペースに含め、`cargo check -p jv_parser_salsa` を実行する。
2. **パイプライン切替**: フロントエンド呼び出し箇所で `SalsaPipeline`（または `jv_parser::Parser` facade）を呼び出し、必要に応じて `ParseOptions` で CST/Trivia を有効化する。
3. **診断正規化**: テストで `support::normalize` を利用し、旧実装との差分を検証する。
4. **ベンチ実行**: `cargo bench -p jv_parser_salsa --bench bench_main` を実行し、`benches/RESULTS.md` に結果を転記する。
5. **回帰テスト**: `cargo test --workspace` を実行し、CI でも同じコマンドを追加する。
6. **リリース判定**: `docs/design/salsa-parser.md` と `benches/RESULTS.md` を参照し、Go/No-Go を決定する。

## トラブルシュート
- 診断件数がズレる場合は、`normalize_diagnostics` のソートキーが正しく生成されているか確認する。
- スパン差分が許容範囲外の場合は、`spans_within_tolerance` の閾値を見直し、旧実装のスパン計算と同期する。
