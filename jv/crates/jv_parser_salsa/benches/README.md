# Benchmark Protocols

ベンチマーク条件と計測ツールの取り扱いを統一する。

## ウォーム条件

- 3 回のウォームアップを実施し、CPU キャッシュと JIT 不要のランタイム初期化を安定化させる。
- 例: `cargo bench --package jv_parser_salsa -- --warm-up-time 3`
- 計測開始前に `echo 3 | sudo tee /proc/sys/vm/drop_caches` は **実施しない**（ウォーム条件のため）。

## コールド条件

- プロセスを再起動し、キャッシュをリセットした状態で 1 回目の計測を取得する。
- 手順:
  1. 別ターミナルで既存の `cargo bench` を停止する。
  2. `cargo clean -p jv_parser_salsa` でビルドキャッシュをクリア。
  3. 新規シェルで `cargo bench --package jv_parser_salsa -- --sample-size 10` を実行。
- OS キャッシュを完全に落とす必要がある場合のみ、管理者権限で `drop_caches` を検討する。

## メモリ計測ツール

- ツール: **heaptrack**
  - 理由: 実行時ヒープのタイムラインとピーク RSS を取得でき、差分比較も容易。
  - 使用方法: `heaptrack cargo bench --package jv_parser_salsa` を実行し、生成された `.gz` を `heaptrack_gui` または `heaptrack_print` で解析。
  - 測定観点: ピーク RSS、総割り当て量、ホットアロケーションスタック。

## ディレクトリ構成

- `benches/` 以下にベンチマーク実装を追加する。Criterion ベースのベンチを推奨。
