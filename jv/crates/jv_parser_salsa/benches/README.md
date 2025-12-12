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

## パイプラインモード別計測

- `SalsaPipeline::execute_with_options` に `ParseOptions` を渡し、`generate_cst` / `generate_trivia_map` のオンオフを切り替えて比較する。
- CST/TriviaMap 無効: `ParseOptions::default()`（高速パス）
- CST/TriviaMap 有効: `ParseOptions { generate_cst: true, generate_trivia_map: true }`
- いずれのモードでもコーパスは `benches/corpus` を利用し、ウォーム/コールド条件を揃えること。

## JDK コーパスシナリオ（型解決前提）

- 背景: 実際の型解決は JDK シンボルの読み込みが前提。パーサー／名前解決パイプラインでも JDK モジュールイメージを直接入力するシナリオをベンチに追加する。
- 入力準備:
  - デフォルト入力は `toolchains/jdk25/lib/modules`（jimage）。`JV_BENCH_JDK_MODULES` 環境変数で差し替え可能。
  - `lib/modules` は展開せずに直接読む。`java.base` に限定せず、デフォルトで解決可能な標準モジュール全体を対象にする。
- 実行ガイド:
  - ベンチ実装側で `JV_BENCH_JDK_MODULES` が未設定かつ `toolchains/jdk25/lib/modules` が存在しない場合はエラーにする（スキップしない）。
  - 単一ファイルを取る `rss_probe --corpus` を使う場合は、モジュールイメージを走査して順次パイプラインへ流すドライバを用意する（仮実装でも可）。
