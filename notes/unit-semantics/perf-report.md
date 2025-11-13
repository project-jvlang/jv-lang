# 単位系性能測定記録

## 目的

- `catalog_perf` テストで 200 件の単位定義カタログから `UnitRegistry` を構築する際の処理時間を計測し、20ms 以内（許容誤差あり）であることを確認する。
- `memory_report` テストで登録された単位ごとのメモリ使用量をざっくり推定し、要件（単位あたり 64B 以内）に対する余裕を把握する。

## 実行環境

- ベンチ対象: `jv_checker` crate 内の `unit_semantics::catalog_perf` / `memory_report` テスト（`#[ignore]`）。
- データ: `tests/unit_semantics/data/catalog_200.toml`（200 件の単位定義を持つ TOML）。
- 環境変数: `JAVA_HOME=$(pwd)/toolchains/java25`（仕様通り Java 25 toolchain を指す）。
- コマンド:
  ```bash
  JAVA_HOME=$(pwd)/toolchains/java25 cargo test -p jv_checker --release -- --ignored unit_semantics::catalog_perf unit_semantics::memory_report
  ```

## 測定結果

- Release ビルドでの上記 `cargo test` 実行（240 秒タイムアウト付き）は依存クレートのビルドが完了せずタイムアウトしました。測定値（処理時間・メモリ）は引き続き収集中です。
