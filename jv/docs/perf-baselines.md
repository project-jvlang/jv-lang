# AST→IR性能ベースラインガイド

本ガイドは AST→IR 性能ハーネスの運用手順と指標をまとめたものです。開発者が 3 秒 / 100 MB の基準を素早く確認し、必要に応じてベースラインを更新できるようにします。

## ベースライン概要

| 指標 | 予算 | 備考 |
| --- | --- | --- |
| Cold total (`cold_total_ms`) | 3,000 ms 以下 | 初回実行の総時間。予算を超えると CLI / CI が失敗します。 |
| Warm average (`warm_average_ms`) | 3,000 ms 以下 | プールがウォームアップした後の平均時間。ウォームスタート時のみ算出されます。 |
| Pool reuse ratio | 0.90 以上 | `TransformPools` の再利用率。セッションとウォームセッション数も併せて確認してください。 |
| Peak RSS | 100 MB 以下 | RSS を取得できるプラットフォームでのみ記録されます。 |

最新の計測結果は `target/perf-reports/ast-ir-phase1.json` の `summary` と `checks` フィールドに保存されます。

## ハーネス実行手順

### テストハーネス

```bash
cargo test --package jv_ir --lib
cargo test --package jv_ir -- --ignored perf_phase1
```

- 1 行目で IR クレートの通常テストを確認します。
- 2 行目でフェーズ 1 性能フィクスチャを実行し、JSON レポートを生成します。

### CLI プロファイル

```bash
cargo run --bin jv_cli -- build --perf --quiet
```

- `jv build --perf` と同等の計測を行い、失敗時は `PerfCapture` が詳細を表示します。
- ローカルでは CLI を任意で利用でき、CI では `perf_phase1` ワークフローが同じ手順を自動実行します。

## ベースライン更新フロー

1. 上記のテストと CLI プロファイルを実行し、最新のレポートを生成します。
2. `target/perf-reports/ast-ir-phase1.json` を開き、`timestamp_ms` が最新であることを確認します。
3. `checks` の各項目が `true`、`pass` が `true` であることを確認します。
4. 値に変動がある場合は本ガイドの表を更新し、PR 説明などに cold / warm / reuse / RSS の最新値を共有します。
5. CI が「30 日以上古い」と報告する場合は、レポートを再生成してください。更新されない場合は `target/perf-reports` を削除して再実行します。

## トリアージガイド

- `Performance report not found`: ハーネスが途中で失敗した可能性があります。`cargo test --package jv_ir -- --ignored perf_phase1` を再実行し、コンソール出力を確認してください。
- `pass == false`: `checks` 内で失敗した項目を調査し、3,000 ms / 100 MB / 0.90 の閾値を満たしているか再確認します。必要に応じて `runs` の各イテレーションを比較してください。
- `Performance report is older than 30 days`: レポートは生成済みです。ハーネスを再実行すれば更新されます。再生成できない場合は `cargo clean -p jv_ir` 後に再試行してください。
