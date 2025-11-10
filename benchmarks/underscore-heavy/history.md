# Underscore Heavy Benchmark History

## 2025-11-04T09-27-10Z (UTC)
- Command: `JAVA_HOME=/home/roomtv/works/project-jv/toolchains/jdk25 cargo bench -p jv_lexer pipeline`
- Input: `jv/crates/jv_lexer/benches/data/underscore-heavy.jv` を 64 回繰り返したデータソース
- Acceptance: 既存ベースラインとの差分を±5%以内に維持すること

| Scenario | Median | Budget | ±5% 判定 | 備考 |
|----------|--------|--------|-----------|------|
| pipeline_baseline | 13.129 ms | 5% | PASS | 参照用ベースライン。 |
| pipeline_trace | 13.008 ms | 5% | PASS | baseline比 -0.92%。 |
| underscore_heavy_baseline | 23.527 ms | 5% | PASS | 新規入力ベースライン。 |
| underscore_heavy_trace | 23.252 ms | 5% | PASS | baseline比 -1.17%。 |

> 備考: `underscore_heavy_*` は今回が初回測定のため、これを将来比較用の基準値として採用する。
