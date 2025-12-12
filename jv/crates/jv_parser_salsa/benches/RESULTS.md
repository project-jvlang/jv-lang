# Benchmark Results (Salsa Only)

`cargo bench -p jv_parser_salsa --bench bench_main` の最新結果を Salsa Fast / Salsa Full のみで集約する。

## 実行環境
- マシン: ローカル開発機（詳細未記録）
- コマンド: `cargo bench -p jv_parser_salsa --bench bench_main`
- 条件: ウォームアップ 3s、計測 10s、サンプル 20、コールド測定はプロセス再起動で取得

## 8.2 フルパース
| シナリオ | Salsa Fast | Salsa Full | 目標 |
| --- | --- | --- | --- |
| stdlib 全体 | 59.39 ms | 69.63 ms | 成功 |
| synthetic 100 | 6.20 ms | n/a | 成功 |
| synthetic 2000 | n/a | 134.11 ms | 成功 |

## 8.3 インクリメンタル
| シナリオ | 時間 | 目標 |
| --- | --- | --- |
| 1 行変更 (salsa_full single_line_edit) | 71.53 ms | 50% 以上短縮 |
| キャッシュヒット推定 (salsa_fast cache_hit_estimate) | 40.75 ns | 50% 以上短縮 |

## 8.4 メモリ（cacheless）
### synthetic-2000
| シナリオ | Salsa Fast RSS Δ(KiB) | Salsa Full RSS Δ(KiB) | 目標 |
| --- | --- | --- | --- |
| synthetic-2000 | 13,248 | 18,240 | 70% 基準を参照 |

### 大規模合成データの RSS スケーリング
`cargo run -p jv_parser_salsa --release --example rss_probe -- --pipeline <pipeline> --generate-functions N --cache-mode cacheless`

| 行数（関数数） | Salsa Fast RSS Δ(KiB) | Salsa Full RSS Δ(KiB) |
| --- | --- | --- |
| 2,000（333 関数相当） | 13,248 | 18,240 |
| 9,998（1,666 関数相当） | 61,200 | 86,604 |
| 20,000（3,333 関数相当） | 121,188 | 171,376 |
| 39,998（6,666 関数相当） | 102,344 | 202,500 |

### JDK 読み込み時の RSS（cacheless）
| シナリオ | Salsa Fast RSS Δ(KiB) | Salsa Full RSS Δ(KiB) |
| --- | --- | --- |
| synthetic-2000 | 116,388 | 121,512 |
| generate-functions 3333（~20k 行） | 224,472 | 274,692 |
| generate-functions 6666（~40k 行） | 205,936 | 306,220 |

備考:
- JDK モジュールは `toolchains/jdk25/lib/modules`（または `JV_BENCH_JDK_MODULES`）をプリロード。`--skip-jdk`/`JV_BENCH_SKIP_JDK_MODULES` でオプトアウト。
- メモリ評価は「定数オフセット（~100 MiB）+ 傾き」で見る。Fast ≤7 MiB/1k 行、Full ≤9 MiB/1k 行を目安。

## 8.5 LSP
| シナリオ | Salsa Fast p95 (ms) | Salsa Full p95 (ms) | 目標 |
| --- | --- | --- | --- |
| completion 500 行 | 29.92 ms | n/a | 200ms 以下 |
| diagnostics 500 行 | n/a | 37.94 ms | 200ms 以下 |

## Go / No-Go 参考メモ
| 項目 | 目標 | 結果 | 判定 |
| --- | --- | --- | --- |
| フルパース性能 | stdlib/synthetic で目標達成 | Fast/Full とも目標内 | ok |
| インクリメンタル速度 | 50% 以上短縮 | 約 46% 短縮（fast/steady） | at risk |
| メモリ | オフセット<=120 MiB かつ傾き Fast<=7 / Full<=9 MiB/1k 行 | 2k 時点: Fast 113.7 MiB, Full 118.7 MiB / 傾き: Fast ~6.1, Full ~8.5 | ok（改善余地あり） |
| LSP 応答 | p95 200ms 以下 | completion/diagnostics ともクリア | ok |

## 次アクションの例
1. `CacheMode::Shared` でのメモリ増分を再計測し、スイープ戦略を検討する。
2. `LegacyToken` 変換の再確保削減（`Arc<str>` 共有など）で RSS を確認する。
3. インクリメンタル短縮率向上のため、差分解決パスのプロファイルを取得する。
