# Benchmark Results (Phase 8)

このドキュメントは `cargo bench -p jv_parser_salsa --bench bench_main` を実行した結果を集約するためのテンプレートである。

## 実行環境
- マシン: _fill_
- コマンド: `cargo bench -p jv_parser_salsa --bench bench_main`
- 条件: ウォームアップ 3s、計測 10s、サンプル数 20、コールド測定はプロセス再起動で取得

## メトリクス概要

### 8.2 フルパース
| シナリオ | Salsa Fast | Salsa Full | Rowan | 目標 |
| --- | --- | --- | --- | --- |
| stdlib 全体 | _tbd_ | _tbd_ | _tbd_ | 成功 |
| synthetic 100 | _tbd_ | _tbd_ | _tbd_ | 成功 |
| synthetic 2000 | _tbd_ | _tbd_ | _tbd_ | 成功 |

### 8.3 インクリメンタル
| シナリオ | 時間 | キャッシュヒット率 | 目標 |
| --- | --- | --- | --- |
| 1 行変更 (synthetic-500) | _tbd_ | _tbd_ | 50% 以上短縮 |

### 8.4 メモリ
| シナリオ | Salsa Fast RSS Δ(KiB) | Salsa Full RSS Δ(KiB) | 目標 |
| --- | --- | --- | --- |
| synthetic-2000 | _tbd_ | _tbd_ | Rowan 比 70% 以下 |

### 8.5 LSP
| シナリオ | Salsa Fast p95 (ms) | Salsa Full p95 (ms) | Rowan p95 (ms) | 目標 |
| --- | --- | --- | --- | --- |
| completion 500 行 | _tbd_ | _tbd_ | _tbd_ | 200ms 以下 |
| diagnostics 500 行 | _tbd_ | _tbd_ | _tbd_ | 200ms 以下 |

## Go / No-Go 判定
| 項目 | 目標 | 結果 | 判定 |
| --- | --- | --- | --- |
| フルパース性能 | stdlib/synthetic で目標達成 | _tbd_ | _pending_ |
| インクリメンタル速度 | 50% 以上短縮 | _tbd_ | _pending_ |
| メモリ | Rowan 比 70% 以下 | _tbd_ | _pending_ |
| LSP 応答 | p95 200ms 以下 | _tbd_ | _pending_ |

## 推奨事項
- 実測値を上記表に反映すること。
- 目標未達項目については最適化案を列挙すること（パーサ設定、arena 削減、キャッシュキー見直しなど）。
- heaptrack によるピーク RSS とホットアロケーションのスクリーンショットを添付すること。
