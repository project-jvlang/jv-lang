# Benchmark Results (Phase 8)

このドキュメントは `cargo bench -p jv_parser_salsa --bench bench_main` を実行した結果を集約する。

## 実行環境
- マシン: ローカル開発機（詳細未記録）
- コマンド: `cargo bench -p jv_parser_salsa --bench bench_main`
- 条件: ウォームアップ 3s、計測 10s、サンプル数 20、コールド測定はプロセス再起動で取得

## メトリクス概要

### 8.2 フルパース
| シナリオ | Salsa Fast | Salsa Full | Rowan | 目標 |
| --- | --- | --- | --- | --- |
| stdlib 全体 | 59.39 ms | 69.63 ms | 269.89 ms | 成功 |
| synthetic 100 | 6.20 ms | n/a | 12.90 ms | 成功 |
| synthetic 2000 | n/a | 134.11 ms | 1.64 s | 成功 |

### 8.3 インクリメンタル
| シナリオ | 時間 | キャッシュヒット率 | 目標 |
| --- | --- | --- | --- |
| 1 行変更 (salsa_full single_line_edit) | 71.53 ms | n/a | 50% 以上短縮 |
| キャッシュヒット推定 (salsa_fast cache_hit_estimate) | 40.75 ns | n/a | 50% 以上短縮 |
| 変更なし再パース (rowan unchanged_reparse) | 154.93 ms | n/a | 50% 以上短縮 |

### 8.4 メモリ
| シナリオ | Salsa Fast RSS Δ(KiB) | Salsa Full RSS Δ(KiB) | 目標 |
| --- | --- | --- | --- |
| synthetic-2000 | 13,852 KiB | 7,680 KiB | Rowan 比 70% 以下（Rowan=384 KiB → 基準 269 KiB） |

### 8.5 LSP
| シナリオ | Salsa Fast p95 (ms) | Salsa Full p95 (ms) | Rowan p95 (ms) | 目標 |
| --- | --- | --- | --- | --- |
| completion 500 行 | 29.92 ms | n/a | 134.93 ms | 200ms 以下 |
| diagnostics 500 行 | n/a | 37.94 ms | n/a | 200ms 以下 |

## Go / No-Go 判定
| 項目 | 目標 | 結果 | 判定 |
| --- | --- | --- | --- |
| フルパース性能 | stdlib/synthetic で目標達成 | Salsa 全項目が Rowan より高速（回帰解消） | _ok_ |
| インクリメンタル速度 | 50% 以上短縮 | 71.5 ms（salsa_full）/40.8 ns（salsa_fast） vs Rowan 154.9 ms（unchanged）で約46%短縮 | _at risk_ |
| メモリ | Rowan 比 70% 以下 | 13,852 / 7,680 KiB（Rowan=384 KiB → 基準269 KiB未満必要、未達） | _ng_ |
| LSP 応答 | p95 200ms 以下 | completion/diagnostics とも 200ms 未満（計測値） | _ok_ |

## 推奨事項
- RSS などメモリ指標を実測し、表を更新すること（heaptrack 等でピーク取得）。
- フルパース回帰（stdlib/synthetic_100）を確認し、原因調査・最適化を行うこと。
- インクリメンタルのベースライン（Rowan/前回値）を取得し、短縮率を算出すること。

## メモリ回帰の原因と対応
- 基準測定: `cargo run -p jv_parser_salsa --example rss_probe --release`（synthetic-2000）。Rowan の実測 RSS 増分は 384 KiB（旧記録 512 KiB は仮値だった）。これを基準に 70% 目標を 269 KiB に更新。
- 改善済み: `OwnedToken` の `lexeme` を `Arc<str>` 化し、パース後のトークン列・イベント列・診断を即座に `mem::take` で解放。Salsa Fast 14,320→13,852 KiB、Salsa Full 7,936→7,680 KiB と 3〜4% 削減。
- 依然のボトルネック: salsa DB がプリプロセス/パース/ローワリングのキャッシュを保持し続けるため、2000 行規模でトークン列・イベント列・HIR が DB 内に常駐する。さらに `LegacyToken` への文字列コピー（String 再ヒープ化）も残存し、Rowan の stateless パイプラインとの差が大きい。
- 次アクション案: (1) パイプライン終了時に salsa runtime のキャッシュを明示的に掃除する（DB を都度再生成 or GC/Sweep 呼び出し）; (2) `LegacyToken` も `Arc<str>` 共有 or ソーススライス参照にし、String への再ヒープ化を回避; (3) `ParseOptions` に応じて不要な CST/Trivia/トークンを早期破棄するモードを設け、メモリ常駐量を最小化する。

## Rowan 比で低性能だった項目と原因考察（修正後の現状）
- フルパース: ベンチを対称化（Rowan stdlib/synthetic_2000 追加、各イテレーションで PipelineSwitcher を新規生成）し、過去の回帰扱いは解消。現状 Salsa が優位で No-Go 要因なし。
- メモリ: ベンチを毎回新規パイプラインで実行し、Rowan も計測。ただし Salsa の RSS 増分（fast 18,220 KiB / full 5,888 KiB）が Rowan 384 KiB を大幅に超過し、70% 基準も未達。原因は DB + lexeme ヒープ化 + CST/Trivia 保持の構造的コスト。改善が必要。
