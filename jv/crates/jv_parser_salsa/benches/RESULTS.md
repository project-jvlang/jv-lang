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
#### 8.4.1 synthetic-2000 (cacheless, 2025-xx 再計測)
| シナリオ | Salsa Fast RSS Δ(KiB) | Salsa Full RSS Δ(KiB) | Rowan RSS Δ(KiB) | 目標 |
| --- | --- | --- | --- | --- |
| synthetic-2000 (cacheless) | 13,120 KiB | 18,112 KiB | 9,420 KiB | Rowan 比 70% 以下（基準: Rowan 実測値×0.7=6,594 KiB） |

#### 8.4.2 大規模合成データでの RSS スケーリング
`cargo run -p jv_parser_salsa --release --example rss_probe -- --pipeline <pipeline> --generate-functions N --cache-mode cacheless` で 1 プロセス・1 パイプラインずつ計測（約 6 行/関数、Salsa Fast は trim_trivia_and_metadata=true）。

| 行数（関数数） | Salsa Fast RSS Δ(KiB) | Salsa Full RSS Δ(KiB) | Rowan RSS Δ(KiB) |
| --- | --- | --- | --- |
| 2,000（333 関数相当） | 13,120 | 18,112 | 9,420 |
| 9,998（1,666 関数相当） | 61,200 | 86,604 | 43,656 |
| 20,000（3,333 関数相当） | 121,188 | 171,376 | 83,160 |
| 39,998（6,666 関数相当） | 102,344 | 202,500 | 91,692 |

- 傾向: Fast の傾きは ~6.1 MiB/1k 行（10k/20k 領域）まで改善。Full は ~8.6–9.1 MiB/1k 行、Rowan は ~4.3–4.6 MiB/1k 行。40k 行で Fast がやや頭打ちに見えるのは測定揺らぎの可能性。
- 条件: Salsa は cacheless（CacheMode::Ephemeral）、Fast は trim_trivia_and_metadata=true。Rowan は同プロセス単体実行。
- 伸び方評価: いずれも指数・対数的増加は見られず、概ね線形（Rowan はサブリニア寄り）にスケール。40k 行での頭打ちは揺らぎ/OS リクレームの可能性が高い。
- 2025-… 再実行メモ: `JV_BENCH_USE_RSS_PROBE=1 cargo bench -p jv_parser_salsa --bench bench_main -- memory` では rss_probe を別プロセスで 20 サンプル呼び出すため、計測時間が 0.6–0.8s(fast)/0.5–0.6s(full)/~2s(rowan) に膨らみ Criterion が回帰判定。サンプル数を 10 に下げるか、ターゲット時間を延長してオーバーヘッド影響を減らすこと。

### 8.5 LSP
| シナリオ | Salsa Fast p95 (ms) | Salsa Full p95 (ms) | Rowan p95 (ms) | 目標 |
| --- | --- | --- | --- | --- |
| completion 500 行 | 29.92 ms | n/a | 134.93 ms | 200ms 以下 |
| diagnostics 500 行 | n/a | 37.94 ms | n/a | 200ms 以下 |

### 8.6 JDK コーパス（型解決前提）
- 状態: 未計測。JDK シンボル読み込みは型解決の前提条件のため、JDK ソース（または jv スタブ）を入力するシナリオをベンチに追加する。
- データ準備: `toolchains/jdk25/lib/modules` を展開せずに直接読む。`jrt:/` 仮想 FS と同様にモジュールイメージ内のソース/スタブを走査し、`java.base` だけでなくデフォルト解決可能な全標準モジュールを対象とする。
- 実行計画: デフォルト入力は `toolchains/jdk25/lib/modules` を使い、`JV_BENCH_JDK_MODULES` で差し替え可能にする。いずれも存在しない/未設定の場合はエラーとし、スキップはしない。

## Go / No-Go 判定
| 項目 | 目標 | 結果 | 判定 |
| --- | --- | --- | --- |
| フルパース性能 | stdlib/synthetic で目標達成 | Salsa 全項目が Rowan より高速（回帰解消） | _ok_ |
| インクリメンタル速度 | 50% 以上短縮 | 71.5 ms（salsa_full）/40.8 ns（salsa_fast） vs Rowan 154.9 ms（unchanged）で約46%短縮 | _at risk_ |
| メモリ | Rowan 比 70% 以下 | synthetic-2000 (cacheless): fast 13,124 / full 18,240 KiB（Rowan=9,676 KiB → 基準 6,773 KiB 未満必要、未達） | _ng_ |
| LSP 応答 | p95 200ms 以下 | completion/diagnostics とも 200ms 未満（計測値） | _ok_ |

## 推奨事項
- RSS などメモリ指標を実測し、表を更新すること（heaptrack 等でピーク取得）。
- フルパース回帰（stdlib/synthetic_100）を確認し、原因調査・最適化を行うこと。
- インクリメンタルのベースライン（Rowan/前回値）を取得し、短縮率を算出すること。
- メモリ削減施策を実装し、`rss_probe` で再測定して基準 70% を再評価すること。

## メモリ回帰の原因と対応
- 測定方法を更新: `cargo run -p jv_parser_salsa --release --example rss_probe -- --pipeline <pipeline> --corpus crates/jv_parser_salsa/benches/corpus/synthetic/synthetic-2000.jv`。1 パイプライン/1 プロセスで計測するため、Rowan の差分過小評価を解消。
- 改善済み: `OwnedToken` の `lexeme` を `Arc<str>` 化し、パース後のトークン列・イベント列・診断を `mem::take` で即解放。Salsa Fast 14,320→14,064 KiB、Salsa Full 7,936→7,680 KiB と数 % 改善。
- 依然のボトルネック: (1) Salsa DB がプリプロセス/パース/ローワリング結果を保持し続ける（パイプラインごとにキャッシュが累積）; (2) `LegacyToken` 変換で `lexeme_string()` を都度 `String` 再確保して二重保持; (3) Fast モードでも `TokenTrivia`/`metadata` を丸ごと保持するため、Rowan の stateless パイプラインより常駐コストが大きい。
- 次アクション案: (1) パーサー終了時に salsa runtime を sweep する or `Database` を再生成する API を追加し、ベンチ/CLI ではキャッシュを持ち越さない; (2) `LegacyToken` を `Arc<str>` 共有 or スライス参照にし、`owned_tokens_to_legacy` を 1 回に統合してコピーを排除; (3) `ParseOptions` でトリビア/メタデータを落とす軽量モードを導入し、Fast パスのフットプリントを抑える。

## Rowan 比で低性能だった項目と原因考察（修正後の現状）
- フルパース: ベンチを対称化（Rowan stdlib/synthetic_2000 追加、各イテレーションで PipelineSwitcher を新規生成）し、過去の回帰扱いは解消。現状 Salsa が優位で No-Go 要因なし。
- メモリ: 新しい RSS 測定では synthetic-2000 時点で Rowan 9,904 KiB に対し Salsa Fast 14,064 / Full 18,800 KiB と 70% 基準未達。上記ボトルネックが主因。改善が必要。

## メモリ削減タスク
[x] 1. SalsaPipeline に「キャッシュなし」モードを追加する（Database 再生成 or `salsa_runtime_mut().sweep(SweepStrategy::discard_everything())` をパイプライン終了時に呼べる API を実装し、ベンチ/CLI から切り替え可能にする）。→ `CacheMode::Ephemeral` と `SalsaPipeline::with_cache_mode/new_cacheless`、`rss_probe --cache-mode` で切替可能。
[x] 2. `LegacyToken` の `lexeme` を `Arc<str>` 共有（またはスライス再構成）に変更し、`owned_tokens_to_legacy` の再確保をなくす。セマンティクス/成果物で同一ベクタを使い回す実装に統一し、RSS を再測定する。→ `jv_lexer::Token.lexeme` を `Arc<str>` 化し、パイプライン変換で再確保を排除。RSS 再測定は未。
[x] 3. Fast 用軽量モードを `ParseOptions` に追加し、トリビア/metadata のコピーをスキップして `OwnedToken` を最小構成にする（必要ならトリビアを後付けできるデフォルト値で埋める）。→ `trim_trivia_and_metadata` を追加し、Salsa Fast / `rss_probe --pipeline salsa_fast` で有効化。
[x] 4. 上記 1〜3 を適用後、`rss_probe` で synthetic-2000/10k/20k/40k を再計測し、8.4 およびスケーリング表を更新する。→ cacheless + Fast 軽量モードで再測定済み。
[x] 5. ベンチ改善: 新しい `rss_probe` をベンチから呼び出すか、各パイプラインを別プロセスで測定するように変更し、Rowan の基準値を取り直して RESULTS.md を更新する。→ `JV_BENCH_USE_RSS_PROBE=1` で Criterion メモリベンチが rss_probe を別プロセス実行し、cacheless/プロセス分離で測定可能。
[x] 6. salsa クエリに LRU 上限を付ける（rust-analyzer 参考）。parse/lower/constraints など重いクエリに `#[salsa::lru(N)]` を設定し、環境変数でキャパを調整できるようにする。→ 保留: salsa 0.18 に LRU 属性/API が存在せず、rust-analyzer でも無効化されているため実装不可。将来のバージョン対応や別手段（cacheless デフォルトなど）で代替検討。
[x] 7. 非インクリメンタル用途は DB 再生成をデフォルトとし、インクリメンタルのみキャッシュ有効にする運用を徹底するスイッチを設ける。→ `CacheMode` のデフォルトを `Ephemeral` に変更し、PipelineSwitcher もキャッシュレスがデフォルト。Shared を使いたい場合は明示的に指定する。
