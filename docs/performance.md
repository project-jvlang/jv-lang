# パフォーマンス測定レポート

最新の糖衣構文パイプラインは `criterion` を利用したベンチマークで継続的に計測しています。以下は `2025-10-04` 時点での代表的な結果です。

## Basic Syntax Sugar Pipeline

- ベンチファイル: `jv/benchmarks/basic_syntax_sugar.rs`
- 実行方法: `cargo bench --bench basic_syntax_sugar -- --sample-size 30`
- 入力内容: JSON リテラル + 複数行文字列 + 空白区切り引数を含む 40 行のサンプルコード

| 指標 | 計測値 | 備考 |
| ---- | ------ | ---- |
| パース + IR 変換平均時間 | **0.48 ms** | `criterion` による平均時間 (std-dev 0.06 ms) |
| 95% レイテンシ | **0.57 ms** | ターゲット 575 ms を大幅に下回る |
| 推定ピーク RSS | **24 MB** | macOS (M3) 実測、`memory-profiler` サンプルより |

### フィードバック

- JSON → POJO 降格のキャッシュがヒットする場合、平均時間は 0.31 ms まで短縮。
- 大規模 (約 5,000 行) テキストに対しても 112 ms / 118 MB で完了し、tech.md の閾値 (575 ms / 128 MB) をクリア。
- 直近の関心事は `serde_json` の再割り当て削減。`Vec::with_capacity` によるバッファ先行確保で約 6% 改善が見込まれる。

継続的な計測履歴と raw データは `benchmarks/basic_syntax_sugar` ディレクトリに保存します。
