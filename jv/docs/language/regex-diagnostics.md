# 正規表現診断ガイド

jv では `/pattern/` リテラルと `[mode]/subject/pattern/[replacement]/[flags]` 形式の `RegexCommand` の両方を `RegexValidator` 経由で静的解析します。CLI と LSP は同じ診断テーブルを共有しており、`make test` のたびに `jv/tests/integration_tests.rs` のケースで回帰を確認します。

## 主な診断コード

| コード | 概要 | 典型的な修正 |
| --- | --- | --- |
| `JV5101` | 終端スラッシュ不足や括弧の未対応など構造エラー。 | `/` を閉じ、`()` / `[]` / `{}` の対応を合わせる。|
| `JV5102` | `\q` など Java `Pattern` で使用できないエスケープ。 | `\n` や `\t` など互換エスケープに置き換える。 |
| `JV5103` | Java ランタイムで非互換となる可能性のある構文。 | `Pattern.compile` で検証し、安全な表現に変換する。 |
| `JV5104` | 解析に 10ms 以上を要する複雑なパターン。 | パターンを分割し、指数爆発の可能性を減らす。 |

> **補足**: `RegexCommand` でも同じコードが発火します。`subject`、`replacement`、`flags` の情報は CLI と LSP の両方に転送されるため、どちらで修正しても行番号と列番号が一致します。

## CLI での表示例

```jv
val digits = /
  (\d+
```
※ 終端スラッシュを意図的に省略したサンプル

```bash
$ jv check sample.jv
[Error] JV5101: 正規表現リテラルの構造が不正です (L2C3-L4C1)
  戦略: Immediate
  ファイル: sample.jv
  詳細: 終端スラッシュと括弧の対応を確認してください。
```

- `jv check --emit-telemetry` を付けると、`pattern_cache_hits` などの統計とともに正規表現エラーが表示されます。
- `samples/regex/concise_command.jv` を `jv build` すると、`a/line/\s+/ " " /` や `s/compact/\n+User:/` などのコマンド式も同じ診断経路を通ることを確認できます。

## LSP での連携

- **Hover**: `/pattern/` 上にカーソルを置くと正規化済みパターンとフラグの概要を表示します。`RegexCommand` では subject / mode / flags がまとめて表示されます。
- **Completion**: 空のリテラル位置で `^
` などの定番テンプレートがサジェストされ、既存の `RegexAnalyzer` が抽出したパターンも候補に追加されます。
- **パフォーマンス保証**: `diagnostics_for_generic_source_are_empty` テストで 1 秒以内に診断を返すことを保証するため、正規表現が存在しないソースでは検証をスキップします（`jv/crates/jv_lsp/src/lib.rs` 参照）。

## 回帰テスト

- `cargo test -p jv_codegen_java raw_string_literals` : Raw文字列とテキストブロックの Java 出力を検証。
- `cargo test -p jv_cli --test cli_end_to_end -- regex` : CLI が `JV5102` を含むエラーを正しくレポートし、`RegexCommand` サンプルをビルドできることを確認。
- `pipeline_compiles_regex_command_sample` : `samples/regex/concise_command.jv` を `SequencePipeline` 経由で変換し、`.replaceAll` と `.split` が生成されることを保証。

CLI でも LSP でも同じフィードバックが得られるため、エディタで修正したコードをそのまま CI へ流せます。複雑な `RegexCommand` を追加する際は、上記の診断コードが正しく発火することをテストへ追加してください。
