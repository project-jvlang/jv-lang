# Salsa Parser Architecture (Final)

本ドキュメントは Phase 9 Validation 時点の Salsa パイプラインの最終アーキテクチャを記録する。

## コンポーネント
- **Lexer (zero-copy)**: `jv_parser_salsa::lexer` が `jv_lexer` の結果に行・桁スパンを再構成し、`OwnedToken` を構築する。
- **Parser**: 再帰下降で `ParseEvent` を生成し、必要に応じて `CstBuilder` で CST を構築。
- **Lowering**: `lower` モジュールが `Statement`/`Expression` を生成し、診断を付与。
- **Pipeline**: `SalsaPipeline::execute_with_options` が Preprocess → Parse → Lower → Semantics を流し、`ParseOptions` で CST/Trivia を切り替える。
- **DSL 拡張**: `dsl::*` が LOG/concurrency/resource/test DSL をパイプラインに統合。
- **Normalization**: `support::normalize` が診断ソートキーとスパン許容差判定を提供し、旧パイプラインとの同等性比較を支える。

## データフロー
```
source
  └─ preprocess (jv_parser_preprocess)
       └─ tokens (legacy) ──► lexer compat ──► OwnedToken
             └─ parser (events) ──► CST(optional)
                   └─ lower ──► Program
                         └─ semantics ──► PipelineArtifacts
```

## テスト/ベンチ
- `tests/stdlib_comparison.rs`: 旧パイプラインとの診断・スパン同等性を検証。
- `benches/bench_main.rs`: Criterion ハーネスでフル/インクリメンタル/メモリ/LSP ベンチを実行。

## 今後の改善
- 診断コードの網羅的比較
- スパン許容差分をトークン数ベースに拡張
- ベンチ結果の自動収集（RESULTS.md への出力）
