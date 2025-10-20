# jv_parser 再構成提案

## 1. 背景と課題

jv_parser クレートは、chumsky を用いた Rust 実装で jv 言語の構文解析を担っている。現状は以下のような問題を抱える。

- `lib.rs` の `preprocess_tokens` が 200 行以上の単一ループで多機能化し、メンテナンス負荷が高い。
- `syntax/expressions.rs`・`syntax/statements.rs` が 1,000 行超の単一モジュールとして肥大化し、関数ごとの責務が曖昧。
- `syntax/support.rs` がトークンフィルタ、スパン操作、型注釈など異なる責務を一箇所で提供している。
- テスト (`tests/green_phase.rs` など) が巨大フィクスチャに依存し、リグレッション時の原因特定に時間がかかる。

これらによりビルド時間が延び、改修時のリスクが高まっている。将来的な DSL (pest/rhai/rule-rs) 導入は見送りとなったため、Rust 実装のまま責務を分離し、段階的なパイプライン構成を整えることが必要である。

## 2. 目標

1. **責務分離**: 前処理・構文解析・メタデータ付与・診断整形の境界を明確化する。
2. **再利用性向上**: LSP や IR が参照できる AST / 診断インターフェースを定義し、共有できるようにする。
3. **段階的実装**: 現在の chumsky ベースを保ちながら小さなステップでリファクタを進め、各段階でテスト可能とする。
4. **ビルド・テストの健全化**: 再コンパイル範囲とテスト単位を見直し、開発体験を改善する。

## 3. 再構成の全体像

```
Tokens (from jv_lexer)
  ├─ Stage 0: 前処理 (preprocess/)
  ├─ Stage 1: 構文解析 (syntax/)
  ├─ Stage 2: 付随情報付与 (semantics/)
  ├─ Stage 3: 診断整形 (diagnostics/)
  └─ Stage 4: 共有インターフェース (frontend/)
              ↳ jv_ir, jv_checker, jv_lsp 等が利用
```

各ステージは Rust 実装で構成し、イベントバスや DSL は導入しない。ステージ間は構造体による明確なデータ受け渡しを行う。

## 4. モジュール再編プラン

### 4.1 Stage 0: 前処理 (`preprocess/`)

- 既存 `preprocess_tokens` を分割し、以下のサブモジュールを新設する。
  - `json.rs`: JSON 文脈判定 (`detect_json_contexts` 等)。
  - `layout.rs`: レイアウト依存配列と `LayoutComma` 挿入処理。
  - `call.rs`: 呼び出し可能性・ suppress flag 判定。
  - `comments.rs`: コメント整理とメタデータ処理。
- `ProcessingPipeline` 構造体を導入し、ステージごとの処理を順に適用できるようにする。

### 4.2 Stage 1: 構文解析 (`syntax/`)

- `statements.rs` を以下のように分割する。
  - `statements/mod.rs`: ルートエントリポイント。
  - `statements/declarations.rs`: val/var/fun/class/data 等の宣言系。
  - `statements/control.rs`: when, if, return など制御構造。
  - `statements/signatures.rs`: 関数シグネチャ、ジェネリクス、アノテーション処理。
- `expressions.rs` は優先順位単位 (`postfix.rs`, `binary.rs`, `unary.rs` 等) にファイル分割し、組み立て関数は `mod.rs` にまとめる。
- `support.rs` を `tokens.rs`, `spans.rs`, `types.rs` 等に再構成し必要な部分のみ import する。

### 4.3 Stage 2: 付随情報付与 (`semantics/`)

- PrimitiveReturnMetadata 等の付与やアノテーション加工をここに集約し、構文解析段階と切り離す。
- 既存メタデータ処理 (`collect_raw_directives_from_token` など) を移動させ、テストしやすくする。

### 4.4 Stage 3: 診断整形 (`diagnostics/`)

- `ParseError::Syntax` などのエラー整形を分離し、診断メッセージフォーマットを一箇所に集約する。
- LSP や CLI で共通利用できる診断データ構造を提供する。

### 4.5 Stage 4: 共有インターフェース (`frontend/`)

- AST と診断のビューを定義し、`jv_ir` や `jv_lsp` から参照可能とする。
- 現在 `Parser::parse` が返す `Program` をラップし、付随情報も含めて返すための API を整理する。

## 5. リファクタ実施ステップ

### フェーズ1: 前処理の独立

1. `preprocess/` モジュールを追加し、既存 `preprocess_tokens` のロジックをファイル単位に切り出す。
2. 新モジュールをテストするために、入力トークンと期待トークンのフィクスチャを追加。
3. `Parser::parse` は `preprocess::run(tokens)` を呼び出すだけに簡素化。

### フェーズ2: 構文モジュール分割

1. `statements.rs` を `declarations.rs` と `control.rs` に分割し、テストを更新。
2. `signatures.rs` を抽出し、関数シグネチャ処理を専用モジュールに移動。
3. `support.rs` を複数のユーティリティモジュールに分解。

### フェーズ3: 付随情報・診断の再構成

1. `semantics/` モジュールを追加し、メタデータ付与処理を移動。
2. `diagnostics/` モジュールでエラー整形ロジックを統一。
3. テストをステージ単位に追加し、既存の受け入れテストを整理 (大規模フィクスチャから機能別ファイルへ分割)。

### フェーズ4: 共有インターフェース整備

1. AST の読み取り専用ビューを定義する `frontend/` モジュールを追加。
2. `Parser::parse` の戻り値を `FrontendOutput` (AST + 診断 + 付随情報) へ拡張。
3. LSP・IR など消費側での利用方法を確認し、必要なら調整タスクを別途設定。

各フェーズは独立して完結できるようにし、段階ごとに `cargo test -p jv_parser` が通ることを確認する。

## 6. テスト・運用指針

- **ステージ単位テスト**: 各モジュールごとに入力と出力のフィクスチャを用意し、局所的な回帰を検知できるようにする。
- **受け入れテストの再構成**: `green_phase.rs` を機能別ファイルに分割し、最小限の AST 断片で検証できるようにする。
- **開発ドキュメント更新**: 本メモをベースに、実装着手時には `.spec-workflow` の設計/タスク文書へ反映する。

## 7. 想定される効果

- モジュールごとのコンパイル時間が短縮され、開発時のフィードバックループが改善される。
- 前処理や診断など、テストしづらかった部分を個別に検証できるようになる。
- LSP・IR・チェッカーとの連携点が明確化し、将来的な機能追加に備えた構造になる。

## 8. 未決事項・フォローアップ

- AST 共有インターフェースの具体的な形 (既存 `jv_ast` との境界) は、フェーズ4 実施時に詳細設計が必要。
- LSP 側の利用状況を調査し、再構成により必要となる更新作業の洗い出しを行う。
- 前処理ステージでの再処理 (例: JSON 判定の再評価) が必要な場合の API を検討し、実装計画に組み込む。

---

本提案は、現行の Rust 実装を保ちつつ `jv_parser` を段階的に再構成するための設計メモである。実装に着手する際は、各フェーズを個別のタスクとして分解し、仕様書 (requirements/design/tasks) に反映すること。
