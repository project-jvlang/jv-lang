# Rowan Parser Pipeline 技術仕様

## 概要

本書は jv 言語パーサーの Rowan ベースパイプラインアーキテクチャを定義し、エラー復元可能な構文解析から型注釈ローワリング、診断統合までを包括的に説明する。

Rowan パイプラインは `jv_parser_rowan::frontend::RowanPipeline`（`../../jv/crates/jv_parser_rowan/src/frontend/pipeline.rs`）をエントリーポイントとし、CLI/LSP/テストハーネスからの要求に応じて字句解析→プリプロセス→Rowan 解析→ローワリング→セマンティクスを段階的に実行する。`RowanPipeline::execute_with_debug` は各ステージの成果物と診断を収集し、`ParserPipeline::execute` へ連結する。

### スコープ
- **対象モジュール**: `jv_lexer`, `jv_parser_preprocess`, `jv_parser_rowan`, `jv_parser_frontend`, `jv_parser_semantics`, `jv_type_inference_java`
- **対象 API**: `jv_parser_frontend::ParserPipeline`, `jv_parser_rowan::frontend::RowanPipeline`, `FrontendOutput`, `PipelineArtifacts`
- **アーキテクチャ**: Rowan (`rowan::GreenNode`) ベースのロスレス構文木、イベントドリブン解析

## パイプライン構造

| ステージ | 実装 | 主な役割 | 出力 |
|----------|------|----------|------|
| 1. 字句解析 | `jv_lexer` | Java 互換トークン列の生成。改行レイアウトやトリビア情報を保持。 | `Vec<Token>` |
| 2. プリプロセス | `jv_parser_preprocess::{ProcessingPipeline, PreprocessStage}` | JSON ブロック、レイアウトカンマ、コメント挙動を整理し `StageStatus` で中断点を管理。 | 整形済み `Vec<Token>` / `PreprocessDiagnostic` |
| 3. Rowan 解析 | `jv_parser_rowan::parser` | `ParserContext` と戦略レジストリが `ParseEvent` 列と `ParserDiagnostic` を生成。 | `ParseOutput` (`events`, `diagnostics`, `recovered`) |
| 4. ローワリング | `jv_parser_rowan::lowering` | `SyntaxNode` を `jv_ast::Statement` 群へ写像。`LoweringContext` がスパンとトークン対応を維持。 | `LoweringResult` (`Vec<Statement>`, `Vec<LoweringDiagnostic>`) |
| 5. セマンティクス | `jv_parser_semantics::run` | ステートメント列の検証と型推論入力を構築。 | `PipelineArtifacts` (`SemanticArtifacts`, `FrontendDiagnostics`) |

## Rowan パイプライン実装

1. **Lexer 起動**: `RowanPipeline::execute_with_debug` が `Lexer::new(source)` でトークン列を生成。字句レベルの失敗は即座に `ParseError` へ変換。
2. **プリプロセス**: `jv_parser_preprocess::run` がステージ化されたトランスフォームを実行し、整形済みトークン・診断・中断ステージを返す。`StageStatus::Halted` が存在する場合は `preprocess_halt_error` で `ParseError::Syntax` を生成。
3. **Rowan 解析**: `parser::parse(&tokens)` が `ParseOutput` を返す。`ParserContext` が `ParseEvent` 列を積み、`ParseBuilder::build_from_events` に備える。同時にエラーレベル診断を走査し、最初の `DiagnosticSeverity::Error` を `ParseError` に昇格。
4. **Green ツリー構築**: `ParseBuilder::build_from_events` がイベント列とトークン列を適用し、`rowan::GreenNode` を生成。`SyntaxNode::<JvLanguage>::new_root` で型付きノードへ昇格。
5. **ローワリング**: `lowering::lower_program(&syntax_node, &tokens)` がトップレベルステートメントを抽出し AST へ写像。`LoweringDiagnosticSeverity::Error` が存在すれば `ParseError::Syntax` を生成。
6. **Program 組み立て**: `assemble_program` が `Statement` 列から `jv_ast::Program` を構築。`package` の抽出、`import` の集約、先頭末尾スパンの統合を担当。
7. **セマンティクス実行**: パーサ/ローワリングで致命的エラーが無い場合のみ `jv_parser_semantics::run(&tokens, program)` を実行。`halted_stage` が返却された場合はセマンティクス由来の `ParseError` を生成。
8. **診断統合**: `frontend::diagnostics::compose_frontend_diagnostics` がパーサ・ローワリング・プリプロセス・セマンティクスの診断を `FrontendDiagnostics` へ整形。ステージ名は `rowan-parser` / `rowan-lowering` で固定。
9. **デバッグ成果物**: `RowanPipelineDebug` が `PipelineArtifacts`、生パーサ診断、ローワリング診断、回復有無、ステートメント AST、および `ParseError` を保持。テストと CLI の詳細分析に使用する。

`ParserPipeline::execute` は上記手順で `pipeline_error` が存在すれば `Err(ParseError)` を返し、無ければ `PipelineArtifacts` をそのまま返却する。

## jv_parser_rowan::parser の内部構造

### ParserContext

- `cursor` と `tokens`: 現在位置と解析対象トークン列を管理。
- `events`: `ParseEvent` を逐次蓄積し、後段の `ParseBuilder` が再生可能にする。
- `diagnostics`: `ParserDiagnostic` を収集。`DiagnosticSeverity` は Error/Warning の2値。
- `recovered`: `recover_statement` が呼ばれた際に `true`。IDE/テストから回復発生有無を可視化。
- `block_depth` と `ExpressionState` スタック: `when` ブロック、ラムダ、波括弧の入れ子を追跡し、同期ポイントや改行継続判定に利用。

### ステートメント戦略レジストリ

`parser::strategies::registry()`（`../../jv/crates/jv_parser_rowan/src/parser/strategies/mod.rs`）が静的配列を返し、`StatementStrategy` 実装を適用順に列挙する。順番は `package`→`import`→`val/var`→`fun`→`class`→制御構造→リソース管理→代入→式の順で、最初にマッチした戦略が `parse` を実行する。戦略は `matches` で軽量判定を行い、`parse` 実行後にトークンを消費しなかった場合は `recover_statement` を発火して整合性を確保する。

### 式解析と同期戦略

`ParserContext::parse_expression_until` が式解析の中心となる。特性:
- 並列に `()`/`{}`/`[]/<>` の深さカウンタを管理し、ステートメント境界を安全に検出。
- `ExpressionState` が `when` ブロック検出と `else` 同期判断を担う。`when` の `{` で専用ブロックを開始し、`}` でスタックを戻す。
- 改行後の継続判定は先読みで `.` `?.` `::` `->` が続くかを確認。継続しない場合はステートメント終端として扱う。
- `is_generic_argument_sequence` が `<...>` をジェネリクス引数と判定した場合のみ山括弧深さを増加し、`List` 構文と区別する。
- `SYNC_TOKENS`（`;`, 改行, `}`, キーワード群, `TokenKind::Eof` など）をトップレベルで検知すると式解析を打ち切り、回復可能性を高める。

### エラー回復とイベント生成

`recover_statement` はエラー時に `SyntaxKind::Error`（またはブロック内では `BlockError`）ノードを開始し、`SYNC_TOKENS` までトークンを消費する。消費できなかった場合でも終端トークン（`}`, `)`, `,` など）を安全側で 1 つ進める。

- `ParseEvent::Error` と `ParserDiagnostic` を同時に発行し、後段の診断組み立てとローワリングで活用。
- `parser::parse` は `ParseOutput` に `recovered = true` をセットし、IDE に部分構文ツリーを提供する。

### ParseBuilder と Green ツリー構築

`ParseEvent` は `StartNode` / `FinishNode` / `Token` / `Error` の4種類。`ParseBuilder::apply_events` はイベント列を順に適用し、ノードバランスが崩れた場合は安全側でスキップする。`Token` イベントは `token_index` から Lexer トークンを再取得して `TokenKind::to_syntax()` を用いて追加する。

- `open_nodes` カウンタにより未バランス終端を検知し、余計な `FinishNode` を無視する。
- トークンがルート外で出力された場合（EOF など）は構文木に含めず `debug_assert!` で検知。

## ローワリング層の詳細

### LoweringContext と TokenStore

`lowering::helpers::LoweringContext`（`../../jv/crates/jv_parser_rowan/src/lowering/helpers.rs`）は `TokenStore` を内部に持ち、Rowan トークンの `TextRange` と元の `jv_lexer::Token` を対応付ける。これにより:
- `tokens_for(node)` でノード配下のトークン列を取得。
- `span_for(node)` がトークン列から `Span` を合成し、AST ノードに正確な位置情報を付与。

### ステートメント抽出フロー

`lower_program` は `collect_statements_from_children` を通じて `SyntaxKind::StatementList` 配下を DFS で走査する。特性:
- `SyntaxKind::Error` / `BlockError` を検出すると即座に `LoweringDiagnosticSeverity::Error` を発行。
- `is_top_level_statement` が対象ノードをフィルタリングし、トップレベル以外は除外。
- `process_candidate` が `lower_single_statement` を呼び、成功時は `Vec<Statement>` に追加。失敗時は診断として記録。

### 型注釈と補助モジュール

- `collect_annotation_texts` / `first_identifier_text` が診断メッセージ用の補足情報を抽出。
- 型注釈は `lower_type_annotation_from_tokens` (`jv_type_inference_java`) を呼び出し、手書きトークンパーサに委譲。`TypeLoweringErrorKind::Parse` は `LoweringDiagnostic` に変換。
- JSON リテラルや正規表現リテラルは `support::literals::regex_literal_from_token` などで構築し、既存の `jv_ast` 表現へ橋渡しする。

### ローワリング診断

`LoweringDiagnostic` は `identifier` や `annotations` を保持し、IDE でのハイライトや CLI 表示に利用できる。Span が計算できない場合（トークン欠落など）は `None` を保持しつつ、後段で `Span::dummy()` が割り当てられる。

## サポートモジュールと文法定義

- `support::spans` は `span_from_token`/`merge_spans`/`expression_span` を提供し、診断や AST 生成で再利用するユーティリティ群。
- `support::literals` は正規表現・文字列補間・JSON コメントなどの構築ロジックを集約。
- `syntax::STATEMENT_GRAMMAR`（`../../jv/crates/jv_parser_rowan/src/syntax/mod.rs`）は将来的な DSL 自動生成を想定した文法スケルトンであり、ungrammar テストで整合性を検証。

## 診断の流れ

1. **プリプロセス診断**: `PreprocessDiagnostic` がステージ名付きで収集され、必要に応じて `ParseError` へ昇格。
2. **Rowan パーサ診断**: `ParserDiagnostic` は `token_span_to_span` で `Span` へ変換し、`ROWAN_PARSER_STAGE` 名で `ParserDiagnosticView` に格納。
3. **ローワリング診断**: `LoweringDiagnosticSeverity` を `DiagnosticSeverity` にマッピングし、Span が `None` の場合は後段で `Span::dummy()` を補完。
4. **セマンティクス診断**: `jv_parser_semantics::run` が返す `SemanticsDiagnostic` と `halted_stage` を `DiagnosticFormatter` へ渡し、最終的な `FrontendDiagnostics` を生成。

`FrontendDiagnostics` は元のプリプロセス診断とセマンティクス診断を保持し、CLI/LSP 双方で整形済み診断一覧を提供する。

## パイプライン成果物

- `PipelineArtifacts`: `Program`, 元トークン列、統合済み診断を保持。下流の型推論や IDE 機能の入力となる。
- `RowanPipelineDebug`: テストや CLI の詳細調査用に、`parser_diagnostics()` / `lowering_diagnostics()` / `statements()` / `pipeline_error()` などのアクセサを提供。大規模フィクスチャでのリグレッション分析に活用する。

## テストと検証

### テストハーネス
- **`rowan_spec_harness`**（`../../jv/crates/jv_parser_rowan/src/harness/rowan_spec.rs`）: TOML フィクスチャ (`tests/parser_rowan_specs/**/*.toml`) を走査し、正規化レポートを出力。
- **`frontend::tests`**: `RowanPipeline` の統合テストを Rust 単体テストとして実施。診断統合や `RowanPipelineDebug` の契約を担保。
- **`lowering_cases.rs`**: ステートメント単位のローワリング結果を検証し、AST 生成のリグレッションを防止。

### 検証コマンド
```bash
# パイプライン全体のテスト実行
cargo test

# 個別クレートのテスト
cargo test -p jv_parser_rowan
cargo test -p jv_parser_frontend

# 型システムの静的検証
cargo check
```

## アーキテクチャ原則

1. **ロスレス構文木**: すべてのトークン・トリビア・メタデータを保持し、元ソースを完全復元可能にする。
2. **エラー復元**: `recover_statement` により構文エラー時も部分的な AST を構築し、IDE 機能を継続。
3. **ステージ分離**: 各パイプラインステージは独立診断を生成し、中断点を明確化。
4. **型注釈統合**: Java 型システムを意識した型注釈ローワリングを `jv_type_inference_java` と協調して実現。

## 拡張ガイドライン

新規ステートメントや型注釈構文を追加する際は、以下を同時更新すること。

1. **`jv_parser_rowan::parser`**: `SyntaxKind` 定義、戦略レジストリ順序、該当戦略の `matches`/`parse` ロジック。
2. **`jv_parser_rowan::lowering`**: `lower_single_statement` と関連ヘルパー、ローワリング診断、トークン紐付けテスト。
3. **`rowan_spec_harness`**: 新構文に対応するフィクスチャ追加と検証ルール。
4. **本仕様書**: パイプライン構造や診断フローへの影響を反映し、参照コードのパスを更新する。
