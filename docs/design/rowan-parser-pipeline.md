# rowan-parser-pipeline: Rowanフロントエンド技術仕様

## 概要

本書は `rowan-parser-integration` 仕様 Task 3.2 の成果物として、Rowan パイプラインを前提とした jv 言語パーサーの技術的な構成を整理する。`STATEMENT_MAPPING.md` に蓄積されたステートメント写像メモ、`.project-todolist/作業指示-20251016.md` の開発指針、`docs/design/java-aware-type-inference.md` の型推論設計を統合し、実装チームとドキュメント利用者が共通認識を持てるようにする。

### スコープ
- 対象モジュール: `jv_lexer`, `jv_parser_preprocess`, `jv_parser_rowan`, `jv_parser_frontend`, `jv_parser_semantics`, `jv_type_inference_java`
- 対象 API: `jv_parser_frontend::ParserPipeline`, `jv_parser_rowan::frontend::RowanPipeline`, `FrontendOutput`, `PipelineArtifacts`
- 非対象: レガシー Chumsky ベースの `jv_parser` / `jv_parser_syntax*` クレート（ワークスペース外の参考実装としてのみ存続）

## パイプライン構造

| ステージ | 実装 | 主な役割 | 出力 |
|----------|------|----------|------|
| 1. 字句解析 | `jv_lexer` | Java 互換のトークン列へ正規化。改行レイアウトやトリビア情報を保持。 | `Vec<Token>` |
| 2. プリプロセス | `jv_parser_preprocess::{ProcessingPipeline, PreprocessStage}` | JSON ブロック、レイアウトカンマ、コメント挙動を整理。`StageStatus` でエラーを伝播。 | 整形済み `Vec<Token>` と `PreprocessDiagnostic` |
| 3. Rowan 解析 | `jv_parser_rowan::parser` | トークン列を `rowan::GreenNode` と診断へ変換。イベントドリブンでエラー復元可能。 | `ParseOutput` (`GreenTree`, `ParserDiagnostic`) |
| 4. ローワリング | `jv_parser_rowan::lowering` | `GreenTree` → `jv_ast::Statement` へ写像。`STATEMENT_MAPPING.md` の契約を担保し、`LoweringDiagnostic` を生成。 | `LoweringResult` (`Vec<Statement>`, `Vec<LoweringDiagnostic>`) |
| 5. セマンティクス | `jv_parser_semantics::run` | ステートメント列を検証。型推論入力やシンボル情報を構築。 | `PipelineArtifacts` (`SemanticArtifacts`, `FrontendDiagnostics`) |

各ステージは `jv_parser_frontend::ParserPipeline` によって合成され、`RowanPipeline::execute` が CLI / LSP / テストハーネスに対して `FrontendOutput` を返す。

## コンポーネント詳細

### 1. 字句解析 (`jv_lexer`)
- `TokenType` は Java 25 の演算子・キーワード・トリビアを網羅。
- `TokenTrivia` とメタデータ (`TokenMetadata::RegexLiteral` など) を通じてローワリング後も元ソースの情報を復元可能。

### 2. プリプロセス (`jv_parser_preprocess`)
- JSON 埋め込み・レイアウトカンマ・コメント統合の 3 段階パイプラインを `ProcessingPipeline::builder` で構築。
- `PreprocessResult` 内の `halted_stage` により CLI が中断地点を把握し、診断を `DiagnosticsFormatter` へ委譲できる。

### 3. Rowan 解析 (`jv_parser_rowan::parser`)
- `ParseBuilder` がイベント列 (`ParseEvent`) から `rowan::GreenNode` を生成。ノード圧縮や未バランスノードの防止を実装。
- `SyntaxKind` は `Root` / `StatementList` / `Expression` 等を定義し、`STATEMENT_MAPPING.md` の想定と一致する。
- 診断 (`ParserDiagnostic`) は `TokenSpan` を保持し、後段で `Span` に変換される。

### 4. ローワリング (`jv_parser_rowan::lowering`)
- `helpers.rs` が Rowan ノードと元トークンのバインディング (`TokenStore`) を提供。
- `support::spans` / `support::literals` は旧 `jv_parser_syntax_support` が担っていたスパン結合・正規表現リテラル生成を置き換え、Rowan クレート内で自給自足する。
- `statements.rs` が `lower_program` を公開し、`Statement` 列と `LoweringDiagnostic` を生成。詳細なノード→AST 対応は `STATEMENT_MAPPING.md` を参照。
- 型注釈は `lower_type_annotation_container` → `jv_type_inference_java::lower_type_annotation_from_tokens` を経由し、構造化された `TypeAnnotation` を生成する。

### 5. セマンティクス (`jv_parser_semantics`)
- `run` が AST・補助情報 (`PipelineArtifacts`) を受け取り、型検証・警告・補完候補の土台を構築。
- `FrontendDiagnostics` との統合は `jv_parser_frontend::compose_frontend_diagnostics` が担当。

## 診断の流れ

1. **プリプロセス診断**: `PreprocessDiagnostic` がステージ名とともに `FrontendDiagnostics` へ挿入。中断 (`StageStatus::Halted`) もここで判定。
2. **Rowan パーサ診断**: `ParserDiagnostic` を `token_span_to_span` で `Span` へ変換。新規 `support::spans` により `Span` 計算の依存を完結。
3. **ローワリング診断**: `LoweringDiagnosticSeverity` (Error/Warning) を `FrontendDiagnostics` へ再ラベル。Span はローワリング中に確定。
4. **セマンティクス診断**: 既存の `jv_parser_semantics::SemanticsDiagnostic` を `DiagnosticFormatter` 経由で統合。

すべての診断は `FrontendDiagnostics` にステージ名 (`rowan-parser`, `rowan-lowering`, `preprocess`, `semantics`) とともに列挙され、CLI/LSP 双方で表示互換性を維持する。

## 型注釈ローワリング

`jv_type_inference_java` は新たに手書きのトークンベースパーサ (`TypeAnnotationParser`) を実装し、以下を実現した。

- Chumsky 依存を排除しつつ、`Function` / `Generic` / `Nullable` / `Array` の型表現を再帰的に解析。
- エラー時は `ParseError` が `Span` とメッセージを保持し、`TypeLoweringErrorKind::Parse` としてフロントエンドへ伝播。
- `java-aware-type-inference` 設計 (プリミティブ/ボックス型の区別、将来の変換ルール) と互換性を維持。

## パイプライン成果物

`RowanPipeline::execute` は以下を包含する `FrontendOutput` を返す。

- `FrontendDiagnostics`: ステージ横断の診断一覧。
- `PipelineArtifacts`: ローワリング済み `Program`, セマンティクス出力、`SemanticArtifacts`。
- `halted_stage`: プリプロセス/セマンティクスの中断情報。Rowan パーサとローワリングの結果は保持され、部分的な IDE 利用も可能。

## テストと検証

- `rowan_spec_harness` (bin) が既存 TOML フィクスチャ (`tests/parser_rowan_specs/**/*.toml`) を実行し、AST/診断のリグレッションを検知。
- `lowering_cases.rs` がシンセティックな構文木を用いた単体テストを提供し、ステートメントごとのローワリング結果を固定。
- `cargo check` / `cargo test` に加え、`.project-todolist/作業指示-20251016.md` が定義する性能ベンチで Rowan パイプラインを監視。

## レガシーパイプラインからの移行状況

- `jv/Cargo.toml` から `jv_parser` および `jv_parser_syntax*` クレートを除外。`chumsky` もワークスペース依存から排除済み。
- 旧クレートは `crates/jv_parser*` 配下にソースを残しつつ、ビルド対象外の参考実装として扱う。ドキュメントからは Rowan パイプラインへの導線のみを提示する。
- CLI/LSP/テストハーネスは `ParserPipeline` 経由で `RowanPipeline` を利用しており、Chumsky ベースの API へはフォールバックしない。

## 参考資料

- `jv/crates/jv_parser_rowan/src/lowering/STATEMENT_MAPPING.md`
- `docs/design/java-aware-type-inference.md`
- `.project-todolist/作業指示-20251016.md`
- `jv_parser_frontend::frontend` 実装 (`src/frontend/{mod.rs,pipeline.rs,diagnostics.rs}`)
- `jv_type_inference_java::TypeAnnotationParser` 実装 (`src/lib.rs`)

本仕様書は Rowan パイプラインを前提とした開発・保守の基準であり、追加ステートメントや診断が導入される際は本書と `STATEMENT_MAPPING.md` を同時に更新すること。
