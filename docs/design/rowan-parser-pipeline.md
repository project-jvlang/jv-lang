# Rowan Parser Pipeline 技術仕様

## 概要

本書は jv 言語パーサーの Rowan ベースパイプラインアーキテクチャを定義する。エラー復元可能な構文解析、診断情報の統合、型注釈のローワリング機構を含む完全なフロントエンド実装を記述する。

### スコープ
- **対象モジュール**: `jv_lexer`, `jv_parser_preprocess`, `jv_parser_rowan`, `jv_parser_frontend`, `jv_parser_semantics`, `jv_type_inference_java`
- **対象 API**: `jv_parser_frontend::ParserPipeline`, `jv_parser_rowan::frontend::RowanPipeline`, `FrontendOutput`, `PipelineArtifacts`
- **アーキテクチャ**: Rowan (`rowan::GreenNode`) ベースのロスレス構文木、イベントドリブン解析

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
- `SyntaxKind` は `Root` / `StatementList` / `Expression` 等を定義し、jv 言語の構文要素を網羅。
- 診断 (`ParserDiagnostic`) は `TokenSpan` を保持し、後段で `Span` に変換される。

### 4. ローワリング (`jv_parser_rowan::lowering`)
- `helpers.rs` が Rowan ノードと元トークンのバインディング (`TokenStore`) を提供。
- `support::spans` / `support::literals` は旧 `jv_parser_syntax_support` が担っていたスパン結合・正規表現リテラル生成を置き換え、Rowan クレート内で自給自足する。
- `statements.rs` が `lower_program` を公開し、`Statement` 列と `LoweringDiagnostic` を生成。GreenNode から AST への写像ロジックを実装。
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

### テストハーネス
- **`rowan_spec_harness`**: TOML フィクスチャベーステスト (`tests/parser_rowan_specs/**/*.toml`)
  - 構文解析の正確性、AST 構造、診断メッセージのリグレッション検証
- **`lowering_cases.rs`**: ユニットテスト
  - ステートメント単位のローワリング結果を検証
  - シンセティックな構文木からの変換テスト

### 検証方法
```bash
# パイプライン全体のテスト実行
cargo test

# 型システムの静的検証
cargo check

# 個別クレートのテスト
cargo test -p jv_parser_rowan
cargo test -p jv_parser_frontend
```

## アーキテクチャ原則

1. **ロスレス構文木**: すべてのトークン、トリビア、メタデータを保持し、元ソースを完全に復元可能
2. **エラー復元**: 構文エラー時も部分的な AST を構築し、IDE 機能を提供
3. **ステージ分離**: 各パイプラインステージは独立した診断を生成し、中断点を明確化
4. **型注釈の統合**: Java 型システムを意識した型注釈ローワリングを `jv_type_inference_java` で実現

## 参考実装

### 主要モジュール
- `jv_parser_frontend::frontend` ([src/frontend/](../../jv/crates/jv_parser_frontend/src/frontend/))
  - `mod.rs`: パイプライン統合
  - `pipeline.rs`: `ParserPipeline` / `RowanPipeline` 実装
  - `diagnostics.rs`: 診断統合ロジック

- `jv_parser_rowan` ([crates/jv_parser_rowan/](../../jv/crates/jv_parser_rowan/))
  - `src/parser/`: イベントドリブンパーサ
  - `src/lowering/`: GreenNode → AST 変換

- `jv_type_inference_java` ([crates/jv_type_inference_java/](../../jv/crates/jv_type_inference_java/))
  - `src/lib.rs`: 型注釈パーサ実装

### 関連設計文書
- [java-aware-type-inference.md](java-aware-type-inference.md): 型推論システム設計

## 拡張ガイドライン

新規ステートメントや型注釈構文を追加する際は、以下を同時更新すること:

1. **`jv_parser_rowan::parser`**: `SyntaxKind` 定義とパースロジック
2. **`jv_parser_rowan::lowering`**: ローワリング実装とテストケース
3. **`rowan_spec_harness`**: 構文解析とローワリングの検証テスト
4. **本仕様書**: パイプライン構造への影響を反映
