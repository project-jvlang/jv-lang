# raw-type-defensive-strategy: Raw型防御コード挿入方針メモ

## 背景

java-generics-interop仕様では、Raw型が流入した場合に**警告を発行しつつ防御的なJavaコードを生成する**ことが必須となっている。Phase2では以下を満たす必要がある:

- Raw型継続ディレクティブ（`RawTypeDirective`）をIR/推論層で解釈し、`RawTypeEvent`として`jv_codegen_java`へ伝搬する。
- デフォルトポリシー（`DefaultPolicy`）では「warn + defensive code」を必ず実施する。
- 継続指定（`AllowWithComment`）時はコメント併記のみで、コード変形は最小限に抑える。
- 生成コードは**Java標準APIのみ**（`java.util.Objects` や安全なキャスト）で実装し、追加ランタイムやユーティリティクラスは導入しない。

本メモは、Task 5-1 としてコード生成層における防御コード挿入パターンと実装方針を整理する。

## 入力シグナルとデータフロー

| レイヤー | データ構造 | 内容 |
|----------|------------|------|
| AST | `RawTypeDirective` | コメントから抽出した継続指示と所有シンボル名 |
| Mapper/IR | `GenericSignature::raw_directives` | メソッド/型宣言単位でRaw指示を保持 |
| 推論層 | `RawTypeEvent { symbol, span, mitigation, continuation }` | `RawTypeAnalyzer`が導出し、`TypeArgumentSolution`および`SolverTelemetry`に記録 |
| コード生成 | (新規) `RawTypeUsage` equivalent | `RawTypeEvent`を受け取り、フィールド/変数/戻り値など該当ノードへマッピング |

`RawTypeMitigation`は現時点で `NullCheck` / `DefensiveCast` / `CommentOnly` の3種類。`AllowWithComment`は `CommentOnly` に、その他は `NullCheck`（将来的に解析結果で `DefensiveCast` に昇格）として扱う。

## 挿入ポイントの整理

| IRノード | Raw型が露出する典型パターン | 推奨防御 |
|----------|------------------------------|-----------|
| `IrStatement::FieldDeclaration` / `VariableDeclaration` | Raw型コレクションを受け取るフィールド初期化、ローカル変数代入 | `Objects.requireNonNull` で即時ガード。必要なら `@SuppressWarnings("unchecked")` を添える |
| `IrStatement::Return` | Raw型戻り値をジェネリック型として返す | `DefensiveCast`（必要条件を満たす場合のみ） or `Objects.requireNonNull`|
| `IrExpression::Assignment` / `MethodCall` | Raw型の結果をパラメータ化型に格納 | キャストまたはnullチェックをラップする補助関数を適用 |
| `IrExpression::ObjectCreation` | Raw型引数をコンストラクタへ渡す | 引数ごとに`requireNonNull`ガード、コメント併記 |

※ Phase2では解析の範囲を「初期化・代入・戻り値」に限定し、呼び出し引数への自動挿入は`DefensiveCast`実装時（Task 5-2）で段階的に広げる。これは既存テストの影響範囲を抑え、`generate_expression`の複雑度増加を回避するため。

## 防御パターン詳細

### 1. Nullガード (`RawTypeMitigation::NullCheck`)

- **目的**: Raw型経由で `null` が潜り込むリスクを抑制する。
- **生成形**: `Objects.requireNonNull(<expr>, "JV: raw type guard")`
- **実装ポイント**:
  - `JavaCodeGenerator::add_import("java.util.Objects")` を通じて必要な import を追加。
  - `generate_expression` / `generate_statement` から呼び出せる `wrap_with_null_guard(expr: String) -> String` ユーティリティを追加。
  - コメント併記が必要な場合は `render_raw_type_comment` で生成した文字列を同じ行に添付。

### 2. 防御キャスト (`RawTypeMitigation::DefensiveCast`)

- **目的**: Raw型を安全にパラメータ化型へキャストする。`ClassCastException` のリスクを許容しつつ、IDEが意図を把握できるよう根拠コメントを出力。
- **生成形**:
  ```java
  @SuppressWarnings("unchecked")
  final var typed = (List<String>) raw; // jv:raw-default <owner>
  ```
- **実装ポイント**:
  - 当面は `RawTypeEvent` にキャスト先ヒントを搭載していないため、Phase2では`NullCheck`パターンが基本。`DefensiveCast`が要求されるケース（型推論結果が存在する場合）はTask 5-2で型情報を拡張してから実装する。
  - 設計としては `RawTypeEvent` に `target_type: Option<JavaType>` を追加し、`generate_type` で再利用する。

### 3. コメントのみ (`RawTypeMitigation::CommentOnly`)

- **目的**: 開発者が意図的にRaw型を許可したケースを可視化する。
- **生成形**: 対象行末に `// jv:raw-allow <owner>` を添付。既にコメントが存在する場合は `append_inline_comment` を使用して結合。
- **副作用**: 防御コードは挿入しない。nullチェックもスキップ。

## 実装タスクリスト（Task 5-2に引き継ぐ）

1. `TypeArgumentSolution` から `RawTypeEvent` を IR ノードに割り当てる中間構造（例: `RawTypeUsageMap`) を `jv_mapper` → `jv_codegen_java` 経路に追加。
2. `JavaCodeGenerator` に以下のユーティリティを実装:
   - `fn wrap_with_null_guard(&mut self, expr: String, note: &RawTypeEvent) -> String`
   - `fn push_raw_comment(&mut self, builder: &mut JavaSourceBuilder, event: &RawTypeEvent)`
3. フィールド/ローカル変数/戻り値生成箇所で、関連する `RawTypeEvent` を確認して上記ユーティリティを適用。
4. `jv_codegen_java/tests/generics.rs` に Raw 型シナリオ（`DefaultPolicy` と `AllowWithComment`）のゴールデンテストを追加。
5. `make test-lowmem-crate CRATE=jv_codegen_java` でスナップショット更新を確認。

## テストと検証

- **単体テスト**: `raw_type_comment` の生成、`wrap_with_null_guard` の動作をユニットテスト化。
- **統合テスト**: Raw型を含む `.jv` サンプルを `jv_codegen_java/tests/generics.rs` に追加し、
  - デフォルトポリシー: `Objects.requireNonNull` が出力されること
  - 継続許可: コメントのみであること
- **CLIテスト**: 既存スナップショットに `RAW_TYPE` 警告が表示され、防御コードが含まれているか確認（Task 6で拡張）。

## リスクとフォローアップ

- `Objects.requireNonNull` による実行時例外はRaw型問題の早期発見が目的。既存コードがnull許容な場合は`AllowWithComment`で明示的に切り替える運用をドキュメントに追記する必要がある。
- キャストパターン実装前に `DefensiveCast` が選ばれた場合は一時的に `NullCheck` にフォールバックし、Telemetryへ `severity_todo=true` を維持する。
- 生成コードの可読性を損なわないよう、1行が長くなる場合は変数導入（`final var ...`）を検討する（Task 5-2で実装可否を判断）。

以上により、Phase2で要求される「warn + defensive code」の方針を明文化できた。次ステップではこの方針に沿って `jv_codegen_java` を拡張し、Raw型継続指示の反映とテスト整備を行う。
