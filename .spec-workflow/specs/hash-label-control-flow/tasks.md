# タスク分解

## 0. 着手準備
- [x] `.spec-workflow/specs/hash-label-control-flow/requirements.md`・`design.md` のレビュー依頼を作成し、承認ワークフローに登録する。
- [x] 既存のハッシュコメント挙動に依存するテスト（`jv_lexer` / `jv_parser_rowan`）を洗い出し、回帰チェック対象として記録する。

## 1. 字句解析 (`jv/crates/jv_lexer`)
- [x] `TokenType` / `TokenKind` へ `HashLabel` を追加し、`serde` 派生と LSP 連携箇所を更新する。
- [x] `char_scanner.rs` で `#` + 識別子を常に `HashLabel` としてトークン化するロジックを実装し、行頭空白付きコメント・非識別子ケースをコメント扱いに残す。
- [x] コメントとラベルの判定条件をドキュメント化し、単体テストで `#comment` / `#label for` / `val x # comment` など代表例を検証する。
- [x] コメント carry-over (`TokenTrivia`) が既存どおり維持されることを確認するテストを追加する。

## 2. 構文解析 (`jv/crates/jv_parser_rowan`)

### 2.1 シンタックス定義の更新
- [x] `syntax/kinds.rs` に `HashLabel` を追加し、`SyntaxKind` / `TokenKind` マッピングを更新する。
- [x] `Rowan` 文法定義へ `LabeledStatement`・`Break/Continue/Return` のラベルオプションを追加し、ラウンドトリップテストを通す。

### 2.2 ステートメントパーサの拡張
- [x] `parser::strategies::statement::parse_statement` に checkpoint 処理を追加し、`HashLabel` の後続トークンでラベル確定／コメントフォールバックを判定する。
- [x] `parse_labeled_statement` を実装し、`for` / `when` / ブロック / ブロックラムダの各分岐をカバーする。

### 2.3 制御構文のラベル対応
- [x] `parser::strategies::control` で `break` / `continue` / `return` が任意の `HashLabel` を受け取れるようにし、既存エラーメッセージを更新する。
- [x] `parser::strategies::expression` にフックを追加し、ブロックラムダ前の `HashLabel` を検出してラムダに紐づける。

### 2.4 テスト・フィクスチャ
- [x] `tests/parser_fixtures.rs` に単一ラベル、ネスト、フォールバック（コメント扱い）ケースを追加する。
- [x] `tests/lowering_cases.rs` にラベル付き構造が AST へ反映されることを確認するケースを追加する。

## 3. AST 更新 (`jv/crates/jv_ast`)
- [x] `ForInStatement`、`Expression::Block` / `When` / `Lambda` へ `label: Option<String>` を追加し、`serde` / JSON シリアライザを更新する。
- [x] `Statement::Break` / `Continue` / `Return` を `label: Option<String>` を含む構造体に更新し、既存コードの `match` 文を修正する。
- [x] AST 単体テスト・スナップショットを更新し、新フィールドが正しく（デ）シリアライズされることを確認する。

## 4. セマンティクス／型解析 (`jv_parser_semantics`, `jv_checker`)

### 4.1 インフラ整備
- [x] `SemanticsContext` にラベルスタックを実装し、push/pop 操作とスコープ終了処理を追加する。
- [x] ラベルスタックの単体テストを用意し、ネスト／シャドーイング／スコープ終了時の挙動を検証する。

### 4.2 検証ロジック実装
- [x] `jv_checker::binding::resolver` で `break` / `continue` / `return` のラベル参照を検証し、未定義・種別不一致・重複宣言エラーを新診断 ID で報告する。
- [x] Null safety・pattern・regex など制御フローを判定するモジュールの `match` を新しい構造体へ更新する。

### 4.3 診断とテスト
- [x] 新診断 ID のメッセージ（日本語・英語）を登録し、LSP/CLI 表示に反映する。
- [x] ラベル診断のユニットテスト（未定義、非ループ continue、非ラムダ return、シャドーイング許容）を作成する。

## 5. IR ローワリング (`jv/crates/jv_ir`)
- [x] `IrStatement::ForEach` / `For` / `Block` などに `label: Option<String>` フィールドを追加し、`Return` からラベル情報を除去する。
- [x] `transform::mod` で AST の `label` フィールドを IR へコピーし、`Break` / `Continue` のラベルを伝搬する。
- [x] `sequence_pipeline` / `naming` などラベル情報を扱う補助モジュールを更新する。
- [x] IR テスト（`jv_ir/src/tests.rs`）にラベル付きループ・break/continue ケースを追加する。

## 6. コード生成 (`jv/crates/jv_codegen_java`)
- [ ] `generator::statements` でラベル付き `for` / `block` 出力を実装し、`break` / `continue` に付属するラベル文字列から `#` を除去して Java 標準の `label:` / `break label;` を生成する。
- [ ] Java 21 フォールバックや既存スナップショットを更新し、ラベルが正しく出力されることを確認する。
- [ ] ラムダ内 `return #label` が通常の `return` に変換されることを確認するテストを追加する。

## 7. LSP / CLI
- [ ] `jv_lsp` のシンボルハイライト・補完へ `HashLabel` を登録し、診断出力が新ラベルエラーを表示できるようにする。
- [ ] LSP の統合テストでラベル警告・エラーが表示されること、CLI の E2E テストで新診断メッセージが確認できることを検証する。

## 8. ドキュメント整備
- [ ] 制御フロー関連ドキュメントへラベル構文を追記し、コメントとの判定ルールを説明する。
- [ ] `docs/stdlib/collections.md` などラムダ利用ドキュメントで `return #label` の使用例を更新する。
- [ ] `.project-todolist/phase2-checklist.md` の該当項目に進捗更新メモを追加する。

## 9. テスト・回帰確認
- [ ] `jv_lexer` から `jv_codegen_java` までの主要クレートで新規テストを実行し、`cargo test --workspace` を通過させる。
- [ ] JSON コメントを含む既存サンプルが従来どおりコンパイルできることを CLI テストで確認する。
- [ ] ラベル付き制御フローを含む `.jv` ファイルを `jv build` でトランスパイルし、生成 Java を `javac` でビルド・実行するスモークテストを追加する。
