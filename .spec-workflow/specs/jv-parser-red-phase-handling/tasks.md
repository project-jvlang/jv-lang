# Tasks Document

- [x] 1. パーサーモジュールの再編成と基盤整備
  - File: jv/crates/jv_parser/src/lib.rs
  - File: jv/crates/jv_parser/src/syntax/mod.rs (新規)
  - 内容: 既存の `statement_parser` / `expression_parser` を構文カテゴリ別のモジュールへ分割し、ヘルパー関数のシグネチャとエラーハンドリング方針を確立する。共通ユーティリティ（パラメータリスト解析、スパン結合など）を `syntax::support` として切り出し、今後のタスクで実装を追加できる骨組みを用意する。
  - Purpose: 後続タスクでの機能追加を安全に進められる parser の基盤を整える。
  - _Leverage: jv/crates/jv_parser/src/lib.rs, jv_ast::types, chumsky combinators_
  - _Requirements: 要件 1, 要件 2_
  - _Prompt: Role: Rust Parser Architect | Task: Implement the task for spec jv-parser-red-phase-handling, first run spec-workflow-guide to get the workflow guide then implement the task: Refactor jv_parser so parsing entry points delegate into new syntax modules, introduce shared helpers, and document the public functions for follow-on work. | Restrictions: Preserve existing public API surface, keep cargo fmt/clippy clean, avoid functional behavior changes beyond module extraction. | _Leverage: jv/crates/jv_parser/src/lib.rs, existing parser helper patterns, chumsky::Parser APIs | _Requirements: 要件 1, 要件 2 | Success: Build succeeds, unit tests still pass, new module boundaries and helper signatures cover all planned syntax categories, comments/docs explain how subsequent tasks extend the scaffolding. | Instructions: Before starting, mark this task as in-progress by changing the checkbox to [-] in .spec-workflow/specs/jv-parser-red-phase-handling/tasks.md; after completing all work and validations, switch it to [x]._

- [x] 2. ステートメント構文（fun/data/use/defer など）の完全実装
  - File: jv/crates/jv_parser/src/syntax/statements.rs
  - File: jv/crates/jv_parser/src/syntax/parameters.rs
  - 内容: 関数宣言、データクラス宣言、var/val、use/defer、拡張関数をフルサポートするパーサーを追加し、既存 AST へ正しくマッピングする。デフォルト引数、戻り値型、修飾子、リソースブロックなどの細部も網羅する。
  - Purpose: トップレベル構文がすべて成功パスに乗るようにして、レッドフェーズだったテストを緑化する準備を整える。
  - _Leverage: 込み入った宣言構文のテストケース (tests.rs)、jv_ast::Statement, TypeAnnotation_
  - _Requirements: 要件 1, 要件 2_
  - _Prompt: Role: Rust Compiler Engineer | Task: Implement the task for spec jv-parser-red-phase-handling, first run spec-workflow-guide to get the workflow guide then implement the task: Add full statement parsing coverage for functions, data classes, resource constructs, and variable declarations using the new syntax modules. | Restrictions: Follow chumsky combinator style established in the codebase, keep error reporting informative, ensure no regression for existing passing tests. | _Leverage: jv/crates/jv_parser/src/syntax/mod.rs scaffolding, jv_ast::Statement variants, tests from jv/crates/jv_parser/src/tests.rs | _Requirements: 要件 1, 要件 2 | Success: All statement-level tests compile and pass once converted to green, parser produces correct AST for representative inputs, new code covered by unit tests. | Instructions: Before starting, change this task’s checkbox to [-]; revert to [x] only after implementation and tests are complete._

- [x] 3. 式レイヤー（when/null安全/配列/ラムダ/文字列補間）の実装強化
  - File: jv/crates/jv_parser/src/syntax/expressions.rs
  - File: jv/crates/jv_parser/src/syntax/patterns.rs
  - 内容: when 式のアーム解析、null セーフティ演算子、配列リテラル、ラムダ式、文字列補間、演算子優先順位を完全対応させる。必要に応じて `Pattern` や `Expression` の補助型を拡張し、エラーケースを包括的にハンドリングする。
  - Purpose: 式ツリーのギャップを解消し、赤テストを順次グリーン化できる状態にする。
  - _Leverage: 既存の when/null/配列テストケース, jv_ast::Expression/Pattern, existing binary_expression_parser_impl_
  - _Requirements: 要件 1, 要件 2_
  - _Prompt: Role: Rust Expression Parsing Specialist | Task: Implement the task for spec jv-parser-red-phase-handling, first run spec-workflow-guide to get the workflow guide then implement the task: Complete expression parsing support for when, null-safe chains, arrays, lambdas, string interpolation, and operator precedence using the refactored modules. | Restrictions: Maintain clean separation between expression stages, ensure new parser branches reuse shared helpers, avoid introducing panics for recoverable parse errors. | _Leverage: jv/crates/jv_parser/src/syntax/support.rs, existing expression parser code, test expectations in tests.rs | _Requirements: 要件 1, 要件 2 | Success: All expression-focused tests parse successfully and validate AST structure, precedence behaves per design, coverage tools show exercised paths. | Instructions: Mark this task as [-] before coding and flip to [x] once validations succeed._

- [ ] 4. テストスイート再編とレッドフェーズ緑化
  - File: jv/crates/jv_parser/src/tests/green_phase.rs (新規)
  - File: jv/crates/jv_parser/src/tests/legacy_red_phase.rs
  - File: jv/crates/jv_parser/src/tests/mod.rs
  - 内容: `tests.rs` を緑フェーズとレガシーレッドフェーズに分割し、実装完了したシナリオを順次グリーン化する。既存の `#[should_panic]` を通常の `#[test]` へ移行し、まだ未達のケースのみ一時的に `#[ignore]` で扱う。テスト支援ヘルパーを `tests::support` に抽出し、`cargo test -p jv_parser` がデフォルトで成功することを確認する。
  - Purpose: 要件 2 を満たし、レッドフェーズ管理をテストコードで明確化する。
  - _Leverage: 既存 tests.rs, test-case クレート, Parser::parse API_
  - _Requirements: 要件 1, 要件 2_
  - _Prompt: Role: Rust Testing Strategist | Task: Implement the task for spec jv-parser-red-phase-handling, first run spec-workflow-guide to get the workflow guide then implement the task: Restructure parser tests into green and legacy modules, convert passing red-phase tests, and ensure default cargo test runs green. | Restrictions: Keep assertions as strict as before or stronger, document any remaining ignored tests with TODO referencing inventory IDs, do not delete coverage without replacement. | _Leverage: jv/crates/jv_parser/src/tests.rs, new syntax modules, cargo test workflow | _Requirements: 要件 1, 要件 2 | Success: `cargo test -p jv_parser` passes without filters, remaining ignored tests list inventory references, helpers reduce duplication. | Instructions: Toggle this task to [-] when starting and to [x] only after tests and documentation updates are complete._

- [ ] 5. 延期構文ドキュメントと同期テスト整備
  - File: jv/crates/jv_parser/docs/deferred-syntax.md
  - File: jv/crates/jv_parser/src/tests/doc_sync.rs (新規)
  - 内容: 実装完了後に残った未対応構文があれば最小限に整理し、Markdown へ反映する。同時に `render_markdown_table()`（仮称）を用いた同期テストを追加し、ドキュメントが常に最新であるか検証する。README やクレートドキュメントからのリンクも更新する。
  - Purpose: 要件 3 を満たし、関係者が延期構文を正確に把握できるようにする。
  - _Leverage: requirements.md のインベントリ表, design.md の data model セクション, 既存 docs_
  - _Requirements: 要件 1, 要件 3_
  - _Prompt: Role: Developer Advocate with Rust tooling expertise | Task: Implement the task for spec jv-parser-red-phase-handling, first run spec-workflow-guide to get the workflow guide then implement the task: Generate/curate deferred syntax documentation from the inventory and add doc-sync tests so drift is caught automatically. | Restrictions: Keep documentation bilingual conventions if applicable, do not introduce new external tooling, ensure tests fail loudly on mismatch. | _Leverage: jv/crates/jv_parser/docs, planned inventory helpers, cargo test doc sync harness | _Requirements: 要件 1, 要件 3 | Success: Deferred syntax doc reflects current inventory, doc-sync test passes, README/crate docs reference the new note. | Instructions: Change this task checkbox to [-] before editing and to [x] after finishing docs and tests._

- [ ] 6. スパン情報を活かした診断/IR マッピングの強化
  - File: jv/crates/jv_parser/src/syntax/expressions.rs
  - File: jv/crates/jv_parser/src/syntax/patterns.rs
  - File: jv/crates/jv_ir/src/transform (既存各モジュール)
  - 内容: パーサーで付与された精度の高い `Span` を活用して、診断メッセージと IR 変換の対応付けを改善する。`ParseError`/`TransformError` にソース位置を反映し、`when` や null セーフティ構文でのマッピングを検証する。必要に応じて IR 変換ロジックへスパンの受け渡しを追加し、ユニットテストで位置情報が反映されていることを確認する。
  - Purpose: エラー報告と IR 生成の信頼性を高め、仕様の診断要件を満たす土台を整備する。
  - _Leverage: jv/crates/jv_parser/src/syntax/support.rs の span ユーティリティ, jv/crates/jv_ir/src/transform/control_flow.rs, 既存 TransformError ケース_
  - _Requirements: 要件 1, 要件 2_
  - _Prompt: Role: Compiler Diagnostics Engineer | Task: Implement the task for spec jv-parser-red-phase-handling, first run spec-workflow-guide to get the workflow guide then implement the task: Thread the newly enriched Span data through parser diagnostics and IR transforms so when/null-safe constructs surface accurate locations. | Restrictions: Avoid widening public APIs without consensus, keep error strings localized, ensure added span data is covered by tests. | _Leverage: parser span helpers, IR transform modules, existing regression tests | _Requirements: 要件 1, 要件 2 | Success: Diagnostics cite precise spans for expression-layer errors, IR mapping retains spans for downstream tooling, added tests exercise the new coverage. | Instructions: Mark this task as [-] before starting and switch to [x] once implementation and tests pass._
