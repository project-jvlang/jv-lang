
# Tasks Document

- [x] 1. パーサとASTメタデータの拡張を実装する
  - File: jv/crates/jv_parser_rowan/src/lowering/, jv/crates/jv_ast/src/
  - `BinaryOp::Is` の右辺が正規表現リテラルか `Pattern` 型式かを判定し、`IsTestMetadata` に `pattern_expr` と新しいガード情報を格納できるようローアリングを拡張する。必要に応じて `jv_ast` の構造体を更新し、既存の型テストとの互換性を保つ。
  - Purpose: 正規表現マッチング用の構文情報を型推論・ローワリング層へ渡す基盤を整備する。
  - _Leverage: jv/crates/jv_parser_rowan/src/lowering/statements.rs, jv/crates/jv_ast/src/expression.rs_
  - _Requirements: Requirement 1 (AC1), Requirement 2 (AC1)_
  - _Prompt: Implement the task for spec regex-is-operator, first run spec-workflow-guide to get the workflow guide then implement the task: 役割: Rust コンパイラエンジニア（構文解析担当） | タスク: `BinaryOp::Is` に対応するローアリングで正規表現リテラルと任意の `Pattern` 型式を区別し、`IsTestMetadata` に正規化済みパターンや式ツリーを格納できるよう AST/メタデータを拡張する | 制約: 既存の型テスト処理を壊さない、序数的な二重評価を導入しない、テストフィクスチャを更新すること | 活用: `jv_parser_rowan` の既存メタデータ生成ロジックと `jv_ast::Expression` の構造 | 成功条件: 新しいメタデータが型推論層で利用可能になり、既存テストが通過しつつ正規表現用の新テストが追加されている_
  - _Success: AST に正規表現メタデータが確実に付与され、既存の `BinaryOp::Is` の振る舞いが退行せず、新規ローアリングテストが通過する_

- [ ] 2. 型推論と診断での正規表現 `is` サポートを追加する
  - File: jv/crates/jv_checker/src/inference/, jv/crates/jv_checker/src/diagnostics/
  - `CompatibilityChecker` を用いて左辺を `CharSequence` 互換か判定し、Optional 左辺では `JV_REGEX_W001` 警告とガード戦略を設定する。右辺が `Pattern` 型かリテラルかに応じた変換ヒントを生成し、`JV_REGEX_E002` などの診断メッセージを更新する。
  - Purpose: 型システムで `text is /pattern/` を安全に検証し、要件どおりの警告・エラーを提供する。
  - _Leverage: jv/crates/jv_checker/src/inference/constraint/generator.rs, jv/crates/jv_checker/src/inference/compatibility.rs, jv/crates/jv_checker/src/diagnostics/messages_
  - _Requirements: Requirement 1 (AC2), Requirement 3 (AC1), Requirement 3 (AC2)_
  - _Prompt: Implement the task for spec regex-is-operator, first run spec-workflow-guide to get the workflow guide then implement the task: 役割: Rust 型システムエンジニア | タスク: `BinaryOp::Is` が正規表現メタデータを持つ場合に型互換性チェックと変換ヒント・警告 (`JV_REGEX_W001`)・エラーを生成できるよう `jv_checker` を拡張し、Optional 左辺には自動ガード戦略を設定する | 制約: 既存の `CompatibilityChecker` API を再利用し、診断コード体系を維持する、パフォーマンス退行を避ける | 活用: `docs/design/java-aware-type-inference.md` に記載された設計、既存の `RegexValidator` |
  - _Success: 型推論が正規表現 `is` を boolean として扱い、Optional 左辺で警告とガード情報を付与し、関連テストが追加される_

- [ ] 3. Null 安全フローと警告統合を更新する
  - File: jv/crates/jv_checker/src/null_safety/
  - `BinaryOp::Is` 成立時に左辺を非 null として扱う既存ロジックを拡張し、解析結果のガード戦略と同期させてスマートキャストを行う。Optional 左辺の警告が null 安全解析と整合するようテレメトリやメッセージを調整する。
  - Purpose: Null 安全解析が正規表現マッチと連携し、Optional 型の分岐で正しくナローイングする。
  - _Leverage: jv/crates/jv_checker/src/null_safety/flow.rs, jv/crates/jv_checker/src/null_safety/operators.rs_
  - _Requirements: Requirement 1 (AC3), Requirement 2 (AC2)_
  - _Prompt: Implement the task for spec regex-is-operator, first run spec-workflow-guide to get the workflow guide then implement the task: 役割: Rust Null 安全解析エンジニア | タスク: 正規表現 `is` の真分岐で左辺を非 null とみなす処理をガード戦略と連携させ、Optional 型に対するナローイングと警告表示を整合させる | 制約: 既存の null 安全演算子の暗黙仕様を壊さない、到達不能パスを導入しない | 活用: `OperatorSemantics` と設計書のフロー図 |
  - _Success: null 安全解析が Optional 左辺で確実に非 null ナローイングを適用し、関連テストが成功する_

- [ ] 4. IR ローワリングと Java コード生成を実装する
  - File: jv/crates/jv_ir/src/transform/, jv/crates/jv_codegen_java/src/generator/expressions.rs
  - `RegexMatchLowering` による一時変数キャプチャと null ガード生成を実装し、`pattern_source` に応じて `Pattern.matcher().matches()` を一度だけ評価するようコード生成を更新する。
  - Purpose: 生成 Java がゼロマジック原則を守りつつ正規表現マッチングを表現する。
  - _Leverage: jv/crates/jv_ir/src/types.rs, jv/crates/jv_ir/src/transform/mod.rs, jv/crates/jv_codegen_java/src/generator/expressions.rs_
  - _Requirements: Requirement 1 (AC4), Requirement 2 (AC1), Requirement 2 (AC2)_
  - _Prompt: Implement the task for spec regex-is-operator, first run spec-workflow-guide to get the workflow guide then implement the task: 役割: Rust/Java コードジェネレーションエンジニア | タスク: 設計で定義した一時変数キャプチャ手順に従い、IR と Java 出力を更新して `Pattern.matcher().matches()` を単一評価で生成し、Optional 左辺の場合は null ガードを付与する | 制約: 生成コードの可読性を保つ、既存 `instanceof` ベースの `is` 変換と共存させる | 活用: `docs/design/java-aware-type-inference.md`、既存の `RegexPattern` 変換ロジック |
  - _Success: IR と Java 出力が一時変数を用いた正規表現マッチを生成し、スナップショットテストが更新される_

- [ ] 5. 診断・メッセージと LSP 連携を更新する
  - File: jv/crates/jv_checker/src/diagnostics/messages.rs, jv/crates/jv_lsp/src/
  - `JV_REGEX_W001` を含む新旧診断の文言と多言語対応を整理し、LSP で Optional 左辺警告や正規表現エラーが分岐単位で表示されるようにする。
  - Purpose: IDE/CLI で一貫した診断を提供し、ユーザーに自動ガード挙動を周知する。
  - _Leverage: jv/crates/jv_checker/src/diagnostics/, jv/crates/jv_lsp/src/handlers/diagnostics.rs_
  - _Requirements: Requirement 1 (AC3), Non-Functional Requirements > Usability_
  - _Prompt: Implement the task for spec regex-is-operator, first run spec-workflow-guide to get the workflow guide then implement the task: 役割: LSP/診断エンジニア | タスク: Optional 左辺の警告と正規表現エラーのメッセージ/多言語対応を整理し、LSP が `when` 分岐単位で診断を提示できるよう更新する | 制約: 既存診断コードを再利用し、英語/日本語翻訳を揃える、診断フォーマットを崩さない | 活用: `jv_checker::diagnostics::EnhancedDiagnostic` の既存使用箇所 |
  - _Success: CLI/LSP で新診断が正しく表示され、関連ユニットテストと LSP テストが通過する_

- [ ] 6. 包括的なテストを追加する
  - File: jv/crates/jv_parser_rowan/tests/, jv/crates/jv_checker/tests.rs, jv/crates/jv_codegen_java/src/tests/
  - パーサ・型推論・コード生成の各レイヤーに対して、正規表現 `is`、Optional 左辺、`when` 分岐、`Pattern` 変数などをカバーするテストを追加する。
  - Purpose: 回帰を防止し、Acceptance Criteria を自動的に検証する。
  - _Leverage: 既存の `regex` 関連テスト、`pattern_switch` スナップショット、`null_safety` テスト_
  - _Requirements: Requirement 1 (AC全般), Requirement 2 (AC全般)_
  - _Prompt: Implement the task for spec regex-is-operator, first run spec-workflow-guide to get the workflow guide then implement the task: 役割: Rust テストエンジニア | タスク: パーサ/型推論/コード生成のテストを追加し、Optional 左辺の自動ガードや `when` 分岐での正規表現マッチを検証する | 制約: 既存スナップショットの更新が必要な場合は整合性を確認し、失敗ケースも含める | 活用: 既存の `regex` E2E テスト、`null_safety` テレメトリテスト |
  - _Success: 各レイヤーで正規表現 `is` の動作をカバーするテストが追加・更新され、`cargo test` が成功する_

- [ ] 7. ドキュメントとハンドブックを更新する
  - File: docs/regex-is-operator.md, docs/handbook/pattern-matching.md
  - 正規表現 `is` の使用例、Optional 左辺で自動挿入されるガードの説明、`when` 式での注意点をドキュメントへ追加する。
  - Purpose: 利用者が新機能の挙動とベストプラクティスを理解できるようにする。
  - _Leverage: 既存の正規表現ドキュメント、パターンマッチングハンドブック_
  - _Requirements: Non-Functional Requirements > Usability, References_
  - _Prompt: Implement the task for spec regex-is-operator, first run spec-workflow-guide to get the workflow guide then implement the task: 役割: 技術ライター | タスク: 正規表現 `is` の説明、Optional 左辺警告/自動ガード、`when` 式での使い方をドキュメントに追記する | 制約: 日本語ドキュメントのスタイルガイドに従う、コード例は jv/Java 両方を示す | 活用: Requirements/Design 文書、既存ドキュメント構成 |
  - _Success: 正規表現 `is` のガイドが追加され、リンクとサンプルコードが整合し、レビューを通過する_
