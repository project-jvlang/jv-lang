# Tasks Document

- [x] 1. プロジェクトルート検出モジュールを実装
  - Files: jv/crates/jv_cli/src/pipeline/project/locator.rs (新規), jv/crates/jv_cli/src/pipeline/mod.rs, jv/crates/jv_cli/src/tests/project_locator.rs (新規)
  - 任意のサブディレクトリから `jv.toml` を探索し、最初に見つかったディレクトリを `ProjectRoot` として返す `ProjectLocator` を実装する
  - ルート未検出時やパス外参照を検知した際に `ToolingDiagnostic(JV1001)` を返すガードを追加する
  - _Leverage: 既存の `jv_cli::tooling_failure`, `jv_cli::Commands::Build` エントリポイント_
  - _Requirements: 要件 2_
  - _Prompt: Implement the task for spec cli-project-structure, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Rust CLI Infrastructure Engineer | Task: Add a reusable `ProjectLocator` that climbs directories to find `jv.toml`, returning a typed `ProjectRoot` and surfacing `JV1001` diagnostics when detection fails | Restrictions: Keep filesystem operations side-effect free (read-only), ensure relative paths stay within the workspace, and add focused unit tests under `jv/crates/jv_cli/src/tests/project_locator.rs` | _Leverage: `jv_cli::tooling_failure`, CLI command wiring | _Requirements: 要件 2 | Success: Locator resolves roots from nested paths, errors carry span/context, and new tests cover success/failure cases | Workflow: Mark this task as [-] in tasks.md before editing and flip to [x] after code/tests are complete_

- [x] 2. マニフェスト読込と設定モデルを拡張
  - Files: jv/crates/jv_cli/src/pipeline/project/manifest.rs (新規), jv/crates/jv_pm/src/lib.rs, jv/crates/jv_pm/tests/manifest_project.rs (新規)
  - `ManifestLoader` を実装し、`jv.toml` から `include`/`exclude`/`output`/`entrypoint` を読み込んで `ProjectSettings` にマッピングする
  - 不正な設定値や欠落項目を検出した際に `ToolingDiagnostic(JV1001)` を生成し CLI へ返す
  - _Leverage: `jv_pm::Manifest::load_from_path`, 既存の `BuildInfo`/`JavaTarget` 型_
  - _Requirements: 要件 1_
  - _Prompt: Implement the task for spec cli-project-structure, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Rust Configuration Engineer | Task: Extend manifest parsing so `jv.toml` feeds `ProjectSettings` with include/exclude patterns, output options, and entrypoint metadata while validating values against requirement 1 | Restrictions: Reuse existing serde/toml infrastructure, add serde structs instead of ad-hoc parsing, and cover edge cases via unit tests in `jv_pm` | _Leverage: Manifest loader, BuildInfo structures | _Requirements: 要件 1 | Success: Valid manifests hydrate settings correctly, invalid fields yield JV1001 diagnostics with location data, and tests document behaviour | Workflow: Mark this task as [-] before editing and to [x] after code/tests are complete_

- [x] 3. ソース列挙とレイアウト構築を実装
  - Files: jv/crates/jv_cli/src/pipeline/project/layout.rs (新規), jv/crates/jv_cli/src/tests/project_layout.rs (新規)
  - `ProjectLayout` を実装し、`ProjectSettings` の include/exclude を適用して `.jv` ソースを列挙、エントリポイント推論を行う
  - ソースがゼロ件の場合やエントリポイント不明時には `ToolingDiagnostic(JV1002)` を返す
  - _Leverage: Rust 標準ライブラリのパス操作, 既存 CLI テストインフラ_
  - _Requirements: 要件 2_
  - _Prompt: Implement the task for spec cli-project-structure, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Rust Build Pipeline Engineer | Task: Create `ProjectLayout` that applies include/exclude patterns, deduplicates sources, resolves entrypoints, and raises JV1002 when nothing is buildable | Restrictions: Avoid introducing heavy glob dependencies (prefer `globset` if already in tree, otherwise implement lightweight matching), ensure deterministic ordering, and add unit tests covering pattern precedence | _Leverage: CLI test helpers, standard fs utilities | _Requirements: 要件 2 | Success: Layout returns ordered sources and a valid entrypoint, diagnostics fire on empty sets, and tests verify pattern handling | Workflow: Mark this task as [-] before editing and to [x] after verification_

- [x] 4. BuildOptions統合とパイプライン接続を実装
  - Files: jv/crates/jv_cli/src/pipeline/build_plan.rs (新規), jv/crates/jv_cli/src/pipeline/mod.rs, jv/crates/jv_cli/src/main.rs, jv/crates/jv_cli/src/tests/build_plan.rs (新規)
  - `BuildOptionsFactory` を実装し、`ProjectLayout` + CLIオーバーライドから `BuildOptions` と `BuildConfig` を構築する
  - `Commands::Build`/`Run` で新しいプロジェクト解決フローを呼び出し、既存の `compile` / `run_program` が `BuildPlan` を受け取るように改修する
  - _Leverage: `jv_build::BuildConfig`, `pipeline::compile`, 既存 CLI オプション解析_
  - _Requirements: 要件 1, 要件 2_
  - _Prompt: Implement the task for spec cli-project-structure, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Rust Application Architect | Task: Wire the new project context into `BuildOptions` so CLI overrides merge with manifest defaults, propagate the plan through compile/run, and maintain backward compatibility | Restrictions: Keep public CLI API stable, update integration tests to reflect new flow, and ensure diagnostics bubble up unchanged | _Leverage: BuildConfig setters, current pipeline::compile | _Requirements: 要件 1, 要件 2 | Success: CLI commands work from subdirectories without extra flags, BuildConfig reflects manifest overrides, and tests prove the new wiring | Workflow: Mark this task as [-] before editing and flip to [x] once code/tests pass_

- [x] 5. 出力ディレクトリ管理とクリーニングを実装
  - Files: jv/crates/jv_cli/src/pipeline/project/output.rs (新規), jv/crates/jv_cli/src/tests/project_output.rs (新規), docs/cli-project-structure.md (新規)
  - `OutputManager` を導入し、ターゲットごとの出力ディレクトリ分離、`--clean` オプション処理、失敗時のクリーンアップを実装する
  - CLI ログとドキュメントに出力ポリシーと利用方法を追記する
  - _Leverage: 標準 fs API, 既存の互換レポート出力 (`pipeline::report`)_
  - _Requirements: 要件 3_
  - _Prompt: Implement the task for spec cli-project-structure, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Rust DevOps Tooling Engineer | Task: Add an `OutputManager` that prepares/cleans target-specific directories, honours manifest/CLI overrides, and documents the behaviour for users | Restrictions: Prevent directory traversal outside the project root, ensure clean runs leave no stale artifacts, and update docs bilingually (JA/EN) | _Leverage: Existing report writers, std::fs utilities | _Requirements: 要件 3 | Success: Builds isolate outputs per target, `--clean` removes stale files safely, documentation reflects new workflow, and unit tests cover success/error paths | Workflow: Mark this task as [-] before editing and to [x] after validation_

- [x] 6. 統合テストとE2Eシナリオを追加
  - Files: jv/crates/jv_cli/tests/project_integration.rs (新規), jv/tests/cli_project_structure.rs (新規)
  - 新しいプロジェクト解決フローを通す統合テスト・端到端テストを追加し、複数ターゲットと `--clean` シナリオを検証する
  - ビルド失敗時に部分生成物が残らないこと、ログに検出結果が表示されることを確認する
  - _Leverage: 既存 `jv/tests` インフラ, `java-target-matrix` のテストユーティリティ_
  - _Requirements: 要件 1, 要件 2, 要件 3_
  - _Prompt: Implement the task for spec cli-project-structure, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Rust QA & Tooling Engineer | Task: Add integration/E2E tests that exercise manifest overrides, subdirectory builds, target switching, and output cleanup to satisfy all requirements | Restrictions: Keep tests hermetic using temp dirs, ensure they pass on CI without external dependencies, and assert log messaging where relevant | _Leverage: Existing CLI test harness, compatibility fixtures | _Requirements: 要件 1, 要件 2, 要件 3 | Success: New tests fail on regressions in detection/output, demonstrate multi-target builds, and verify cleanup semantics | Workflow: Mark this task as [-] before editing and flip to [x] after tests are green_
