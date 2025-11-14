# Tasks Document

> ステータス記号: `[ ]` 未着手 / `[-]` 進行中 / `[x]` 完了。作業を始める際は該当タスクを`[-]`にし、完了後に`[x]`へ更新してください。

## Phase 1 — CLIモード判定とコマンドフィルタ

- [x] 1. `wrapper` モジュール基盤作成
  - Files: `jv/crates/jv_pm/src/wrapper/mod.rs`, `jv/crates/jv_pm/src/lib.rs`
  - `mod wrapper;` を `lib.rs` に追加し、サブモジュール（error, context, pipeline, lockfile, integration）の基盤を用意。
  - _Requirements:_ 構造基盤。

- [x] 2. `WrapperError` enum 定義
  - Files: `jv/crates/jv_pm/src/wrapper/error.rs`
  - ラッパーモード固有のエラー型を定義:
    - `NativeProjectDetected`: `jv.toml` 存在時
    - `MixedProjectConfiguration`: 両ファイル存在時
    - その他のWrapper操作エラー
  - _Design Reference:_ design.md:113-125 (WrapperError)
  - _Requirements:_ R1-1, Error Scenario 4, 5

- [x] 3. `jvpm` CLIエントリのラッパーモード分岐追加
  - Files: `jv/crates/jv_pm/src/bin/jvpm.rs`
  - バイナリ名が `jvpm` の場合はラッパーモード候補として次ステップ（Task 5）へ。`jv` CLI からの呼び出し（`jv pm ...`）の場合はネイティブモード確定。
  - _Design Reference:_ design.md:57-59 (モード判定ロジック - 1. バイナリ名の確認)
  - _Leverage:_ 既存の `Commands` enum、`handle_maven_passthrough`
  - _Requirements:_ R1-1, R1-2

- [x] 4. `WrapperCommandFilter` でJV専用コマンドを拒否
  - Files: `jv/crates/jv_pm/src/wrapper/filter.rs`
  - `fn validate(command: &Commands, mode: CliMode) -> Result<(), WrapperError>` を実装。
  - ラッパーモード中に `export` などJV専用コマンドが指定された場合はガードし、`jv` CLIの使用を案内する明示的メッセージを返す。
  - _Design Reference:_ design.md:128-132 (Component 5)
  - _Requirements:_ R1-2, R2-2

## Phase 2 — Wrapper Context & Lockfile

- [x] 5. `WrapperContext::detect` によるモード判定
  - Files: `jv/crates/jv_pm/src/wrapper/context.rs`
  - プロジェクトルートで `jv.toml` / `pom.xml` の存在を検査し、以下の4パターンに対応:
    - **`jv.toml` のみ存在** → `Err(WrapperError::NativeProjectDetected)`、エラーメッセージ: "このプロジェクトは JV ネイティブプロジェクトです。`jv pm` コマンドを使用してください。"
    - **両方存在** → `Err(WrapperError::MixedProjectConfiguration)`、エラーメッセージ: "プロジェクト構成の混在を検出しました。JV ネイティブプロジェクトでは `jv pm` を、Maven プロジェクトでは `jvpm` を使用してください。"
    - **`pom.xml` のみ存在** → ラッパーモード確定、`WrapperContext` を構築
    - **両方不在** → Task 6 へ
  - _Design Reference:_ design.md:53-84 (モード判定ロジック), design.md:88-96 (Component 1)
  - _Requirements:_ R1-1, Usability (62-63行)

- [x] 6. テンプレート生成（`pom.xml`/`settings.xml` 不在時）
  - Files: `jv/crates/jv_pm/src/wrapper/context.rs`
  - Task 5 で両方不在の場合、`pom.xml` + `settings.xml` の最小構成を生成。生成失敗時は Error Scenario 1 として終了コード 1。
  - _Design Reference:_ design.md:76-79 (両方不在パターン)
  - _Leverage:_ `maven::pom_generator`, `maven::settings`
  - _Requirements:_ Reliability (54行), Error Scenario 1

- [x] 7. `WrapperLockfileWriter` と `LockfileService` 拡張
  - Files: `jv/crates/jv_pm/src/wrapper/lockfile.rs`, `jv/crates/jv_pm/src/lockfile/mod.rs`
  - Manifest 不在でも Resolver の結果から `jv.lock` を生成・保存できる補助ユーティリティを実装。
  - **Implementation Note (design.md:122-126):**
    - **推奨アプローチ**: `LockfileService` に `generate_from_resolved(resolved: &ResolvedDependencies) -> Lockfile` メソッドを追加し、`ResolvedDependencies` から直接 `Lockfile` 構造体を生成
    - 代替案: WrapperLockfileWriter内でDummy Manifestを内部生成（非推奨、Zero Magic 原則に反する）
  - _Leverage:_ `LockfileService::save`
  - _Requirements:_ R1-1, R2-1

## Phase 3 — Wrapper Pipeline (Add/Remove)

- [x] 8. `WrapperPipeline` 実装
  - Files: `jv/crates/jv_pm/src/wrapper/pipeline.rs`
  - `add`/`remove` で Resolver → Download → Lockfile → `pom.xml` 同期までを orchestration。
  - _Leverage:_ 既存 `handle_add_command`、`DependencyCache`, `ResolverDispatcher`。
  - _Requirements:_ R1-2, R2-1。

- [x] 9. CLI から WrapperPipeline を呼び出す
  - Files: `jv/crates/jv_pm/src/bin/jvpm.rs`
  - `Commands::Add/Remove` 実装をラッパーモード/ネイティブモードで切り替え、ラッパー時には `WrapperPipeline` を実行する。
  - _Requirements:_ R1-2。

## Phase 4 — Maven Integration Strategy

- [ ] 10. `WrapperIntegrationStrategy` と `MavenIntegrationConfig` 拡張
  - Files: `jv/crates/jv_pm/src/wrapper/integration.rs`, `jv/crates/jv_pm/src/maven/mod.rs`
  - `MavenIntegrationDispatcher` に `wrapper-default` を追加し、`pom.xml`/`settings.xml` をプロジェクト直下へ出力するストラテジを実装。
  - **Implementation Note (design.md:112-116):**
    - **推奨アプローチ**: `MavenIntegrationConfig` の `manifest` フィールドを `Option<&'a Manifest>` に変更し、既存戦略との互換性を保つ
    - 代替案: `WrapperMavenConfig` 構造体を新規定義し、`resolved`, `repositories`, `mirrors` のみを保持
  - _Leverage:_ 既存 `Maven3IntegrationStrategy`、`PomGenerator`, `settings::generate_settings_xml`
  - _Requirements:_ R2-1

- [ ] 11. Wrapper 出力パスと `jv.lock` 同期
  - Files: `jv/crates/jv_pm/src/wrapper/sync.rs`
  - `WrapperUpdateSummary` を用意し、`pom.xml`/`settings.xml`/`jv.lock` の更新フラグを返す。
  - _Requirements:_ Non-functional Reliability。

## Phase 5 — アルゴリズム切替 & テスト

- [ ] 12. Resolver アルゴリズム切替サポート
  - Files: `jv/crates/jv_pm/src/bin/jvpm.rs`, `jv/crates/jv_pm/src/wrapper/pipeline.rs`
  - `--strategy` フラグ等をラッパーモードでも受け取り、**PubGrub/BreadthFirst/MavenCompat** を切り替えられるようにする。
  - _Design Reference:_ design.md:161 (Integration Testing - 全戦略テスト)
  - _Requirements:_ requirements.md:57 (Non-functional Reliability - アルゴリズム切替テスト)

- [ ] 13. Unit テスト
  - Files: `jv/crates/jv_pm/src/wrapper/{context,lockfile,pipeline,filter}.rs`, `jv/crates/jv_pm/tests/wrapper_tests.rs`
  - 以下をテスト:
    - `WrapperContext::detect` のファイル検査4パターン（`jv.toml`のみ、`pom.xml`のみ、両方、両方不在）
    - テンプレート生成の成功/失敗
    - `WrapperLockfileWriter` がManifest なしでも安定ソートな `jv.lock` を生成
    - `WrapperCommandFilter` が `export` 等のJV専用コマンドを拒否
  - _Design Reference:_ design.md:152-156 (Unit Testing)
  - _Requirements:_ Reliability, Usability

- [ ] 14. Integration / E2E テスト
  - Files: `tests/e2e/jvpm_wrapper_mode.rs`
  - 以下のシナリオを確認:
    - **Maven Wrapper**: `jvpm add junit` → `pom.xml` + `jv.lock` 同期
    - **プロジェクト構成検証**:
      - `jv.toml` 存在下で `jvpm` 実行 → エラー表示 + 終了コード 1
      - 両ファイル存在下で `jvpm` 実行 → エラー表示 + 終了コード 1
    - **JV専用コマンド拒否**: `jvpm export` → エラー表示 + `jv` CLI案内
    - **アルゴリズム切替**: `--strategy pubgrub/breadth-first/maven-compat` すべてで依存解決が動作
    - **キャッシュ再利用**: 別プロジェクトで `~/.jv/cache` のハードリンクが使われる
  - _Design Reference:_ design.md:163-169 (End-to-End Testing)
  - _Requirements:_ 全要件

## Phase 6 — Docs & Cleanup

- [ ] 15. ドキュメント & CLIヘルプ更新
  - Files: `docs/design/jvpm-tui-specification.md`, `jv/crates/jv_pm/src/bin/jvpm.rs` (help text)
  - ラッパーモード挙動、サポート/非サポートコマンド、`jv.lock` の役割を追記。
  - _Requirements:_ Usability。

- [ ] 16. 手動確認チェックリスト
  - Files: `.project-todolist/作業指示-20251112-jvpm-tui.md` (該当セクション)
  - 新しいラッパーモード動作の手動テスト項目を追記し、CI/CD手順を更新。
  - _Requirements:_ 全体整合。
