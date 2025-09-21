# Tasks Document

- [x] 1. Environment Manager Implementation
  - File: crates/jv_cli/src/tour/environment.rs
  - JDK環境チェック機能とOS別セットアップガイド実装
  - JDK検出、バージョン確認、インストール完了後のメニュー復帰機能
  - Purpose: 学習開始前の環境構築を自動化
  - _Leverage: crates/jv_build/src/jdk.rs_
  - _Requirements: 10.1, 10.2, 10.3, 10.6_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Rust Developer specializing in CLI applications and system integration | Task: Create Environment Manager module for JDK environment checking and OS-specific setup guides following requirements 10.1-10.3 and 10.6, leveraging existing JDK detection logic from crates/jv_build/src/jdk.rs | Restrictions: Do not modify existing jv_build JDK detection, maintain cross-platform compatibility, ensure secure system command execution | _Leverage: crates/jv_build/src/jdk.rs for JDK detection patterns | _Requirements: 10.1 (auto JDK check), 10.2 (setup guide display), 10.3 (OS-specific guides), 10.6 (auto return to menu) | Success: JDK environment is properly detected, OS-specific setup guides are displayed correctly, automatic menu return works after setup completion | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 2. CLI Interface and Main Menu System
  - File: crates/jv_cli/src/tour/cli.rs
  - ウェルカムメッセージ、メインメニュー、ナビゲーション実装
  - 9つの学習セクション + 進捗確認 + 終了オプション
  - Purpose: ユーザーフレンドリーなCLIインタラクション提供
  - _Leverage: crates/jv_cli/src/main.rs, clap patterns_
  - _Requirements: 1.1, 1.2, 1.3, 1.5_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: UI/UX Developer with CLI expertise and Rust clap framework knowledge | Task: Create comprehensive CLI interface with welcome message and navigation menu following requirements 1.1-1.3 and 1.5, using existing clap patterns from crates/jv_cli/src/main.rs | Restrictions: Must follow existing CLI conventions, maintain consistency with jv CLI style, ensure keyboard navigation accessibility | _Leverage: crates/jv_cli/src/main.rs for CLI patterns | _Requirements: 1.1 (welcome message), 1.2 (section menu), 1.3 (code examples), 1.5 (navigation options) | Success: Welcome message displays correctly, menu navigation works intuitively, section transitions are smooth | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 3. Learning Section Modules (Basic Syntax)
  - File: crates/jv_cli/src/tour/sections/basic_syntax.rs
  - Hello World、val/var宣言、型推論、null安全性デモ実装
  - jvコード例とJava出力の並列表示機能
  - Purpose: 基本構文の段階的学習提供
  - _Leverage: crates/jv_parser/src, crates/jv_codegen_java/src_
  - _Requirements: 2.1, 6.2, 6.3_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Programming Language Educator with Rust and Java expertise | Task: Create Basic Syntax learning module with Hello World, variables, type inference, and null safety examples following requirements 2.1, 6.2-6.3, leveraging parser and codegen from existing crates | Restrictions: Must generate valid Java 25 code, ensure examples are beginner-friendly, maintain educational progression | _Leverage: crates/jv_parser/src for parsing, crates/jv_codegen_java/src for Java generation | _Requirements: 2.1 (basic syntax demo), 6.2 (executable samples), 6.3 (progressive complexity) | Success: Basic syntax examples compile and run correctly, Java output is educational and clear, progression is logical for beginners | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_
  - Follow-up: 現状のパーサーが `String?` を扱えないため null 安全デモは `validate: false` で構文チェックを回避。パーサー対応後に再検証を有効化し、例示 Java 出力もコード生成パイプライン整備後に差し替える。

- [x] 4. Learning Section Modules (Control Flow and Data Classes)
  - File: crates/jv_cli/src/tour/sections/control_flow.rs, data_classes.rs
  - when式、パターンマッチング、data class実装
  - record生成とmutableクラス生成のデモ
  - Purpose: 中級レベルのjv機能理解促進
  - _Leverage: crates/jv_parser/src, crates/jv_codegen_java/src_
  - _Requirements: 2.2, 2.3, 6.2_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Programming Language Educator specializing in modern language features | Task: Create Control Flow and Data Classes modules with when expressions, pattern matching, and data class examples following requirements 2.2-2.3 and 6.2, using existing parser and codegen infrastructure | Restrictions: Must showcase Java 25 pattern matching, ensure data class examples are practical, maintain code quality | _Leverage: crates/jv_parser/src for syntax parsing, crates/jv_codegen_java/src for Java record/class generation | _Requirements: 2.2 (control flow demo), 2.3 (data classes), 6.2 (executable samples) | Success: When expressions translate to Java switch patterns correctly, data classes generate appropriate Java records/classes, examples are practical and educational | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_
  - Follow-up: record パターンと data class の Java 変換を jv_codegen 側で自動生成できるようになった段階で、手書きの Java 出力を置き換えて再検証する。

- [x] 5. Learning Section Modules (Functions and Concurrency)
  - File: crates/jv_cli/src/tour/sections/functions.rs, concurrency.rs, async_prog.rs
  - 拡張関数、デフォルト引数、spawn{}、async{}.await()実装
  - 仮想スレッドとCompletableFutureのJava出力
  - Purpose: 高度なjv機能の実践的理解提供
  - _Leverage: crates/jv_parser/src, crates/jv_codegen_java/src_
  - _Requirements: 2.4, 3.1, 3.2, 3.3, 3.4_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Concurrency Expert and Modern Java Developer | Task: Create Functions, Concurrency, and Async modules with extension functions, virtual threads, and async/await examples following requirements 2.4, 3.1-3.4, leveraging existing compiler infrastructure | Restrictions: Must generate Java 25 virtual thread code, ensure thread safety examples, demonstrate proper async patterns | _Leverage: crates/jv_parser/src for syntax, crates/jv_codegen_java/src for thread and future generation | _Requirements: 2.4 (functions), 3.1-3.2 (concurrency), 3.3-3.4 (async) | Success: Extension functions generate static methods correctly, spawn blocks create virtual threads, async patterns use CompletableFuture appropriately | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 6. Build Tool Experience Module
  - File: crates/jv_cli/src/tour/sections/build_tools.rs
  - jv.toml設定、依存管理、JDKバージョン固定の実演
  - `jv init`から`jv build`まで5段階ハンズオン実装
  - Purpose: 実際の開発ワークフロー習得支援
  - _Leverage: crates/jv_pm/src, crates/jv_build/src_
  - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: DevOps Engineer with build system and package management expertise | Task: Create Build Tools experience module with project initialization, dependency management, and build process demonstration following requirements 4.1-4.5, leveraging existing package manager and build system | Restrictions: Must use real jv.toml configuration, ensure javac --release 25 integration, maintain build reproducibility | _Leverage: crates/jv_pm/src for package management, crates/jv_build/src for build orchestration | _Requirements: 4.1-4.4 (build tool demos), 4.5 (javac integration) | Success: Project initialization works correctly, dependencies are managed properly, build process demonstrates Java 25 compilation | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 7. Interactive Editor Module
  - File: crates/jv_cli/src/tour/sections/interactive.rs
  - リアルタイムコード編集、構文検証、Java出力表示実装
  - サンドボックス環境での安全な実行機能
  - Purpose: 実験的学習とコード理解深化
  - _Leverage: crates/jv_parser/src, crates/jv_codegen_java/src, crates/jv_checker/src_
  - _Requirements: 5.1, 5.2, 5.3, 5.4_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Interactive Development Environment Engineer with security expertise | Task: Create Interactive Editor module with real-time code editing, syntax validation, and safe execution following requirements 5.1-5.4, using parser, codegen, and checker components | Restrictions: Must implement proper sandboxing, prevent system access, ensure memory safety, provide helpful error messages | _Leverage: crates/jv_parser/src for parsing, crates/jv_codegen_java/src for generation, crates/jv_checker/src for validation | _Requirements: 5.1-5.4 (interactive editing and validation) | Success: Real-time editing works smoothly, syntax errors are caught and displayed helpfully, sandbox prevents unsafe operations | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 8. Progress Tracker and Achievement System
  - File: crates/jv_cli/src/tour/progress.rs
  - 学習進捗保存、可視化、達成証明書生成実装
  - ✅🔄⭕マークでの進捗表示とモチベーション機能
  - Purpose: 継続的学習支援と達成感提供
  - _Leverage: serde, std::fs for persistence_
  - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Gamification and User Experience Developer | Task: Create Progress Tracker and Achievement System with visual progress indicators, persistence, and certificate generation following requirements 7.1-7.5, using serde for serialization and std::fs for file operations | Restrictions: Must persist progress across sessions, ensure data integrity, provide meaningful achievements, maintain user privacy | _Leverage: serde for serialization, std::fs for file persistence | _Requirements: 7.1-7.5 (progress tracking and achievements) | Success: Progress is saved and restored correctly, visual indicators are intuitive, achievements motivate continued learning | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 9. Mini Project Builder System
  - File: crates/jv_cli/src/tour/projects/mod.rs, todo_app.rs, calculator.rs, game.rs
  - ToDoアプリ、電卓、じゃんけんゲームの段階的構築
  - 学習機能組み合わせと実行可能.jarファイル出力
  - Purpose: 実践的プログラミングスキル定着
  - _Leverage: crates/jv_build/src, all learning modules_
  - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Project-Based Learning Specialist and Application Developer | Task: Create Mini Project Builder with three progressive projects (ToDo, Calculator, Game) following requirements 8.1-8.5, integrating all learned jv features and producing executable artifacts | Restrictions: Must use features from previous sections, ensure projects are practical and educational, maintain code quality standards | _Leverage: crates/jv_build/src for executable generation, all tour sections for feature integration | _Requirements: 8.1-8.5 (mini project construction and execution) | Success: Projects combine learned features effectively, executables run correctly, progression builds confidence | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 10. Portfolio Generator and Documentation
  - File: crates/jv_cli/src/tour/portfolio.rs
  - GitHub風ディレクトリ構造、README生成、zipパッケージ化
  - 学習履歴とスクリーンショット/実行例の自動ドキュメント化
  - Purpose: 学習成果の可視化と共有支援
  - _Leverage: std::fs, zip crate for packaging_
  - _Requirements: 9.1, 9.2, 9.3, 9.4, 9.5_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Technical Documentation Specialist and Portfolio Developer | Task: Create Portfolio Generator with GitHub-style organization, automatic documentation, and zip packaging following requirements 9.1-9.5, using filesystem operations and zip compression | Restrictions: Must create professional-quality documentation, ensure proper file organization, maintain cross-platform compatibility | _Leverage: std::fs for file operations, zip crate for packaging | _Requirements: 9.1-9.5 (portfolio generation and packaging) | Success: Portfolio structure is professional and organized, documentation is comprehensive and clear, zip package is complete and portable | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 11. Main Tour Orchestrator and CLI Integration
  - File: crates/jv_cli/src/tour/mod.rs, main.rs (modify)
  - `jv tour`サブコマンド統合とモジュール間調整
  - エラーハンドリングとユーザーエクスペリエンス最適化
  - Purpose: 統一された学習体験の提供
  - _Leverage: clap, anyhow, all tour modules_
  - _Requirements: 1.1, 1.2, 1.5, All modules integration_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: System Integration Engineer and CLI Architecture Specialist | Task: Create main tour orchestrator integrating all modules with jv CLI following requirements 1.1-1.2, 1.5 and ensuring seamless module coordination, using clap for CLI integration and anyhow for error handling | Restrictions: Must not break existing jv CLI functionality, ensure consistent error handling across modules, maintain performance standards | _Leverage: clap for CLI framework, anyhow for error handling, all implemented tour modules | _Requirements: 1.1-1.2, 1.5 (CLI integration) and coordination of all tour modules | Success: Tour integrates seamlessly with jv CLI, all modules work together cohesively, user experience is smooth and consistent | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_

- [x] 12. Comprehensive Testing and Documentation
  - File: crates/jv_cli/tests/tour_tests.rs, README.md updates
  - 全モジュールの統合テスト、E2Eテスト、ドキュメント更新
  - クロスプラットフォーム動作確認とパフォーマンス検証
  - Purpose: 品質保証と保守性確保
  - _Leverage: Rust test framework, existing test patterns_
  - _Requirements: All requirements verification_
  - _Prompt: Implement the task for spec jv-language-tour, first run spec-workflow-guide to get the workflow guide then implement the task: Role: QA Engineer and Technical Writer with Rust testing expertise | Task: Create comprehensive test suite and documentation covering all tour functionality and requirements, using Rust testing framework and following existing test patterns for quality assurance | Restrictions: Must test all user journeys, ensure cross-platform compatibility, maintain test reliability and performance | _Leverage: Rust test framework, existing test patterns in crates/jv_cli/tests/ | _Requirements: Verification of all specified requirements through comprehensive testing | Success: All functionality is thoroughly tested, documentation is complete and accurate, cross-platform operation is verified | Instructions: Set this task to in-progress [-] when starting and mark complete [x] when finished_
