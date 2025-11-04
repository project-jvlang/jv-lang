# タスク文書

- [x] 1. テストDSL構文解析とAST拡張  
  - ファイル: `jv/crates/jv_parser_rowan/src/parser/strategies/test.rs`（新規）、`jv/crates/jv_parser_rowan/src/parser/strategies/mod.rs`、`jv/crates/jv_ast/src/statement.rs`、`jv/crates/jv_ast/src/tests.rs`  
  - 内容: `test "..." { ... }` および `test "..." [dataset] { ... }` を解析する戦略を追加し、`TestDeclaration` AST ノードと関連構造体（`TestDataset`, `TestParameter` 等）を定義する。DisplayName、パラメータリスト、注釈、ボディを保持し、`Span` を必ず設定する。既存の関数／変数戦略と競合しないよう先読みルールを実装し、単体テストを整備する。  
  - 目的: 要求1・要求2を満たすテスト DSL のエントリポイントを構築する。  
  - 参照: 設計書「Component 1」「Model 1」、要求1・要求2。  
  - 役割/プロンプト: Rust パーサエンジニア | Rowan ベースのイベント生成と AST 変換を追加し、コメント／注釈の保持とエラーハンドリングを設計書通りに実装すること。  
  - 成功条件: 新規 DSL の構文解析が通り、AST に `TestDeclaration` が生成される; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._

- [x] 2. TestIdentifierNormalizer と命名正規化の実装  
  - ファイル: `jv/crates/jv_ir/src/naming/test_identifiers.rs`（新規）、`jv/crates/jv_ir/src/naming/mod.rs`、`jv/crates/jv_ir/src/transform/utils.rs`、`jv/crates/jv_ir/src/tests.rs`  
  - 内容: DisplayName と `Span` を入力に Unicode NFKD → ASCII 化 → 非英数字 `_` 置換 → 連続 `_` 折り畳みを行い、固定シード `AHasher::new_with_keys(K1, K2)`（`const K1: u64 = 0x6a09e667f3bcc908; const K2: u64 = 0xbb67ae8584caa73b`）から 64bit ハッシュを算出して `_xxxxxxxx` サフィックスを付与する。`normalize_dataset` も同様に位置情報を取り、MethodSource 名と一貫するハッシュ入力を保証する。単体テストで決定性を検証する。  
  - 目的: 要求1 の「安定化されたメソッド名」「MethodSource 名一致」条件を満たす。  
  - 参照: 設計書「Component 5」、要求1。  
  - 役割/プロンプト: Rust ユーティリティエンジニア | 既存 `sanitize_type_identifier` ロジックを再利用し、テストで DisplayName が同一でも行番号が異なる場合に異なるハッシュとなることを確認する。  
  - 成功条件: 正規化関数が決定的に動作し、Method 名・データセット名双方で衝突が解消される; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._

- [x] 3. テストDSLローワリングと IR 統合  
  - ファイル: `jv/crates/jv_ir/src/transform/tests/mod.rs`（新規）、`jv/crates/jv_ir/src/transform/mod.rs`、`jv/crates/jv_ir/src/sequence_pipeline.rs`、`jv/crates/jv_ir/src/types.rs`  
  - 内容: `TestDeclaration` から `TestSuiteLoweringResult` を生成し、`IrStatement::ClassDeclaration`（`<ClassName>Test`）と `IrStatement::MethodDeclaration`、`IrStatement::SampleDeclaration` を構築する。`@BeforeEach` 等のライフサイクル関数を透過的に組み込み、アサーション演算子を `AssertionPattern` に分類する。`SampleRegistry` を導入し、同一サンプルファイルを参照するテスト間で `IrSampleDeclaration` を共有する。  
  - 目的: 要求1・要求2・要求3の核心となる IR 生成フローを確立する。  
  - 参照: 設計書「Component 2」「Component 3」「Architecture」、要求1〜3。  
  - 役割/プロンプト: Rust IR エンジニア | 既存の `transform_program` パイプラインに統合し、Span／ハッシュ情報を引き継ぎつつエラーシナリオ（パラメータ列数不一致など）を診断として返す。  
  - 成功条件: `.jv` テスト DSL から IR へ変換でき、`IrProgram::type_declarations` にテストクラスが追加される; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._

- [x] 4. JUnit 5 向けコード生成拡張  
  - ファイル: `jv/crates/jv_codegen_java/src/generator/tests.rs`（新規）、`jv/crates/jv_codegen_java/src/generator/mod.rs`、`jv/crates/jv_codegen_java/src/generator/declarations.rs`、`jv/crates/jv_codegen_java/src/generator/statements.rs`、`jv/crates/jv_codegen_java/src/tests.rs`  
  - 内容: テストクラスに `@Test`, `@DisplayName`, `@ParameterizedTest`, `@MethodSource`, `Stream<Arguments>` を生成し、必要なインポート（`org.junit.jupiter.api.*`, `org.junit.jupiter.params.*`）を追加する。アサーションパターンを `assertEquals`/`assertNotEquals`/`assertTrue` へ展開し、`@Sample` で生成されたネスト型・データフィールドを `static final` として挿入する。  
  - 目的: JUnit 5 互換の Java コード生成を実現し、要求1〜3 を Java 出力レベルで満たす。  
  - 参照: 設計書「Component 4」「Architecture」、要求1〜3。  
  - 役割/プロンプト: Rust コードジェネレータ | `generate_class` フックを拡張し、MethodSource 名とメソッド名の一致、`Stream<Arguments>` 構築、`@Sample` 共有構造を正しく描画する。  
  - 成功条件: 生成された Java ソースに JUnit 5 アノテーションとデータプロバイダが含まれ、コンパイル可能なコードとなる; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._

- [x] 5. アサーション・データソース検証ロジック  
  - ファイル: `jv/crates/jv_ir/src/transform/tests/assertions.rs`（新規）、`jv/crates/jv_ir/src/transform/tests/dataset.rs`、`jv/crates/jv_checker/src/diagnostics/messages.rs`、`jv/crates/jv_checker/src/tests.rs`  
  - 内容: `expr == expected` / `expr != expected` / 単独布値のパターン分類と、パラメータ数とデータ列数の検証を行い、違反時は `JV5301` 等の新規診断コードで報告する。Quick Fix 提案（列数調整、アサーション書き換え）を `EnhancedDiagnostic` に設定し、多言語メッセージを追加する。  
  - 目的: 要求1・要求2 の受入条件（衝突検出、診断報告）を満たす。  
  - 参照: 設計書「Error Handling Scenario 3」「Scenario 1」、要求1・要求2。  
  - 役割/プロンプト: Rust チェッカエンジニア | 既存診断基盤に従い、Span とカテゴリ（`test.assertion`）を設定する。ユニットテストでエラーパスを検証する。  
  - 成功条件: 不一致ケースで適切な診断が発生し、Quick Fix と多言語メッセージが提供される; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._

- [x] 6. CLI・ビルド統合と生成物配置  
  - ファイル: `jv/crates/jv_cli/src/commands/test.rs`、`jv/crates/jv_cli/src/tour/sections/build_tools.rs`、`jv/crates/jv_build/src/lib.rs`、`jv/crates/jv_build/src/config.rs`  
  - 内容: `jv test` コマンドで生成されたテストクラスを `target/generated-tests`（仮）へ配置し、toolchains/java25 を `JAVA_HOME` として設定した状態で `mvn test` を呼び出せるようにする。CLI メッセージ・ドキュメントを更新し、`@Sample` 共有キャッシュと整合する出力パスを確立する。また Java 21 ターゲット用に toolchains/java21 を `JAVA_HOME` に設定した実行パスを用意し、フラグ指定（例: `--target 21`）で生成コードを切り替えられるようにする。  
  - 目的: 要求3・非機能要件（セキュリティ/設計ドキュメント）に沿ったテスト実行フローを提供する。  
  - 参照: 設計書「Architecture」「Testing Strategy End-to-End」、要求3、非機能（セキュリティ・ユーザビリティ）。  
  - 役割/プロンプト: Rust CLI エンジニア | toolchains/java25 と toolchains/java21 を環境変数から設定し、ターゲット別に `mvn test` を実行できるコマンドパスを実装する。既存ビルドシステムとの互換性を保つ。  
  - 成功条件: `jv test` が JUnit 5 クラスを生成し、Java 25 ターゲット（toolchains/java25）と Java 21 ターゲット（toolchains/java21）の双方で `mvn test` が成功して CLI でパスが報告される; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._

- [x] 7. パフォーマンスベンチマークとメトリクス収集  
  - ファイル: `examples/junit5_benchmark`（新規ディレクトリ）、`jv/crates/jv_ir/benches/junit5_codegen.rs`（ignored テスト）、`jv/crates/jv_ir/src/profiling.rs`  
  - 内容: ベンチマーク用 `.jv` プロジェクトを作成し、`transform_program_with_context_profiled` を利用した測定コードを追加する。`cargo test -p jv_codegen_java --features junit5_integration_bench -- --ignored bench::junit5_codegen` で要求値（10% 以内）を検証できるよう、Baseline（JUnit 無し）との比較を組み込む。  
  - 目的: 非機能要件（パフォーマンス）の測定条件を満たす。  
  - 参照: 設計書「Performance」「Testing Strategy Integration」。  
  - 役割/プロンプト: Rust パフォーマンスエンジニア | TransformProfiler を活用し、計測結果をドキュメント化する。  
  - 成功条件: ベンチテストが実行可能で、計測結果が 10% 増加以内であることを検証できる; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._

- [x] 8. テストスイート整備（Unit / Integration / E2E）  
  - ファイル: `jv/crates/jv_parser_rowan/tests/test_dsl.rs`、`jv/crates/jv_ir/tests/junit5_lowering.rs`、`jv/crates/jv_codegen_java/src/tests/junit5.rs`、`jv/crates/jv_cli/tests/junit5_integration.rs`  
  - 内容: 解析〜コード生成〜CLI までのユニット／統合テスト、および toolchains/java25 を用いた E2E テストを追加する。さらに toolchains/java21 を使用して Java 21 ターゲットで生成されたテストクラスを `mvn test` で検証するシナリオを加える。生成対象ごとに `JAVA_HOME` を切り替え、`--target 21` 出力が確実に成功することを確認する。  
  - 目的: 要求1〜3、非機能（信頼性・ユーザビリティ）の検証を自動化する。  
  - 参照: 設計書「Testing Strategy Unit/Integration/End-to-End」。  
  - 役割/プロンプト: QA エンジニア | パラメータ化テスト、`@Sample` データ駆動テスト、アサーション誤用ケースの回帰をカバーする。  
  - 成功条件: すべてのテストがパスし、Regression を抑止するカバレッジが確保される。E2E テストでは Java 25（toolchains/java25）と Java 21（toolchains/java21）の両ターゲットで `mvn test` 成功を確認済みである; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._

- [ ] 9. ドキュメント更新と開発者体験整備  
  - ファイル: `docs/testing/junit5-integration.md`（新設）、`docs/cli-reference.md`、`.project-todolist/phase2-checklist.md` 12.1 セクション、`jv/crates/jv_cli/src/tour/sections/build_tools.rs`  
  - 内容: テスト DSL の構文、生成 Java 例、`jv test` のワークフロー、`@Sample` 共有ルール、命名正規化ポリシーをドキュメント化する。チェックリストに着手/完了マーカーを追加し、ユーザーガイドに手順を記載する。  
  - 目的: 非機能要件（ユーザビリティ・ドキュメント）を満たす。  
  - 参照: 設計書「Steering Document Alignment」「Testing Strategy」、要求1〜3。  
  - 役割/プロンプト: テクニカルライター | CLI メッセージと一致する説明を用い、サンプルコードを日本語で記載する。  
  - 成功条件: ドキュメントに必要情報が整備され、フェーズチェックリストが更新される; structures, documentation/comments added where necessary。  
  - _Instructions: Mark this task as [-] in tasks.md when you start it and change to [x] after completing and validating the work._
