# Tasks Document

- [x] 1. stdlib Sequence API を SequenceCore/SequenceFactory/拡張関数へ移行  
  - File: `jv/stdlib/collections/sequence.jv`  
  - 既存 `class Sequence` と `companion` を削除し、`data SequenceCore`・`object SequenceFactory`・`SequenceCore` 拡張関数群に再構成する  
  - 既存の map/filter/reduce/take/drop/flatMap/sorted/sortedBy/toList/fold/count/sum 等のロジックを拡張関数へ移植し API シグネチャを維持  
  - `SequenceFactory.fromIterable/fromStream` を導入し、 doc コメントで内部用途とゼロランタイム方針を明記  
  - Dependencies: なし（基盤タスク）  
  - _Leverage: 既存 `Sequence<T>` 実装、`StreamSupport` ユーティリティ_  
  - _Requirements: 要件1, 要件3_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: 標準ライブラリ開発者（Stream API 変換に精通） | Task: `jv/stdlib/collections/sequence.jv` を Kotlin 風クラスから `SequenceCore` データ構造＋ `SequenceFactory` トップレベルオブジェクト＋ `SequenceCore` 拡張関数へ移行し、既存 API ロジックを維持する | Restrictions: Kotlin 風構文を残さない、Java 以外のランタイム依存を追加しない、命名規則を既存 stdlib に合わせる | _Leverage: `Sequence<T>` 既存メソッド実装、`StreamSupport` | _Requirements: 要件1, 要件3 | Success: `sequence.jv` が data/オブジェクト/拡張関数構成へ置き換わり、既存テストがコンパイルする_

- [x] 2. Sequence stdlib テスト更新  
  - File: `jv/stdlib/collections/tests/sequence_spec.jv`  
  - 新しい API 名（`SequenceCore` と `SequenceFactory.fromIterable` など）に合わせてテストを更新  
  - 追加で `fun main` サンプルコード相当のケースをカバーし、単段 map/filter/reduce の動作を確認  
  - Dependencies: タスク1  
  - _Leverage: 既存テストケース、提供サンプルコード_  
  - _Requirements: 要件1, 要件2_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: テストエンジニア（jv stdlib 拡張） | Task: `sequence_spec.jv` を新 API に合わせて更新し、単段パイプラインの回帰確認を追加する | Restrictions: 不要なテスト削除は避ける、期待する Java 出力には変更を反映させる | _Leverage: 既存テスト、サンプルコード | _Requirements: 要件1, 要件2 | Success: テストが新 API に基づき成功し、単段操作の挙動が保証される_

- [x] 3. TypeChecker の SequenceCore/拡張関数対応  
  - Files: `jv/crates/jv_typechecker/src/symbols/*.rs`, `.../resolver/*.rs`  
  - `SequenceCore` データ構造および `SequenceFactory` をトップレベルシンボルとして登録  
  - `SequenceCore` に対する拡張関数チェーンが従来通り型推論されるよう `ExtensionRegistry` 相当を更新  
  - `map/filter/reduce` 等の多段チェーンで型推論が破綻しないかユニットテスト追加  
  - Dependencies: タスク1  
  - _Leverage: 既存 `Sequence` シンボル登録ロジック、拡張関数解析コード_  
  - _Requirements: 要件1, 要件2_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: 型検査エンジニア | Task: TypeChecker を更新して `SequenceCore` とその拡張関数が型解決されるようにし、新テストで map/filter 連結の推論を保証する | Restrictions: 既存の他の型シンボルに副作用を与えない、Kotlin 風構文を再導入しない | _Leverage: 既存 Sequence 関連コード, ExtensionRegistry | _Requirements: 要件1, 要件2 | Success: `cargo test` で型検査テストが通り、Sequence チェーンの推論が維持される_

- [ ] 4. IR `PipelineShape` 拡張  
  - Files: `jv/crates/jv_ir/src/sequence/*.rs`  
  - `PipelineShape` に `MultiStage { stages, repeated_transforms, has_terminal }` を導入  
  - 変換ステージ数をカウントし、単段・多段・明示的ソースを区別するロジックを実装  
  - 重複変換（同種 map が複数）を `repeated_transforms` フラグに反映  
  - Dependencies: タスク3（TypeChecker のシンボル形状を前提に IR を生成）  
  - _Leverage: 現行 `SequencePipeline` 構造体と解析ロジック_  
  - _Requirements: 要件2_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: IR アーキテクト | Task: Sequence IR にパイプライン形状解析 (`PipelineShape`) を追加し、単段/多段/繰り返し変換フラグを計算する | Restrictions: 既存 IR API を壊さない、未使用フィールドを残さない | _Leverage: 既存 SequencePipeline ロジック | _Requirements: 要件2 | Success: IR が PipelineShape を保持し、単体テストでステージ判定が検証される_

- [ ] 5. Java CodeGen 分岐と補助クラス生成最適化  
  - Files: `jv/crates/jv_codegen_java/src/sequence/*.rs`  
  - `PipelineShape` を参照し、単段 map/filter/reduce のみの場合は必ず inline Stream チェーンを生成  
  - 変換ステージが 2 回以上、または `repeated_transforms` が true の場合にのみ `JvSequence` 補助クラスを生成  
  - 補助クラス生成が必要な場合の Java 出力テンプレートを更新し、ゼロランタイムを維持  
  - Dependencies: タスク4  
  - _Leverage: 既存 `render_sequence_pipeline` 実装、Java 21 フォールバックハンドリング_  
  - _Requirements: 要件2, 要件3_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: Java コード生成エンジニア | Task: CodeGen を更新して PipelineShape に応じた inline 生成と補助クラス生成最適化を実装する | Restrictions: 不要なクラス出力をしない、Java 21 フォールバックを壊さない | _Leverage: `render_sequence_pipeline`, 既存補助クラス出力 | _Requirements: 要件2, 要件3 | Success: ゴールデンテストで単段ケースが inline 生成、多段ケースでのみ補助クラスが出力される_

- [ ] 6. ゴールデンテスト追加 (`jv_codegen_java`)  
  - Files: `jv/crates/jv_codegen_java/tests/golden/sequence_*.rs`, `.../expected/*.java`  
  - 単段 map の inline Stream チェーン、単段 reduce の inline reduce、明示的 `SequenceFactory.fromIterable` の補助クラス生成をテスト  
  - 多段 map/filter パイプラインで補助クラスが生成されることをゴールデンファイルで確認  
  - Dependencies: タスク5  
  - _Leverage: 既存ゴールデンテスト基盤、追加サンプル_  
  - _Requirements: 要件2, 要件3_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: テストエンジニア（コードジェネレーション） | Task: `jv_codegen_java` のゴールデンテストを追加し、形状ごとの Java 出力を固定化する | Restrictions: ゴールデンファイルは最小限に保つ、テスト命名規則を守る | _Leverage: 既存 golden テスト | _Requirements: 要件2, 要件3 | Success: ゴールデンテストがターゲットケースを網羅し、CI で差分検出できる_

- [ ] 7. CLI / examples 動作確認  
  - Files: `examples/collections_sequence.jv`（新規 or 更新）、`jv_cli` 実行ログ  
  - `fun main` サンプルコードを例として `jv run` で動作確認し、生成 Java が inline Stream チェーンになることを検証  
  - CLI が stdlib を常にビルド対象に含めているか確認し、必要であれば挙動を調整  
  - Dependencies: タスク1, タスク5, タスク6  
  - _Leverage: `jv run`、提供サンプルコード_  
  - _Requirements: 要件2, 要件3_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: CLI / 統合テスト担当 | Task: `jv run` を用いて Sequence サンプルを検証し、必要に応じて CLI 側のビルド対象設定を調整する | Restrictions: CLI の他機能に影響を与えない、生成 Java の確認を記録する | _Leverage: CLI 実行ログ, サンプルコード | _Requirements: 要件2, 要件3 | Success: CLI 実行で期待通りの Java が生成され、ログ確認でゼロランタイムを裏付ける_

- [ ] 8. ドキュメント更新  
  - Files: `docs/stdlib/collections.md`, `README.md`  
  - Sequence API 章を `SequenceCore` / `SequenceFactory` / 拡張関数の説明に置き換え  
  - Kotlin 風構文から jv 固有構文へ移行した旨を記載し、コード例を更新  
  - Dependencies: タスク1, タスク5, タスク6, タスク7  
  - _Leverage: 既存ドキュメント構成、サンプルコード_  
  - _Requirements: 要件1, 要件3_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: ドキュメントエディタ | Task: Sequence 関連ドキュメントを新 API に合わせて更新し、移行方針とゼロランタイムを強調する | Restrictions: 既存スタイルガイドを守る、不要なセクションを削除しない | _Leverage: 現行 docs, README | _Requirements: 要件1, 要件3 | Success: 文書が新 API を反映し、読者が移行手順を理解できる_

- [ ] 9. ワークスペーステスト & 検証  
  - Actions: `cargo test --workspace`, `javac` 実行ログ確認  
  - Sequence 関連ユニットテスト・ゴールデンテストが全て成功することを確認  
  - `javac` ログで追加ランタイム依存がないことを確認し、ゼロランタイム原則を裏付ける  
  - Dependencies: タスク1〜8 完了後  
  - _Leverage: CI スクリプト, `cargo` コマンド_  
  - _Requirements: 要件2, 要件3_  
  - _Prompt: Implement the task for spec stdlib-sequence-rewrite, first run spec-workflow-guide to get the workflow guide then implement the task: Role: リリースエンジニア | Task: ワークスペース全体のテストと `javac` 検証を実施し、Sequence 書き換えによる回帰がないことを確認する | Restrictions: テスト失敗時は原因を特定し解消するまで完了としない | _Leverage: `cargo test`, `javac` ログ | _Requirements: 要件2, 要件3 | Success: ワークスペース全テストが成功し、生成 Java が追加依存なしでコンパイルされる_
