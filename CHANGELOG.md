# Changelog

## [0.1.1] - 2025-10-09

### Added
- Kotlinスタイルの遅延SequenceコレクションAPIを有効化し、Java 25/21 両ターゲットでの`Stream`ブリッジをサポート。

### Documentation
- `docs/stdlib/collections.md` を追加し、暗黙Sequenceチェーン、フォールバック方針、ラムダ記法ルールを整理。
- CLIヘルプにSequenceパイプラインの概要とStream再利用禁止ポリシーを追記。
- v0.1 Alphaリリースノートを更新し、KotlinスタイルコレクションAPI導入を告知。

## [0.1.0] - 2025-09-19

### Added
- Completed the IR transform pipeline with scoped context management and precise error diagnostics.
- Finalized Java 25 code generation covering records, pattern matching, virtual threads, and async flows.
- Delivered source mapping support and end-to-end CLI integration across the toolchain.
- Extended automated test coverage with IR, codegen, mapper, and CLI verification suites.

### Quality
- Ran `cargo fmt --all` and targeted tests via `cargo test -p jv_ir -p jv_codegen_java -p jv_mapper -p jv_cli` to verify the release surface.
- Updated documentation to reflect the ir-java-codegen milestone and link the new release notes.
