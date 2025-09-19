# Changelog

## [0.1.0] - 2025-09-19

### Added
- Completed the IR transform pipeline with scoped context management and precise error diagnostics.
- Finalized Java 25 code generation covering records, pattern matching, virtual threads, and async flows.
- Delivered source mapping support and end-to-end CLI integration across the toolchain.
- Extended automated test coverage with IR, codegen, mapper, and CLI verification suites.

### Quality
- Ran `cargo fmt --all` and targeted tests via `cargo test -p jv_ir -p jv_codegen_java -p jv_mapper -p jv_cli` to verify the release surface.
- Updated documentation to reflect the ir-java-codegen milestone and link the new release notes.
