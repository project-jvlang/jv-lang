# ir-java-codegen Release Notes (0.1.0)

## Summary

The ir-java-codegen milestone completes the desugaring pipeline from the jv AST to Java 25 source code. Transform context management, diagnostic error reporting, Java emission, source mapping, and CLI orchestration are now production-ready.

## Highlights

- Scope-aware `TransformContext` with reusable APIs across all transform modules.
- Rich `TransformError` diagnostics with actionable guidance and precise `Span` metadata.
- Finalized IR definitions and Java code generation for records, pattern matching, virtual threads, async flows, and resource management constructs.
- Source mapping support surfaced through `jv_mapper` for IDE tooling.
- End-to-end CLI workflows (`jv_cli`) wired to lexer → parser → IR → codegen → javac validation.
- Expanded unit, integration, and snapshot tests in `jv_ir`, `jv_codegen_java`, `jv_mapper`, and `jv_cli`.

## Quality Gates

- `cargo fmt --all`
- `cargo test -p jv_ir -p jv_codegen_java -p jv_mapper -p jv_cli`

## Upgrade Guidance

- Update to version `0.1.0` to pick up all completed IR and Java codegen features.
- Review the enriched diagnostics coming from the new `TransformError` variants when triaging failed builds.
- Regenerate snapshots after updating if you maintain downstream forks with custom IR nodes.
