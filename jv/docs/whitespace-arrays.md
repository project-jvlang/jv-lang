# Whitespace Arrays and Arguments

The contextual parsing feature lets you express homogeneous sequences without commas. This page explains how whitespace-delimited arrays and call arguments behave across the toolchain.

## Syntax overview

- **Array literals**: `val numbers = [1 2 3]`
- **Call arguments**: `plot(temperature readings average)` groups the positional arguments by layout.
- **Trailing lambdas**: `plot(1 2) { sample -> sample * sample }` keeps the lambda as a distinct argument.

Use layout-aware sequences only when every element shares the same type. Mixing named arguments or commas will emit diagnostics.

## Diagnostics

| Code   | Description                                                           | Fix                                                                 |
| ------ | --------------------------------------------------------------------- | ------------------------------------------------------------------- |
| JV1007 | Mixed whitespace and comma delimiters in an array literal             | Stick to commas only or remove every comma to rely on whitespace.  |
| JV1009 | Named arguments or incompatible types inside a whitespace call group  | Convert the call back to comma-delimited form or reorder arguments. |

The `jv check` command fails when either diagnostic appears, returning a non-zero exit code so CI can detect the regression.

## Tooling integration

- **CLI**: `jv build path/to/file.jv --java-only` emits Java sources that render whitespace arrays as `List.of(...)` when modern features are enabled, or `Arrays.asList(...).stream().toList()` otherwise.
- **Checker**: Layout-aware sequences flow through the checker’s `SequenceStyleCache`, so repeated invocations remain fast while keeping cache entries isolated per compilation.
- **Benchmarks**: Run `cargo bench -p jv_parser sequence_layout_bench` to observe parser + transform performance for whitespace-heavy programs.

## Tips

- Prefer whitespace sequences for short, homogeneous literals; long lists remain easier to read with commas.
- Keep unit and integration tests in sync by covering both whitespace and comma scenarios.
- Regenerate CLI snapshots when diagnostic messages change to avoid stale expectations.
