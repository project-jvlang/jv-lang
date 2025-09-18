# @Sample – Sample Data Retrieval and Automatic Type Inference (Design)

**English** | [日本語](sample-annotation.md)

## Purpose
Automatically infer static types from sample data based on declarations like `@Sample("users.json") val users`, providing end-to-end processing from sample data to Java 25 source generation. Users can obtain safely typed code and initial data by simply placing small samples.

## Overview
- Supported schemas: File paths (relative/absolute), `file://`, `https://`, `s3://`
- Operation mode: Switch with `mode` option (default: `Embed`)
  - `Embed` (default): Retrieve and analyze sample data at compile time, embed into generated code
  - `Load`: Generate code (loader) that loads sample data at runtime
- Additional options (optional):
  - `format`: `json|csv|tsv|auto` (default `auto`, inferred from extension/Content-Type)
  - `sha256`: Content verification hash (replay/tampering prevention)
  - `limitBytes`: Embedding limit (default 1 MiB, only for `Embed`)

Examples:
```
@Sample("users.json") val users             // => mode=Embed (default)
@Sample("https://example.com/users.json", mode=Load) val users
@Sample(source="s3://bucket/key.json", mode=Embed, sha256="<hex>") val users
```

## Type Inference
- JSON objects → Generate `record` (immutable) or regular class (mutable) (jv uses data class → Java record as basis)
- JSON arrays → Infer as `List<T>`. Element unions compose least upper bound with structural commonality (missing fields represented as `Optional<T>`/nullable)
- Primitive inference:
  - Integers: `int`/`long`/`BigInteger` (selected by range)
  - Decimals: `double`/`BigDecimal` (selected by digits/precision)
  - String/boolean/null → `String`/`boolean`/`Optional<T>` (or nullable)
- CSV/TSV infers first row header as field names, subsequent rows as record columns. Types determined from entire column.

## Embed (Compile-time Embedding)
- Retrieve data at compile time → parse → serialize and embed into Java source
- Large volume countermeasure: Build failure on `limitBytes` excess or automatic fallback to `Load` (`--sample-embed-fallback=load|fail`)
- Generated artifacts:
  - `record`/class groups corresponding to inferred schema
  - Initialization code like `public static final List<User> users = List.of( … );`
- Network default: Disabled by default. Explicitly allow with `--sample-network=allow` or `jv.toml`. Recommend `sha256` when using `https://`/`s3://`.

## Load (Runtime Loading)
- Generated code uses the following loaders:
  - `file://`/path: `java.nio.file.Files`
  - `https://`/`http://`: Java 11+ `HttpClient`
  - `s3://`: Not bundled by default (zero-dependency policy). Only integrated when `jv_s3` optional extension is available. Recommended to issue presigned URLs in advance and use `https://`.
- Exception/retry: Simple retry (exponential backoff) and explicit throwing of `IOException`/`HttpTimeoutException`
- Cache: Optional disk cache at `~/.cache/jv/sample/<sha256>` (`--sample-cache=on|off`)

## Security
- Default: Network retrieval disabled (both `Embed`/`Load`)
- Support `sha256` pinning (error on mismatch)
- `s3://` not bundled by default due to credential handling. Explicit introduction via optional extension only when needed.

## Configuration/CLI
- Annotation:
  - `@Sample(source: String, mode: Embed|Load = Embed, format: String = "auto", sha256: String? = null, limitBytes: Int? = null)`
- `jv.toml` (example):
```
[sample]
mode = "embed"               # default: embed|load
allow_network = false        # default: false
embed_max_bytes = 1048576    # default: 1 MiB
fallback_on_oversize = "fail"  # fail|load
cache = true
```
- CLI flags (`jv build`):
  - `--sample-mode=embed|load` (override default)
  - `--sample-network=allow|deny` (default deny)
  - `--sample-embed-max-bytes=N`
  - `--sample-embed-fallback=load|fail`
  - `--sample-cache=on|off`

## Implementation Approach (Stages)
1) Introduce annotation representation to parser/AST (`@Name(args)` attached to `val/var/class` etc.)
2) Detect `@Sample` in IR → pass sample source to type inference phase
3) Implement type inferrer (JSON/CSV), generate record/class definitions and initialization expressions
4) `Embed` path: Embed initialization expressions into Java AST
5) `Load` path: Generate loader (file/http) with minimal dependencies, deserialize → map to types
6) CLI/configuration integration, security (network permission/hash verification/capacity limits/cache)

## Error Design
- Source unreachable/404/permission errors: Output diagnostics with specific URI and hints
- Type mismatch (significant inconsistency in array elements): Present least upper bound inference failure reasons, allow mitigation with `--sample-infer=loose|strict`
- Embedding size excess: Recommend countermeasures (subsetting, switch to `Load`, increase `embed_max_bytes`)

## Examples
```
// Embed local JSON with default (Embed)
@Sample("data/users.json") val users

// Load from HTTP at runtime
@Sample("https://example.com/users.json", mode=Load) val users

// Retrieve s3 at compile time with hash verification and embed
@Sample(source="s3://my-bucket/users.json", mode=Embed, sha256="ab12…") val users
```

---
This specification provides advance definition of documentation and CLI consensus interfaces, with language-side (AST/IR/code generation) implementation to be added in stages.