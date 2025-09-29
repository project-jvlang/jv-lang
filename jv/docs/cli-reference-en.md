# jv CLI Reference

**English** | [日本語](cli-reference.md)

Complete reference for the jv command-line interface.

## Overview

The `jv` command is the primary interface for working with jv projects. It provides tools for project management, compilation, and development.

```
jv [COMMAND] [OPTIONS] [ARGUMENTS]
```

Notes:
- Running `jv` with no arguments starts the interactive REPL.
- You can also start it explicitly with `jv repl`.
- A separate `jvx` binary is available for quick execution of files/snippets.

## Global Options

- `-h, --help`: Show help information
- `-V, --version`: Show version information
- `--verbose`: Enable verbose output
- `--quiet`: Suppress non-essential output

## Commands

### `jvx`

Standalone quick executor for jv files or snippets.

```bash
# Execute a file
jvx src/main.jv -- arg1 arg2

# Execute inline snippet
jvx 'fun main(){ println("Hi") }'

# Read from stdin
echo 'fun main(){ println("stdin") }' | jvx -
```

Notes:
- Mirrors the behavior of `jv x`/`jv exec` but as a separate binary.
- Useful for shebang-style or pipeline workflows.

<!-- Quick exec is provided via standalone `jvx` binary, not `jv exec`. -->

### `jv repl`

Start an interactive Read–Eval–Print Loop.

```bash
jv repl
# or simply
jv
```

Features:
- Parses jv snippets interactively and reports parse errors.
- Meta commands: `:help`, `:quit`.
- Future: Java preview and execution hooks.

### `jv init`

Initialize a new jv project in the current directory.

```bash
jv init [OPTIONS] [PROJECT_NAME]
```

**Options:**
- `--template <TEMPLATE>`: Use a specific project template
  - `basic` (default): Simple console application
  - `library`: Library project structure
  - `web`: Web application template
- `--package <PACKAGE>`: Set the package name
- `--jdk <VERSION>`: Specify target JDK version (default: 25)

**Examples:**
```bash
# Initialize basic project in current directory
jv init

# Initialize with project name
jv init my-project

# Initialize library project
jv init --template library my-lib

# Initialize with specific package
jv init --package com.example.myapp
```

**Generated Structure:**
```
project/
├── jv.toml          # Project configuration
├── src/
│   └── main.jv      # Main source file
├── test/
│   └── main_test.jv # Test file
└── README.md        # Project documentation
```

### `jv build`

Compile jv source code to Java and generate bytecode.

```bash
jv build [OPTIONS] [FILES...]
```

**Options:**
- `--check`: Check code without generating output
- `--format`: Format generated Java code
- `--preview`: Show generated Java without compiling
- `--output <DIR>`: Output directory (default: `out/`)
- `--target <DIR>`: Target directory for compiled classes
- `--classpath <PATH>`: Additional classpath entries
- `--jdk <VERSION>`: Target JDK version
- `--optimization <LEVEL>`: Optimization level (0-3)
- `--debug`: Include debug information
- `--perf`: Collect AST→IR performance metrics and persist `target/perf-reports/ast-ir-phase1.json`
- `--emit-types`: Emit inferred type facts as JSON to stdout (implicitly enables `--check`)
- `--emit-telemetry`: Print inference telemetry summary to stdout (implicitly runs `--check`)
  
#### Performance Mode (`--perf`)

Passing `--perf` instructs the build to profile the AST→IR lowering pipeline. The command records parse time, lowering time, allocator reuse ratio, and peak RSS (when available) into a JSON report. The hard limits are 3,000ms / 100MB / reuse ratio ≥ 0.90. If the run misses any budget, the CLI exits with a non-zero status and prints the failing checks.

- Output: `target/perf-reports/ast-ir-phase1.json`
- Schema: `jv_support::perf::report::PerfReport`
- Consumers: Local runs, CI workflow `perf_phase1`

For interpreting results, updating baselines, and triage instructions see the [AST→IR Performance Baseline Guide](perf-baselines-en.md). On failure, check the `checks` section in the JSON as well as the freshness of the stored baseline.

```bash
# Collect AST→IR metrics while previewing the generated Java
jv build --perf --preview
```

#### Type Facts Output (`--emit-types`)

Passing `--emit-types` serialises the `TypeFacts` snapshot as pretty JSON to stdout. The flag implicitly enables `--check`, so type inference always runs before Java generation.

```bash
jv build src/app/main.jv --java-only --check --emit-types
Output directory: ...
{
  "environment": {
    "greeting": "Primitive(\"String\")"
  },
  "bindings": [ ... ],
  "schemes": { ... },
  "node_types": { ... }
}
```

Our integration tests (`cargo test -p jv_checker`, `cargo test -p jv_inference`) assert that the emitted snapshot reflects code edits—rebuilding the same plan after adding `val incremented = base + 1` exposes the new binding immediately in the JSON output.

#### Inference Telemetry (`--emit-telemetry`)

Enable `--emit-telemetry` to get a concise telemetry snapshot of the most recent inference pass. The current release emits the following fields:

- `constraints_emitted`: Number of constraints produced by the generator
- `bindings_resolved`: Count of type variables resolved by the solver
- `inference_duration_ms`: Wall-clock time spent in inference (milliseconds)
- `preserved_constraints`: Constraints reused by incremental inference (currently 0 until incremental mode lands)
- `cache_hit_rate`: Hit ratio for the inference cache (reported when available, otherwise `n/a`)
- `invalidation_cascade_depth`: Depth of cache invalidation cascades (0 when no incremental data is present)

```bash
jv build src/app/main.jv --check --emit-telemetry
Telemetry (src/app/main.jv):
  constraints_emitted: 42
  bindings_resolved: 37
  inference_duration_ms: 3.215
  preserved_constraints: 0
  cache_hit_rate: n/a
  invalidation_cascade_depth: 0
```

As incremental inference evolves, the preserved constraint count and cache hit rate will begin to reflect real measurements. For now, zero or `n/a` indicates that those signals are not yet collected for the current build.

Sample/@Sample options:
- `--sample-mode=embed|load`: Override default `@Sample` mode (default: embed)
- `--sample-network=allow|deny`: Allow network access for sample fetching (default: deny)
- `--sample-embed-max-bytes=<N>`: Max bytes to embed in code (default: 1048576)
- `--sample-embed-fallback=load|fail`: Behavior when embed exceeds limit (default: fail)
- `--sample-cache=on|off`: Enable local cache for runtime loads (default: on)

Binary packaging options:
- `--binary jar|native`: Produce a single-file artifact after compilation.
  - `jar`: Creates `<output>/app.jar` (or `--bin-name`). Requires `jar` tool.
  - `native`: Attempts GraalVM `native-image` to produce a native binary in `<output>`. Errors if not installed.
- `--bin-name <NAME>`: Output base name for the artifact (default: `app`).

**Examples:**
```bash
# Build entire project
jv build

# Build specific files
jv build src/main.jv src/utils.jv

# Build with formatting and preview
jv build --format --preview

# Build with debug information
jv build --debug

# Build and check only (no output)
jv build --check

# Build and package into a single JAR
jv build src/main.jv --binary jar --bin-name myapp

# Build and attempt native image (requires GraalVM native-image)
jv build src/main.jv --binary native --bin-name myapp
```

**Build Process:**
1. **Lexical Analysis**: Tokenize jv source files
2. **Parsing**: Generate Abstract Syntax Tree (AST)
3. **Type Checking**: Validate types and perform inference
4. **IR Generation**: Convert to Intermediate Representation
5. **Java Generation**: Generate readable Java 25 source code
6. **Java Compilation**: Compile with `javac` to bytecode

### `jv run`

Run a compiled jv program.

```bash
jv run [OPTIONS] [MAIN_CLASS] [PROGRAM_ARGS...]
```

**Options:**
- `--classpath <PATH>`: Additional classpath entries
- `--jvm-args <ARGS>`: Arguments to pass to the JVM
- `--main <CLASS>`: Specify main class to run
- `--args <ARGS>`: Arguments to pass to the program

**Examples:**
```bash
# Run main class
jv run

# Run with JVM arguments
jv run --jvm-args "-Xmx1g -Xms512m"

# Run specific class
jv run --main com.example.MyApp

# Run with program arguments
jv run --args "arg1 arg2 arg3"

# Combined example
jv run --main MyApp --jvm-args "-Xmx2g" --args "input.txt output.txt"
```

### `jv debug --stage ir --emit ast`

Reconstructs AST artifacts from desugared IR and emits diagnostics for debugging the pipeline.

```bash
jv debug --stage ir --emit ast \\
  --input build/fixtures/sample.ir.json \\
  --output - \\
  --format json
```

Key options:
- `--stage ir`: Selects the IR stage (currently the only supported stage).
- `--emit ast`: Emits the reconstructed AST artifact.
- `--input <PATH>`: IR JSON source. Use `-` (or omit) to read from stdin.
- `--output <PATH>`: Destination for the artifact. Use `-` for stdout or provide a file path.
- `--format json|pretty`: Choose JSON or human-readable output (`json` by default).
- `--no-stats`: Suppress the summary footer.
- `--fail-on-timeout <ms>`: Exit with a non-zero status if reconstruction exceeds the timeout.

See the golden snapshot at `jv/crates/jv_cli/tests/fixtures/debug_ir_expected.json` for a canonical JSON example. With `--format pretty`, the tool prints statistics, warning summaries, and the AST in a readable layout while writing the summary footer to stderr by default.

Regression coverage lives in `jv/crates/jv_cli/tests/debug_ir.rs`. Run `cargo test -p jv_cli` to execute the suite.

### `jv fmt`

Format jv source code according to standard style guidelines.

```bash
jv fmt [OPTIONS] [FILES...]
```

**Options:**
- `--check`: Check if files are formatted without modifying them
- `--diff`: Show differences that would be made
- `--write`: Write changes to files (default behavior)
- `--indent <SIZE>`: Indentation size (default: 4)
- `--line-width <WIDTH>`: Maximum line width (default: 100)
- `--trailing-comma`: Always use trailing commas

**Examples:**
```bash
# Format all jv files in project
jv fmt

# Format specific files
jv fmt src/main.jv src/utils.jv

# Check formatting without changes
jv fmt --check

# Show formatting differences
jv fmt --diff

# Format with custom line width
jv fmt --line-width 80
```

**Formatting Rules:**
- **Indentation**: 4 spaces (configurable)
- **Line Length**: 100 characters (configurable)  
- **Braces**: K&R style (opening brace on same line)
- **Spacing**: Consistent spacing around operators
- **Import Organization**: Automatic import sorting and deduplication

### `jv check`

Perform static analysis and type checking on jv source code.

```bash
jv check [OPTIONS] [FILES...]
```

**Options:**
- `--strict`: Enable strict checking mode
- `--null-safety`: Enable enhanced null safety checks
- `--unused`: Check for unused variables and imports
- `--performance`: Check for performance anti-patterns
- `--security`: Enable security vulnerability checks
- `--json`: Output results in JSON format
- `--fix`: Automatically fix issues where possible

**Examples:**
```bash
# Basic type checking
jv check

# Strict checking with all warnings
jv check --strict --unused --performance

# Check specific files
jv check src/main.jv src/models/

# Security-focused checking
jv check --security --strict

# Get machine-readable output
jv check --json > check-results.json
```

**Check Categories:**
- **Type Safety**: Type mismatches, null safety violations
- **Code Quality**: Unused variables, unreachable code, complexity
- **Performance**: Inefficient patterns, memory leaks
- **Security**: SQL injection, XSS vulnerabilities, insecure patterns
- **Style**: Naming conventions, code organization

### `jv version`

Display version information for jv and its components.

```bash
jv version [OPTIONS]
```

**Options:**
- `--verbose`: Show detailed version information
- `--json`: Output version information in JSON format

**Examples:**
```bash
# Show basic version
jv version
# Output: jv 0.1.0 - Java Syntactic Sugar compiler

# Show detailed version information
jv version --verbose
# Output:
# jv 0.1.0 - Java Syntactic Sugar compiler
# Built: 2024-09-05
# Rust: 1.70.0
# Target: Java 25
# Features: null-safety, async, pattern-matching

# JSON output
jv version --json
```

## Configuration

### Project Configuration (`jv.toml`)

```toml
[project]
name = "my-project"
version = "1.0.0"
authors = ["Your Name <you@example.com>"]
description = "A jv project"
license = "MIT"

[build]
jdk_version = "25"
optimization_level = 2
debug = false
target_dir = "out"

[dependencies]
# jv registry dependencies
math-utils = "1.2.0"

# Maven dependencies  
[dependencies.maven]
"org.apache.commons:commons-lang3" = "3.12.0"
"com.fasterxml.jackson.core:jackson-core" = "2.15.0"

[dev-dependencies]
junit = "5.9.0"

[toolchain]
jdk = "temurin-25"
prefer_system = false

[format]
line_width = 100
indent_size = 4
trailing_comma = true

[check]
strict = true
null_safety = true
unused_warnings = true
performance_warnings = true
```

### Global Configuration

Global jv configuration is stored in:
- **Linux/macOS**: `~/.config/jv/config.toml`
- **Windows**: `%APPDATA%\jv\config.toml`

```toml
[defaults]
jdk_version = "25"
editor = "code"
format_on_save = true

[toolchain]
auto_install = true
preferred_vendor = "temurin"

[registry]
default = "https://registry.jv-lang.org"
mirrors = [
    "https://mirror1.jv-lang.org",
    "https://mirror2.jv-lang.org"
]

[check]
default_strict = true
auto_fix = false
```

## Environment Variables

- `JV_HOME`: jv installation directory
- `JV_REGISTRY`: Override default registry URL
- `JV_JDK_HOME`: Override JDK detection
- `JV_LOG_LEVEL`: Set logging level (debug, info, warn, error)
- `JV_NO_COLOR`: Disable colored output
- `JV_OFFLINE`: Work in offline mode (no network requests)

## Exit Codes

- `0`: Success
- `1`: General error
- `2`: Compilation error
- `3`: Runtime error
- `4`: Configuration error
- `5`: Network error
- `101`: Internal error (please report as bug)

## Shell Completions

Generate shell completions for your shell:

```bash
# Bash
jv completion bash > ~/.local/share/bash-completion/completions/jv

# Zsh  
jv completion zsh > ~/.local/share/zsh/site-functions/_jv

# Fish
jv completion fish > ~/.config/fish/completions/jv.fish

# PowerShell
jv completion powershell > $PROFILE
```

## Integration with IDEs

### VS Code

Install the jv extension for VS Code:
- Syntax highlighting
- Error diagnostics
- Code formatting
- IntelliSense support

### IntelliJ IDEA

The jv language server provides:
- Code completion
- Error highlighting  
- Refactoring support
- Debugging integration

### Vim/Neovim

Use the `jv-vim` plugin or configure with LSP clients like `nvim-lspconfig`.

## Troubleshooting

### Common Issues

**"jv: command not found"**
- Ensure jv is in your PATH
- Try running with full path: `./target/release/jv`

**"Java 25 required but Java X found"**
- Install Java 25 or set `JAVA_HOME`
- Use `jv toolchain install` to manage JDK versions

**Build fails with classpath errors**
- Check dependencies in `jv.toml`
- Verify Maven dependencies are available

**Performance issues**
- Use `--optimization 3` for release builds
- Check for performance warnings with `jv check --performance`

### Debug Mode

Enable verbose logging for troubleshooting:

```bash
JV_LOG_LEVEL=debug jv build --verbose
```

### Getting Help

- Check the [GitHub Issues](https://github.com/project-jvlang/jv-lang/issues)
- Join the community [Discussions](https://github.com/project-jvlang/jv-lang/discussions)
