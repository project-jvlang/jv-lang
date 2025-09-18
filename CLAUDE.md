# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**jv** is a Java Sugar Language that compiles to readable Java 25 source code. It provides Kotlin-style syntax sugar while maintaining zero runtime dependencies and full JVM compatibility.

- **Language**: jv (Java Sugar Language)
- **File Extension**: `.jv`
- **Target**: Java 25 LTS
- **Implementation**: Rust workspace with multiple crates
- **Output**: Pure Java 25 source code (no additional runtime)

## Architecture

This is a Rust workspace containing multiple crates for building a transpiler and toolchain:

### Core Crates Structure
```
crates/
├─ jv_lexer         # Lexical analysis
├─ jv_parser        # Syntax parsing (using chumsky)
├─ jv_ast           # Abstract Syntax Tree definitions
├─ jv_ir            # Intermediate representation for desugaring
├─ jv_codegen_java  # Java 25 code generation
├─ jv_mapper        # Source maps (.jv ↔ .java)
├─ jv_checker       # Static analysis (null safety, forbidden syntax)
├─ jv_fmt           # Code formatter
├─ jv_pm            # Package manager
├─ jv_build         # Build system and javac integration
├─ jv_lsp           # Language Server Protocol implementation
└─ jv_cli           # CLI entry point (jv/jvlang commands)
```

### Key Technical Components
- **Parser**: Uses `chumsky` for parsing
- **Code Generation**: Custom Java code generator (JavaPoet equivalent in Rust)
- **CLI Framework**: `clap`, `anyhow`, `indicatif`
- **LSP**: `tower-lsp`
- **Dependency Resolution**: PubGrub algorithm
- **JDK Management**: Integration with Adoptium/Corretto/GraalVM APIs

## Development Commands

Since this is a Rust project, use standard Rust toolchain commands:

```bash
# Build the entire workspace
cargo build

# Run specific binary (e.g., CLI)
cargo run --bin jv_cli

# Run tests
cargo test

# Check code without building
cargo check

# Format code
cargo fmt

# Lint code
cargo clippy
```

## jv Language Commands (when implemented)

The target CLI commands that this project will implement:

```bash
jv init                # Create new project
jv add <pkg>           # Add dependencies (jv registry or maven)
jv build [--preview]   # Build to Java (calls javac --release 25)
jv run                 # Execute
jv test                # Run tests (JUnit integration)
jv fmt                 # Format jv source files
jv lint                # Static analysis
jv toolchain install   # Install JDK versions
jv toolchain list      # List available JDKs
jv use <jdk>           # Set JDK for project
jv jlink               # Generate minimal JRE
jv publish             # Publish to registry
jv doctor              # Environment diagnostics
```

## Language Features

jv transpiles the following sugar syntax to pure Java 25:
- `val/var` with type inference
- Null safety operators: `?`, `?.`, `?:`
- `when` expressions → Java switch/pattern matching
- `data class` → records (immutable) or classes (mutable)
- Extension functions → static methods
- String interpolation `"Hello, ${name}"`
- `spawn {}` → Virtual threads
- `async {}.await()` → CompletableFuture
- `use {}/defer {}` → try-with-resources/finally
- Default/named arguments → method overloads
- Top-level functions → utility classes

## Package Management

- **Declaration**: `jv.toml`
- **Lock file**: `jv.lock` (dependencies + JDK versions)
- **Registry**: Custom jv registry + Maven bridge
- **JDK Management**: Automated JDK installation and project pinning
- **Security**: CVE checking with `jv audit`

## Project Status

This is a language specification and implementation plan. The project is designed to be built over 13 weeks following the roadmap in the specification document.