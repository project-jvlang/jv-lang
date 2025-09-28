# jv Documentation

**English** | [日本語](README.md)

Welcome to the jv (Java Syntactic Sugar) documentation.

## Table of Contents

### User Guide
- [Getting Started](getting-started-en.md) - Installation and first steps
- [Language Guide](language-guide-en.md) - Complete jv language reference
- [CLI Reference](cli-reference-en.md) - Command-line interface documentation
- [Project Structure](project-structure-en.md) - How to organize jv projects

### Developer Guide
- [Architecture](architecture-en.md) - Compiler architecture overview
- [Contributing](contributing-en.md) - How to contribute to jv
- [AST→IR Performance Baseline Guide](perf-baselines-en.md) - Operating the performance harness and managing baselines
- Build system documentation (coming soon)
- Testing guide (coming soon)

### Reference
- [Language Specification](language-spec-en.md) - Formal jv language specification
- [Java Interop](java-interop-en.md) - Working with Java libraries
- Error reference (coming soon)
- Migration guide (coming soon)

### Examples
- [Examples](../examples/) - Sample jv programs
- Advanced examples (coming soon)
- Best practices (coming soon)

## About jv

**jv** (pronounced /jawa/, jv-lang is pronounced /jawa læŋ/) is Java syntactic sugar that compiles to readable Java 25 source code. It provides Kotlin-style syntax sugar while maintaining zero runtime dependencies and full JVM compatibility.

jv transpiles modern, concise syntax into pure Java 25 source code, giving you the best of both worlds: developer productivity through Kotlin-like features and seamless integration with the Java ecosystem.

### Overview
- **Target**: Java 25 LTS
- **Output**: Pure Java source code (no additional runtime)
- **Implementation**: Rust-based compiler toolchain
- **Philosophy**: Zero runtime overhead, maximum compatibility

### Key Features
- **Zero Runtime**: Output is pure Java 25 source code
- **Static Typing**: No dynamic dispatch, fully static
- **Java 25 Optimization**: Leverages records, pattern matching, virtual threads
- **Unified Environment**: Manages dependencies, JDK, and JRE

## Quick Links

- **GitHub Repository**: [project-jvlang/jv-lang](https://github.com/project-jvlang/jv-lang)
- **Issue Tracker**: [GitHub Issues](https://github.com/project-jvlang/jv-lang/issues)
- **Community**: [Discussions](https://github.com/project-jvlang/jv-lang/discussions)

For the full project introduction, see the top-level [README](../../README-en.md).
