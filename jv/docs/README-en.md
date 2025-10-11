# jv Documentation

**English** | [日本語](README.md)

Welcome to the jv (Java Syntactic Sugar) documentation.

## Table of Contents

### User Guide
- [Getting Started](getting-started-en.md) - Installation and first steps
- [Language Guide](language-guide-en.md) - Complete jv language reference
- [CLI Reference](cli-reference-en.md) - Command-line interface documentation
- [Project Structure](project-structure-en.md) - How to organize jv projects
- [Pattern Matching Guide](pattern-matching-en.md) - when expressions and pattern matching
- [JSON Literals and POJO Generation](language/json-en.md) - Native JSON support

### Language Features
- [Whitespace Arrays and Arguments](whitespace-arrays-en.md) - Contextual parsing and homogeneous sequences
- [Testing Framework Integration](testing-frameworks-en.md) - JUnit, Testcontainers, Playwright, etc. (coming soon)
- [Logging Framework Integration](logging-frameworks-en.md) - SLF4J, Log4j2, etc. (coming soon)
- [DSL Embedding System](dsl-embedding-en.md) - Integrating custom DSLs (coming soon)
- [Unit System](unit-system-en.md) - Type-safe physical quantities (coming soon)
- [Math System](math-system-en.md) - Complex numbers, rationals, matrices (coming soon)
- [Regex Support](regex-support-en.md) - Regex literals and validation (coming soon)

### Developer Guide
- [Architecture](architecture-en.md) - Compiler architecture overview
- [Contributing](contributing-en.md) - How to contribute to jv
- [AST→IR Performance Baseline Guide](perf-baselines-en.md) - Operating the performance harness and managing baselines
- [Android SDK Non-Support Policy](android-policy-en.md) - Why Android is not supported and alternatives
- [ir-java-codegen Release Notes](release-notes-ir-java-codegen-en.md) - 0.1.0 milestone
- Build system documentation (coming soon)

### Reference
- [Language Specification](language-spec-en.md) - Formal jv language specification
- [Java Interop](java-interop-en.md) - Working with Java libraries
- [Java Target Matrix](java-target-matrix.md) - Java 21/25 feature support matrix
- [Annotation Specification](sample-annotation-en.md) - @Sample annotation
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
- **Zero Runtime**: Output is pure Java 25 source code (Java 21 compatible output also supported)
- **Static Typing**: No dynamic dispatch, fully static
- **Java 25 Optimization**: Leverages records, pattern matching, virtual threads
- **Unified Environment**: Manages dependencies, JDK, and JRE
- **Unified Control Flow**: when expressions for conditionals, for loops for iteration
- **Native JSON Support**: Auto-generate POJOs from JSON literals with comments
- **Testing Framework Integration**: Seamless integration with JUnit, Testcontainers, Playwright, etc.
- **DSL Embedding**: Type-safe integration of external DSLs like SQL, YAML, Drools
- **Unit System**: Dimensional analysis and compile-time validation for physical quantities

## Quick Links

- **GitHub Repository**: [project-jvlang/jv-lang](https://github.com/project-jvlang/jv-lang)
- **Issue Tracker**: [GitHub Issues](https://github.com/project-jvlang/jv-lang/issues)
- **Community**: [Discussions](https://github.com/project-jvlang/jv-lang/discussions)

For the full project introduction, see the top-level [README](../../README-en.md).
