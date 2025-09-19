# jv [/jawa/] - Java Syntactic Sugar

**English** | [日本語](README.md)

**jv** (pronounced /jawa/, jv-lang is pronounced /jawa læŋ/) is Java syntactic sugar that compiles to readable Java 25 source code. It provides Kotlin-style syntax sugar while maintaining zero runtime dependencies and full JVM compatibility.

## Overview

jv transpiles modern, concise syntax into pure Java 25 source code, giving you the best of both worlds: developer productivity with Kotlin-like features and seamless Java ecosystem integration.

- **Target**: Java 25 LTS
- **Output**: Pure Java source code (no additional runtime)
- **Implementation**: Rust-based compiler toolchain
- **Philosophy**: Zero runtime overhead, maximum compatibility

## Features

### Language Features
- `val/var` declarations with type inference
- Null safety operators: `?`, `?.`, `?:`
- `when` expressions → Java switch/pattern matching
- `data class` → records (immutable) or classes (mutable)
- Extension functions → static utility methods
- String interpolation: `"Hello, ${name}"`
- Virtual threads: `spawn {}`
- Async/await: `async {}.await()` → CompletableFuture
- Resource management: `use {}` → try-with-resources
- Default and named arguments → method overloads
- Top-level functions → utility classes

### Toolchain Features
- **Fast compilation** to readable Java 25
- **Source maps** for debugging support
- **Package manager** with jv registry + Maven bridge
- **JDK management** with automatic installation
- **IDE integration** via Language Server Protocol
- **Build system** with javac integration
- **Code formatter** and static analysis

## Quick Start

### Installation

```bash
# Install from GitHub releases
curl -L https://github.com/project-jvlang/jv-lang/releases/latest/download/install.sh | sh

# Or using cargo
cargo install jv-cli

# Or build from source
git clone https://github.com/project-jvlang/jv-lang.git
cd jv-lang
cargo build --release
```

### Hello World

Create a new project:
```bash
jv init hello-world
cd hello-world
```

Write some jv code (`src/main.jv`):
```kotlin
fun main() {
    val name = "World"
    println("Hello, ${name}!")

    val numbers = listOf(1, 2, 3, 4, 5)
    val doubled = numbers.map { it * 2 }
    println("Doubled: ${doubled}")
}
```

Build and run:
```bash
jv build
jv run
```

The generated Java code is clean and readable:
```java
public class Main {
    public static void main(String[] args) {
        final var name = "World";
        System.out.println("Hello, " + name + "!");

        final var numbers = List.of(1, 2, 3, 4, 5);
        final var doubled = numbers.stream()
            .map(it -> it * 2)
            .toList();
        System.out.println("Doubled: " + doubled);
    }
}
```

## Project Structure

This repository contains a Rust workspace with multiple crates:

```
crates/
├── jv_lexer         # Lexical analysis
├── jv_parser        # Syntax parsing (using chumsky)
├── jv_ast           # Abstract Syntax Tree definitions
├── jv_ir            # Intermediate representation for desugaring
├── jv_codegen_java  # Java 25 code generation
├── jv_mapper        # Source maps (.jv ↔ .java)
├── jv_checker       # Static analysis (null safety, forbidden syntax)
├── jv_fmt           # Code formatter
├── jv_pm            # Package manager
├── jv_build         # Build system and javac integration
├── jv_lsp           # Language Server Protocol implementation
└── jv_cli           # CLI entry point (jv/jvlang commands)
```

## Development

### Prerequisites
- Rust 1.75+
- Java 21+ (for testing generated code)

### Building
```bash
# Build the entire workspace
cargo build

# Run CLI
cargo run --bin jv_cli

# Run tests
cargo test

# Check without building
cargo check
```

### CLI Commands (Target Implementation)

```bash
jv init                # Create new project
jv add <pkg>           # Add dependencies
jv build [--preview]   # Build to Java
jv run                 # Execute
jv test                # Run tests
jv fmt                 # Format source
jv lint                # Static analysis
jv toolchain install   # Install JDK versions
jv use <jdk>           # Set project JDK
jv publish             # Publish to registry
jv doctor              # Environment diagnostics
```

## Language Guide

### Variables and Type Inference
```kotlin
val immutable = "Cannot change"
var mutable = 42
val inferred = listOf(1, 2, 3)  // List<Integer>
```

### Null Safety
```kotlin
val nullable: String? = getName()
val length = nullable?.length ?: 0
val safe = nullable ?: "default"
```

### Data Classes
```kotlin
data class Person(val name: String, val age: Int)

val person = Person("Alice", 30)
val older = person.copy(age = 31)
```

### When Expressions
```kotlin
val result = when (value) {
    is String -> "text: ${value}"
    is Int -> "number: ${value}"
    else -> "unknown"
}
```

### Extension Functions
```kotlin
fun String.isPalindrome(): Boolean {
    return this == this.reversed()
}

"racecar".isPalindrome()  // true
```

### Async Programming
```kotlin
async {
    val data = fetchData().await()
    processData(data)
}.await()
```

## Package Management

### jv.toml
```toml
[package]
name = "my-app"
version = "1.0.0"
jdk = "21"

[dependencies]
commons-lang = "3.14.0"  # Maven dependency
jv-json = "1.0"          # jv registry
```

### Commands
```bash
jv add commons-lang      # Add Maven dependency
jv add jv-json           # Add jv dependency
jv audit                 # Security audit
```

## IDE Support

jv includes a Language Server Protocol implementation for IDE integration:

- **Syntax highlighting**
- **Error diagnostics**
- **Auto-completion**
- **Go to definition**
- **Refactoring support**
- **Debugging** (via source maps)

### VS Code
Install the jv extension from the marketplace.

### IntelliJ IDEA
Plugin available for IDEA Ultimate and Community.

## Comparison with Java

| Feature | Java 25 | jv |
|---------|---------|-----|
| Variable declaration | `final var name = "value";` | `val name = "value"` |
| Null safety | Manual null checks | `?.`, `?:` operators |
| Pattern matching | `switch` expressions | `when` expressions |
| Data classes | Records + boilerplate | `data class` |
| Collections | Verbose stream API | Extension functions |
| String templates | Text blocks + concat | `"Hello ${name}"` |
| Async programming | CompletableFuture | `async/await` |

## Contributing

See [CONTRIBUTING.md](jv/docs/contributing.md) for development setup and guidelines.

## License

Licensed under either of:
- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE))
- MIT License ([LICENSE-MIT](LICENSE-MIT))

at your option.

## Roadmap

- [ ] **Phase 1**: Core compiler (lexer, parser, basic codegen)
- [ ] **Phase 2**: Advanced features (null safety, pattern matching)
- [ ] **Phase 3**: Tooling (LSP, formatter, package manager)
- [ ] **Phase 4**: Ecosystem (registry, IDE plugins, documentation)

## Support

If this project is helpful to you, please consider supporting its development:

<a href="https://buymeacoffee.com/asopitechia"><img src="assets/yellow-button.png" alt="Buy Me A Coffee" width="150"></a>

## Links

- [Language Specification](jv/docs/language-spec.md)
- [Getting Started Guide](jv/docs/getting-started.md)
- [Architecture Overview](jv/docs/architecture.md)
- [CLI Reference](jv/docs/cli-reference.md)
