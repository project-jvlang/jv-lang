# Contributing to jv

Thank you for your interest in contributing to jv! This guide will help you get started with development and contribution workflows.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Development Setup](#development-setup)
3. [Project Structure](#project-structure)
4. [Development Workflow](#development-workflow)
5. [Testing](#testing)
6. [Code Style](#code-style)
7. [Pull Request Process](#pull-request-process)
8. [Issue Guidelines](#issue-guidelines)
9. [Community](#community)

## Getting Started

### Prerequisites

- **Rust 1.70+**: Install via [rustup](https://rustup.rs/)
- **Java 25+**: Required for testing generated code
- **Git**: For version control
- **IDE**: VS Code with rust-analyzer or IntelliJ with Rust plugin

### Quick Setup

```bash
# Clone the repository
git clone https://github.com/jv-lang/jv.git
cd jv

# Build the project
cargo build

# Run tests
cargo test

# Install pre-commit hooks (optional but recommended)
cargo install --git https://github.com/jv-lang/jv-tools pre-commit
pre-commit install
```

## Development Setup

### Development Dependencies

```bash
# Install additional development tools
cargo install cargo-watch         # Auto-rebuild on changes
cargo install cargo-audit         # Security auditing
cargo install cargo-tarpaulin     # Code coverage
cargo install cargo-deny          # Dependency checking
```

### IDE Configuration

#### VS Code

Install these extensions:
- **rust-analyzer**: Rust language support
- **Error Lens**: Inline error display
- **GitLens**: Git integration
- **Better TOML**: TOML file support

Recommended settings (`.vscode/settings.json`):
```json
{
    "rust-analyzer.checkOnSave.command": "clippy",
    "rust-analyzer.cargo.features": "all",
    "editor.formatOnSave": true,
    "files.trimTrailingWhitespace": true
}
```

#### IntelliJ IDEA

- Install the **Rust** plugin
- Configure **Rustfmt** for code formatting
- Enable **Clippy** for linting

### Environment Variables

```bash
export RUST_LOG=debug              # Enable debug logging
export JV_TEST_DATA=/path/to/test  # Custom test data directory
export JAVA_HOME=/path/to/java25   # Java 25 installation
```

## Project Structure

```
jv/
├── crates/                    # Rust workspace crates
│   ├── jv_lexer/             # Lexical analysis
│   ├── jv_parser/            # Syntax parsing
│   ├── jv_ast/               # AST definitions
│   ├── jv_ir/                # Intermediate representation
│   ├── jv_codegen_java/      # Java code generation
│   ├── jv_checker/           # Static analysis
│   ├── jv_fmt/               # Code formatting
│   ├── jv_build/             # Build system
│   ├── jv_mapper/            # Source mapping
│   ├── jv_pm/                # Package manager
│   ├── jv_lsp/               # Language server
│   └── jv_cli/               # Command-line interface
├── docs/                     # Documentation
├── examples/                 # Example jv programs
├── tests/                    # Integration tests
├── tools/                    # Development tools
├── Cargo.toml               # Workspace configuration
└── README.md
```

### Crate Responsibilities

- **jv_lexer**: Tokenization of jv source code
- **jv_parser**: Parse tokens into Abstract Syntax Tree (AST)
- **jv_ast**: AST node definitions and utilities
- **jv_ir**: Intermediate Representation and transformations
- **jv_codegen_java**: Generate Java source code from IR
- **jv_checker**: Type checking and static analysis
- **jv_fmt**: Source code formatting
- **jv_build**: Build system integration and javac interaction
- **jv_mapper**: Source map generation for debugging
- **jv_pm**: Package management and dependency resolution
- **jv_lsp**: Language Server Protocol implementation
- **jv_cli**: Command-line interface and main entry point

## Development Workflow

### Branch Strategy

We use a simplified Git Flow:

- **main**: Stable release branch
- **develop**: Integration branch for features
- **feature/**: Feature development branches
- **bugfix/**: Bug fix branches
- **hotfix/**: Critical fixes for main

### Feature Development

1. **Create a feature branch**:
   ```bash
   git checkout develop
   git pull origin develop
   git checkout -b feature/my-new-feature
   ```

2. **Make changes with frequent commits**:
   ```bash
   git add .
   git commit -m "Add lexer support for new operator"
   ```

3. **Keep branch up-to-date**:
   ```bash
   git fetch origin
   git rebase origin/develop
   ```

4. **Push and create Pull Request**:
   ```bash
   git push origin feature/my-new-feature
   # Create PR on GitHub
   ```

### Adding Language Features

When adding new jv language features, follow this sequence:

1. **Update the lexer** (`jv_lexer`):
   - Add new token types if needed
   - Update tokenization logic

2. **Update the parser** (`jv_parser`):
   - Extend grammar to handle new syntax
   - Add AST nodes for new constructs

3. **Update AST definitions** (`jv_ast`):
   - Add new node types and visitors
   - Update serialization/deserialization

4. **Update type checker** (`jv_checker`):
   - Add semantic validation
   - Handle new type rules

5. **Update IR transformation** (`jv_ir`):
   - Define how new constructs desugar
   - Add transformation passes

6. **Update code generator** (`jv_codegen_java`):
   - Implement Java code generation
   - Ensure idiomatic output

7. **Update formatter** (`jv_fmt`):
   - Add formatting rules for new syntax

8. **Add comprehensive tests**:
   - Unit tests for each component
   - Integration tests for full pipeline
   - Error case testing

9. **Update documentation**:
   - Language guide examples
   - Specification updates

### Debugging

#### Compiler Debugging

Enable verbose logging:
```bash
RUST_LOG=debug cargo run -- build test.jv
```

View intermediate representations:
```bash
# View generated AST
cargo run -- build --debug-ast test.jv

# View generated IR
cargo run -- build --debug-ir test.jv

# View generated Java (before compilation)
cargo run -- build --preview test.jv
```

#### Test Debugging

Run specific tests:
```bash
# Run tests for a specific crate
cargo test -p jv_parser

# Run specific test
cargo test test_string_interpolation

# Run with output
cargo test -- --nocapture
```

## Testing

### Test Categories

#### Unit Tests
Each crate has unit tests in `src/` directories:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_lexer_string_literal() {
        let input = r#""Hello, world!""#;
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        
        assert_eq!(token.kind, TokenKind::StringLiteral("Hello, world!".to_string()));
    }
}
```

#### Integration Tests
Full compilation pipeline tests in `tests/`:

```rust
#[test]
fn test_compile_hello_world() {
    let jv_source = r#"
        fun main() {
            println("Hello, world!")
        }
    "#;
    
    let result = compile_jv_to_java(jv_source);
    assert!(result.is_ok());
    
    let java_code = result.unwrap();
    assert!(java_code.contains("System.out.println"));
}
```

#### Golden Tests
Compare generated output against expected files:

```rust
#[test]
fn test_codegen_golden() {
    let jv_file = "tests/golden/input.jv";
    let expected_java = "tests/golden/expected.java";
    
    let actual_java = compile_file(jv_file).unwrap();
    let expected = fs::read_to_string(expected_java).unwrap();
    
    assert_eq!(normalize_whitespace(&actual_java), normalize_whitespace(&expected));
}
```

### Running Tests

```bash
# Run all tests
cargo test

# Run tests with coverage
cargo tarpaulin --out html

# Run tests for specific crate
cargo test -p jv_parser

# Run integration tests only
cargo test --test integration

# Run with verbose output
cargo test -- --nocapture

# Run tests matching pattern
cargo test string_interpolation
```

### Test Data

Test files are organized in `tests/data/`:
```
tests/data/
├── valid/              # Valid jv programs
│   ├── basic/         # Basic language features
│   ├── advanced/      # Advanced features
│   └── edge_cases/    # Edge cases and corner cases
├── invalid/           # Invalid programs (for error testing)
├── golden/            # Golden test files
│   ├── input.jv
│   └── expected.java
└── performance/       # Performance benchmarks
```

### Writing Good Tests

1. **Test naming**: Use descriptive names
   ```rust
   #[test]
   fn test_null_safe_member_access_with_chaining() { ... }
   ```

2. **Arrange-Act-Assert pattern**:
   ```rust
   #[test]
   fn test_parser_function_declaration() {
       // Arrange
       let input = "fun add(a: Int, b: Int): Int = a + b";
       let mut parser = Parser::new(input);
       
       // Act
       let result = parser.parse_function();
       
       // Assert
       assert!(result.is_ok());
       let func = result.unwrap();
       assert_eq!(func.name, "add");
       assert_eq!(func.parameters.len(), 2);
   }
   ```

3. **Test error cases**:
   ```rust
   #[test]
   fn test_parser_missing_semicolon_error() {
       let input = "val x = 42"; // Missing semicolon
       let result = Parser::new(input).parse();
       
       assert!(result.is_err());
       let error = result.unwrap_err();
       assert!(error.message.contains("Expected ';'"));
   }
   ```

## Code Style

### Rust Code Style

Follow standard Rust conventions:

#### Formatting
```bash
# Format all code
cargo fmt

# Check formatting
cargo fmt -- --check
```

#### Linting
```bash
# Run clippy
cargo clippy

# Clippy with all features
cargo clippy --all-features -- -D warnings
```

#### Naming Conventions

- **Types**: `PascalCase`
- **Functions**: `snake_case`
- **Variables**: `snake_case`
- **Constants**: `SCREAMING_SNAKE_CASE`
- **Modules**: `snake_case`

#### Documentation

Use Rust doc comments:
```rust
/// Parses a jv expression into an AST node.
/// 
/// # Arguments
/// 
/// * `input` - The jv source code to parse
/// * `options` - Parsing options and configuration
/// 
/// # Returns
/// 
/// Returns `Ok(Expression)` if parsing succeeds, or `Err(ParseError)` if parsing fails.
/// 
/// # Examples
/// 
/// ```
/// use jv_parser::Parser;
/// 
/// let expr = Parser::new("1 + 2").parse_expression().unwrap();
/// assert_eq!(expr.kind, ExpressionKind::BinaryOperation);
/// ```
pub fn parse_expression(input: &str, options: ParseOptions) -> Result<Expression, ParseError> {
    // Implementation...
}
```

#### Error Handling

Use `Result` types consistently:

```rust
use anyhow::{Result, Context};

pub fn compile_file(path: &Path) -> Result<String> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path.display()))?;
    
    let ast = parse(&source)
        .context("Failed to parse jv source")?;
    
    let java_code = generate_java(&ast)
        .context("Failed to generate Java code")?;
    
    Ok(java_code)
}
```

### Git Commit Messages

Follow conventional commit format:

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Build process or auxiliary tool changes

**Examples:**
```
feat(parser): add support for when expressions

Implements parsing for when expressions including pattern matching
and guard clauses. Supports both expression and statement forms.

Closes #123
```

```
fix(codegen): handle null safety in method chaining

Previously, chained method calls on nullable types would generate
incorrect Java code. Now properly generates null-safe call chains.

Fixes #456
```

## Pull Request Process

### Before Submitting

1. **Ensure tests pass**:
   ```bash
   cargo test
   cargo clippy -- -D warnings
   cargo fmt -- --check
   ```

2. **Update documentation** if needed

3. **Add tests** for new functionality

4. **Run integration tests**:
   ```bash
   cargo test --test integration
   ```

### Pull Request Template

When creating a PR, include:

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update

## Testing
- [ ] Unit tests added/updated
- [ ] Integration tests added/updated
- [ ] All tests pass locally

## Checklist
- [ ] Code follows style guidelines
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] No breaking changes (or clearly documented)
```

### Review Process

1. **Automated checks** must pass (CI/CD)
2. **Code review** by maintainer
3. **Discussion** and feedback
4. **Approval** and merge

### Addressing Feedback

```bash
# Make changes based on feedback
git add .
git commit -m "Address PR feedback: improve error messages"
git push origin feature/my-feature
```

## Issue Guidelines

### Bug Reports

Use the bug report template:

```markdown
## Bug Description
Clear description of the bug

## Steps to Reproduce
1. Create file `test.jv` with content: ...
2. Run `jv build test.jv`
3. Observe error: ...

## Expected Behavior
What should happen

## Actual Behavior
What actually happens

## Environment
- jv version: `jv version`
- OS: Linux/macOS/Windows
- Java version: `java -version`

## Additional Context
Any additional information
```

### Feature Requests

```markdown
## Feature Description
Clear description of the proposed feature

## Use Case
Why is this feature needed?

## Proposed Solution
How should this feature work?

## Alternatives Considered
What other solutions were considered?

## Additional Context
Examples, mockups, etc.
```

### Labels

- `bug`: Something isn't working
- `enhancement`: New feature or request
- `documentation`: Improvements or additions to documentation
- `good-first-issue`: Good for newcomers
- `help-wanted`: Extra attention is needed
- `priority-high`: High priority issue
- `component-parser`: Parser-related issues
- `component-codegen`: Code generation issues

## Community

### Communication Channels

- **GitHub Issues**: Bug reports and feature requests
- **GitHub Discussions**: General questions and discussions
- **Discord**: Real-time chat (invite in README)
- **Reddit**: r/jv_lang community discussions

### Code of Conduct

We follow the [Contributor Covenant](https://www.contributor-covenant.org/):

- **Be respectful** and inclusive
- **Be constructive** in feedback
- **Be collaborative** and helpful
- **Focus on the code**, not the person

### Getting Help

- **Documentation**: Check the docs first
- **Search issues**: Someone may have had the same problem
- **Ask questions**: Use GitHub Discussions for questions
- **Join Discord**: Real-time help from community

### Recognition

Contributors are recognized in:
- **CONTRIBUTORS.md** file
- **Release notes** for significant contributions
- **GitHub contributors** page
- **Discord contributor** role

Thank you for contributing to jv! 🎉