# jv Compiler Architecture

**English** | [日本語](architecture.md)

This document describes the internal architecture of the jv compiler, including the compilation pipeline, core components, and design decisions.

## High-Level Architecture

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   .jv file  │ -> │   Lexer     │ -> │   Parser    │ -> │     AST     │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
                                                                 │
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ .class file │ <- │    javac    │ <- │ Java source │ <- │     IR      │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
```

## Compilation Pipeline

### Phase 1: Lexical Analysis (`jv_lexer`)

**Purpose**: Convert raw jv source text into tokens.

**Input**: Raw source code string
**Output**: Stream of tokens

**Key Components**:
- `Lexer` struct: Main tokenizer
- `Token` enum: All jv token types
- `Span` struct: Source location tracking

**Token Types**:
```rust
pub enum Token {
    // Literals
    StringLiteral(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    BooleanLiteral(bool),
    
    // Keywords
    Val, Var, Fun, Class, Data, When, If, Else,
    Null, True, False, Return, Import,
    
    // Operators
    Plus, Minus, Star, Slash, Percent,
    Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
    
    // Null safety
    Question, QuestionDot, QuestionColon,
    
    // Delimiters
    LeftParen, RightParen, LeftBrace, RightBrace,
    LeftBracket, RightBracket, Comma, Semicolon,
    
    // Special
    Identifier(String),
    StringInterpolation(Vec<StringPart>),
    Eof,
}
```

**Error Handling**:
- Lexical errors with precise source locations
- Recovery mechanisms for malformed tokens
- Unicode support for identifiers and strings

### Phase 2: Syntax Analysis (`jv_parser`)

**Purpose**: Parse tokens into an Abstract Syntax Tree (AST).

**Input**: Token stream from lexer
**Output**: AST representing the program structure

**Parser Implementation**:
- Based on `chumsky` parser combinator library
- Recursive descent parser with operator precedence
- Error recovery for partial parsing

**Key Features**:
- **Expression parsing**: Handles precedence and associativity
- **Statement parsing**: All jv statement types
- **Type annotation parsing**: Optional and nullable types
- **Pattern matching**: Complex pattern syntax
- **String interpolation**: Embedded expressions in strings

**AST Node Types**:
```rust
pub enum Statement {
    ValDeclaration { name: String, type_annotation: Option<TypeAnnotation>, initializer: Expression, origin: ValBindingOrigin, span: Span },
    VarDeclaration { name: String, type_annotation: Option<TypeAnnotation>, initializer: Option<Expression>, span: Span },
    FunctionDeclaration { name: String, parameters: Vec<Parameter>, return_type: Option<TypeAnnotation>, body: Box<Expression>, span: Span },
    ClassDeclaration { name: String, parameters: Vec<Parameter>, body: Vec<Statement>, span: Span },
    DataClassDeclaration { name: String, parameters: Vec<Parameter>, is_mutable: bool, span: Span },
    Return { value: Option<Box<Expression>>, span: Span },
    Expression { expression: Box<Expression>, span: Span },
}

pub enum Expression {
    Literal(Literal, Span),
    Identifier(String, Span),
    BinaryOperation { left: Box<Expression>, operator: BinaryOperator, right: Box<Expression>, span: Span },
    FunctionCall { function: Box<Expression>, arguments: Vec<Argument>, span: Span },
    MemberAccess { object: Box<Expression>, property: String, span: Span },
    WhenExpression { subject: Option<Box<Expression>>, arms: Vec<WhenArm>, span: Span },
    StringInterpolation { parts: Vec<StringPart>, span: Span },
    // ... more expression types
}
```

### Phase 3: Semantic Analysis (`jv_ast` + `jv_checker`)

**Purpose**: Validate semantics and perform type checking.

**Components**:

**Type Inference** (`jv_ast`):
- Hindley-Milner style type inference
- Support for generics and constraints
- Nullable type handling

**Static Analysis** (`jv_checker`):
- Variable scoping validation
- Type compatibility checking
- Null safety enforcement
- Dead code detection
- Performance anti-pattern detection

**Type System**:
```rust
pub enum TypeAnnotation {
    Simple(String),
    Nullable(Box<TypeAnnotation>),
    Generic { name: String, type_args: Vec<TypeAnnotation> },
    Function { parameters: Vec<TypeAnnotation>, return_type: Box<TypeAnnotation> },
    Array(Box<TypeAnnotation>),
}
```

### Phase 4: IR Transformation (`jv_ir`)

**Purpose**: Transform high-level jv constructs into simpler forms suitable for Java generation.

**Key Transformations**:

**Sugar Elimination**:
- `val/var` with type inference → explicit types
- `when` expressions → Java switch statements
- Extension functions → static method calls
- String interpolation → `String.format()` calls
- Default parameters → method overloads
- Top-level functions → utility class methods

**Null Safety Desugaring**:
```jv
user?.name?.length
```
Becomes:
```java
user != null ? (user.getName() != null ? user.getName().length() : null) : null
```

**Concurrency Desugaring**:
```jv
spawn { doWork() }
```
Becomes:
```java
Thread.ofVirtual().start(() -> doWork())
```

**IR Node Types**:
```rust
pub enum IrStatement {
    ClassDeclaration { name: String, modifiers: IrModifiers, fields: Vec<IrField>, methods: Vec<IrMethod> },
    MethodDeclaration { name: String, modifiers: IrModifiers, parameters: Vec<IrParameter>, return_type: JavaType, body: Vec<IrStatement> },
    VariableDeclaration { name: String, java_type: JavaType, initializer: Option<IrExpression> },
    Assignment { target: String, value: IrExpression },
    Return { value: Option<IrExpression> },
    If { condition: IrExpression, then_branch: Vec<IrStatement>, else_branch: Option<Vec<IrStatement>> },
    Block(Vec<IrStatement>),
}

pub enum IrExpression {
    Literal(IrLiteral),
    Variable(String),
    MethodCall { receiver: Option<Box<IrExpression>>, method: String, arguments: Vec<IrExpression> },
    FieldAccess { object: Box<IrExpression>, field: String },
    BinaryOperation { left: Box<IrExpression>, operator: IrBinaryOperator, right: Box<IrExpression> },
    SwitchExpression { discriminant: Box<IrExpression>, cases: Vec<IrSwitchCase> },
    // Java 25 specific constructs
    PatternMatch { value: Box<IrExpression>, patterns: Vec<IrPattern> },
    VirtualThreadCreation { runnable: Box<IrExpression> },
    CompletableFutureChain { future: Box<IrExpression>, operations: Vec<IrFutureOperation> },
}
```

### Phase 5: Java Code Generation (`jv_codegen_java`)

**Purpose**: Generate readable, idiomatic Java 25 source code from IR.

**Code Generation Strategy**:
- **Readable Output**: Generated Java looks hand-written
- **Java 25 Features**: Leverages records, pattern matching, virtual threads
- **Performance**: Generates efficient Java code
- **Debugging**: Maintains source location information

**Java 25 Feature Usage**:

**Records** (for immutable data classes):
```java
public record Point(double x, double y) {
    public Point translate(double dx, double dy) {
        return new Point(x + dx, y + dy);
    }
}
```

**Pattern Matching** (for when expressions):
```java
public static String describe(Object obj) {
    return switch (obj) {
        case String s -> "String: " + s;
        case Integer i when i > 0 -> "Positive: " + i;
        case Integer i -> "Non-positive: " + i;
        case null -> "null value";
        default -> "Unknown type";
    };
}
```

**Virtual Threads** (for spawn blocks):
```java
public void processAsync() {
    Thread.ofVirtual().name("worker").start(() -> {
        // Background work
        processData();
    });
}
```

**Code Generation Components**:
```rust
pub struct JavaCodeGenerator {
    imports: ImportManager,
    source_builder: JavaSourceBuilder,
    config: JavaCodeGenConfig,
}

pub struct JavaSourceBuilder {
    content: String,
    indent_level: usize,
    current_line: String,
}

pub struct ImportManager {
    imports: BTreeSet<String>,
    static_imports: BTreeSet<String>,
}
```

### Phase 6: Source Mapping (`jv_mapper`)

**Purpose**: Generate source maps for debugging support.

**Source Map Features**:
- Bidirectional mapping between .jv and .java
- Line and column precision
- Support for IDE debugging
- Stack trace translation

**Source Map Format**:
```json
{
  "version": 3,
  "file": "Main.java",
  "sourceRoot": "",
  "sources": ["main.jv"],
  "sourcesContent": ["val greeting = \"Hello, jv!\""],
  "mappings": "AAAA,MAAM,QAAQ,GAAG,YAAY"
}
```

### Phase 7: Build Integration (`jv_build`)

**Purpose**: Integrate with Java build tools and manage compilation.

**Features**:
- **JDK Detection**: Find and validate JDK installations
- **Classpath Management**: Handle dependencies and classpaths
- **Incremental Compilation**: Only recompile changed files
- **Error Reporting**: Unified error messages across tools
- **Cross-Platform**: Works on Linux, macOS, Windows

**Build Process**:
1. **Dependency Resolution**: Resolve jv and Maven dependencies
2. **Source Discovery**: Find all .jv files in project
3. **Compilation Order**: Determine compilation order based on dependencies
4. **Java Generation**: Generate .java files with source maps
5. **javac Invocation**: Compile Java files to bytecode
6. **Post-processing**: Copy resources, generate manifests

## Design Decisions

### Why Rust for Implementation?

**Performance**: Rust provides near-C performance for fast compilation
**Memory Safety**: Prevents common compiler bugs like buffer overflows
**Concurrency**: Excellent support for parallel compilation
**Ecosystem**: Rich ecosystem with parser combinators, CLI tools

### Why Target Java 25?

**Modern Features**: Records, pattern matching, virtual threads
**Performance**: Latest JVM optimizations and features
**Compatibility**: Maintains backward compatibility with older Java
**Future-Proof**: Positions jv for upcoming Java features

### AST vs IR Design

**AST (Abstract Syntax Tree)**:
- Direct representation of jv syntax
- Preserves original structure for tooling
- Used for formatting, refactoring, LSP

**IR (Intermediate Representation)**:
- Simplified form suitable for code generation
- Sugar constructs are eliminated
- Optimizations can be applied

### Error Handling Strategy

**Fail-Fast Parsing**: Stop on first syntax error for clarity
**Error Recovery**: Attempt to continue parsing for better error messages  
**Staged Validation**: Type checking after successful parsing
**Rich Diagnostics**: Precise error locations with suggestions

### Extension Points

**Language Features**: Easy to add new syntax and semantics
**Target Languages**: IR can be adapted for other targets (JavaScript, native)
**Optimization Passes**: IR transformations for performance
**IDE Integration**: AST suitable for language server features

## Performance Characteristics

### Compilation Speed

**Single-threaded**: ~10,000 lines/second on modern hardware
**Multi-threaded**: Scales linearly with CPU cores
**Incremental**: Only recompiles changed files and dependencies

### Memory Usage

**Lexer**: O(n) where n is source file size
**Parser**: O(n) for AST storage
**IR**: O(n) similar to AST size
**Code Generation**: O(n) with temporary string buffers

### Generated Code Quality

**Runtime Performance**: Matches hand-written Java
**Binary Size**: No additional runtime dependencies
**Memory Efficiency**: Uses Java's memory model directly

## Testing Strategy

### Unit Tests

Each component has comprehensive unit tests:
- **Lexer**: Token generation and error cases
- **Parser**: AST generation for all syntax forms
- **IR**: Transformation correctness
- **Code Generation**: Java output validation

### Integration Tests

End-to-end compilation tests:
- **Golden Tests**: Compare generated Java against expected output
- **Compilation Tests**: Verify generated Java compiles correctly
- **Runtime Tests**: Execute generated Java and verify behavior

### Property-Based Testing

- **Round-trip Testing**: Parse and unparse should preserve semantics
- **Fuzzing**: Random input generation to find edge cases
- **Invariant Testing**: Type safety and memory safety properties

### Performance Benchmarks

- **Compilation Speed**: Track compilation performance over time
- **Memory Usage**: Monitor memory consumption during compilation
- **Generated Code Quality**: Benchmark generated Java performance

## Development Workflow

### Adding New Language Features

1. **Design**: Specify syntax and semantics
2. **Lexer**: Add new tokens if needed
3. **Parser**: Extend grammar and AST nodes
4. **Type Checker**: Add semantic validation
5. **IR**: Define desugaring transformation  
6. **Code Generation**: Implement Java generation
7. **Tests**: Add comprehensive test cases
8. **Documentation**: Update language guide

### Debugging Compilation Issues

1. **Enable Verbose Logging**: Use `JV_LOG_LEVEL=debug`
2. **Inspect Generated Files**: Check intermediate outputs
3. **Use Source Maps**: Trace back to original jv source
4. **Test Minimal Examples**: Isolate the issue
5. **Check Test Suite**: Verify existing functionality

### Contributing to the Compiler

See [Contributing Guide](contributing-en.md) for detailed instructions on:
- Setting up development environment
- Code style and conventions
- Submitting pull requests
- Writing tests
