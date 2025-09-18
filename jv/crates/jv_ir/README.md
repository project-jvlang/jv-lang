# jv_ir - Intermediate Representation for jv Language Desugaring

This crate handles the transformation of jv language constructs into desugared intermediate representation (IR) that can be easily compiled to Java 25 source code.

## Purpose

The IR serves as a clean intermediate layer between jv's syntax sugar and Java's more verbose constructs. It:

1. **Removes all jv sugar** while preserving semantics
2. **Provides explicit Java types** for all expressions and statements
3. **Enables straightforward Java code generation** without complex logic in the codegen phase
4. **Maintains source mapping** information for debugging

## Current Status: TDD Red Phase ✅

The crate is in the **Red phase** of Test-Driven Development:

- ✅ **Comprehensive IR node structure** designed for all jv language constructs
- ✅ **Complete type system** (`JavaType`) representing Java 25 types
- ✅ **Transformation context** with scope management and utility class tracking
- ✅ **34 comprehensive failing tests** covering all desugaring transformations
- ✅ **Error types** for all transformation scenarios
- ✅ **Public API** designed for integration with `jv_codegen_java`

All transformation functions correctly panic with "not yet implemented" messages, ready for the Green phase implementation.

## Desugaring Transformations Covered

### Variable Declarations
- `val x = 42` → `final int x = 42;`
- `var y: String?` → `String y = null;` (with null safety analysis)
- Type inference from initializers

### When Expressions
- `when (x) { 1 -> "one", 2 -> "two", else -> "other" }` → Java switch expressions
- Pattern matching with guards → case labels with conditions
- Range patterns → multiple case labels or guard conditions

### Extension Functions
- `fun String.reversed(): String` → Static method in utility class
- Generic extension functions → Parameterized static methods

### String Interpolation
- `"Hello, ${name}!"` → `String.format("Hello, %s!", name)` or concatenation

### Concurrency Constructs
- `spawn { ... }` → `Thread.ofVirtual().start(() -> { ... })`
- `async { ... }` → `CompletableFuture.supplyAsync(() -> { ... })`
- `future.await()` → `future.get()` or `future.join()`

### Resource Management
- `use(resource) { ... }` → try-with-resources statements
- `defer { ... }` → finally blocks

### Default Parameters
- Functions with default parameters → Multiple method overloads
- Named arguments → Proper parameter ordering

### Data Classes
- `data class User(val id: Int, var name: String)` → Java records or classes
- Immutable data classes → Java records
- Mutable data classes → Regular Java classes with getters/setters

### Null Safety
- `obj?.property` → Explicit null checks with conditional expressions
- `arr?[index]` → Null-safe array access
- `nullableValue ?: "default"` → Conditional expressions

### Top-level Functions
- Top-level functions → Static methods in utility classes
- Package-based utility class naming

## IR Node Structure

### Core Types
- **`JavaType`**: Represents all Java 25 types (primitives, references, arrays, functionals)
- **`IrExpression`**: Desugared expressions with explicit Java semantics
- **`IrStatement`**: Desugared statements including class/method declarations
- **`TransformContext`**: Maintains type information and transformation state

### Key Features
- **Explicit typing**: Every expression has a resolved Java type
- **Source preservation**: All nodes maintain span information
- **Java compatibility**: All constructs map directly to valid Java 25 code
- **Utility tracking**: Generated utility classes and method overloads are tracked

## Test Coverage

The test suite includes:
- **Basic transformation tests** for all jv constructs
- **Complex integration tests** for combined features
- **Error handling tests** for all error scenarios
- **Type system tests** for Java type compatibility
- **Context management tests** for scope tracking

## Integration Points

- **Input**: `jv_ast::Program` from the parser
- **Output**: `IrProgram` ready for Java code generation
- **Dependencies**: 
  - `jv_ast` for input AST nodes
  - `serde` for serialization support
  - `thiserror` for error handling

## Next Steps (Green Phase)

The Red phase is complete. The next phase will implement:

1. **Type inference engine** for inferring Java types from jv expressions
2. **Pattern desugaring** for when expressions to switch statements
3. **Null safety analysis** for safe nullable operations
4. **Extension function transformation** to static method calls
5. **Concurrency desugaring** to Java virtual threads and CompletableFuture
6. **Resource management** to try-with-resources and finally blocks
7. **Utility class generation** for top-level functions and extensions

## Architecture

The IR transformation follows a multi-pass approach:

1. **Analysis Pass**: Type inference and scope resolution
2. **Desugaring Pass**: Transform high-level constructs to basic operations
3. **Generation Pass**: Create utility classes and method overloads
4. **Validation Pass**: Ensure all IR nodes are valid for Java generation

The design enables incremental implementation and thorough testing of each transformation.