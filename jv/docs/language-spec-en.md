# jv Language Specification

**English** | [日本語](language-spec.md)

This document is the formal specification of the jv (Java syntactic sugar) programming language.

## Table of Contents

1. [Overview](#overview)
2. [Lexical Structure](#lexical-structure)
3. [Grammar](#grammar)
4. [Type System](#type-system)
5. [Semantics](#semantics)
6. [Standard Library](#standard-library)
7. [Java Interoperability](#java-interoperability)

## Overview

jv is a statically typed programming language that compiles to readable Java 25 source code. It provides modern syntax while preserving full compatibility with the Java ecosystem.

### Design Goals

1. **Zero Runtime Overhead**: Compiles to pure Java with no additional runtime.
2. **Java Ecosystem Compatibility**: Seamless integration with Java libraries.
3. **Modern Syntax**: Kotlin-inspired syntax improvements.
4. **Type Safety**: Enhanced null safety and type inference.
5. **Readability**: Generated Java code remains human friendly.

### Compilation Model

```
jv source (.jv) → AST → IR → Java source (.java) → bytecode (.class)
```

## Lexical Structure

### Character Set

jv source files are encoded in UTF-8. The lexical structure is case sensitive.

### Comments

```jv
// Line comment

/// jv-only line comment (omitted from generated Java)

/*
 * Block comment
 * Can span multiple lines
 */

//*
 * jv-only multiline comment (omitted from generated Java)
 *
 * Useful for notes that live only in jv source
 *//

/**
 * Documentation comment
 * Used for API documentation
 */
```

**Note**: Each comment form serves a distinct purpose.

- `//` and `/* ... */` are **passthrough comments**. They retain their position through parsing, IR, and code generation, and appear unmodified in the emitted Java source.
- `///` and `//* ... *//` are treated as **jv-only comments**. They survive lexing and parsing but are filtered out before code generation, making them suitable for compiler-only notes or TODOs.
- JavaDoc comments (`/** ... */`) are always copied to the generated Java source so that API documentation remains available to JavaDoc tooling.

### Identifiers

```bnf
identifier ::= letter (letter | digit | '_')*
letter     ::= 'a'..'z' | 'A'..'Z' | unicode_letter
digit      ::= '0'..'9'
```

**Reserved Words:**
```
abstract, async, await, break, class, continue, data, defer, do, else,
enum, false, final, for, fun, import, in, interface, is, null,
object, override, package, private, protected, public, return, spawn,
super, this, throw, true, try, use, val, var, when, while
```

### Literals

#### Integer Literals

```bnf
integer_literal ::= decimal_literal | hex_literal | binary_literal
decimal_literal ::= digit+ ('_' digit+)*
hex_literal     ::= '0' [xX] hex_digit+ ('_' hex_digit+)*
binary_literal  ::= '0' [bB] binary_digit+ ('_' binary_digit+)*
```

Examples:
```jv
42
1_000_000
0xFF_FF_FF
0b1010_1010
```

#### Floating-Point Literals

```bnf
float_literal ::= digit+ '.' digit+ ([eE] [+-]? digit+)?
               | digit+ [eE] [+-]? digit+
```

Examples:
```jv
3.14159
2.5e10
1.23e-4
```

#### String Literals

```jv
// Simple string
"Hello, world!"

// Escape sequences
"Line 1\nLine 2\tTabbed"

// String interpolation
"Hello, $name!"
"Result: ${2 + 2}"

// Raw string
"""
Multiline
string content
"""
```

#### Character Literals

```jv
'a'
'\n'
'\u0041'  // Unicode
```

#### Boolean Literals

```jv
true
false
```

#### Null Literal

```jv
null
```

## Grammar

### Program Structure

```bnf
program ::= package_declaration? import_declaration* top_level_declaration*

package_declaration ::= 'package' qualified_name

import_declaration ::= 'import' qualified_name ('.' '*' | 'as' identifier)?

top_level_declaration ::= class_declaration
                       | function_declaration
                       | property_declaration
```

### Type Declarations

#### Class Declarations

```bnf
class_declaration ::= class_modifier* 'class' identifier type_parameters?
                     primary_constructor? (':' supertype_list)? class_body?

class_modifier ::= 'abstract' | 'final' | 'data' | visibility_modifier

primary_constructor ::= '(' parameter_list? ')'

class_body ::= '{' class_member* '}'

class_member ::= function_declaration
              | property_declaration
              | class_declaration
              | constructor_declaration
```

Examples:
```jv
// Basic class
class Person(val name: String, var age: Int) {
    fun greet(): String = "Hello, I'm $name"
}

// Data class
data class Point(val x: Double, val y: Double)

// Inheritance
class Student(name: String, age: Int, val studentId: String) : Person(name, age) {
    override fun greet(): String = "Hi, I'm student $name"
}
```

#### Interface Declarations

```bnf
interface_declaration ::= 'interface' identifier type_parameters?
                        (':' supertype_list)? interface_body?

interface_body ::= '{' interface_member* '}'

interface_member ::= function_declaration | property_declaration
```

Examples:
```jv
interface Drawable {
    fun draw()
    val color: String
        get() = "black"
}
```

### Function Declarations

```bnf
function_declaration ::= function_modifier* 'fun' type_parameters? identifier
                        '(' parameter_list? ')' (':' type)? function_body?

function_modifier ::= 'override' | 'abstract' | 'final' | visibility_modifier

parameter ::= identifier ':' type ('=' expression)?

function_body ::= '=' expression | block_statement
```

Examples:
```jv
// Basic function
fun add(a: Int, b: Int): Int = a + b

// Default argument
fun greet(name: String = "World"): String {
    return "Hello, $name!"
}

// Generic function
fun <T> identity(value: T): T = value

// Extension function
fun String.isPalindrome(): Boolean {
    return this == this.reversed()
}
```

### Variable Declarations

```bnf
property_declaration ::= property_modifier* ('val' | 'var') identifier
                        (':' type)? ('=' expression)?

property_modifier ::= visibility_modifier
```

Examples:
```jv
val immutable = 42
var mutable = "Hello"
val inferredType = listOf(1, 2, 3)  // List<Int>
val nullable: String? = null
```

## Type System

### Basic Types

| jv Type | Java Type | Description |
|---------|-----------|-------------|
| `Unit` | `void` | No return value |
| `Boolean` | `boolean` | Boolean value |
| `Byte` | `byte` | 8-bit signed integer |
| `Short` | `short` | 16-bit signed integer |
| `Int` | `int` | 32-bit signed integer |
| `Long` | `long` | 64-bit signed integer |
| `Float` | `float` | 32-bit floating point |
| `Double` | `double` | 64-bit floating point |
| `Char` | `char` | 16-bit character |
| `String` | `String` | String value |

### Nullable Types

```jv
var nullable: String? = null    // Nullable string
val nonNull: String = "value"   // Non-null string

// Safe call
val length = nullable?.length   // Int?

// Elvis operator
val name = nullable ?: "default"

// Non-null assertion
val definitelyNotNull = nullable!!
```

### Type Inference

```jv
val number = 42              // Int
val text = "Hello"           // String
val list = mutableListOf(1)  // MutableList<Int>
val map = mapOf("key" to 1)  // Map<String, Int>
```

### Generics

```jv
// Generic class
class Box<T>(val value: T)

// Constrained generics
class NumberBox<T : Number>(val value: T)

// Use-site variance
val readOnlyList: List<out Number> = listOf(1, 2, 3)
val writeOnlyList: MutableList<in Int> = mutableListOf()
```

### Function Types

```jv
// Function types
val operation: (Int, Int) -> Int = { a, b -> a + b }
val predicate: (String) -> Boolean = { it.isNotEmpty() }

// Higher-order function
fun processItems(items: List<String>, processor: (String) -> String): List<String> {
    return items.map(processor)
}
```

## Semantics

### Expressions

#### Arithmetic Expressions

```jv
val result = a + b * c / d - e % f
val power = base.pow(exponent)
```

#### Comparison Expressions

```jv
val isEqual = a == b
val isIdentical = a === b  // Reference equality
val isLess = a < b
val isNullable = value != null
```

#### Logical Expressions

```jv
val and = condition1 && condition2
val or = condition1 || condition2
val not = !condition
```

#### when Expressions (with subject)

```jv
val result = when (value) {
    1 -> "one"
    2, 3 -> "two or three"
    in 4..10 -> "four to ten"
    is String -> "string: $value"
    else -> "other"
}
```

#### when Expressions (subjectless guards)

```jv
val status = when {
    user.isActive -> "active"
    user.isPending && !user.hasTicket -> "pending without ticket"
    else -> "inactive"
}
```

> Notes:
> - When used as an expression, `when` must include an `else` branch. Missing `else` triggers diagnostic `E_WHEN_002` (an implicit `else -> Unit` is injected automatically in `Unit` contexts).
> - jv does not provide an `if` syntax. Writing `if` triggers diagnostic `JV3103`; use `when` for every conditional.

For side-effect only scenarios you may omit `else`. The compiler treats the expression as returning `Unit` and inserts an implicit `else -> Unit`.

```jv
when {
    shouldRetry() -> return retry()
    payload.isEmpty() -> log("empty payload")
    // else implicitly yields Unit
}
```

### Statements

#### Loops

```jv
// for-in loop
for (item in list) {
    println(item)
}

// Range loop
for (i in 1..10) {
    println(i)
}

// Indexed loop
for ((index, value) in list.withIndex()) {
    println("$index: $value")
}
```

#### try-catch

```jv
try {
    riskyOperation()
} catch (e: IOException) {
    handleError(e)
} catch (e: Exception) {
    handleGenericError(e)
} finally {
    cleanup()
}
```

## Standard Library

### Collections

```jv
// List
val list = listOf(1, 2, 3)
val mutableList = mutableListOf("a", "b")

// Set
val set = setOf(1, 2, 3)
val mutableSet = mutableSetOf<String>()

// Map
val map = mapOf("key1" to "value1", "key2" to "value2")
val mutableMap = mutableMapOf<String, Int>()
```

### Collection Operations

```jv
val numbers = listOf(1, 2, 3, 4, 5)

val doubled = numbers.map { it * 2 }
val evens = numbers.filter { it % 2 == 0 }
val sum = numbers.reduce { acc, n -> acc + n }
val first = numbers.firstOrNull { it > 3 }
```

### Concurrency

```jv
// Virtual thread
spawn {
    println("Running in virtual thread")
}

// async/await
async fun fetchData(): CompletableFuture<String> {
    return CompletableFuture.supplyAsync {
        "data"
    }
}

val result = fetchData().await()
```

### Resource Management

```jv
// use block (try-with-resources)
use(FileInputStream("file.txt")) { input ->
    val data = input.readAllBytes()
    processData(data)
}

// defer block
fun processFile() {
    val resource = acquireResource()
    defer {
        resource.release()
    }
    // Use the resource...
}
```

## Java Interoperability

### Using Java Classes

```jv
import java.util.ArrayList
import java.time.LocalDateTime

fun useJavaLibraries() {
    val list = ArrayList<String>()
    list.add("item")

    val now = LocalDateTime.now()
    println(now)
}
```

### Generated Java Code

jv code generates readable, idiomatic Java code:

```jv
// jv
data class User(val name: String, var age: Int) {
    fun greet(): String = "Hello, $name"
}
```

```java
// Generated Java
public class User {
    private final String name;
    private int age;

    public User(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String getName() { return name; }
    public int getAge() { return age; }
    public void setAge(int age) { this.age = age; }

    public String greet() {
        return "Hello, " + name;
    }
}
```

### Null Safety Mapping

```jv
val nullable: String? = getValue()
val result = nullable?.length ?: 0
```

```java
String nullable = getValue();
int result = nullable != null ? nullable.length() : 0;
```

This specification provides the complete definition of the jv language so that implementers and users understand its precise behavior.
