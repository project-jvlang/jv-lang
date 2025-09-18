# jv Language Specification

Formal specification for the jv (Java Sugar Language) programming language.

## Table of Contents

1. [Overview](#overview)
2. [Lexical Structure](#lexical-structure)
3. [Grammar](#grammar)
4. [Type System](#type-system)
5. [Semantics](#semantics)
6. [Standard Library](#standard-library)
7. [Java Interoperability](#java-interoperability)

## Overview

jv is a statically typed programming language that compiles to readable Java 25 source code. It provides modern syntax while maintaining full compatibility with the Java ecosystem.

### Design Goals

1. **Zero Runtime Overhead**: Compiles to pure Java with no additional runtime
2. **Java Ecosystem Compatibility**: Seamless integration with Java libraries
3. **Modern Syntax**: Kotlin-inspired syntax improvements
4. **Type Safety**: Enhanced null safety and type inference
5. **Readability**: Generated Java code should be human-readable

### Compilation Model

```
jv source (.jv) → AST → IR → Java source (.java) → bytecode (.class)
```

## Lexical Structure

### Character Set

jv source files are encoded in UTF-8. The lexical structure is case-sensitive.

### Comments

```jv
// Line comment

/*
 * Block comment
 * Can span multiple lines
 */

/**
 * Documentation comment
 * Used for API documentation
 */
```

### Identifiers

```bnf
identifier ::= letter (letter | digit | '_')*
letter     ::= 'a'..'z' | 'A'..'Z' | unicode_letter
digit      ::= '0'..'9'
```

**Reserved Words:**
```
abstract, async, await, break, class, continue, data, defer, do, else,
enum, false, final, for, fun, if, import, in, interface, is, null,
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
1e-5
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

#### String Literals

```bnf
string_literal ::= '"' string_content* '"'
string_content ::= escape_sequence | ~["\n\r]
escape_sequence ::= '\' ('n' | 'r' | 't' | '\\' | '\'' | '"' | unicode_escape)
unicode_escape ::= 'u' hex_digit{4} | 'U' hex_digit{8}
```

Examples:
```jv
"Hello, world!"
"Line 1\nLine 2"
"Unicode: \u03B1\u03B2\u03B3"
```

#### String Interpolation

```bnf
interpolated_string ::= '"' interpolation_part* '"'
interpolation_part  ::= string_content | '${' expression '}'
```

Examples:
```jv
val name = "Alice"
val age = 30
val message = "Hello, $name! You are ${age + 1} years old."
```

### Operators and Punctuation

**Arithmetic Operators:**
```
+  -  *  /  %  ++  --
```

**Comparison Operators:**
```
==  !=  <  >  <=  >=
```

**Logical Operators:**
```
&&  ||  !
```

**Bitwise Operators:**
```
&  |  ^  ~  <<  >>  >>>
```

**Assignment Operators:**
```
=  +=  -=  *=  /=  %=  &=  |=  ^=  <<=  >>=  >>>=
```

**Null Safety Operators:**
```
?  ?.  ?:  ?[]
```

**Range Operators:**
```
..  ..< 
```

**Other Operators:**
```
::  ->  =>
```

**Punctuation:**
```
(  )  [  ]  {  }  ,  ;  :  .
```

## Grammar

### Program Structure

```bnf
program ::= package_declaration? import_declaration* top_level_declaration*

package_declaration ::= 'package' qualified_identifier ';'?

import_declaration ::= 'import' qualified_identifier ('.' '*')? ';'?

top_level_declaration ::= class_declaration
                       | function_declaration  
                       | property_declaration
```

### Declarations

#### Class Declarations

```bnf
class_declaration ::= modifiers? 'class' identifier type_parameters?
                     ('(' parameter_list ')')? (':' type_list)?
                     class_body?

data_class_declaration ::= modifiers? 'data' 'class' identifier type_parameters?
                          '(' parameter_list ')' (':' type_list)?

modifiers ::= modifier+
modifier  ::= 'public' | 'private' | 'protected' | 'abstract' | 'final' 
           | 'override' | 'open' | 'sealed' | 'inner' | 'mutable'

type_parameters ::= '<' type_parameter (',' type_parameter)* '>'
type_parameter  ::= identifier (':' type)?

parameter_list ::= parameter (',' parameter)*
parameter      ::= identifier ':' type ('=' expression)?

type_list ::= type (',' type)*

class_body ::= '{' class_member* '}'
class_member ::= function_declaration | property_declaration | class_declaration
```

#### Function Declarations

```bnf
function_declaration ::= modifiers? 'fun' identifier type_parameters?
                        '(' parameter_list? ')' (':' type)?
                        (function_body | '=' expression)

function_body ::= '{' statement* '}'
```

#### Property Declarations

```bnf
property_declaration ::= modifiers? ('val' | 'var') identifier (':' type)? 
                        ('=' expression)? property_accessors?

property_accessors ::= getter setter?
                    | setter getter?

getter ::= 'get' function_body?
setter ::= 'set' '(' parameter ')' function_body?
```

### Types

```bnf
type ::= nullable_type | non_null_type

nullable_type ::= non_null_type '?'

non_null_type ::= simple_type | function_type | array_type

simple_type ::= qualified_identifier type_arguments?

function_type ::= ('(' parameter_types? ')')? '->' type
parameter_types ::= type (',' type)*

array_type ::= type '[' ']'

type_arguments ::= '<' type (',' type)* '>'
```

### Expressions

```bnf
expression ::= assignment_expression

assignment_expression ::= conditional_expression
                       | unary_expression assignment_operator assignment_expression

conditional_expression ::= logical_or_expression
                        | logical_or_expression '?' expression ':' conditional_expression

logical_or_expression ::= logical_and_expression
                       | logical_or_expression '||' logical_and_expression

logical_and_expression ::= equality_expression
                        | logical_and_expression '&&' equality_expression

equality_expression ::= relational_expression
                     | equality_expression ('==' | '!=') relational_expression

relational_expression ::= additive_expression
                       | relational_expression ('<' | '>' | '<=' | '>=') additive_expression
                       | relational_expression ('is' | '!is') type

additive_expression ::= multiplicative_expression
                     | additive_expression ('+' | '-') multiplicative_expression

multiplicative_expression ::= unary_expression
                           | multiplicative_expression ('*' | '/' | '%') unary_expression

unary_expression ::= postfix_expression
                  | ('++' | '--' | '+' | '-' | '!' | '~') unary_expression

postfix_expression ::= primary_expression postfix_suffix*

postfix_suffix ::= '.' identifier
                | '.' identifier '(' argument_list? ')'
                | '[' expression ']'
                | '(' argument_list? ')'
                | '?.' identifier
                | '?.' identifier '(' argument_list? ')'
                | '?[' expression ']'
                | '++' | '--'

primary_expression ::= literal
                    | identifier
                    | '(' expression ')'
                    | 'this'
                    | 'super'
                    | when_expression
                    | if_expression
                    | lambda_expression

when_expression ::= 'when' ('(' expression ')')? '{' when_entry* '}'
when_entry      ::= when_condition '->' (expression | statement)
when_condition  ::= expression (',' expression)*
                 | 'is' type
                 | 'in' expression
                 | 'else'

if_expression ::= 'if' '(' expression ')' expression ('else' expression)?

lambda_expression ::= '{' lambda_parameters? '->' statement* '}'
lambda_parameters ::= identifier (',' identifier)*
```

### Statements

```bnf
statement ::= declaration_statement
           | expression_statement
           | assignment_statement
           | if_statement
           | when_statement
           | for_statement
           | while_statement
           | do_while_statement
           | try_statement
           | return_statement
           | break_statement
           | continue_statement
           | block_statement

declaration_statement ::= property_declaration
                       | function_declaration
                       | class_declaration

expression_statement ::= expression ';'?

assignment_statement ::= assignment_expression ';'?

if_statement ::= 'if' '(' expression ')' statement ('else' statement)?

when_statement ::= 'when' ('(' expression ')')? '{' when_entry* '}'

for_statement ::= 'for' '(' (identifier | '(' identifier (',' identifier)* ')') 'in' expression ')' statement

while_statement ::= 'while' '(' expression ')' statement

do_while_statement ::= 'do' statement 'while' '(' expression ')' ';'?

try_statement ::= 'try' block_statement catch_clause* finally_clause?
catch_clause  ::= 'catch' '(' parameter ')' block_statement
finally_clause ::= 'finally' block_statement

return_statement ::= 'return' expression? ';'?

break_statement ::= 'break' ';'?

continue_statement ::= 'continue' ';'?

block_statement ::= '{' statement* '}'
```

## Type System

### Type Hierarchy

```
Any
├── Null
└── NotNull
    ├── Primitive Types
    │   ├── Boolean
    │   ├── Byte
    │   ├── Short
    │   ├── Int
    │   ├── Long
    │   ├── Float
    │   ├── Double
    │   └── Char
    └── Reference Types
        ├── String
        ├── Array<T>
        ├── Collection<T>
        └── User-defined types
```

### Null Safety

#### Nullable Types

Every type `T` has a nullable counterpart `T?`:

```jv
val nonNull: String = "Hello"        // Cannot be null
val nullable: String? = null         // Can be null
```

#### Type Relationships

```
T <: T?     // Every non-null type is a subtype of its nullable version
T? </: T    // Nullable types are not subtypes of non-null types
```

#### Null Safety Operators

**Safe Call (`?.`)**:
```jv
val length: Int? = str?.length  // Returns null if str is null
```

**Elvis Operator (`?:`)**:
```jv
val length: Int = str?.length ?: 0  // Provides default value
```

**Safe Index (`?[]`)**:
```jv
val item: T? = array?[index]  // Returns null if array is null
```

**Not-null Assertion (`!!`)**:
```jv
val length: Int = str!!.length  // Throws exception if str is null
```

### Type Inference

jv uses Hindley-Milner type inference with extensions for:
- Nullable types
- Generic type parameters
- Function types
- Array types

#### Type Inference Rules

**Variable Declaration**:
```jv
val x = 42          // Inferred as Int
val y = 42.0        // Inferred as Double
val z = "hello"     // Inferred as String
val w = null        // Error: Cannot infer type
```

**Function Return Types**:
```jv
fun add(a: Int, b: Int) = a + b  // Return type inferred as Int
```

**Generic Type Inference**:
```jv
val list = listOf(1, 2, 3)      // Inferred as List<Int>
val map = mapOf("a" to 1)       // Inferred as Map<String, Int>
```

### Generic Types

#### Declaration
```jv
class Container<T>(val value: T)
```

#### Type Bounds
```jv
class NumberContainer<T : Number>(val value: T)
```

#### Variance
```jv
interface Producer<out T> {      // Covariant
    fun produce(): T
}

interface Consumer<in T> {       // Contravariant
    fun consume(item: T)
}
```

### Function Types

```jv
// Function type syntax
val operation: (Int, Int) -> Int = { a, b -> a + b }

// Higher-order functions
fun apply<T, R>(value: T, transform: (T) -> R): R = transform(value)
```

## Semantics

### Variable Semantics

#### Immutable Variables (`val`)
- Must be initialized at declaration
- Cannot be reassigned
- Reference is immutable, but referenced object may be mutable

#### Mutable Variables (`var`)
- Can be declared without initialization
- Can be reassigned
- Type must be compatible with declared type

### Function Semantics

#### Function Calls
- Arguments are evaluated left-to-right
- Pass-by-value for primitives
- Pass-by-reference for objects

#### Default Parameters
```jv
fun greet(name: String, greeting: String = "Hello") = "$greeting, $name!"

// Generates method overloads in Java:
// greet(String name)
// greet(String name, String greeting)
```

#### Named Arguments
```jv
greet(name = "Alice", greeting = "Hi")

// Resolves to appropriate overload based on parameters
```

### Class Semantics

#### Data Classes
- Generate `equals()`, `hashCode()`, `toString()`
- Immutable data classes compile to Java records
- Mutable data classes compile to regular Java classes

#### Inheritance
- Single inheritance for classes
- Multiple inheritance for interfaces
- `override` keyword required for overriding methods

### Null Safety Semantics

#### Smart Casts
```jv
val s: String? = getString()
if (s != null) {
    // s is smart-cast to String (non-null) here
    println(s.length)
}
```

#### Null Safety in Collections
```jv
val list: List<String?> = listOf("a", null, "b")
val nonNullList: List<String> = list.filterNotNull()
```

### Concurrency Semantics

#### Spawn Blocks
```jv
spawn {
    // Executes in a virtual thread
    doBackgroundWork()
}
```

Compiles to:
```java
Thread.ofVirtual().start(() -> {
    doBackgroundWork();
});
```

#### Async/Await
```jv
val future: CompletableFuture<String> = async {
    fetchDataFromAPI()
}

val result = future.await()  // Blocks until completion
```

### Resource Management Semantics

#### Use Blocks
```jv
use(FileInputStream("file.txt")) { stream ->
    // stream is automatically closed
    processFile(stream)
}
```

Compiles to Java try-with-resources:
```java
try (FileInputStream stream = new FileInputStream("file.txt")) {
    processFile(stream);
}
```

#### Defer Blocks
```jv
fun processData() {
    val resource = acquireResource()
    defer { releaseResource(resource) }
    
    // Process data...
    // defer block executes on function exit
}
```

Compiles to:
```java
public void processData() {
    Resource resource = acquireResource();
    try {
        // Process data...
    } finally {
        releaseResource(resource);
    }
}
```

## Standard Library

### Core Types

#### Primitive Types
- `Boolean`: true/false values
- `Byte`: 8-bit signed integer
- `Short`: 16-bit signed integer  
- `Int`: 32-bit signed integer
- `Long`: 64-bit signed integer
- `Float`: 32-bit IEEE 754 floating point
- `Double`: 64-bit IEEE 754 floating point
- `Char`: Unicode character
- `String`: Unicode string

#### Collection Types
- `Array<T>`: Fixed-size array
- `List<T>`: Ordered collection
- `MutableList<T>`: Mutable ordered collection
- `Set<T>`: Unique elements
- `MutableSet<T>`: Mutable unique elements
- `Map<K, V>`: Key-value mapping
- `MutableMap<K, V>`: Mutable key-value mapping

### Standard Functions

#### Collection Creation
```jv
listOf(1, 2, 3)                    // Immutable list
mutableListOf(1, 2, 3)            // Mutable list
setOf(1, 2, 3, 2)                 // Set {1, 2, 3}
mapOf("a" to 1, "b" to 2)         // Map
```

#### Higher-Order Functions
```jv
list.map { it * 2 }               // Transform elements
list.filter { it > 0 }            // Filter elements
list.reduce { acc, x -> acc + x } // Reduce to single value
list.forEach { println(it) }      // Side effects
```

#### String Functions
```jv
str.length                        // String length
str.uppercase()                   // Convert to uppercase
str.substring(0, 5)               // Extract substring
str.split(",")                    // Split into list
```

### Type Aliases

```jv
typealias UserId = String
typealias Handler<T> = (T) -> Unit
```

## Java Interoperability

### Calling Java from jv

Java classes and methods can be used directly:

```jv
import java.util.ArrayList
import java.time.LocalDateTime

val list = ArrayList<String>()
list.add("Hello")

val now = LocalDateTime.now()
```

### Generated Java Structure

#### Package Structure
jv files in package `com.example` generate Java files in the same package.

#### Class Mapping
- jv classes → Java classes
- jv data classes (immutable) → Java records
- jv data classes (mutable) → Java classes with getters/setters
- jv objects → Java classes with static members

#### Function Mapping
- Top-level functions → Static methods in utility classes
- Extension functions → Static methods with receiver as first parameter

#### Type Mapping
- jv `Int` → Java `int`
- jv `String` → Java `String`  
- jv `List<T>` → Java `List<T>`
- jv `T?` → Java `@Nullable T` (with appropriate null checks)

### Annotations

jv supports Java annotations:

```jv
@Override
fun toString(): String = "Custom string"

@Deprecated("Use newMethod instead")
fun oldMethod() = 42
```

#### `@Sample`（言語拡張）
サンプルデータから型推論と初期化を行うアノテーション。モードは `Embed`（デフォルト）/`Load` を指定可能。

```
@Sample("data/users.json") val users
@Sample("https://example.com/users.json", mode=Load) val users
```

詳細仕様および CLI/設定は `jv/docs/sample-annotation.md` を参照。

### Interoperability Guidelines

1. **Null Safety**: Java methods returning nullable types should be handled carefully
2. **Exceptions**: Java checked exceptions are treated as unchecked in jv
3. **Generics**: Java raw types are discouraged; use parameterized types
4. **Collections**: Prefer jv collection literals over Java constructors
