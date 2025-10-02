# jv Language Guide

Complete reference for the jv (Java Syntactic Sugar) syntax and features.

## Table of Contents

1. [Variables and Types](#variables-and-types)
2. [Functions](#functions)
3. [Classes and Data Classes](#classes-and-data-classes)
4. [Null Safety](#null-safety)
5. [Control Flow](#control-flow)
6. [Collections](#collections)
7. [String Interpolation](#string-interpolation)
8. [Concurrency](#concurrency)
9. [Resource Management](#resource-management)
10. [Extension Functions](#extension-functions)
11. [Java Interop](#java-interop)
12. [JSON Literals & POJO Generation](#json-literals--pojo-generation)

## Variables and Types

### Variable Declarations

```jv
// Immutable variable (final in Java)
val name = "Alice"
val age = 30

// Mutable variable
var count = 0
var isActive = true

// Explicit types (usually inferred)
val pi: Double = 3.14159
var items: List<String> = mutableListOf()
```

**Generated Java:**
```java
final String name = "Alice";
final int age = 30;

int count = 0;
boolean isActive = true;
```

### Type Inference

jv automatically infers types in most cases:

```jv
val numbers = listOf(1, 2, 3)        // List<Int>
val map = mapOf("key" to "value")    // Map<String, String>
val lambda = { x: Int -> x * 2 }     // Function1<Int, Int>
```

## Functions

### Basic Functions

```jv
fun greet(name: String): String {
    return "Hello, $name!"
}

// Expression body
fun add(a: Int, b: Int): Int = a + b

// Unit return type (void)
fun printInfo(message: String) {
    println(message)
}
```

### Default Parameters

```jv
fun createUser(name: String, age: Int = 18, active: Boolean = true): User {
    return User(name, age, active)
}

// Usage
val user1 = createUser("Alice")
val user2 = createUser("Bob", 25)
val user3 = createUser("Charlie", 30, false)
```

**Generated Java** (method overloads):
```java
public static User createUser(String name) {
    return createUser(name, 18, true);
}

public static User createUser(String name, int age) {
    return createUser(name, age, true);
}

public static User createUser(String name, int age, boolean active) {
    return new User(name, age, active);
}
```

### Named Arguments

```jv
fun configureServer(
    host: String,
    port: Int,
    ssl: Boolean = false,
    timeout: Int = 30
) { /* ... */ }

// Usage with named arguments
configureServer(
    host = "localhost",
    port = 8080,
    ssl = true
)
```

### Top-level Functions

```jv
// Top-level functions become static methods in utility classes
fun calculateDistance(x1: Double, y1: Double, x2: Double, y2: Double): Double {
    return sqrt((x2 - x1).pow(2) + (y2 - y1).pow(2))
}
```

**Generated Java:**
```java
public class MathUtils {
    public static double calculateDistance(double x1, double y1, double x2, double y2) {
        return Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));
    }
}
```

## Classes and Data Classes

### Regular Classes

```jv
class Person(val name: String, var age: Int) {
    fun greet(): String {
        return "Hello, I'm $name and I'm $age years old"
    }
    
    fun haveBirthday() {
        age++
    }
}
```

### Data Classes

```jv
// Immutable data class -> Java record
data class Point(val x: Double, val y: Double)

// Mutable data class -> Java class with getters/setters
data class mutable Counter(var value: Int) {
    fun increment() {
        value++
    }
}
```

**Generated Java** (immutable):
```java
public record Point(double x, double y) {}
```

**Generated Java** (mutable):
```java
public class Counter {
    private int value;
    
    public Counter(int value) {
        this.value = value;
    }
    
    public int getValue() { return value; }
    public void setValue(int value) { this.value = value; }
    
    public void increment() {
        value++;
    }
}
```

### Inheritance

```jv
abstract class Animal(val name: String) {
    abstract fun makeSound(): String
}

class Dog(name: String, val breed: String) : Animal(name) {
    override fun makeSound(): String = "Woof!"
}

interface Flyable {
    fun fly(): String
}

class Bird(name: String) : Animal(name), Flyable {
    override fun makeSound(): String = "Tweet!"
    override fun fly(): String = "$name is flying"
}
```

## Null Safety

### Nullable Types

```jv
var nullableName: String? = null
val nonNullName: String = "Alice"

// Compile error: nullableName = nonNullName  // OK
// nonNullName = nullableName  // Error!
```

### Safe Call Operator

```jv
val length = nullableName?.length  // Returns Int? (null if nullableName is null)

// Chaining
val firstChar = person?.name?.firstOrNull()?.uppercase()
```

**Generated Java:**
```java
Integer length = nullableName != null ? nullableName.length() : null;
```

### Elvis Operator

```jv
val name = nullableName ?: "Default Name"
val length = nullableName?.length ?: 0
```

**Generated Java:**
```java
String name = nullableName != null ? nullableName : "Default Name";
```

### Safe Index Access

```jv
val list: List<String>? = getList()
val firstItem = list?[0]  // Safe array/list access
```

## Control Flow

### When Expressions

```jv
fun describe(x: Any): String = when (x) {
    is String -> "String of length ${x.length}"
    is Int -> when {
        x > 0 -> "Positive integer: $x"
        x < 0 -> "Negative integer: $x"
        else -> "Zero"
    }
    is List<*> -> "List with ${x.size} items"
    else -> "Unknown type"
}
```

For deep dives into pattern forms, exhaustiveness analysis, and code generation strategies, see the dedicated [Pattern Matching Guide](pattern-matching-en.md).

**Generated Java** (using Java 25 pattern matching):
```java
public static String describe(Object x) {
    return switch (x) {
        case String s -> "String of length " + s.length();
        case Integer i when i > 0 -> "Positive integer: " + i;
        case Integer i when i < 0 -> "Negative integer: " + i;
        case Integer i -> "Zero";
        case List<?> list -> "List with " + list.size() + " items";
        default -> "Unknown type";
    };
}
```

### Subjectless `when` blocks

Use `when { ... }` without a subject when the branch conditions come from standalone boolean guards. Each arrow arm is checked in order and evaluation short-circuits on the first match.

```jv
when {
    request.isAuthenticated -> allow(request)
    request.hasSession -> resume(request)
    else -> reject(request)
}
```

**Generated Java:**
```java
if (request.isAuthenticated()) {
    allow(request);
} else if (request.hasSession()) {
    resume(request);
} else {
    reject(request);
}
```

### Guarded branches with `&&`

Branch patterns can add boolean guards with `&&`. Guard expressions may reference smart-cast variables that originate from the pattern on the same line.

```jv
fun render(model: Any): String = when (model) {
    is User && model.isActive -> "Welcome back, ${model.displayName}"
    is User -> "Please activate your account"
    is Error && model.isRecoverable -> "Retry later"
    else -> "Unsupported"
}
```

The guard is preserved through AST, type checking, and IR so that code generation evaluates it exactly once. If every branch fails in an expression context, diagnostic `E_WHEN_002` asks for an explicit `else` branch.

### Implicit `else -> Unit`

When a `when` appears in a `Unit` context (statements, `Unit`-returning functions), the compiler inserts an implicit `else -> Unit`. You may therefore omit a final branch and still pass control-flow analysis.

```jv
fun process(messages: List<Message>) {
    when {
        messages.isEmpty() -> log("Nothing to do")
        messages.size > 100 -> warn("Large batch")
        messages.any { it.isError } -> report(messages)
        // No explicit else needed: implicit else -> Unit is inserted
    }
}
```

In expression contexts (non-`Unit` return types), omitting `else` still triggers `E_WHEN_002`, mirroring the requirement that every code path produce a value.

### Conditional logic with `when`

```jv
val max = when {
    a > b -> a
    else -> b
}

val status = when {
    user.isActive -> "User is active"
    else -> "User is inactive"
}
```

> Note: Always include an `else` branch when using `when` as an expression. Missing `else` triggers diagnostic `E_WHEN_002` (an implicit `else -> Unit` is inserted for `Unit` contexts). jv does not ship an `if` syntax—writing `if` emits diagnostic `JV3103`. Use `when` for every conditional.

### Loops

Loops in jv use the `for (binding in source)` syntax, which works with numeric ranges and
iterable values.

#### Numeric Ranges

```jv
for (index in 0..limit) {
    println(index) // exclusive upper bound
}

for (day in 1..=7) {
    println(day) // inclusive upper bound
}
```

- `a..b` iterates while `a < b`; `a..=b` includes the final value.
- Range endpoints must resolve to the same numeric type. Mismatches trigger diagnostic
  `E_LOOP_002`.

#### Iterable Sources

```jv
val items = arrayOf("north", "south", "east", "west")

for (direction in items) {
    println(direction)
}
```

- The source expression must expose the iterable protocol (arrays, sequences, or custom
  types with `iterator`). Missing protocol support emits `E_LOOP_003`.
- Lazy sequences that implement `AutoCloseable` automatically gain cleanup logic in the
  generated Java code.

#### Replacing Classic `while` Patterns

Conditional loops that would otherwise use `while (condition)` can be expressed with
`for` and sequence utilities:

```jv
// Read values while a predicate holds
for (token in scanner.intoSequence().takeWhile { it != null }) {
    process(token)
}

// Traverse ancestors while they match a type (while (node is Tree) ...)
for (node in generateSequence(root) { it?.parent }.takeWhile { it is Tree }) {
    visit(node as Tree)
}

// Counter progression with custom steps
for (index in generateSequence(start) { it + step }.takeWhile { it < limit }) {
    println(index)
}

// Sentinel-driven loops remain explicit
for (_ in sequenceOf(Unit).repeat()) {
    when {
        done() -> break
    }
    performWork()
}
```

- Pull-state loops map naturally to sequences created from iterators or generators and
  can use `takeWhile` / `takeUntil` helpers to stop when the condition becomes false.
- Counter-based loops should become numeric ranges (`start until end`, `start..=end`).
- Sentinel-style loops can keep explicit `break` statements inside the `for` body.

#### Diagnostics

- `E_LOOP_001`: A non-supported loop construct (such as `while`/`do-while`) was detected.
- `E_LOOP_002`: Numeric range bounds use incompatible types.
- `E_LOOP_003`: Loop source is not iterable.

## Collections

### Creating Collections

```jv
val list = listOf(1, 2, 3, 4, 5)
val mutableList = mutableListOf("a", "b", "c")

val set = setOf(1, 2, 3, 2)  // {1, 2, 3}
val map = mapOf("key1" to "value1", "key2" to "value2")
```

### Collection Operations

```jv
val numbers = listOf(1, 2, 3, 4, 5)

val doubled = numbers.map { it * 2 }
val evens = numbers.filter { it % 2 == 0 }
val sum = numbers.reduce { acc, n -> acc + n }

val firstPositive = numbers.firstOrNull { it > 0 }
val hasNegative = numbers.any { it < 0 }
val allPositive = numbers.all { it > 0 }
```

## String Interpolation

```jv
val name = "Alice"
val age = 30

val message = "Hello, my name is $name and I'm $age years old"
val calculation = "The result is ${2 + 2}"
val nested = "User: ${user.name.uppercase()}"
```

**Generated Java:**
```java
String message = String.format("Hello, my name is %s and I'm %d years old", name, age);
String calculation = "The result is " + (2 + 2);
```

## Concurrency

### Virtual Threads (spawn)

```jv
fun processData() {
    spawn {
        // This runs in a virtual thread
        val result = heavyComputation()
        println("Result: $result")
    }
}
```

**Generated Java:**
```java
public void processData() {
    Thread.ofVirtual().start(() -> {
        var result = heavyComputation();
        System.out.println("Result: " + result);
    });
}
```

### Async/Await

```jv
async fun fetchData(): CompletableFuture<String> {
    return CompletableFuture.supplyAsync {
        // Simulate API call
        Thread.sleep(1000)
        "Data fetched"
    }
}

fun main() {
    val future = fetchData()
    val result = future.await()  // Blocks until complete
    println(result)
}
```

## Resource Management

### Use Blocks (Try-with-resources)

```jv
use(FileInputStream("file.txt")) { input ->
    val data = input.readAllBytes()
    processData(data)
}
```

**Generated Java:**
```java
try (FileInputStream input = new FileInputStream("file.txt")) {
    byte[] data = input.readAllBytes();
    processData(data);
}
```

### Defer Blocks

```jv
fun processFile(filename: String) {
    val file = File(filename)
    
    defer {
        println("Cleaning up...")
        file.delete()
    }
    
    // Process file...
    when {
        error -> return  // defer block still executes
    }
}
```

**Generated Java:**
```java
public void processFile(String filename) {
    File file = new File(filename);
    try {
        // Process file...
        if (error) return;
    } finally {
        System.out.println("Cleaning up...");
        file.delete();
    }
}
```

## Extension Functions

```jv
// Extend existing types
fun String.isPalindrome(): Boolean {
    return this == this.reversed()
}

fun <T> List<T>.secondOrNull(): T? {
    return when {
        size >= 2 -> this[1]
        else -> null
    }
}

// Usage
val text = "racecar"
when {
    text.isPalindrome() -> println("It's a palindrome!")
}

val second = listOf(1, 2, 3).secondOrNull()  // 2
```

**Generated Java** (static methods):
```java
public class StringExtensions {
    public static boolean isPalindrome(String self) {
        return self.equals(reverse(self));
    }
}

public class ListExtensions {
    public static <T> T secondOrNull(List<T> self) {
        return self.size() >= 2 ? self.get(1) : null;
    }
}
```

## Java Interop

jv can seamlessly use Java libraries:

```jv
import java.util.concurrent.ConcurrentHashMap
import java.time.LocalDateTime

fun useJavaLibrary() {
    val map = ConcurrentHashMap<String, String>()
    map.put("key", "value")
    
    val now = LocalDateTime.now()
    println("Current time: $now")
}
```

The generated Java code directly uses these Java APIs without any wrapper layers.

## JSON Literals & POJO Generation

Inline literals such as `val payload = { ... }` are desugared into Java 25 `record`s, giving you type-safe access like `payload.user.name`. For end-to-end details—including JSONC comment preservation and schema inference—see the [JSON Literals & POJO Generation guide](json-en.md).

## Best Practices

1. **Prefer `val` over `var`**: Use immutable variables when possible
2. **Use data classes**: For simple data containers
3. **Leverage type inference**: Don't specify types unnecessarily
4. **Use null safety**: Take advantage of nullable types and safe operators
5. **Prefer expression syntax**: Model conditionals with `when` expressions
6. **Use extension functions**: To add functionality to existing types
7. **Keep functions pure**: Avoid side effects when possible
