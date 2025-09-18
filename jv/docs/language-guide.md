# jv Language Guide

Complete reference for the jv (Java Sugar Language) syntax and features.

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

### If Expressions

```jv
val max = if (a > b) a else b

val status = if (user.isActive) {
    "User is active"
} else {
    "User is inactive"
}
```

### Loops

```jv
// For loops
for (i in 1..10) {
    println(i)
}

for (item in list) {
    println(item)
}

for ((index, value) in list.withIndex()) {
    println("$index: $value")
}

// While loops
while (condition) {
    // ...
}

do {
    // ...
} while (condition)
```

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
    if (error) return  // defer block still executes
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
    return if (size >= 2) this[1] else null
}

// Usage
val text = "racecar"
if (text.isPalindrome()) {
    println("It's a palindrome!")
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

## Best Practices

1. **Prefer `val` over `var`**: Use immutable variables when possible
2. **Use data classes**: For simple data containers
3. **Leverage type inference**: Don't specify types unnecessarily
4. **Use null safety**: Take advantage of nullable types and safe operators
5. **Prefer expression syntax**: Use `when` and `if` as expressions
6. **Use extension functions**: To add functionality to existing types
7. **Keep functions pure**: Avoid side effects when possible