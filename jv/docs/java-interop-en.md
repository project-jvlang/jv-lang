# Java Interoperability

This guide covers how jv interoperates with Java code, libraries, and the broader Java ecosystem.

## Table of Contents

1. [Overview](#overview)
2. [Calling Java from jv](#calling-java-from-jv)
3. [Generated Java Code](#generated-java-code)
4. [Type Mapping](#type-mapping)
5. [Null Safety Integration](#null-safety-integration)
6. [Collections and Generics](#collections-and-generics)
7. [Exception Handling](#exception-handling)
8. [Annotations](#annotations)
9. [Best Practices](#best-practices)
10. [Common Patterns](#common-patterns)

## Overview

jv is designed for seamless interoperability with Java:

- **Zero Runtime Overhead**: jv compiles to pure Java with no additional runtime
- **Direct Library Access**: Use any Java library without wrappers
- **Readable Output**: Generated Java looks like hand-written code
- **Standard Compliance**: Generated code follows Java conventions

## Calling Java from jv

### Basic Java Library Usage

Java classes and methods can be used directly in jv:

```jv
import java.util.ArrayList
import java.util.HashMap
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun demonstrateJavaInterop() {
    // Using Java collections
    val list = ArrayList<String>()
    list.add("Hello")
    list.add("World")
    
    // Using Java time API
    val now = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formatted = now.format(formatter)
    
    println("Current time: $formatted")
    
    // Using Java maps
    val map = HashMap<String, Int>()
    map.put("apple", 5)
    map.put("banana", 3)
    
    // Java 8+ streams
    val total = map.values()
        .stream()
        .mapToInt { value -> value }
        .sum()
    
    println("Total fruits: $total")
}
```

**Generated Java:**
```java
import java.util.ArrayList;
import java.util.HashMap;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class MainUtils {
    public static void demonstrateJavaInterop() {
        ArrayList<String> list = new ArrayList<>();
        list.add("Hello");
        list.add("World");
        
        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        String formatted = now.format(formatter);
        
        System.out.println("Current time: " + formatted);
        
        HashMap<String, Integer> map = new HashMap<>();
        map.put("apple", 5);
        map.put("banana", 3);
        
        int total = map.values()
            .stream()
            .mapToInt(Integer::intValue)
            .sum();
        
        System.out.println("Total fruits: " + total);
    }
}
```

### Static Methods and Fields

```jv
import java.lang.Math
import java.lang.System
import java.util.Collections

fun useStaticMembers() {
    // Static methods
    val sqrt = Math.sqrt(16.0)        // 4.0
    val max = Math.max(10, 20)        // 20
    
    // Static fields
    val pi = Math.PI                  // 3.141592653589793
    val lineSeparator = System.lineSeparator()
    
    // Static utility methods
    val list = mutableListOf(3, 1, 4, 1, 5)
    Collections.sort(list)
    println(list)  // [1, 1, 3, 4, 5]
}
```

### Builder Patterns

Java builder patterns work naturally:

```jv
import java.time.LocalDate
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

fun useBuilderPattern() {
    val formatter = DateTimeFormatterBuilder()
        .appendValue(ChronoField.YEAR, 4)
        .appendLiteral('-')
        .appendValue(ChronoField.MONTH_OF_YEAR, 2)
        .appendLiteral('-')
        .appendValue(ChronoField.DAY_OF_MONTH, 2)
        .toFormatter()
    
    val date = LocalDate.now()
    val formatted = date.format(formatter)
    println(formatted)
}
```

## Generated Java Code

### Class Generation

jv classes compile to standard Java classes:

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

**Generated Java:**
```java
public class Person {
    private final String name;
    private int age;
    
    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
    
    public String getName() {
        return name;
    }
    
    public int getAge() {
        return age;
    }
    
    public void setAge(int age) {
        this.age = age;
    }
    
    public String greet() {
        return "Hello, I'm " + name + " and I'm " + age + " years old";
    }
    
    public void haveBirthday() {
        age++;
    }
}
```

### Data Classes to Records

Immutable data classes become Java records:

```jv
data class Point(val x: Double, val y: Double) {
    fun distanceFromOrigin(): Double {
        return Math.sqrt(x * x + y * y)
    }
}
```

**Generated Java:**
```java
public record Point(double x, double y) {
    public double distanceFromOrigin() {
        return Math.sqrt(x * x + y * y);
    }
}
```

### Extension Functions

Extension functions become static utility methods:

```jv
fun String.isPalindrome(): Boolean {
    val cleaned = this.lowercase().replace(Regex("\\W"), "")
    return cleaned == cleaned.reversed()
}

fun <T> List<T>.secondOrNull(): T? {
    return if (size >= 2) this[1] else null
}
```

**Generated Java:**
```java
public class StringExtensions {
    public static boolean isPalindrome(String self) {
        String cleaned = self.toLowerCase().replaceAll("\\W", "");
        return cleaned.equals(new StringBuilder(cleaned).reverse().toString());
    }
}

public class ListExtensions {
    public static <T> T secondOrNull(List<T> self) {
        return self.size() >= 2 ? self.get(1) : null;
    }
}
```

### Regex Literals

Regex literals provide a direct bridge to `java.util.regex.Pattern` while keeping the generated
code explicit and dependency-free:

```jv
val pattern = /\d{3}-\d{4}/
```

**Generated Java:**
```java
import java.util.regex.Pattern;

private static final Pattern PATTERN = Pattern.compile("\\d{3}-\\d{4}");
```

- Static hoisting ensures the pattern is compiled once per class.
- Unsupported escapes or unbalanced groups surface as checker diagnostics (`JV5101`–`JV5104`) so
  invalid patterns never reach the Java backend.
- String literals with slashes remain untouched; only the `/.../` form without quotes is recognized.

### Top-level Functions

Top-level functions are placed in utility classes:

```jv
// In file: math_utils.jv
fun factorial(n: Int): Long {
    return if (n <= 1) 1L else n * factorial(n - 1)
}

fun gcd(a: Int, b: Int): Int {
    return if (b == 0) a else gcd(b, a % b)
}
```

**Generated Java:**
```java
public class MathUtils {
    public static long factorial(int n) {
        return n <= 1 ? 1L : n * factorial(n - 1);
    }
    
    public static int gcd(int a, int b) {
        return b == 0 ? a : gcd(b, a % b);
    }
}
```

## Type Mapping

### Primitive Types

| jv Type | Java Type | Notes |
|---------|-----------|-------|
| `Boolean` | `boolean` | Primitive boolean |
| `Byte` | `byte` | 8-bit signed integer |
| `Short` | `short` | 16-bit signed integer |
| `Int` | `int` | 32-bit signed integer |
| `Long` | `long` | 64-bit signed integer |
| `Float` | `float` | 32-bit IEEE 754 |
| `Double` | `double` | 64-bit IEEE 754 |
| `Char` | `char` | Unicode character |

### Reference Types

| jv Type | Java Type | Notes |
|---------|-----------|-------|
| `String` | `String` | Immutable string |
| `Array<T>` | `T[]` | Native Java arrays |
| `List<T>` | `List<T>` | Java List interface |
| `MutableList<T>` | `List<T>` | Mutable Java List |
| `Set<T>` | `Set<T>` | Java Set interface |
| `Map<K,V>` | `Map<K,V>` | Java Map interface |

### Nullable Types

```jv
val nullable: String? = null
val nonNull: String = "hello"
```

**Generated Java:**
```java
@Nullable String nullable = null;
String nonNull = "hello";  // Non-null by convention
```

### Generic Types

```jv
class Container<T>(val value: T) {
    fun <R> map(transform: (T) -> R): Container<R> {
        return Container(transform(value))
    }
}
```

**Generated Java:**
```java
public class Container<T> {
    private final T value;
    
    public Container(T value) {
        this.value = value;
    }
    
    public T getValue() {
        return value;
    }
    
    public <R> Container<R> map(Function<T, R> transform) {
        return new Container<>(transform.apply(value));
    }
}
```

## Null Safety Integration

### Java Libraries with Nullable Returns

When calling Java methods that may return null:

```jv
import java.util.HashMap

fun handleNullableJavaReturns() {
    val map = HashMap<String, String>()
    
    // Java's get() can return null
    val value: String? = map.get("key")  // Explicitly nullable
    
    // Safe handling
    val length = value?.length ?: 0
    
    // Or with smart cast
    if (value != null) {
        println("Length: ${value.length}")  // value is smart-cast to String
    }
}
```

**Generated Java:**
```java
import java.util.HashMap;

public class MainUtils {
    public static void handleNullableJavaReturns() {
        HashMap<String, String> map = new HashMap<>();
        
        String value = map.get("key");
        
        int length = value != null ? value.length() : 0;
        
        if (value != null) {
            System.out.println("Length: " + value.length());
        }
    }
}
```

### Annotations for Better Integration

Use nullable annotations for clearer contracts:

```jv
import org.jetbrains.annotations.Nullable
import org.jetbrains.annotations.NotNull

// This would be in Java
interface JavaService {
    @Nullable 
    fun findUser(id: String): User?
    
    @NotNull
    fun createUser(name: String): User
}
```

In jv, this allows the compiler to understand nullability:

```jv
fun useJavaService(service: JavaService) {
    val user = service.findUser("123")  // Compiler knows this is nullable
    user?.let { 
        println("Found user: ${it.name}")
    }
    
    val newUser = service.createUser("Alice")  // Compiler knows this is non-null
    println("Created user: ${newUser.name}")   // No null check needed
}
```

## Collections and Generics

### Java Collections in jv

```jv
import java.util.*
import java.util.stream.Collectors

fun workWithJavaCollections() {
    // ArrayList
    val list = ArrayList<String>()
    list.addAll(listOf("a", "b", "c"))
    
    // HashMap  
    val map = HashMap<String, Int>()
    map.putAll(mapOf("x" to 1, "y" to 2))
    
    // Streams
    val filtered = list.stream()
        .filter { value -> value.startsWith("a") }
        .collect(Collectors.toList())
    
    // TreeSet (sorted)
    val sorted = TreeSet(listOf(3, 1, 4, 1, 5))
    println(sorted)  // [1, 3, 4, 5]
}
```

### Converting Between jv and Java Collections

```jv
fun convertCollections() {
    // jv list to Java
    val jvList = listOf(1, 2, 3)
    val javaList = ArrayList(jvList)
    
    // Java list to jv  
    val javaArrayList = ArrayList<String>()
    javaArrayList.addAll(listOf("a", "b"))
    val jvListFromJava = javaArrayList.toList()
    
    // Working with both
    val combined = jvList + javaList.map { value -> value * 2 }
}
```

### Generic Constraints

```jv
// Java-style bounded generics work in jv
fun <T : Comparable<T>> sort(items: MutableList<T>) {
    Collections.sort(items)
}

// Multiple bounds
fun <T> process(item: T) where T : Serializable, T : Cloneable {
    // T must implement both interfaces
}
```

## Exception Handling

### Java Checked Exceptions

jv treats all Java exceptions as unchecked:

```jv
import java.io.FileInputStream
import java.io.IOException

fun readFile(filename: String): String {
    // No need to declare throws or catch IOException
    val input = FileInputStream(filename)
    val content = input.readAllBytes()
    input.close()
    return String(content)
}
```

**Generated Java:**
```java
import java.io.FileInputStream;
import java.io.IOException;

public class FileUtils {
    public static String readFile(String filename) {
        try {
            FileInputStream input = new FileInputStream(filename);
            byte[] content = input.readAllBytes();
            input.close();
            return new String(content);
        } catch (IOException e) {
            throw new RuntimeException(e);  // Wrap checked exception
        }
    }
}
```

### Exception Handling Patterns

```jv
import java.io.File
import java.nio.file.Files

fun safeReadFile(filename: String): String? {
    return try {
        Files.readString(File(filename).toPath())
    } catch (e: Exception) {
        println("Error reading file: ${e.message}")
        null
    }
}
```

## Annotations

### Java Annotations in jv

```jv
import java.lang.Deprecated
import java.lang.Override
import org.junit.jupiter.api.Test

class UserService {
    @Override
    fun toString(): String {
        return "UserService"
    }
    
    @Deprecated("Use createUser instead")
    fun addUser(name: String): User {
        return createUser(name)
    }
    
    fun createUser(name: String): User {
        return User(name)
    }
}

class UserServiceTest {
    @Test
    fun testCreateUser() {
        val service = UserService()
        val user = service.createUser("Alice")
        assert(user.name == "Alice")
    }
}
```

### Annotation Parameters

```jv
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.stereotype.Service

@Service("userService")
class UserController {
    
    @RequestMapping(
        value = ["/users"],
        method = [RequestMethod.GET],
        produces = ["application/json"]
    )
    fun getUsers(): List<User> {
        return userService.findAll()
    }
}
```

### Custom Annotations

Define annotations in jv:

```jv
@Target(AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class Benchmark(val iterations: Int = 1000)

class MathService {
    @Benchmark(iterations = 10000)
    fun fibonacci(n: Int): Long {
        return if (n <= 1) n.toLong() else fibonacci(n-1) + fibonacci(n-2)
    }
}
```

## Best Practices

### Sequence Pipelines

```jv
val numbers = listOf(1, 2, 3, 4, 5)

val doubled = numbers.map { value -> value * 2 }
val evens = numbers.filter { value -> value % 2 == 0 }
val sum = numbers.reduce { acc, value -> acc + value }

// Use forEach { value -> ... } when the pipeline performs side effects.
// toList()/toSet() emit .stream().toList()/toSet() for Java 25 targets and
// fall back to Collectors.toList()/Collectors.toSet() on Java 21.
```

Iterable and array sources automatically promote to lazy `Sequence` pipelines, so
there is no need to introduce `asSequence()`. Sequence extension lambdas must
declare explicit parameter names; the implicit `it` placeholder is not supported.

### Library Integration

1. **Use Idiomatic jv**: Don't just translate Java code
   ```jv
   // Good: Use jv collections and null safety
   fun processUsers(users: List<User?>): List<String> {
       return users.mapNotNull { user -> user?.name }
   }
   
   // Less ideal: Direct Java style  
   fun processUsersJavaStyle(users: ArrayList<User>): ArrayList<String> {
       val result = ArrayList<String>()
       for (user in users) {
           if (user != null) {
               result.add(user.name)
           }
       }
       return result
   }
   ```

2. **Leverage Extension Functions**: Add jv-style APIs to Java classes
   ```jv
   // Extend Java classes with jv convenience
   fun File.readText(): String = Files.readString(this.toPath())
   fun Path.writeText(text: String) = Files.writeString(this, text)
   
   // Usage becomes more jv-like
   val content = File("config.txt").readText()
   Paths.get("output.txt").writeText("Hello, World!")
   ```

3. **Handle Nullability Explicitly**: Be clear about nullable Java APIs
   ```jv
   // Make nullability explicit for Java interop
   fun findUserById(id: String): User? {
       return userRepository.findById(id)  // Java method that returns Optional
           ?.orElse(null)
   }
   ```

### Performance Considerations

1. **Minimize Boxing**: Use primitives when possible
   ```jv
   // Good: Uses primitive int
   fun sum(numbers: IntArray): Int {
       return numbers.sum()
   }
   
   // Less efficient: Uses Integer objects
   fun sumBoxed(numbers: List<Int>): Int {
       return numbers.reduce { acc, n -> acc + n }
   }
   ```

2. **Reuse Collections**: Don't create unnecessary collections
   ```jv
   // Good: Transform in place
   fun processInPlace(list: MutableList<String>) {
       list.replaceAll { value -> value.uppercase() }
   }
   
   // Less efficient: Create new collection
   fun processNew(list: List<String>): List<String> {
       return list.map { value -> value.uppercase() }
   }
   ```

### Type Safety

1. **Use Specific Types**: Prefer specific Java types over generic ones
   ```jv
   // Good: Specific type
   fun processUsers(users: ArrayList<User>) { ... }
   
   // Less specific: Generic collection
   fun processUsers(users: Collection<User>) { ... }
   ```

2. **Generic Constraints**: Use bounded generics for type safety
   ```jv
   fun <T : Number> average(numbers: List<T>): Double {
       return numbers.map { value -> value.toDouble() }.average()
   }
   ```

## Common Patterns

### Builder Pattern Integration

```jv
// Works seamlessly with Java builders
import java.time.LocalDateTime
import java.time.format.DateTimeFormatterBuilder

fun createCustomFormatter(): DateTimeFormatter {
    return DateTimeFormatterBuilder()
        .appendValue(ChronoField.YEAR)
        .appendLiteral("-")
        .appendValue(ChronoField.MONTH_OF_YEAR, 2)
        .appendLiteral("-")  
        .appendValue(ChronoField.DAY_OF_MONTH, 2)
        .toFormatter()
}
```

### Spring Framework Integration

```jv
import org.springframework.stereotype.Service
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("/api/users")
class UserController @Autowired constructor(
    private val userService: UserService
) {
    
    @GetMapping
    fun getAllUsers(): List<User> = userService.findAll()
    
    @PostMapping  
    fun createUser(@RequestBody user: User): User {
        return userService.save(user)
    }
    
    @GetMapping("/{id}")
    fun getUser(@PathVariable id: Long): User? {
        return userService.findById(id)
    }
}
```

### Stream API Integration

```jv
import java.util.stream.Collectors

fun processData(data: List<String>): Map<Int, List<String>> {
    return data.stream()
        .filter { value -> value.isNotEmpty() }
        .collect(Collectors.groupingBy { value -> value.length })
}
```

This comprehensive integration makes jv a natural fit for Java ecosystems while providing modern language features and improved developer experience.
