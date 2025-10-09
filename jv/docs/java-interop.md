# Java相互運用性

**日本語** | [English](java-interop-en.md)

このガイドでは、jvがJavaコード、ライブラリ、そしてより広いJavaエコシステムとどのように相互運用するかを説明します。

## 目次

1. [概要](#概要)
2. [jvからJavaを呼び出す](#jvからjavaを呼び出す)
3. [生成されるJavaコード](#生成されるjavaコード)
4. [型マッピング](#型マッピング)
5. [null安全性統合](#null安全性統合)
6. [コレクションとジェネリクス](#コレクションとジェネリクス)
7. [例外処理](#例外処理)
8. [アノテーション](#アノテーション)
9. [ベストプラクティス](#ベストプラクティス)
10. [共通パターン](#共通パターン)

## 概要

jvはJavaとのシームレスな相互運用性を持つよう設計されています：

- **ゼロランタイムオーバーヘッド**: jvは追加ランタイムなしで純粋なJavaにコンパイル
- **直接ライブラリアクセス**: ラッパーなしであらゆるJavaライブラリを使用
- **読みやすい出力**: 生成されるJavaは手書きコードのように見える
- **標準準拠**: 生成されるコードはJava規約に従う

## jvからJavaを呼び出す

### 基本的なJavaライブラリ使用

Javaクラスとメソッドはjvで直接使用できます：

```jv
import java.util.ArrayList
import java.util.HashMap
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun demonstrateJavaInterop() {
    // Javaコレクションの使用
    val list = ArrayList<String>()
    list.add("Hello")
    list.add("World")

    // Java time APIの使用
    val now = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formatted = now.format(formatter)

    println("Current time: $formatted")

    // Javaマップの使用
    val map = HashMap<String, Int>()
    map.put("apple", 5)
    map.put("banana", 3)

    // Java 8+ ストリーム
    val total = map.values()
        .stream()
        .mapToInt { value -> value }
        .sum()

    println("Total fruits: $total")
}
```

**生成されるJava:**
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

### 静的メソッドとフィールド

```jv
import java.lang.Math
import java.lang.System
import java.util.Collections

fun useStaticMembers() {
    // 静的メソッド
    val sqrt = Math.sqrt(16.0)        // 4.0
    val max = Math.max(10, 20)        // 20

    // 静的フィールド
    val pi = Math.PI                  // 3.141592653589793
    val lineSeparator = System.lineSeparator()

    // 静的ユーティリティメソッド
    val list = mutableListOf(3, 1, 4, 1, 5)
    Collections.sort(list)
    println(list)  // [1, 1, 3, 4, 5]
}
```

### ビルダーパターン

Javaビルダーパターンは自然に動作します：

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

## 生成されるJavaコード

### クラス生成

jvクラスは慣用的なJavaコードを生成：

```jv
// jvクラス
class User(val name: String, var age: Int) {
    fun greet(): String = "Hello, I'm $name"

    fun haveBirthday() {
        age++
    }
}
```

**生成されるJava:**
```java
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
        return "Hello, I'm " + name;
    }

    public void haveBirthday() {
        age++;
    }
}
```

### データクラス

```jv
// 不変データクラス
data class Point(val x: Double, val y: Double)

// 可変データクラス
data class mutable Counter(var value: Int)
```

**生成されるJava:**
```java
// 不変 -> record
public record Point(double x, double y) {}

// 可変 -> 完全クラス
public class Counter {
    private int value;

    public Counter(int value) { this.value = value; }

    public int getValue() { return value; }
    public void setValue(int value) { this.value = value; }

    // equals, hashCode, toString...
}
```

## 型マッピング

### プリミティブ型

| jv型 | Java型 | 注意 |
|------|--------|------|
| `Int` | `int` | 32ビット整数 |
| `Long` | `long` | 64ビット整数 |
| `Float` | `float` | 32ビット浮動小数点 |
| `Double` | `double` | 64ビット浮動小数点 |
| `Boolean` | `boolean` | 真偽値 |
| `Char` | `char` | 16ビット文字 |
| `String` | `String` | 文字列 |
| `Unit` | `void` | 戻り値なし |

### nullable型

```jv
val nullable: String? = null
val nonNull: String = "value"
```

**生成されるJava:**
```java
String nullable = null;         // String（nullable）
String nonNull = "value";       // String（non-null、実行時チェック）
```

### コレクション型

| jv型 | Java型 |
|------|--------|
| `List<T>` | `List<T>` |
| `MutableList<T>` | `List<T>` |
| `Set<T>` | `Set<T>` |
| `Map<K, V>` | `Map<K, V>` |
| `Array<T>` | `T[]` |

## 正規表現リテラル

`/.../` リテラルは追加のライブラリを必要とせず、Java の `java.util.regex.Pattern` へ
直接変換されます。

```jv
val postalCode = /\d{3}-\d{4}/

fun normalize(input: String): Boolean {
    return postalCode.matcher(input).matches()
}
```

**生成されるJava:**
```java
import java.util.regex.Pattern;

private static final Pattern POSTAL_CODE = Pattern.compile("\\d{3}-\\d{4}");

public static boolean normalize(String input) {
    return POSTAL_CODE.matcher(input).matches();
}
```

- スラッシュは `\/` でエスケープします。末尾に単独のバックスラッシュがあると
  `JV5102` 診断が発生します。
- 括弧の未閉鎖や互換性のないエスケープは `RegexValidator` が静的に検出し、エラーを
  提示します。
- 引用符付きの文字列リテラル（`"/path/"` など）は従来どおり文字列として扱われます。

## null安全性統合

### 安全呼び出し演算子

```jv
val user: User? = getUser()
val name = user?.name  // 安全なアクセス
```

**生成されるJava:**
```java
User user = getUser();
String name = user != null ? user.getName() : null;
```

### エルビス演算子

```jv
val name = user?.name ?: "Unknown"
```

**生成されるJava:**
```java
String name = user != null ? user.getName() : "Unknown";
```

### null安全なキャスト

```jv
val str = obj as? String  // 失敗時はnull
```

**生成されるJava:**
```java
String str = obj instanceof String ? (String) obj : null;
```

## コレクションとジェネリクス

### 型安全性

```jv
val list: List<String> = listOf("a", "b", "c")
val map: Map<String, Int> = mapOf("key" to 1)
```

**生成されるJava:**
```java
List<String> list = List.of("a", "b", "c");
Map<String, Integer> map = Map.of("key", 1);
```

### ジェネリック関数

```jv
fun <T> identity(value: T): T = value
fun <T, R> map(list: List<T>, transform: (T) -> R): List<R> {
    return list.map(transform)
}
```

**生成されるJava:**
```java
public static <T> T identity(T value) {
    return value;
}

public static <T, R> List<R> map(List<T> list, Function<T, R> transform) {
    return list.stream().map(transform).collect(Collectors.toList());
}
```

## 例外処理

### Java例外の処理

```jv
fun readFile(path: String): String {
    try {
        return Files.readString(Paths.get(path))
    } catch (e: IOException) {
        println("Error reading file: ${e.message}")
        return ""
    }
}
```

**生成されるJava:**
```java
public static String readFile(String path) {
    try {
        return Files.readString(Paths.get(path));
    } catch (IOException e) {
        System.out.println("Error reading file: " + e.getMessage());
        return "";
    }
}
```

### checked例外の処理

jvはchecked例外を自動的に処理：

```jv
fun parseUrl(url: String): URL {
    return URL(url)  // MalformedURLExceptionは自動的にthrows句に追加
}
```

**生成されるJava:**
```java
public static URL parseUrl(String url) throws MalformedURLException {
    return new URL(url);
}
```

## アノテーション

### Javaアノテーションの使用

```jv
import javax.annotation.Nullable
import javax.annotation.Nonnull

class UserService {
    @Nullable
    fun findUser(id: String): User? {
        // 実装...
        return null
    }

    @Nonnull
    fun createUser(name: String): User {
        return User(name, 0)
    }
}
```

**生成されるJava:**
```java
import javax.annotation.Nullable;
import javax.annotation.Nonnull;

public class UserService {
    @Nullable
    public User findUser(String id) {
        // 実装...
        return null;
    }

    @Nonnull
    public User createUser(String name) {
        return new User(name, 0);
    }
}
```

## ベストプラクティス

### 1. 慣用的なJavaパターンを使用

```jv
// 良い例：Javaの慣用句に従う
fun processItems(items: List<String>): List<String> {
    return items
        .stream()
        .filter { value -> value.isNotEmpty() }
        .map { value -> value.uppercase() }
        .collect(Collectors.toList())
}
```

### 2. null安全性を活用

```jv
// Javaライブラリを使用する際のnull安全性
fun safeGetProperty(props: Properties, key: String): String? {
    return props.getProperty(key)?.takeIf { value -> value.isNotEmpty() }
}
```

### 3. 適切な型を使用

```jv
// プリミティブ型を適切に使用
fun calculateSum(numbers: IntArray): Long {
    return numbers.map { value -> value.toLong() }.sum()
}

`Iterable`/配列から暗黙の`Sequence`チェーンが構築されるため、Kotlin同様に`asSequence()`を挟む必要はありません。
`map`/`filter`/`flatMap`/`groupBy`/`associate`/`fold`/`reduce`/`forEach`などの呼び出しは、そのままJava `Stream`
APIへデシュガリングされます。終端操作の`toList()`/`toSet()`はJava 25ターゲットでは`.stream().toList()`/
`.stream().toSet()`となり、Java 21ターゲットでは`Collectors.toList()`/`Collectors.toSet()`へフォールバックします。
```

### 4. 例外を適切に処理

```jv
// checked例外を適切に処理
fun parseDate(dateStr: String): LocalDate? {
    return try {
        LocalDate.parse(dateStr)
    } catch (e: DateTimeParseException) {
        null
    }
}
```

## 共通パターン

### 1. ファクトリーパターン

```jv
class ConnectionFactory {
    companion object {
        fun createConnection(url: String): Connection {
            return DriverManager.getConnection(url)
        }
    }
}
```

### 2. ビルダーパターン

```jv
class HttpRequestBuilder {
    private var url: String = ""
    private var method: String = "GET"
    private val headers = mutableMapOf<String, String>()

    fun url(url: String): HttpRequestBuilder {
        this.url = url
        return this
    }

    fun method(method: String): HttpRequestBuilder {
        this.method = method
        return this
    }

    fun header(name: String, value: String): HttpRequestBuilder {
        headers[name] = value
        return this
    }

    fun build(): HttpRequest {
        return HttpRequest.newBuilder()
            .uri(URI.create(url))
            .method(method, HttpRequest.BodyPublishers.noBody())
            .apply {
                headers.forEach { (name, value) ->
                    header(name, value)
                }
            }
            .build()
    }
}
```

### 3. コールバックパターン

```jv
fun processAsync(callback: (String) -> Unit) {
    CompletableFuture.supplyAsync {
        // 非同期処理
        "Result"
    }.thenAccept(callback)
}
```

jvとJavaの相互運用性により、既存のJavaエコシステムの豊富なライブラリとツールを活用しながら、より表現力豊かで安全なコードを書くことができます。
