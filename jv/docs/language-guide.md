# jv言語ガイド

**日本語** | [English](language-guide-en.md)

jv（Javaシンタックスシュガー）の構文と機能の完全なリファレンスです。

## 目次

1. [変数と型](#変数と型)
2. [関数](#関数)
3. [クラスとデータクラス](#クラスとデータクラス)
4. [null安全性](#null安全性)
5. [制御フロー](#制御フロー)
6. [コレクション](#コレクション)
7. [文字列補間](#文字列補間)
8. [並行性](#並行性)
9. [リソース管理](#リソース管理)
10. [拡張関数](#拡張関数)
11. [Java相互運用](#java相互運用)
12. [JSONリテラルとPOJO生成](#jsonリテラルとpojo生成)

## 変数と型

### 変数宣言

```jv
// 不変変数（Javaのfinal）
val name = "Alice"
val age = 30

// 可変変数
var count = 0
var isActive = true

// 明示的型（通常は推論される）
val pi: Double = 3.14159
var items: List<String> = mutableListOf()
```

**生成されるJava:**
```java
final String name = "Alice";
final int age = 30;

int count = 0;
boolean isActive = true;
```

### 型推論

jvはほとんどの場合、型を自動的に推論します：

```jv
val numbers = listOf(1, 2, 3)        // List<Int>
val map = mapOf("key" to "value")    // Map<String, String>
val lambda = { x: Int -> x * 2 }     // Function1<Int, Int>
```

## 関数

### 基本的な関数

```jv
fun greet(name: String): String {
    return "Hello, $name!"
}

// 式本体
fun add(a: Int, b: Int): Int = a + b

// Unit戻り型（void）
fun printInfo(message: String) {
    println(message)
}
```

### デフォルト引数

```jv
fun createUser(name: String, age: Int = 18, active: Boolean = true): User {
    return User(name, age, active)
}

// 使用法
val user1 = createUser("Alice")
val user2 = createUser("Bob", 25)
val user3 = createUser("Charlie", 30, false)
```

**生成されるJava**（メソッドオーバーロード）:
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

### 名前付き引数

```jv
fun configureServer(
    host: String,
    port: Int,
    ssl: Boolean = false,
    timeout: Int = 30
) { /* ... */ }

// 名前付き引数を使用
configureServer(
    host = "localhost",
    port = 8080,
    ssl = true
)
```

### トップレベル関数

```jv
// トップレベル関数はユーティリティクラスの静的メソッドになります
fun calculateDistance(x1: Double, y1: Double, x2: Double, y2: Double): Double {
    return sqrt((x2 - x1).pow(2) + (y2 - y1).pow(2))
}
```

**生成されるJava:**
```java
public class MathUtils {
    public static double calculateDistance(double x1, double y1, double x2, double y2) {
        return Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2));
    }
}
```

## クラスとデータクラス

### 通常のクラス

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

### データクラス

```jv
// 不変データクラス -> Javaレコード
data class Point(val x: Double, val y: Double)

// 可変データクラス -> ゲッター/セッターを持つJavaクラス
data class mutable Counter(var value: Int) {
    fun increment() {
        value++
    }
}
```

**生成されるJava**（不変）:
```java
public record Point(double x, double y) {}
```

**生成されるJava**（可変）:
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

### 継承

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

## null安全性

### nullable型

```jv
var nullableName: String? = null
val nonNullName: String = "Alice"

// コンパイルエラー: nullableName = nonNullName  // OK
// nonNullName = nullableName  // エラー!
```

### 安全呼び出し演算子

```jv
val length = nullableName?.length  // Int?を返す（nullableNameがnullの場合はnull）

// チェーン
val firstChar = person?.name?.firstOrNull()?.uppercase()
```

**生成されるJava:**
```java
Integer length = nullableName != null ? nullableName.length() : null;
```

### エルビス演算子

```jv
val name = nullableName ?: "Default Name"
val length = nullableName?.length ?: 0
```

**生成されるJava:**
```java
String name = nullableName != null ? nullableName : "Default Name";
```

### 安全インデックスアクセス

```jv
val list: List<String>? = getList()
val firstItem = list?[0]  // 安全な配列/リストアクセス
```

## 制御フロー

### when式

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

より高度なパターン構成・網羅性検査・コード生成戦略については、専用ガイド「[パターンマッチングガイド](pattern-matching.md)」を参照してください。

**生成されるJava**（Java 25パターンマッチング使用）:
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

### when式による条件分岐

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

> メモ: `when` を式として使う場合は必ず `else` を記述してください。欠落していると `E_WHEN_002` が発生します（文コンテキストでは暗黙に `else -> Unit` が補完されます）。jv は `if` 構文を提供していないため、`if` を記述すると `JV3103` が報告されます。条件分岐は `when` に統一してください。

### ループ

ループは`for (binding in source)`構文で記述し、数値レンジおよびイテラブル値の双方に
対応します。

#### 数値レンジ

```jv
for (index in 0..limit) {
    println(index) // 上限は含まない
}

for (day in 1..=7) {
    println(day) // 上限を含む
}
```

- `a..b`は`a < b`の間を走査し、`a..=b`は終端値を含みます。
- レンジの開始値と終了値は同じ数値型でなければならず、型が一致しない場合は
  `E_LOOP_002`が発生します。

#### イテラブルソース

```jv
val items = arrayOf("north", "south", "east", "west")

for (direction in items) {
    println(direction)
}
```

- ループ対象は配列・シーケンス・`iterator`を公開する独自型など、型チェッカーが
  認識するイテラブルプロトコルを実装している必要があります。未対応の場合は
  `E_LOOP_003`が報告されます。
- `AutoCloseable`を実装する遅延シーケンスはJavaコード生成時にクリーンアップ処理
  が自動付与されます。

#### 条件付き反復の表現

`while (condition)`に相当する反復も、`for`構文とシーケンス操作で記述できます。

```jv
// 条件が成り立つ間だけ要素を読み込む
for (token in scanner.intoSequence().takeWhile { it != null }) {
    process(token)
}

// 型が期待通りである間だけ処理を続ける (while (node is Tree) ...)
for (node in generateSequence(root) { it?.parent }.takeWhile { it is Tree }) {
    visit(node as Tree)
}

// カウンタを任意のステップで更新しながら走査する
for (index in generateSequence(start) { it + step }.takeWhile { it < limit }) {
    println(index)
}

// 明示的な終了条件を持つループ
for (_ in sequenceOf(Unit).repeat()) {
    when {
        done() -> break
    }
    performWork()
}
```

- イテレータ由来の反復では`intoSequence()`や`takeWhile`で条件を宣言的に表現できます。
- 連続する数値領域は`start until end`、`start..=end`、`generateSequence`などで生成するのが
  推奨です。
- センチネル条件を扱う場合でも、本体で`break`/`continue`を使用した明示的制御が可能です。

#### 診断コード

- `E_LOOP_001`: サポート対象外のループ構文（`while`/`do-while`など）が検出された場合。
- `E_LOOP_002`: レンジ境界の数値型が一致しない場合。
- `E_LOOP_003`: ループ対象がイテラブルとして認識されない場合。

## コレクション

### コレクションの作成

```jv
val list = listOf(1, 2, 3, 4, 5)
val mutableList = mutableListOf("a", "b", "c")

val set = setOf(1, 2, 3, 2)  // {1, 2, 3}
val map = mapOf("key1" to "value1", "key2" to "value2")
```

### コレクション操作

```jv
val numbers = listOf(1, 2, 3, 4, 5)

val doubled = numbers.map { it * 2 }
val evens = numbers.filter { it % 2 == 0 }
val sum = numbers.reduce { acc, n -> acc + n }

val firstPositive = numbers.firstOrNull { it > 0 }
val hasNegative = numbers.any { it < 0 }
val allPositive = numbers.all { it > 0 }
```

## 文字列補間

```jv
val name = "Alice"
val age = 30

val message = "Hello, my name is $name and I'm $age years old"
val calculation = "The result is ${2 + 2}"
val nested = "User: ${user.name.uppercase()}"
```

**生成されるJava:**
```java
String message = String.format("Hello, my name is %s and I'm %d years old", name, age);
String calculation = "The result is " + (2 + 2);
```

## 並行性

### 仮想スレッド（spawn）

```jv
fun processData() {
    spawn {
        // これは仮想スレッドで実行されます
        val result = heavyComputation()
        println("Result: $result")
    }
}
```

**生成されるJava:**
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
        // API呼び出しをシミュレート
        Thread.sleep(1000)
        "Data fetched"
    }
}

fun main() {
    val future = fetchData()
    val result = future.await()  // 完了まで待機
    println(result)
}
```

## リソース管理

### useブロック（Try-with-resources）

```jv
use(FileInputStream("file.txt")) { input ->
    val data = input.readAllBytes()
    processData(data)
}
```

**生成されるJava:**
```java
try (FileInputStream input = new FileInputStream("file.txt")) {
    byte[] data = input.readAllBytes();
    processData(data);
}
```

### deferブロック

```jv
fun processFile(filename: String) {
    val file = File(filename)

    defer {
        println("Cleaning up...")
        file.delete()
    }

    // ファイル処理...
    when {
        error -> return  // deferブロックは依然として実行される
    }
}
```

**生成されるJava:**
```java
public void processFile(String filename) {
    File file = new File(filename);
    try {
        // ファイル処理...
        if (error) return;
    } finally {
        System.out.println("Cleaning up...");
        file.delete();
    }
}
```

## 拡張関数

```jv
// 既存の型を拡張
fun String.isPalindrome(): Boolean {
    return this == this.reversed()
}

fun <T> List<T>.secondOrNull(): T? {
    return when {
        size >= 2 -> this[1]
        else -> null
    }
}

// 使用法
val text = "racecar"
when {
    text.isPalindrome() -> println("It's a palindrome!")
}

val second = listOf(1, 2, 3).secondOrNull()  // 2
```

**生成されるJava**（静的メソッド）:
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

## Java相互運用

jvはJavaライブラリをシームレスに使用できます：

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

生成されるJavaコードは、ラッパー層なしにこれらのJava APIを直接使用します。

## JSONリテラルとPOJO生成

`val payload = { ... }` のようなインライン JSON リテラルは自動的に Java 25 の `record` として生成され、`payload.user.name` といった型安全なアクセスが可能です。コメント付き JSONC や大型オブジェクトの取り扱いについては、[JSON リテラルとPOJO生成ガイド](json.md) を参照してください。

## ベストプラクティス

1. **`var`より`val`を優先**: 可能な場合は不変変数を使用
2. **データクラスを使用**: シンプルなデータコンテナに
3. **型推論を活用**: 不必要に型を指定しない
4. **null安全性を使用**: nullable型と安全演算子を活用
5. **式構文を優先**: 条件分岐は `when` の式で表現
6. **拡張関数を使用**: 既存の型に機能を追加
7. **関数を純粋に保つ**: 可能な限り副作用を避ける
