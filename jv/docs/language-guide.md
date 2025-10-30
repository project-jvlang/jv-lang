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
// 暗黙不変（Javaのfinal）: キーワード省略で自動的に final
name = "Alice"
age: Int = 30

// 可変にしたい場合のみ var を明示
var count = 0
var isActive = true

// `val`キーワードを明示的に書くこともできます
val legacy = "still supported"
```

`identifier = 式`形式の宣言は暗黙に不変変数（Javaの`final`）として扱われます。必要に応じて`identifier: 型 = 式`で型を明示できます。`var`は再代入が必要な場合のみ使用し、既存コードとの互換性のため`val`キーワードもそのまま受け入れられます。

**生成されるJava:**
```java
final String name = "Alice";
final int age = 30;

int count = 0;
boolean isActive = true;
final String legacy = "still supported";
```

### 型推論

jvはほとんどの場合、型を自動的に推論します：

```jv
numbers = listOf(1, 2, 3)         // List<Int>
mapping = mapOf("key" to "value") // Map<String, String>
lambda = { x: Int -> x * 2 }      // Function1<Int, Int>
```

暗黙不変の変数でも型推論は働きます。意図的に再代入する場合のみ`var`を付けてください。

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
data Point(x: Double, y: Double)

// 既存の data class 構文もサポートされます
data class Person(val name: String, val age: Int)

// デフォルト値や型推論も利用可能
data Metrics(total, average = 0.0)

// 可変データクラス -> ゲッター/セッターを持つJavaクラス
data Counter(var value: Int) {
    fun increment() {
        value++
    }
}
```

`data Name(field ...)`形式は`data class`と同じASTを生成し、型注釈がないフィールドも推論されます。フィールドが`var`の場合はJavaクラス、すべて不変の場合はJava recordとして出力されます。

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

### ラベル付き制御フロー

`#` を先頭に付けた識別子でブロック・ループ・`when`・ラムダにラベルを与えると、深いネストから
安全に脱出できます。ラベルはその直後に現れる制御構造へ結び付き、同名ラベルは内側の定義が優先されます。

#### ラベル宣言とスコープ

```jv
#outer for (row in rows) {
    #scan when (row.kind) {
        is Header -> continue #outer
        is Data -> {
            process(row)
            break #scan
        }
        else -> break #outer
    }
}
```

- `#label` は `for` / `when` / ブロック `{ ... }` / ラムダ呼び出しの直前に書きます。
- ラベルの有効範囲は対応するブロック全体です。同名ラベルを内側で再定義すると外側のラベルは隠蔽されます。
- `continue` はループラベルにのみ使用できます。`break` はラベルが指す任意のブロックを終了させます。

#### ラムダでの `return #label`

高階関数内ではラベル付き `return` で早期脱出できます。

```jv
numbers.forEach #loop { value ->
    when {
        value < 0 -> return #loop
        value % 2 == 0 -> println("even: $value")
    }
}
```

対応する Java では単純な `return;` が生成され、追加のフラグや例外を使わずに脱出できます。
- `return #loop result` のように値を返す場合も同様で、生成 Java では `return result;` となります。

#### コメントとの判定ルール

`#` はJSON互換コメントにも使用されます。以下の判定に従ってラベル/コメントが区別されます。

| パターン | 判定 | 例 |
|----------|------|-----|
| `#` の直後が空白 | コメント | `# これはコメント` |
| `#identifier` の後ろに空白・`{`・制御構文トークンが続く | ラベル | `#outer for`, `#lambda {` |
| 行途中で `#` の前に式がある | コメント | `value = 10 # メモ` |

ラベルが未定義・種別不一致の場合は `#label` を含む診断が発行され、宣言の位置が併記されます。

#### 診断コード

- `E_LOOP_001`: サポート対象外のループ構文（`while`/`do-while`など）が検出された場合。
- `E_LOOP_002`: レンジ境界の数値型が一致しない場合。
- `E_LOOP_003`: ループ対象がイテラブルとして認識されない場合。

## コレクション

### コレクションの作成

```jv
numbers = [1 2 3 4 5]              // 空白区切りリテラル
list = listOf(1, 2, 3, 4, 5)
mutableList = mutableListOf("a", "b", "c")

set = setOf(1, 2, 3, 2)            // {1, 2, 3}
mapping = mapOf("key1" to "value1", "key2" to "value2")
```

空白区切り配列と引数の詳細は[Whitespace Arrays and Arguments](whitespace-arrays.md)を参照してください。桁区切りカンマと混在させると`JV2101`/`JV2102`診断が発生します。

### コレクション操作

```jv
numbers = listOf(1, 2, 3, 4, 5)

doubled = numbers.map { value -> value * 2 }
evens = numbers.filter { value -> value % 2 == 0 }
sum = numbers.reduce { acc, value -> acc + value }

Sequenceスタイルの拡張関数は`Iterable`から直接呼び出せます。`asSequence()`を挟む必要はなく、
`map`/`filter`/`flatMap`/`sorted`/`groupBy`/`associate`/`fold`/`reduce`/`count`/`sum`/`forEach`といった
演算が遅延評価チェーンとして自動的に構築されます。ラムダ式の引数は必ず明示し、Kotlinの暗黙
パラメータ`it`は使用できません。副作用を伴う処理は`forEach { value -> ... }`で記述し、終端処理
として`toList()`や`toSet()`を呼ぶとJava 25ターゲットでは`.stream().toList()`/`.stream().toSet()`が、
Java 21ターゲットでは`Collectors.toList()`/`Collectors.toSet()`が生成されます。

firstPositive = numbers.firstOrNull { it > 0 }
hasNegative = numbers.any { it < 0 }
allPositive = numbers.all { it > 0 }
```

## 文字列補間

```jv
name = "Alice"
age = 30

message = "Hello, my name is $name and I'm $age years old"
calculation = "The result is ${2 + 2}"
nested = "User: ${user.name.uppercase()}"
```

**生成されるJava:**
```java
String message = String.format("Hello, my name is %s and I'm %d years old", name, age);
String calculation = "The result is " + (2 + 2);
```

## 正規表現リテラル

正規表現はスラッシュ区切りのリテラル `/<pattern>/` として記述でき、コンパイル後は
`java.util.regex.Pattern` に変換されます。トップレベル `val` は静的フィールドへ
ホイストされ、実行時の再コンパイルを避けます。

```jv
val pattern = /\d+/

fun isNumber(input: String): Boolean {
    return pattern.matcher(input).matches()
}
```

**生成されるJava:**
```java
import java.util.regex.Pattern;

private static final Pattern PATTERN = Pattern.compile("\\d+");

public static boolean isNumber(String input) {
    return PATTERN.matcher(input).matches();
}
```

### 構文と制約
- パターン内のスラッシュは `\/` でエスケープします。末尾のバックスラッシュは無効で
  `JV5102` が報告されます。
- 改行やタブなど制御文字は許可されません。必要な場合は通常の文字列リテラルで記述
  し、`Pattern.compile` を手動で呼び出してください。
- リテラルは `RegexValidator` によって静的解析され、括弧の未閉鎖や対応しない閉じ括弧
  などは `JV5101`/`JV5103` シリーズの診断として表示されます。
- 文字列リテラル（`"/not/regex/"`）は従来どおり `TokenType::String` として扱われ、
  正規表現リテラルと混同されません。
- 追加のランタイム依存関係は不要で、`java.util.regex.Pattern` のみを使用します。

## スマートインポート

jv の `import` 文はソースの形に応じて型・パッケージ・静的メンバー・モジュールを自動的に判別します。

- `import foo.Bar as Baz` … 型に別名を付けます（別名は jv ソース内のみで使用され、生成コードでは完全修飾名が使われます）。
- `import foo.*` … パッケージ全体をワイルドカードで取り込みます。
- `import foo.Bar.*` … クラス `foo.Bar` の静的メンバーをワイルドカードで取り込みます。
- Java 25 をターゲットにすると、必要な場合に `import module <name>;` が自動で追加されます（Java 21 ではモジュール宣言は抑制されます）。

```jv
import java.time.LocalDate as Date
import java.util.*
import java.util.Collections.*
import java.lang.Math.max
import java.sql.DriverManager

fun report(): String {
    val today = Date.now()
    val list: ArrayList<Int> = ArrayList()
    list.add(3)
    list.add(1)
    list.add(4)
    sort(list)
    val biggest = max(list.get(0), list.get(2))
    val drivers = DriverManager.getDrivers()
    return "${today.toString()} ${biggest} drivers=${drivers.hasMoreElements()}"
}
```

**生成されるJava (Java 25ターゲット):**
```java
import module java.sql;
import java.sql.DriverManager;
import java.time.LocalDate;
import java.util.ArrayList;
import static java.lang.Math.max;
import static java.util.Collections.*;

final class Main {
    static String report() {
        LocalDate today = LocalDate.now();
        ArrayList<Integer> list = new ArrayList<>();
        sort(list);
        int biggest = max(list.get(0), list.get(2));
        var drivers = DriverManager.getDrivers();
        return today.toString() + " " + biggest + " drivers=" + drivers.hasMoreElements();
    }
}
```

CLI で `--verbose` を付けると、解決済みインポートの一覧と別名・モジュール依存がバイリンガル表示されます。

```console
$ jv build --verbose --java-only
解決済み import 一覧 / Resolved import list
  - import java.time.LocalDate → 型 import: java.time.LocalDate / Type import: java.time.LocalDate (別名: Date / Alias: Date)
  - import static java.lang.Math.max → 静的 import: java.lang.Math.max / Static import: java.lang.Math.max
  - import static java.util.Collections.* → 静的 import: java.util.Collections.* / Static import: java.util.Collections.*
  - import module java.sql → モジュール import: java.sql / Module import: java.sql (モジュール依存: java.sql / Module dependency: java.sql)
```

初回ビルドで作成されるシンボルインデックスは `target/jv/symbol-index/` にキャッシュされ、所要時間・入力アーティファクト・メモリ使用量がメトリクスとして記録されます。大規模クラスパスを扱う場合は `RUST_LOG=jv::build::symbol_index=info` を指定してビルドすると、`Indexed symbols from scratch ... artifact_inputs=<N>` といったログで性能ガードの値を確認できます。

## 並行性

### 仮想スレッド（spawn）

```jv
fun processData() {
    spawn {
        // これは仮想スレッドで実行されます
        result = heavyComputation()
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
