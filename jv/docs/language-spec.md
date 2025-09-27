# jv言語仕様

**日本語** | [English](language-spec-en.md)

jv（Javaシンタックスシュガー）プログラミング言語の正式仕様です。

## 目次

1. [概要](#概要)
2. [字句構造](#字句構造)
3. [文法](#文法)
4. [型システム](#型システム)
5. [意味論](#意味論)
6. [標準ライブラリ](#標準ライブラリ)
7. [Java相互運用性](#java相互運用性)

## 概要

jvは静的型付けプログラミング言語で、読みやすいJava 25ソースコードにコンパイルされます。Javaエコシステムとの完全な互換性を維持しながら、モダンな構文を提供します。

### 設計目標

1. **ゼロランタイムオーバーヘッド**: 追加ランタイムなしで純粋なJavaにコンパイル
2. **Javaエコシステム互換性**: Javaライブラリとのシームレス統合
3. **モダンな構文**: Kotlinインスパイアされた構文改善
4. **型安全性**: 拡張されたnull安全性と型推論
5. **可読性**: 生成されるJavaコードは人間が読める

### コンパイルモデル

```
jvソース(.jv) → AST → IR → Javaソース(.java) → バイトコード(.class)
```

## 字句構造

### 文字セット

jvソースファイルはUTF-8でエンコードされます。字句構造は大文字小文字を区別します。

### コメント

```jv
// 行コメント

/*
 * ブロックコメント
 * 複数行にまたがることができます
 */

/**
 * ドキュメンテーションコメント
 * APIドキュメントに使用されます
 */
```

### 識別子

```bnf
identifier ::= letter (letter | digit | '_')*
letter     ::= 'a'..'z' | 'A'..'Z' | unicode_letter
digit      ::= '0'..'9'
```

**予約語:**
```
abstract, async, await, break, class, continue, data, defer, do, else,
enum, false, final, for, fun, if, import, in, interface, is, null,
object, override, package, private, protected, public, return, spawn,
super, this, throw, true, try, use, val, var, when, while
```

### リテラル

#### 整数リテラル

```bnf
integer_literal ::= decimal_literal | hex_literal | binary_literal
decimal_literal ::= digit+ ('_' digit+)*
hex_literal     ::= '0' [xX] hex_digit+ ('_' hex_digit+)*
binary_literal  ::= '0' [bB] binary_digit+ ('_' binary_digit+)*
```

例:
```jv
42
1_000_000
0xFF_FF_FF
0b1010_1010
```

#### 浮動小数点リテラル

```bnf
float_literal ::= digit+ '.' digit+ ([eE] [+-]? digit+)?
               | digit+ [eE] [+-]? digit+
```

例:
```jv
3.14159
2.5e10
1.23e-4
```

#### 文字列リテラル

```jv
// 単純な文字列
"Hello, world!"

// エスケープシーケンス
"Line 1\nLine 2\tTabbed"

// 文字列補間
"Hello, $name!"
"Result: ${2 + 2}"

// Raw文字列
"""
複数行の
文字列コンテンツ
"""
```

#### 文字リテラル

```jv
'a'
'\n'
'\u0041'  // Unicode
```

#### 真偽値リテラル

```jv
true
false
```

#### nullリテラル

```jv
null
```

## 文法

### プログラム構造

```bnf
program ::= package_declaration? import_declaration* top_level_declaration*

package_declaration ::= 'package' qualified_name

import_declaration ::= 'import' qualified_name ('.' '*' | 'as' identifier)?

top_level_declaration ::= class_declaration
                       | function_declaration
                       | property_declaration
```

### 型宣言

#### クラス宣言

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

例:
```jv
// 基本クラス
class Person(val name: String, var age: Int) {
    fun greet(): String = "Hello, I'm $name"
}

// データクラス
data class Point(val x: Double, val y: Double)

// 継承
class Student(name: String, age: Int, val studentId: String) : Person(name, age) {
    override fun greet(): String = "Hi, I'm student $name"
}
```

#### インターフェース宣言

```bnf
interface_declaration ::= 'interface' identifier type_parameters?
                        (':' supertype_list)? interface_body?

interface_body ::= '{' interface_member* '}'

interface_member ::= function_declaration | property_declaration
```

例:
```jv
interface Drawable {
    fun draw()
    val color: String
        get() = "black"
}
```

### 関数宣言

```bnf
function_declaration ::= function_modifier* 'fun' type_parameters? identifier
                        '(' parameter_list? ')' (':' type)? function_body?

function_modifier ::= 'override' | 'abstract' | 'final' | visibility_modifier

parameter ::= identifier ':' type ('=' expression)?

function_body ::= '=' expression | block_statement
```

例:
```jv
// 基本関数
fun add(a: Int, b: Int): Int = a + b

// デフォルト引数
fun greet(name: String = "World"): String {
    return "Hello, $name!"
}

// ジェネリック関数
fun <T> identity(value: T): T = value

// 拡張関数
fun String.isPalindrome(): Boolean {
    return this == this.reversed()
}
```

### 変数宣言

```bnf
property_declaration ::= property_modifier* ('val' | 'var') identifier
                        (':' type)? ('=' expression)?

property_modifier ::= visibility_modifier
```

例:
```jv
val immutable = 42
var mutable = "Hello"
val inferredType = listOf(1, 2, 3)  // List<Int>
val nullable: String? = null
```

## 型システム

### 基本型

| jv型 | Java型 | 説明 |
|------|--------|------|
| `Unit` | `void` | 戻り値なし |
| `Boolean` | `boolean` | 真偽値 |
| `Byte` | `byte` | 8ビット符号付き整数 |
| `Short` | `short` | 16ビット符号付き整数 |
| `Int` | `int` | 32ビット符号付き整数 |
| `Long` | `long` | 64ビット符号付き整数 |
| `Float` | `float` | 32ビット浮動小数点 |
| `Double` | `double` | 64ビット浮動小数点 |
| `Char` | `char` | 16ビット文字 |
| `String` | `String` | 文字列 |

### nullable型

```jv
var nullable: String? = null    // nullableな文字列
val nonNull: String = "value"   // non-null文字列

// 安全呼び出し
val length = nullable?.length   // Int?

// エルビス演算子
val name = nullable ?: "default"

// 非null保証
val definitelyNotNull = nullable!!
```

### 型推論

```jv
val number = 42              // Int
val text = "Hello"           // String
val list = mutableListOf(1)  // MutableList<Int>
val map = mapOf("key" to 1)  // Map<String, Int>
```

### ジェネリクス

```jv
// ジェネリッククラス
class Box<T>(val value: T)

// 制約付きジェネリクス
class NumberBox<T : Number>(val value: T)

// 使用サイト変性
val readOnlyList: List<out Number> = listOf(1, 2, 3)
val writeOnlyList: MutableList<in Int> = mutableListOf()
```

### 関数型

```jv
// 関数型
val operation: (Int, Int) -> Int = { a, b -> a + b }
val predicate: (String) -> Boolean = { it.isNotEmpty() }

// 高階関数
fun processItems(items: List<String>, processor: (String) -> String): List<String> {
    return items.map(processor)
}
```

## 意味論

### 式

#### 算術式

```jv
val result = a + b * c / d - e % f
val power = base.pow(exponent)
```

#### 比較式

```jv
val isEqual = a == b
val isIdentical = a === b  // 参照等価性
val isLess = a < b
val isNullable = value != null
```

#### 論理式

```jv
val and = condition1 && condition2
val or = condition1 || condition2
val not = !condition
```

#### when式

```jv
val result = when (value) {
    1 -> "one"
    2, 3 -> "two or three"
    in 4..10 -> "four to ten"
    is String -> "string: $value"
    else -> "other"
}
```

#### if式

```jv
val max = if (a > b) a else b

val status = if (user.isActive) {
    "active"
} else {
    "inactive"
}
```

### 文

#### ループ

```jv
// for-in ループ
for (item in list) {
    println(item)
}

// 範囲ループ
for (i in 1..10) {
    println(i)
}

// インデックス付きループ
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

## 標準ライブラリ

### コレクション

```jv
// リスト
val list = listOf(1, 2, 3)
val mutableList = mutableListOf("a", "b")

// セット
val set = setOf(1, 2, 3)
val mutableSet = mutableSetOf<String>()

// マップ
val map = mapOf("key1" to "value1", "key2" to "value2")
val mutableMap = mutableMapOf<String, Int>()
```

### コレクション操作

```jv
val numbers = listOf(1, 2, 3, 4, 5)

val doubled = numbers.map { it * 2 }
val evens = numbers.filter { it % 2 == 0 }
val sum = numbers.reduce { acc, n -> acc + n }
val first = numbers.firstOrNull { it > 3 }
```

### 並行性

```jv
// 仮想スレッド
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

### リソース管理

```jv
// use文（try-with-resources）
use(FileInputStream("file.txt")) { input ->
    val data = input.readAllBytes()
    processData(data)
}

// defer文
fun processFile() {
    val resource = acquireResource()
    defer {
        resource.release()
    }
    // リソース使用...
}
```

## Java相互運用性

### Javaクラスの使用

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

### 生成されるJavaコード

jvコードは読みやすい、慣用的なJavaコードを生成：

```jv
// jv
data class User(val name: String, var age: Int) {
    fun greet(): String = "Hello, $name"
}
```

```java
// 生成されるJava
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

### null安全性マッピング

```jv
val nullable: String? = getValue()
val result = nullable?.length ?: 0
```

```java
String nullable = getValue();
int result = nullable != null ? nullable.length() : 0;
```

この仕様は、jv言語の完全な定義を提供し、実装者とユーザーの両方がjvの正確な動作を理解できるようにします。
