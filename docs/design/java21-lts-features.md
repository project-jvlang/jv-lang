# java21-lts-features: Java 21 LTS 機能対応設計書

## 概要

本ドキュメントは Java 21 LTS (2023年9月リリース) で導入された主要機能と、jv 言語における対応戦略を定義します。Java 21 は Java 17 LTS 以降の最初の LTS リリースであり、15の JEP (1 incubator, 5 preview, 9 finalized) を含む重要なアップデートです。パターンマッチング、仮想スレッド、シーケンスコレクションなど、jv のデザインゴールと高い親和性を持つ機能が確定しています。

## デザインゴール

- Java 21 の確定機能を jv から完全にサポート
- preview/incubator 機能については明示的なオプトイン機構を提供
- jv の既存シンタックスシュガーと自然に統合
- Java 21 をデフォルトターゲットとして推奨
- 後方互換性: Java 17/18/19/20 へのフォールバック戦略を保持

## Java 21 機能一覧

### 確定機能 (Finalized)

| JEP | 機能名 | 影響範囲 | jv 対応優先度 |
|-----|--------|---------|--------------|
| JEP 431 | Sequenced Collections | 標準ライブラリ (Collections) | **High** |
| JEP 440 | Record Patterns | 構文・パターンマッチング | **High** |
| JEP 441 | Pattern Matching for switch | 構文・パターンマッチング | **High** |
| JEP 444 | Virtual Threads | 並行処理 Runtime | **High** |
| JEP 452 | Key Encapsulation Mechanism API | セキュリティ API | Low |

### 非推奨・削除 (Deprecations & Removals)

| JEP | 機能名 | 影響範囲 | jv 対応 |
|-----|--------|---------|---------|
| JEP 449 | Deprecate Windows 32-bit x86 Port | プラットフォーム | N/A |
| JEP 451 | Prepare to Disallow Dynamic Agent Loading | JVM | Monitor |

### プレビュー機能 (Preview)

| JEP | 機能名 | 影響範囲 | jv 対応優先度 |
|-----|--------|---------|--------------|
| JEP 430 | String Templates (Preview) | 構文・文字列処理 | **High** (注: Java 23 で削除) |
| JEP 442 | Foreign Function & Memory API (3rd Preview) | ネイティブ相互運用 | Medium |
| JEP 443 | Unnamed Patterns and Variables (Preview) | 構文・パターンマッチング | **High** |
| JEP 445 | Unnamed Classes and Instance Main Methods (Preview) | エントリーポイント | Medium |
| JEP 446 | Scoped Values (Preview) | スレッドローカル代替 | Medium |
| JEP 453 | Structured Concurrency (Preview) | 並行処理 API | Medium |

### インキュベーター機能 (Incubator)

| JEP | 機能名 | 影響範囲 | jv 対応優先度 |
|-----|--------|---------|--------------|
| JEP 448 | Vector API (6th Incubator) | SIMD パフォーマンス | Low |

---

## 優先対応機能詳細

### 1. JEP 440: Record Patterns (Finalized)

#### 機能概要

record 型のインスタンスを分解 (destructuring) し、パターンマッチングで直接コンポーネントにアクセス可能にします。ネストされた record パターンもサポートします。

#### Java 21 構文例

```java
// Basic record pattern with instanceof
record Point(double x, double y) {}

static void printAngleFromXAxis(Object obj) {
    if (obj instanceof Point(double x, double y)) {
        System.out.println(Math.toDegrees(Math.atan2(y, x)));
    }
}

// Using var for type inference
if (obj instanceof Point(var x, var y)) {
    // Use x and y directly
}

// Nested record patterns
record Box<T>(T value) {}

static void test(Box<Box<String>> bbs) {
    if (bbs instanceof Box(Box(var s))) {
        System.out.println("String " + s);
    }
}

// Switch expression with record patterns
interface Event {}
record UserCreatedEvent(User user, Instant instant) implements Event {}
record UserDeletedEvent(User user, Instant instant, String reason) implements Event {}

String fireEvent(Event event) {
    return switch (event) {
        case UserCreatedEvent(var user, var instant) ->
            "User created at " + instant + " with username " + user.username();
        case UserDeletedEvent(var user, var instant, var reason) ->
            "User deleted at " + instant + " because " + reason;
    };
}
```

#### jv での対応戦略

**シンタックス**: jv の `when` 式と `data class` は既に record へトランスパイルされており、record パターンは自然にサポートできます。

**jv 構文例**:
```jv
data class Point(val x: Double, val y: Double)

fun printAngle(obj: Any) {
    when (obj) {
        is Point(val x, val y) -> println(atan2(y, x).toDegrees())
        else -> println("Not a point")
    }
}

// Nested patterns
data class Box<T>(val value: T)

fun test(bbs: Box<Box<String>>) {
    when (bbs) {
        is Box(Box(val s)) -> println("String: $s")
    }
}
```

**実装タスク**:
1. `jv_parser` で `is` パターンの record destructuring 構文サポート追加
2. `jv_ast` に `RecordPattern` ノード追加 (コンポーネント名とパターンのリスト)
3. `jv_checker` で record コンポーネントの型検証と exhaustiveness チェック
4. `jv_codegen_java` で Java 21 の record pattern 構文生成
5. テスト: `jv_codegen_java/tests/record_patterns.rs` 新設

**フォールバック**: Java 17-20 では手動での `instanceof` + アクセサメソッド呼び出しに展開します。

---

### 2. JEP 441: Pattern Matching for switch (Finalized)

#### 機能概要

`switch` 式/文で型パターン、record パターン、when ガードを利用可能にし、複雑なデータ指向クエリを安全かつ簡潔に記述できます。

#### Java 21 構文例

```java
// Type patterns with when guards
static String processResponse(String response) {
    return switch (response) {
        case null -> "No response";
        case String s when s.equalsIgnoreCase("YES") -> "You got it";
        case String s when s.equalsIgnoreCase("NO") -> "Shame";
        case String s -> "Sorry?";
    };
}

// Integer patterns with guards
static String categorize(Integer i) {
    return switch (i) {
        case null -> "null";
        case -1, 1 -> "Special cases";
        case Integer j when j > 0 -> "Positive: " + j;
        case Integer j -> "Non-positive: " + j;
    };
}

// Enum with guards
enum Suit { CLUBS, DIAMONDS, HEARTS, SPADES }

static void printSuit(Card c) {
    switch (c) {
        case Suit s when s == Suit.CLUBS -> System.out.println("It's clubs");
        case Suit s when s == Suit.DIAMONDS -> System.out.println("It's diamonds");
        case Suit s when s == Suit.HEARTS -> System.out.println("It's hearts");
        case Suit s -> System.out.println("It's spades");
    }
}
```

**重要**: when ガードは boolean 式であり、コンパイラは解釈できないため、少なくとも1つのアンガード (guard なし) パターンが必要です。

#### jv での対応戦略

**シンタックス**: jv の `when` 式は既に when ガードをサポートしており、構文変更は不要です。

**jv 構文例**:
```jv
fun processResponse(response: String?): String {
    return when (response) {
        null -> "No response"
        is String when response.equalsIgnoreCase("YES") -> "You got it"
        is String when response.equalsIgnoreCase("NO") -> "Shame"
        is String -> "Sorry?"
    }
}

fun categorize(i: Int?): String {
    return when (i) {
        null -> "null"
        -1, 1 -> "Special cases"
        is Int when i > 0 -> "Positive: $i"
        is Int -> "Non-positive: $i"
    }
}
```

**実装タスク**:
1. `jv_parser` の `when` 式パーサーに when 条件式 (guard) のサポート追加 (既存実装確認)
2. `jv_ast::WhenArm` に `guard: Option<Expr>` フィールド追加
3. `jv_checker` で guard 式の boolean 型検証と exhaustiveness チェック拡張
4. `jv_codegen_java` で Java 21 の guarded pattern 構文生成
5. テスト: `jv_codegen_java/tests/switch_guards.rs` 新設

**フォールバック**: Java 17-20 では when ガードを `if` 文のネストに展開します。

---

### 3. JEP 431: Sequenced Collections (Finalized)

#### 機能概要

順序を持つコレクション用の新しいインターフェース (`SequencedCollection`, `SequencedSet`, `SequencedMap`) を導入し、先頭/末尾要素への統一的なアクセスと逆順ビューを提供します。

#### Java 21 API

```java
// SequencedCollection interface
interface SequencedCollection<E> extends Collection<E> {
    SequencedCollection<E> reversed();
    void addFirst(E);
    void addLast(E);
    E getFirst();
    E getLast();
    E removeFirst();
    E removeLast();
}

// SequencedSet interface
interface SequencedSet<E> extends Set<E>, SequencedCollection<E> {
    SequencedSet<E> reversed();
}

// SequencedMap interface
interface SequencedMap<K, V> extends Map<K, V> {
    SequencedMap<K, V> reversed();
    SequencedSet<K> sequencedKeySet();
    SequencedCollection<V> sequencedValues();
    SequencedSet<Entry<K, V>> sequencedEntrySet();
    V putFirst(K, V);
    V putLast(K, V);
    Entry<K, V> firstEntry();
    Entry<K, V> lastEntry();
    Entry<K, V> pollFirstEntry();
    Entry<K, V> pollLastEntry();
}
```

#### Java 21 使用例

```java
// LinkedHashSet (implements SequencedSet)
SequencedSet<Integer> set = new LinkedHashSet<>();
set.add(5);
set.add(8);
set.add(12);
set.addFirst(3);  // [3, 5, 8, 12]
set.addLast(15);  // [3, 5, 8, 12, 15]
Integer first = set.getFirst();  // 3
Integer last = set.getLast();    // 15
SequencedSet<Integer> reversed = set.reversed();  // [15, 12, 8, 5, 3]

// LinkedHashMap (implements SequencedMap)
SequencedMap<String, Integer> map = new LinkedHashMap<>();
map.put("Apple", 10);
map.put("Banana", 20);
map.putFirst("Cherry", 5);   // Cherry is now first
map.putLast("Date", 30);     // Date is now last
Map.Entry<String, Integer> first = map.firstEntry();  // Cherry=5
Map.Entry<String, Integer> last = map.lastEntry();    // Date=30
```

**統合**: `List`, `Deque`, `LinkedHashSet`, `SortedSet`, `LinkedHashMap`, `SortedMap` が新しいインターフェースを実装しています。

#### jv での対応戦略

**標準ライブラリ**: `jv_stdlib` で Kotlin 風の拡張メソッドスタイルで同等の API を提供します。

**jv 構文例**:
```jv
// Sequenced operations on List
val list = mutableListOf(1, 2, 3)
list.addFirst(0)     // [0, 1, 2, 3]
list.addLast(4)      // [0, 1, 2, 3, 4]
val first = list.first()  // 0
val last = list.last()    // 4

// LinkedHashSet with sequenced operations
val set = linkedSetOf(5, 8, 12)
set.addFirst(3)
set.addLast(15)
val reversed = set.reversed()  // [15, 12, 8, 5, 3]

// LinkedHashMap with sequenced operations
val map = linkedMapOf("Apple" to 10, "Banana" to 20)
map.putFirst("Cherry", 5)
map.putLast("Date", 30)
val firstEntry = map.firstEntry()  // Cherry=5
```

**実装タスク**:
1. `jv_stdlib/collections` に `SequencedCollection` 拡張メソッド追加
2. Java 21 の `SequencedSet`/`SequencedMap` への直接マッピング
3. `jv_checker::symbol_index` で Java 21 の新しいインターフェースを認識
4. ドキュメント: stdlib API リファレンス更新
5. テスト: `jv_stdlib/tests/sequenced_collections.jv` 新設

**フォールバック**: Java 17-20 では従来の `list.get(0)`, `list.get(list.size()-1)` などに展開します。

---

### 4. JEP 444: Virtual Threads (Finalized)

#### 機能概要

軽量スレッド (virtual threads) を導入し、高スループットの並行アプリケーションの記述を劇的に簡素化します。仮想スレッドは OS スレッドのラッパーではなく、JDK が提供する軽量実装です。

#### Java 21 構文例

```java
// Creating and starting a virtual thread
Thread thread = Thread.ofVirtual()
    .name("duke")
    .start(() -> System.out.println("Hello from virtual thread"));
thread.join();

// Convenient method for virtual threads
Thread.startVirtualThread(() -> {
    System.out.println("Running in virtual thread");
});

// ExecutorService with virtual threads
try (ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor()) {
    executor.submit(() -> {
        // Task runs in a new virtual thread
        performIOOperation();
    });
    executor.submit(() -> {
        // Another task in another virtual thread
        fetchData();
    });
} // Automatically waits for all tasks to complete

// Platform threads for comparison
Thread platformThread = Thread.ofPlatform()
    .name("platform-thread")
    .start(() -> System.out.println("Platform thread"));
```

**ベストプラクティス**:
- 仮想スレッドは並行タスク数が多く、ネットワーク I/O でブロックする場合に有用
- CPU 集約型タスクには利点なし
- ブロッキング I/O の使用が推奨 (仮想スレッドは安価)

#### jv での対応戦略

**シンタックス**: jv の `spawn {}` ブロックを仮想スレッドで実装します。

**jv 構文例**:
```jv
// spawn block transpiles to virtual threads
spawn {
    println("Hello from virtual thread")
}

// Multiple concurrent tasks
val result1 = spawn { fetchData() }
val result2 = spawn { computeSomething() }
result1.await() + result2.await()

// spawn with structured concurrency (Java 21 preview feature)
spawn {
    val tasks = listOf(
        spawn { task1() },
        spawn { task2() },
        spawn { task3() }
    )
    tasks.map { it.await() }
}
```

**実装タスク**:
1. `jv_codegen_java::concurrency` モジュール拡張
2. `spawn {}` を `Thread.ofVirtual().start()` または `Executors.newVirtualThreadPerTaskExecutor()` へトランスパイル
3. `await()` を `Future.get()` または structured concurrency API へマッピング
4. `jv.toml` で `java.virtual_threads = true` の設定サポート
5. テスト: `jv_codegen_java/tests/virtual_threads.rs` 新設

**フォールバック**: Java 17-20 では `CompletableFuture` + `ForkJoinPool` で代替します。

---

## 中優先度機能

### JEP 443: Unnamed Patterns and Variables (Preview)

アンダースコア `_` を使って不要なパターンや変数を省略できます。

```java
// Unnamed pattern in record pattern
record WindowFrame(Position position, Size size) {}
record Size(int width, int height) {}

if (obj instanceof WindowFrame(_, Size(_, int height))) {
    System.out.println("Height: " + height);
}

// Unnamed variables in loops
for (int i = 0, _ = sideEffect(); i < 10; i++) {
    // _ is used for side effect only
}
```

**jv 対応**: jv の `_` は既に wildcard パターンとしてサポートされており、そのまま Java 21 の unnamed pattern へマッピングします。

### JEP 430: String Templates (Preview - Java 23 で削除)

**注意**: String Templates は Java 21/22 で preview として導入されましたが、Java 23 で削除されました。そのため、jv での実装優先度は低くなります。

```java
String name = "Duke";
String info = STR."My name is \{name}";
// Output: My name is Duke

int age = 42;
String message = STR."Hello, \{name}! Next year, you'll be \{age + 1}.";
// Output: Hello, Fred! Next year, you'll be 43.
```

**jv 対応**: jv の文字列補間 `"Hello, ${name}"` は既存の実装を維持し、Java 21 では従来の `String.format()` または `StringBuilder` へトランスパイルします。

### JEP 446: Scoped Values (Preview)

スレッドローカル変数の代替として、スコープ値を提供します。仮想スレッドに最適化されています。

**jv 対応**: `jv_stdlib` に `ScopedValue` ラッパーを追加し、必要に応じて利用可能にします。

### JEP 453: Structured Concurrency (Preview)

関連する並行タスクを単一の作業単位として扱い、構造化されたライフサイクル管理を提供します。

```java
try (var scope = StructuredTaskScope.ShutdownOnFailure.open()) {
    Future<String> task1 = scope.fork(() -> fetchData());
    Future<String> task2 = scope.fork(() -> computeSomething());

    scope.join();           // Wait for all tasks
    scope.throwIfFailed();  // Throw if any failed

    return task1.result() + task2.result();
}
```

**jv 対応**: `spawn {}` ブロックの高度な使用シナリオで内部的に `StructuredTaskScope` を利用します。

---

## フィーチャーフラグ管理

### jv.toml 設定

```toml
[java]
target_version = "21"        # Target Java version (default: 21)
enable_preview = false       # Enable preview features (default: false)
virtual_threads = true       # Use virtual threads for spawn {} (default: true)

[java.features]
record_patterns = true       # JEP 440
pattern_matching = true      # JEP 441
sequenced_collections = true # JEP 431
virtual_threads = true       # JEP 444
unnamed_patterns = false     # JEP 443 (preview)
string_templates = false     # JEP 430 (preview, removed in Java 23)
```

### コンパイラフラグ生成

`jv_build` は以下のフラグを `javac` に渡します:

```bash
# Java 21 with finalized features only
javac --release 21 --source 21 <files>

# Java 21 with preview features
javac --release 21 --enable-preview --source 21 <files>
```

### フォールバック戦略

| Java Version | Fallback Strategy |
|--------------|-------------------|
| 17-20 | Record パターン → `instanceof` + アクセサメソッド |
| 17-20 | Pattern matching → ネストされた `if` 文 |
| 17-20 | Sequenced Collections → 従来の `get(0)`, `get(size()-1)` |
| 17-20 | Virtual Threads → `CompletableFuture` + `ForkJoinPool` |
| 17-20 | Unnamed Patterns → 通常の変数名 (使用しない) |

---

## テスト戦略

### ユニットテスト
- `jv_parser/tests/java21_syntax.rs` - record パターン、switch ガードのパース検証
- `jv_checker/tests/java21_validation.rs` - 型チェック、exhaustiveness 検証

### 統合テスト
- `jv_codegen_java/tests/java21_codegen.rs` - Java 21 コード生成の正確性
- `jv_cli/tests/java21_integration.rs` - E2E コンパイル・実行テスト

### ゴールデンテスト
- `jv/tests/fixtures/java21/` - スナップショット比較用の jv ソースと期待される Java 出力

### 互換性テスト
- `jv_build/tests/version_compatibility.rs` - Java 17/18/19/20 へのフォールバック検証

### パフォーマンステスト
- `jv_cli/tests/virtual_threads_perf.rs` - 仮想スレッドのスループット測定

---

## 実装スケジュール

| Phase | 機能 | 期間 | 依存関係 |
|-------|------|------|---------|
| Phase 1 | JEP 440 (Record Patterns) | Week 1-2 | AST 拡張、型推論システム |
| Phase 2 | JEP 441 (Pattern Matching for switch) | Week 3 | JEP 440 完了 |
| Phase 3 | JEP 431 (Sequenced Collections) | Week 4 | stdlib 基盤完成 |
| Phase 4 | JEP 444 (Virtual Threads) | Week 5-6 | コード生成基盤完成 |
| Phase 5 | JEP 443 (Unnamed Patterns - preview) | Week 7 | JEP 440/441 完了 |
| Phase 6 | フォールバック実装 | Week 8 | 全機能完了 |
| Phase 7 | 統合テスト・ドキュメント | Week 9 | 全フェーズ完了 |

---

## パフォーマンス考慮事項

### コンパイル時間
- Record パターン、switch パターンマッチングは純粋な構文機能でコンパイル時間への影響は最小限
- Preview 機能の有効化により javac の処理時間が 5-10% 増加する可能性

### ランタイムパフォーマンス
- **Virtual Threads**: I/O バウンドタスクで劇的なスループット向上 (10-100倍)
- **Sequenced Collections**: 従来の `get(0)` と同等のパフォーマンス (オーバーヘッドなし)
- **Record Patterns**: switch のパターンマッチングは最適化されており、従来の `instanceof` チェーンより高速
- **Pattern Matching**: JVM の最適化により、手動実装と同等またはそれ以上のパフォーマンス

### メモリ使用量
- **Virtual Threads**: プラットフォームスレッドと比較して 1/10 以下のメモリフットプリント (スタックサイズ: 数百バイト～数KB)
- **Sequenced Collections**: 既存のコレクション実装を拡張するため追加メモリなし

---

## セキュリティ考慮事項

### JEP 444: Virtual Threads
- 仮想スレッドは ThreadLocal に対応していますが、大量の仮想スレッドでは メモリリークのリスクがあります
- `ScopedValue` (JEP 446) の使用を推奨

### JEP 451: Prepare to Disallow Dynamic Agent Loading
- 動的エージェント読み込みが将来的に禁止される可能性があるため、`jv_build` でワーニングを表示

### Preview 機能
- 本番環境での使用には `--enable-preview` の明示的なオプトインが必要であることを警告

---

## 移行ガイド

### Java 17 → Java 21

**推奨移行パス**:
1. `jv.toml` で `target_version = "21"` に更新
2. 既存の `when` 式が自動的に Java 21 の pattern matching に変換されることを確認
3. `spawn {}` ブロックが仮想スレッドを使用することを確認
4. Sequenced Collections API を活用してコードを簡素化
5. テストスイートを実行して互換性を検証

**破壊的変更**:
- なし (Java 21 は Java 17 との完全な後方互換性を維持)

---

## 参照

- [JEP 431: Sequenced Collections](https://openjdk.org/jeps/431)
- [JEP 440: Record Patterns](https://openjdk.org/jeps/440)
- [JEP 441: Pattern Matching for switch](https://openjdk.org/jeps/441)
- [JEP 444: Virtual Threads](https://openjdk.org/jeps/444)
- [JEP 430: String Templates (Preview)](https://openjdk.org/jeps/430)
- [JEP 443: Unnamed Patterns and Variables (Preview)](https://openjdk.org/jeps/443)
- [JEP 445: Unnamed Classes and Instance Main Methods (Preview)](https://openjdk.org/jeps/445)
- [JEP 446: Scoped Values (Preview)](https://openjdk.org/jeps/446)
- [JEP 453: Structured Concurrency (Preview)](https://openjdk.org/jeps/453)
- [JDK 21 Release Notes](https://jdk.java.net/21/)
- [docs/design/java25-syntax-features.md](java25-syntax-features.md)
- [docs/design/type-inference-system.md](type-inference-system.md)
