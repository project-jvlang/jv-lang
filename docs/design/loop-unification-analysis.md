# jv言語ループ統一設計 - 比較検討資料

## 概要

jv言語でのfor文とwhile文の統一について、メリット・デメリット・実装方針を検討した資料。`for(; condition; )`構文廃止を前提とした設計決定。

## 設計決定事項

**採用方針**: for文統一 + 高階関数補完（一括実装）
**廃止事項**: while文、`for(; condition; )`構文の完全廃止
**理由**: 安全性向上、表現力維持、実装一貫性の実現

## 統一案比較分析

### 検討した統一案

#### 案1: for文統一（採用）
- **許可構文**: `for (item in items)`, `for (i in 0..10)`
- **代替手段**: 高階関数による複雑制御フロー
- **安全性**: 終了保証されたループのみ

#### 案2: while文統一（却下）
- **許可構文**: `while (condition)`のみ
- **問題**: 冗長性増加、現代的言語からの逸脱

#### 案3: for文 + `for(; condition; )`構文（却下）
- **問題**: 意図の曖昧性、安全性の低下

## 採用案の詳細仕様

### 基本for文構文
```jv
// 許可される構文
for (item in items) { }         // for-each（イテレータベース）
for (i in 0..10) { }            // range（排他的）
for (i in 0..=10) { }           // range（包括的）

// 廃止される構文
for (; condition; ) { }         // ❌ 廃止
while (condition) { }           // ❌ 廃止
```

### 高階関数による制御フロー補完
```jv
// while相当の代替
items.takeWhile { condition }
     .forEach { process(it) }

// 無限ループ相当
generateSequence(0) { it + 1 }
    .takeWhile { running.get() }
    .forEach { process(it) }

// 条件リトライ
repeat(maxRetries) { attempt ->
    when {
        tryOperation() -> return@repeat
    }
}
```

## Java開発者向けガイド

### Java → jv 基本変換パターン

#### Enhanced-for文
```java
// Java
for (String item : items) {
    process(item);
}
```
```jv
// jv
for (item in items) {
    process(item)
}
```

#### 範囲ループ
```java
// Java
for (int i = 0; i < 10; i++) {
    process(i);
}
```
```jv
// jv
for (i in 0..10) {
    process(i)
}
```

#### while文の代替パターン

**パターン1: 単純なコレクション処理**
```java
// Java
Iterator<String> iter = items.iterator();
while (iter.hasNext()) {
    String item = iter.next();
    if (!item.isValid()) break;
    process(item);
}
```
```jv
// jv
for (item in items.takeWhile { it.isValid() }) {
    process(item)
}
```

**パターン2: 条件付き繰り返し**
```java
// Java
while (condition()) {
    doWork();
}
```
```jv
// jv
generateSequence { doWork() }
    .takeWhile { condition() }
    .forEach { }
```

**パターン3: リトライ処理**
```java
// Java
int attempts = 0;
while (attempts < maxRetries && !success) {
    success = tryOperation();
    attempts++;
}
```
```jv
// jv
repeat(maxRetries) { attempt ->
    when {
        tryOperation() -> return@repeat
    }
}
```

### while代替の実用サンプル

#### API呼び出しリトライパターン
```java
// Java - API呼び出しエラー時に2回まで再試行
boolean success = false;
int retryCount = 0;
Exception lastException = null;

while (!success && retryCount <= 2) {
    try {
        ApiResponse response = apiClient.call("/endpoint");
        if (response.isSuccess()) {
            success = true;
            processResponse(response);
        } else {
            throw new ApiException("Request failed: " + response.getError());
        }
    } catch (Exception e) {
        lastException = e;
        retryCount++;
        if (retryCount <= 2) {
            System.out.println("Retry attempt " + retryCount + "/2");
            Thread.sleep(1000 * retryCount); // exponential backoff
        }
    }
}

if (!success) {
    throw new RuntimeException("API call failed after 3 attempts", lastException);
}
```

```jv
// jv - 高階関数でリトライロジックを表現
fun <T> retryWithBackoff(
    maxAttempts: Int = 3,
    baseDelay: Duration = Duration.ofSeconds(1),
    operation: () -> T
): Result<T> {
    var lastException: Exception? = null

    for (attempt in 1..maxAttempts) {
        try {
            val result = operation()
            return Result.success(result)
        } catch (e: Exception) {
            lastException = e
            when {
                attempt < maxAttempts -> {
                    println("Retry attempt $attempt/$maxAttempts")
                    Thread.sleep(baseDelay.toMillis() * attempt)
                }
            }
        }
    }

    return Result.failure(
        RuntimeException("Operation failed after $maxAttempts attempts", lastException)
    )
}

// 使用例
val result = retryWithBackoff(maxAttempts = 3) {
    val response = apiClient.call("/endpoint")
    when {
        response.isSuccess() -> {
            processResponse(response)
            response
        }
        else -> throw ApiException("Request failed: ${response.error}")
    }
}

result.fold(
    onSuccess = { response -> println("API call succeeded") },
    onFailure = { error -> println("API call failed: ${error.message}") }
)
```

#### ディレクトリツリー再帰表示パターン
```java
// Java - 再帰的ディレクトリツリー表示
public void printDirectoryTree(File directory, String prefix) {
    if (!directory.isDirectory()) return;

    File[] files = directory.listFiles();
    if (files == null) return;

    Arrays.sort(files, (a, b) -> {
        if (a.isDirectory() != b.isDirectory()) {
            return a.isDirectory() ? -1 : 1; // directories first
        }
        return a.getName().compareToIgnoreCase(b.getName());
    });

    for (int i = 0; i < files.length; i++) {
        File file = files[i];
        boolean isLast = (i == files.length - 1);

        System.out.println(prefix + (isLast ? "└── " : "├── ") + file.getName());

        if (file.isDirectory()) {
            String newPrefix = prefix + (isLast ? "    " : "│   ");
            printDirectoryTree(file, newPrefix);
        }
    }
}

// 使用
printDirectoryTree(new File("/path/to/directory"), "");
```

```jv
// jv - 関数型アプローチでディレクトリツリー処理
data class TreeNode(
    val name: String,
    val isDirectory: Boolean,
    val children: List<TreeNode> = emptyList()
)

fun Path.toTreeNode(): TreeNode? {
    when {
        !exists() -> return null
    }

    val children = when {
        isDirectory() ->
            listDirectoryEntries()
                .sortedWith { a, b ->
                    when {
                        a.isDirectory() != b.isDirectory() ->
                            when {
                                a.isDirectory() -> -1
                                else -> 1
                            }
                        else -> a.name.compareTo(b.name, ignoreCase = true)
                    }
                }
                .mapNotNull { it.toTreeNode() }
        else -> emptyList()
    }

    return TreeNode(name, isDirectory(), children)
}

fun TreeNode.printTree(prefix: String = "", isLast: Boolean = true) {
    val connector = when {
        isLast -> "└── "
        else -> "├── "
    }
    println("$prefix$connector$name")

    when {
        children.isNotEmpty() -> {
            val newPrefix = prefix + when {
                isLast -> "    "
                else -> "│   "
            }

            for ((index, child) in children.withIndex()) {
                val childIsLast = (index == children.lastIndex)
                child.printTree(newPrefix, childIsLast)
            }
        }
    }
}

// 使用例
fun printDirectoryTree(path: String) {
    Path.of(path).toTreeNode()?.printTree()
        ?: println("Directory not found: $path")
}

// より関数型的なアプローチ
fun generateDirectorySequence(rootPath: Path): Sequence<Pair<Path, Int>> =
    generateSequence(listOf(rootPath to 0)) { currentLevel ->
        currentLevel
            .filter { (path, _) -> path.isDirectory() }
            .flatMap { (path, depth) ->
                path.listDirectoryEntries()
                    .map { it to depth + 1 }
            }
            .takeIf { it.isNotEmpty() }
    }
    .flatten()

fun printDirectoryTreeFunctional(rootPath: Path) {
    generateDirectorySequence(rootPath)
        .groupBy { (_, depth) -> depth }
        .toSortedMap()
        .forEach { (depth, paths) ->
            val indent = "  ".repeat(depth)
            for ((path, _) in paths.sortedBy { it.first.name }) {
                val type = when {
                    path.isDirectory() -> "📁"
                    else -> "📄"
                }
                println("$indent$type ${path.name}")
            }
        }
}
```

### Java Stream API対応
```java
// Java Stream
items.stream()
     .takeWhile(item -> item.isValid())
     .forEach(item -> process(item));
```
```jv
// jv高階関数
for (item in items.takeWhile { it.isValid() }) {
    process(item)
}
```

## 実装仕様

### AST設計
```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // 統一されたfor文のみ
    For {
        variable: String,
        iterable: Expression,  // Collection, Range, Iterator
        body: Box<Expression>,
        span: Span,
    },
    // while文は完全に廃止
    // 他のステートメント...
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // Range式
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
        inclusive: bool,  // ..= の場合true
        span: Span,
    },
    // 高階関数呼び出し
    MethodCall {
        receiver: Box<Expression>,
        method: String,
        arguments: Vec<Expression>,
        span: Span,
    },
    // その他の式...
}
```

### 必要な標準ライブラリ関数
```jv
// Collection拡張
fun <T> Collection<T>.takeWhile(predicate: (T) -> Boolean): Collection<T>
fun <T> Collection<T>.dropWhile(predicate: (T) -> Boolean): Collection<T>
fun <T> Collection<T>.forEach(action: (T) -> Unit)
fun <T> Collection<T>.filter(predicate: (T) -> Boolean): Collection<T>

// Sequence生成
fun <T> generateSequence(seed: T, next: (T) -> T?): Sequence<T>
fun repeat(times: Int, action: (Int) -> Unit)

// Iterator拡張
fun <T> Iterator<T>.takeWhile(predicate: (T) -> Boolean): Iterator<T>
```

### Java変換パターン
```java
// jv: for (item in items)
for (var item : items) { /* body */ }

// jv: for (i in 0..10)
for (int i = 0; i < 10; i++) { /* body */ }

// jv: for (i in 0..=10)
for (int i = 0; i <= 10; i++) { /* body */ }

// jv: items.takeWhile { it.isValid() }.forEach { process(it) }
items.stream()
     .takeWhile(it -> it.isValid())
     .forEach(it -> process(it));

// jv: generateSequence(0) { it + 1 }.takeWhile { it < 100 }
Stream.iterate(0, i -> i + 1)
      .takeWhile(i -> i < 100)
      .forEach(/* action */);

// jv: repeat(5) { println("Hello") }
IntStream.range(0, 5)
         .forEach(i -> System.out.println("Hello"));
```

## 実装工数見積もり

### 一括実装での工数
```yaml
合計実装工数: 12-14日

詳細内訳:
  AST更新: 1日
    - While enum削除
    - Range expression追加

  パーサー実装: 3日
    - while文パーサー削除
    - for文パーサー拡張（range対応）
    - 高階関数呼び出しパーサー

  標準ライブラリ: 4日
    - takeWhile, dropWhile, forEach実装
    - generateSequence, repeat実装
    - Iterator拡張メソッド

  Java生成: 3日
    - for文 → Java for/enhanced-for変換
    - 高階関数 → Stream API変換
    - Range → for文変換

  テスト: 3-4日
    - 全パターンの網羅的テスト
    - パフォーマンステスト
    - Java出力検証
```

## メリット・デメリット分析

### メリット

#### 1. 安全性の向上
- 無限ループの排除（終了保証されたループのみ）
- イテレータの自動管理
- off-by-oneエラーの削減

#### 2. 表現力の維持・向上
- 高階関数による柔軟な制御フロー
- Stream APIとの自然な対応
- 関数型プログラミングパラダイムの採用

#### 3. 実装の一貫性
- パーサー実装の簡略化
- AST構造の単純化
- Java変換ロジックの統一

#### 4. 現代的な言語設計
- Kotlin、Swift等との整合性
- 関数型言語の特徴取り込み

### デメリット

#### 1. 学習コストの一時的増加
- Java開発者のwhile文への慣れ
- 高階関数パラダイムへの移行

#### 2. 一部パターンの複雑化
```jv
// Java開発者が期待する単純な表現（廃止された構文）
// while (!queue.isEmpty()) { process(queue.poll()) }

// jvでの代替案（やや複雑だが安全）
generateSequence { queue.poll() }
    .takeWhile { queue.isNotEmpty() }
    .forEach { process(it) }

// より直感的な代替案
for (item in queue.drainIterator()) {
    process(item)
}
```

#### 3. 実装工数の増加
- 高階関数ライブラリの実装が必要
- Stream API変換の複雑性

## Javaの直感的書き方に潜む安全性問題

### 従来のJava開発で頻発する問題

#### 1. 無限ループと予期しない終了条件
```java
// Java - よくある危険パターン
List<String> items = getItems();
int i = 0;
while (i < items.size()) {
    String item = items.get(i);
    if (shouldProcess(item)) {
        items.add(processedItem(item));  // ❌ サイズが変わって無限ループ
    }
    i++;
}

// Java - null安全性の欠如
String input = getUserInput();  // nullの可能性
while (input != null && input.length() > 0) {  // ❌ nullチェック漏れがちELSE
    input = input.substring(1);  // ❌ IndexOutOfBoundsException
    // ...
    input = getNextInput();  // ❌ 途中でnullになる可能性
}
```

```jv
// jv - 安全な代替
val items = getItems()  // List<String> (immutable)
val processed = items
    .filter { shouldProcess(it) }
    .map { processedItem(it) }
    .toMutableList()

// jv - null安全性保証
val input: String? = getUserInput()  // null安全型
input?.takeWhile { it.isNotEmpty() }
     ?.map { it.drop(1) }
     ?.forEach { process(it) }
```

#### 2. イテレータの不正使用とConcurrentModificationException
```java
// Java - 実行時エラーが起こりがちな典型例
List<String> items = new ArrayList<>(Arrays.asList("a", "b", "c"));
Iterator<String> iter = items.iterator();

while (iter.hasNext()) {
    String item = iter.next();
    if (item.equals("b")) {
        items.remove(item);  // ❌ ConcurrentModificationException
    }
}

// Java - 二重イテレータ問題
while (outerIter.hasNext()) {
    OuterItem outer = outerIter.next();
    Iterator<InnerItem> innerIter = outer.getInnerItems().iterator();

    while (innerIter.hasNext()) {  // ❌ ネストした可変状態
        InnerItem inner = innerIter.next();
        if (complexCondition(outer, inner)) {
            outerIter.remove();  // ❌ 外側の状態変更が内側に影響
            break;  // ❌ どのループからのbreak？
        }
    }
}
```

```jv
// jv - 安全で明確
val items = mutableListOf("a", "b", "c")
val filtered = items.filterNot { it == "b" }  // 新しいリスト生成

// jv - 関数型アプローチ
val result = outerItems
    .flatMap { outer ->
        outer.innerItems.mapNotNull { inner ->
            when {
                complexCondition(outer, inner) -> ProcessedItem(outer, inner)
                else -> null
            }
        }
    }
```

#### 3. off-by-oneエラーと境界条件バグ
```java
// Java - 境界条件エラー頻発
int[] array = {1, 2, 3, 4, 5};
int i = 0;
while (i <= array.length) {  // ❌ <= により配列外アクセス
    System.out.println(array[i]);
    i++;
}

// Java - 複雑な終了条件
while (condition1 && condition2 || condition3) {  // ❌ 論理演算の優先度曖昧
    // 複雑な処理
    updateConditions();  // ❌ どの条件がいつ変わるか不明
}
```

```jv
// jv - 境界安全保証
val array = arrayOf(1, 2, 3, 4, 5)
for (value in array) {  // 境界チェック自動
    println(value)
}

// jv - 明確な条件表現
generateSequence { getCurrentState() }
    .takeWhile { state ->
        when {
            state.condition1 && state.condition2 -> true
            state.condition3 -> true
            else -> false
        }
    }
    .forEach { processState(it) }
```

### jvの強い制約による安全性向上

#### 1. コンパイル時null安全性保証
```java
// Java - 実行時NullPointerException
String getValue() {
    return database.query().getResult();  // どこでnullになるか不明
}

while (getValue() != null) {  // ❌ 毎回null可能性
    process(getValue().toUpperCase());  // ❌ NPE発生可能
}
```

```jv
// jv - コンパイル時null安全性
fun getValue(): String? = database.query()?.getResult()

generateSequence { getValue() }
    .takeWhile { it != null }  // null安全保証
    .map { it!!.uppercase() }  // 明示的non-null
    .forEach { process(it) }

// より安全なアプローチ
val values = generateSequence { getValue() }
    .takeWhile { it != null }
    .filterNotNull()  // 型レベルでnull除去
    .map { it.uppercase() }  // String型確定
```

#### 2. 不変性による副作用制御
```java
// Java - 予期しない副作用
List<String> processItems(List<String> items) {
    List<String> result = new ArrayList<>();
    int index = 0;

    while (index < items.size()) {
        String item = items.get(index);
        String processed = processItem(item);

        // ❌ グローバル状態変更の副作用
        updateGlobalCounter();
        logProgress(index);

        if (processed != null) {
            result.add(processed);
            items.set(index, "PROCESSED");  // ❌ 入力リスト変更
        }
        index++;
    }
    return result;
}
```

```jv
// jv - 不変性と副作用分離
fun processItems(items: List<String>): Pair<List<String>, ProcessingStats> {
    val results = items
        .mapIndexed { index, item ->
            val processed = processItem(item)
            ProcessingResult(index, item, processed)
        }
        .filter { it.processed != null }

    val processedItems = results.map { it.processed!! }
    val stats = ProcessingStats(
        totalProcessed = results.size,
        originalItems = items  // 元のリストは不変
    )

    return processedItems to stats
}
```

#### 3. 型安全性とパターンマッチング
```java
// Java - 型安全性の欠如
Object[] mixed = {1, "hello", 3.14, null};
int i = 0;
while (i < mixed.length) {
    Object item = mixed[i];

    // ❌ 実行時型チェック、キャスト例外の可能性
    if (item instanceof String) {
        String str = (String) item;
        process(str.toUpperCase());
    } else if (item instanceof Integer) {
        Integer num = (Integer) item;
        process(num * 2);
    }
    // ❌ nullケース忘れがち
    i++;
}
```

```jv
// jv - 型安全性とパターンマッチング
val mixed: List<Any?> = listOf(1, "hello", 3.14, null)

for (item in mixed) {
    when (item) {
        is String -> process(item.uppercase())  // 型自動推論
        is Int -> process(item * 2)
        is Double -> process(item.toString())
        null -> println("null value encountered")
        else -> println("Unknown type: ${item::class}")
    }  // 網羅性チェック - 全ケース必須
}
```

## 思考パラダイムの変化によるメリット

### 1. 命令型から宣言型への移行

**従来の命令型思考（危険）**:
「どうやって」実現するかに焦点
- ループカウンタの管理
- 可変状態の追跡
- 副作用の制御

**jvの宣言型思考（安全）**:
「何を」実現するかに焦点
- データの変換表現
- 不変性による予測可能性
- 型システムによる保証

### 2. エラー発見の早期化

```java
// Java - 実行時まで発見されないバグ
public List<String> processUserData(List<User> users) {
    List<String> results = new ArrayList<>();
    Iterator<User> iter = users.iterator();

    while (iter.hasNext()) {
        User user = iter.next();
        String email = user.getEmail();  // ❌ null可能性

        if (email.contains("@")) {  // ❌ NPE (実行時エラー)
            results.add(email.toUpperCase());
        }
    }
    return results;
}
```

```jv
// jv - コンパイル時エラーで安全性保証
fun processUserData(users: List<User>): List<String> {
    return users
        .mapNotNull { user -> user.email }  // コンパイル時null安全
        .filter { email -> email.contains("@") }  // 型安全保証
        .map { email -> email.uppercase() }
}

// コンパイル時に以下をチェック:
// - null安全性違反
// - 型不一致
// - when式の網羅性
// - 不変性違反
```

### 3. 認知負荷の軽減

**Java開発者が常に意識する必要があること**:
- nullチェック漏れ
- 配列境界
- イテレータ状態
- 可変状態の副作用
- スレッド安全性
- メモリリーク

**jvで言語レベルで解決されること**:
- null安全性は型システムで保証
- 境界チェックは自動
- イテレータは不変
- 副作用は型で分離
- 並行性は安全な構造で提供

## 設計判断の根拠

### 採用理由
1. **安全性優先**: 無限ループや制御エラーのリスク排除
2. **表現力**: 高階関数で複雑な制御フローに対応
3. **モダンさ**: 現代的言語トレンドへの準拠
4. **Java互換性**: Stream APIとの自然な変換

### リスク軽減策
1. **包括的ドキュメント**: Java開発者向けガイドの充実
2. **IDE支援**: 自動補完とリファクタリング機能
3. **エラーメッセージ改善**: while使用時の適切な代替案提示

## 結論

**for文統一 + 高階関数補完**により、jvは以下を実現：

- **安全で予測可能なループ構文**
- **表現力豊かな制御フロー**
- **Java開発者にとって理解しやすい変換パターン**
- **実装の一貫性と保守性**

この設計により、jvは**Java Sugar Language**としての目標を達成し、開発者に**安全で効率的なプログラミング体験**を提供する。