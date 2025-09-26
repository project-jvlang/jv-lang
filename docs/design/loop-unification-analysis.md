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
    if (tryOperation()) return@repeat
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
    if (tryOperation()) return@repeat
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
// 単純なwhile文
while (!queue.isEmpty()) { process(queue.poll()) }

// 代替案（やや複雑）
for (item in queue.drainIterator()) { process(item) }
```

#### 3. 実装工数の増加
- 高階関数ライブラリの実装が必要
- Stream API変換の複雑性

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