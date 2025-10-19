# KotlinスタイルコレクションAPIガイド

## 概要
- Kotlin互換のコレクション拡張関数（`map`/`filter`/`flatMap`/`reduce`/`fold`/`associate`/`groupBy`/`sorted`/`take`/`drop` など）を jv 標準ライブラリ `stdlib/collections/sequence.jv` に追加しました。
- `asSequence()` 呼び出しは不要で、`Iterable` や空白区切り配列 `[1 2 3]` に対して変換操作を記述すると暗黙的に遅延 Sequence パイプラインが構築されます。
- 終端操作（`toList`/`toSet`/`count`/`sum`/`forEach` など）を呼ぶまで評価は遅延され、実行時には Java 25 `Stream` API または逐次 `for` ループへデシュガリングされます。
- `Stream<T>.sum()` はプリミティブ境界 (`where T : int, T : char, T : short`) を解釈し、`char`/`short` を `int` 系へ正規化する補助ラムダを自動合成します。
- Java 21 互換出力では `Collectors` ベースのフォールバックを自動生成し、ゼロランタイム依存を維持します。

## クイックスタート
```jv
numbers = [1 2 3 4 5]

// 暗黙 Sequence チェーン
evensDoubled = numbers
    .map { x -> x * 2 }
    .filter { value -> value % 2 == 0 }
    .toList()

// associate / groupBy などの終端操作も Kotlin と同じノリで利用できます
catalog = products
    .groupBy { item -> item.category }
    .associate { entry -> entry.key to entry.value.toList() }
```

### 明示ラムダ引数ルール
- `numbers.map { x -> x * 2 }` のようにラムダ引数を必ず明示してください。
- Kotlin の暗黙 `it` パラメータ（`{ it * 2 }`）はコンパイルエラーになります。
- タプル分解 `{ (acc, value) -> ... }` はサポートされ、IR と生成 Java で中間タプルなしに展開されます。

## Sequence ⇔ Kotlin パリティ表
| Kotlin 操作 | jv の書き方 | Java 25 出力 | Java 21 出力 | 注記 |
|-------------|-------------|---------------|---------------|------|
| `asSequence()` | ❌ 不要 | – | – | Iterable/配列への変換操作開始で暗黙遅延化 |
| `map { it * 2 }` | `map { x -> x * 2 }` | `.map(x -> x * 2)` | 同左 | 引数名必須 |
| `filter { it > 0 }` | `filter { value -> value > 0 }` | `.filter(value -> value > 0)` | 同左 | |
| `flatMap { it.items }` | `flatMap { entry -> entry.items }` | `.flatMap(entry -> entry.items().stream())` | `.flatMap(entry -> entry.items().stream())` | `Stream`/`Iterator` ブリッジは自動 |
| `sorted()` | `sorted()` | `.sorted()` | `.sorted()` | 安定ソート保証 |
| `sortedBy { it.name }` | `sortedBy { user -> user.name }` | `.sorted(Comparator.comparing(user -> user.name()))` | 同左 | |
| `take(n)` | `take(n)` | `.limit(n)` | `.limit(n)` | |
| `drop(n)` | `drop(n)` | `.skip(n)` | `.skip(n)` | |
| `toList()` | `toList()` | `.toList()` | `.collect(Collectors.toList())` | |
| `toSet()` | `toSet()` | `.collect(Collectors.toSet())` | `.collect(Collectors.toSet())` | Java 25 でも安全性確保のため Collectors を使用 |
| `fold(initial)` | `fold(initial) { acc, x -> ... }` | `reduce(initial, (acc, x) -> ...)` | 同左 | 逐次評価・副作用なし |
| `reduce { acc, x -> ... }` | `reduce { acc, x -> ... }` | `.reduce((acc, x) -> ...)` | 同左 | 空コレクション時は `IllegalArgumentException` |
| `groupBy { ... }` | `groupBy { item -> item.category }` | `.collect(Collectors.groupingBy(...))` | 同左 | 値は `List` で収集 |
| `associate { k to v }` | `associate { entry -> entry.id to entry }` | `.collect(Collectors.toMap(...))` | 同左 | キー重複は後勝ち |
| `count()` | `count()` | `.count()` | `.count()` | long を返す |
| `sum()` | `sum()` | `mapToInt(...).sum()` / `mapToLong(...).sum()` / `mapToDouble(...).sum()` | 同左 | `char`/`short` は `int` 系へ正規化し、`Character` 判定ガードを自動生成 |
| `forEach { ... }` | `forEach { value -> println(value) }` | `.forEach(value -> println(value))` | 同左 | 終端副作用操作 |

## 評価モデルと型遷移
- 変換操作のみで終端操作がない場合は遅延 Sequence オブジェクトが返ります。`toList`/`toSet` 等を呼ぶまでソースコレクションは走査されません。
- 終端操作を呼ぶと即時評価に切り替わり、`List`/`Set`/`Map` などの不変コレクションが生成されます。
- 空配列リテラルや JSON 配列 (`json.nodes`) からのチェーンも同じ規約で動作します。
- `Sequence` から `Stream` へ、また `Stream` から `Sequence` へのブリッジを行う際は単一評価ポリシーを守り、`Stream` を二度使わないでください。

### プリミティブ sum の正規化
- `Stream<T>.sum()` は `where T : int, T : char, T : short` を満たす場合にカノニカル型として `int` を採用し、`char`/`short` などの互換プリミティブをボクシング／アンボクシングなしで扱います。
- 生成 Java では `mapToInt(value -> ...)` が使用され、`Character` インスタンスを検出した場合のみ `charValue()` → `int` 変換を行い、その他は `Number.intValue()` へフォールバックします。
- これにより `Stream<Character>.sum()` と `Stream<Int>.sum()` が同一の呼び心地で利用でき、Sequence パイプラインから得られたヒントも同じ最適化に接続されます。

## Java 25 / Java 21 フォールバック
| 操作 | Java 25 出力 | Java 21 フォールバック | 備考 |
|------|---------------|-------------------------|------|
| `toList` | `.toList()` | `.collect(Collectors.toList())` | Java 21 互換 |
| `toSet` | `.collect(Collectors.toSet())` | `.collect(Collectors.toSet())` | Java 25/21 で統一挙動 |
| `associate` | `.collect(Collectors.toMap(...))` | 同左 | キー重複ポリシーを統一 |
| `groupBy` | `.collect(Collectors.groupingBy(...))` | 同左 | 値は `List` |
| `sum` | `mapToDouble(...).sum()` 等 | 同左 | 型ごとに最適化 |
| `forEach` | `.forEach(...)` | 同左 | `AutoCloseable` ソースは `try-with-resources` 生成 |

- Java 25 ターゲットでも `Stream#toSet()` を提供しない JDK リリースが存在するため、常に `Collectors.toSet()` へフォールバックして互換性を維持します。

## Java Stream ブリッジと注意点
- `Iterable`/配列 → `Stream` 変換は必要な場合にだけ生成され、短いチェーンでは逐次 `for` ループに切り替わります。
- `AutoCloseable` なソース (`BufferedReader` など) は `try-with-resources` でラップされ、終端後に自動クローズされます。
- `Stream` から生成された結果は終端後に再評価できません。再利用が必要な場合は `toList()` `toSet()` などで具現化してください。
- `reduce` の空コレクションは Kotlin と同様に `IllegalArgumentException` をスローします。`fold(initial)` を使用すると安全に初期値を指定できます。

## FAQ
- **Q. `asSequence()` や `sequenceOf()` を書く必要はありますか？**  
  **A.** いいえ。`Iterable` または空白区切り配列／JSON 配列に対して変換操作を呼ぶと自動的に遅延 Sequence が開始されます。
- **Q. `map { it * 2 }` のような書き方はできますか？**  
  **A.** できません。`{ value -> value * 2 }` のように引数名を明示してください。
- **Q. Java 21 プロジェクトでも利用できますか？**  
  **A.** はい。`toList()` などは自動で `Collectors` フォールバックに置き換えられます。生成コードは Java 21 LTS コンパイラで検証済みです。
