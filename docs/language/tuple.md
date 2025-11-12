# タプルリテラルと多値返却

## 基本構文

- 括弧の中に空白区切りで 2 つ以上の式を並べるとタプルを作成できる。
- 分割代入は `(a b)`（または `[a b]`）の形で宣言側に記述する。

```jv
val divmodResult = (
    dividend / divisor // quotient
    dividend % divisor // remainder
)

val (quotient remainder) = divmodResult
val [quotientAlt remainderAlt] = divmodResult
```

- 角括弧パターン `[a b]` でも同じようにタプルを分割できる。
- 代入側の要素数はタプルの要素数と一致している必要がある。

## フィールド名の決定順序

- **コメント**: `// label` や `/* label */` が最優先でレコードのアクセサ名になる。
- **識別子ヒント**: 同じ式を変数へ代入している場合、その変数名が使われる。
- **フォールバック**: どちらも無い場合は `_1()`, `_2()` のような番号付きアクセサが生成される。

```jv
val fallbackValue: Int = 99
val labelPriority = (
    quotient // commentPrimary
    fallbackValue
    42
)

println(
    "comment=${labelPriority.commentPrimary()} identifier=${labelPriority.fallbackValue()} literal=${labelPriority._3()}"
)
```

## 関数の戻り値としてのタプル

- 戻り値にタプル型を記述すると、呼び出し側が複数の値を一度に受け取れる。
- `.jv` コードの例は `jv/examples/tuple_demo.jv` に含まれている。

```jv
fun divmod(dividend: Int, divisor: Int): (Int Int) {
    val quotient: Int = dividend / divisor
    val remainder: Int = dividend % divisor

    return (
        quotient // quotient
        remainder // remainder
    )
}

val (quotient remainder) = divmod(27 4)
```

## 実行例とテスト

- `jv/examples/tuple_demo.jv` ではタプル生成・分割代入・フィールド名フォールバックをまとめて確認できる。
- 生成される Java コードは `jv build` コマンドで確認できる（プロジェクト内の `jv` CLI を利用）。
- ゴールデンテストは `jv/tests/integration/tuple_literal.rs` が `divmod`, `find_user`, `calculate_stats` の各シナリオをカバーしている。

## 実装メモ

タプルの内部処理（`FieldNameLabel`, `TupleRecordPlan`, IR 展開など）の詳細は `docs/design/tuple-record-plan.md` を参照してください。
