# 簡潔な正規表現コマンド ハンドブック

## 概要
- `[mode]/subject/pattern/[arg]/[flags]` 形式で `Pattern` / `Matcher` API 呼び出しを糖衣化する構文。
- `regex-is-operator` を補完し、置換（`a`/`f`）、マッチ判定（`m`）、分割（`s`）、逐次処理（`i`）を1行で記述できる。
- 型推論・診断は `docs/design/java-aware-type-inference.md` に従って `String` / `Boolean` / `String[]` / `Stream<MatchResult>` を安全に扱う。

## Pattern 型推論と定数化戦略
- `/.../` リテラルや簡潔構文が生成する正規表現は、型推論フェーズで自動的に `java.util.regex.Pattern` へ昇格する。`val pattern = /\d+/` のような宣言は追加のアノテーションなしで `Pattern` として扱われ、`List<Pattern>` や `Map<String, Pattern>` の型変数にも伝播する。
- 型判定は `PatternTypeBinder` が担い、推論環境へ `TypeKind::Reference("java.util.regex.Pattern")` を注入するため、関数戻り値やプロパティ初期化でも `Pattern` 型が維持される。
- 定数判定に合格したパターンは `PatternConstRegistry` を経由して `static final Pattern` フィールドへ集約される。同一ソースのパターンは1度だけ `Pattern.compile` され、CLI でも共有フィールド名が報告される。
- 動的に評価されるパターンは `RuntimePatternGuard` で `PatternSyntaxException` を捕捉し、`.jv` のファイル名・行情報付きで再スローされる。

```jv
import java.util.regex.Pattern

val patterns: List<Pattern> = listOf(
    /\d{3}-\d{4}/,
    i/users/"([\w.]+)@example.com"/
)

fun provideValidator(): Pattern = /[A-Z]{2}\d{6}/
```

## モード早見表
| モード | 意味 | 典型的な戻り値 | 生成されるJava呼び出し |
| --- | --- | --- | --- |
| `a` | すべて置換 (replaceAll) | `String` | `matcher.replaceAll(...)` |
| `f` | 先頭置換 (replaceFirst) | `String` | `matcher.replaceFirst(...)` |
| `m` | 全体一致判定 | `Boolean` | `matcher.matches()` |
| `s` | 分割 | `String[]` | `pattern.split(subject, -1)` |
| `i` | イテレーション | `Stream<MatchResult>` または `String`（ラムダ置換時） | `matcher.results()` / `appendReplacement` |

- モード省略時は置換引数があれば `a`、なければ `m` が暗黙指定される。
- フラグ列（`imdsxlcu`）は末尾スラッシュの後に続ける。例: `a/text/"pattern"/"replacement"/mi`

## 置換引数
- 文字列リテラルか、生文字列（`'...'` / `'''...'''`）を指定可能。正規表現の記述には生文字列が便利。
- `${ ... }` 形式でラムダ置換を記述でき、`it: MatchResult` からグループを参照する。複雑な書式化は `docs/design/java-aware-type-inference.md` 記載の `RegexCommandTyping` に基づき `String` 戻り値へ強制される。

```jv
val formatted = a/text/"(\d{4})(\d{2})(\d{2})"/$ {
    "${it.group(1)}/${it.group(2)}/${it.group(3)}"
}/
```

## `split` モードの使い方
- `s/subject/pattern/` で `String[]` を返す。戻り値は `TypeKind::Reference("java.lang.String[]")` として表現され、Java 25 では直接配列操作が可能。
- 例: カンマ区切りを配列化し、`for-in` で列挙する。

```jv
val parts = s/csv/'[,]\s*'/
for (part in parts) {
    println(part)
}
```

## `iterate` モードと `as List`
- `i` モードは `Stream<MatchResult>` を返し、大規模データでの遅延処理に向く。
- `.../ as List` を追記すると `List<MatchResult>` に強制変換され、複数回の走査や添字アクセスが容易になる。型システムは `docs/design/java-aware-type-inference.md` の `RegexCommandTyping` を用いて `List<MatchResult>` へ整合させる。
- ラムダ置換を指定すると `String` を返す形に切り替わる（`appendReplacement` ベース）。

```jv
val matches = i/text/'([\w.]+)@([\w.]+)'/ as List
for (result in matches) {
    println("${result.group(1)} -> ${result.group(2)}")
}
```

## CLI サンプル
- サンプルコード: `samples/regex/concise_command.jv`
- 実行手順: `cargo run --bin jv -- run samples/regex/concise_command.jv`
- 期待される出力例:

```text
replaceAll結果: alice@example.dev,bob@example.dev,carol@example.dev
先頭置換: ALICE@example.com,bob@example.org,carol@example.net
全体マッチ: true
split結果:
  alice@example.com
  bob@example.org
  carol@example.net
iterate結果:
  alice -> example.com
  bob -> example.org
  carol -> example.net
```

## テレメトリ
- `jv check --telemetry` を有効にすると、`pattern_const.static_ratio` が出力され、定数化できたパターンの割合を把握できる。
- `pattern_const.total` / `static` / `dynamic` に加え、`literal_*` と `command_*` の内訳でリテラル起源と簡潔構文起源を区別できる。
- 正規表現関連の実行統計は `regex_command.*` と併せて確認し、キャッシュヒット率や動的コンパイルの発生状況を継続的に監視する。

## 関連資料
- `.spec-workflow/specs/concise-regex-syntax/requirements.md`
- `.spec-workflow/specs/concise-regex-syntax/design.md`
- `.project-todolist/作業指示-20251031.md` §9.4
