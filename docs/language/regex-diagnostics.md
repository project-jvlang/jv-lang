# 正規表現診断リファレンス

この文書は `regex-java-integration-error-handling` 仕様に基づき、jv コンパイラが出力する正規表現関連診断の概要と対処方法をまとめたものです。CLI の `jv --explain <診断コード>` や IDE ホバー表示の参照元として利用できます。

- Java 公式リファレンス: <https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/regex/Pattern.html>
- CLI での詳細表示: `jv --explain JV_REGEX_E201` のようにコードを指定してください。
- LSP では Quick Fix ID をもとに自動修正やガイドを提示します。

## カテゴリ対応表

| カテゴリキー | 想定範囲 | 主な対象診断 |
|--------------|----------|--------------|
| `regex.literal.syntax` | リテラル区切り・終端記号 | `JV_REGEX_E201` など |
| `regex.literal.structure` | 括弧や定数構造 | `JV_REGEX_E202`, `JV_REGEX_E204` |
| `regex.literal.escape` | エスケープと制御文字 | `JV_REGEX_E203` |
| `regex.performance` | 検証コストに関する警告 | `JV_REGEX_I401` |
| `regex.flag` | フラグ指定の誤り | `JV_REGEX_E101`, `JV_REGEX_E103` |
| `regex.mode` | モード選択の誤り・情報 | `JV_REGEX_I001`, `JV_REGEX_I002` |
| `regex.replacement` | 置換ロジックの問題 | `JV_REGEX_E102` |
| `regex.general` | それ以外のコマンド系メタ情報 | 今後拡張予定 |

## 診断一覧

### JV_REGEX_E201 — 正規表現リテラルが未終端です
- **重大度**: Error / **カテゴリ**: `regex.literal.syntax`
- **概要**: 開始スラッシュ `/` に対応する終端スラッシュがありません。
- **推奨修正**: 末尾に `/` を追加するか、必要であれば文字列内で `\/` としてエスケープします。
- **例**:
  ```jv
  // 発生例
  val pattern = /abc

  // 修正後
  val pattern = /abc/
  ```

### JV_REGEX_E202 — 正規表現リテラルの括弧が不均衡です
- **重大度**: Error / **カテゴリ**: `regex.literal.structure`
- **概要**: `()`, `[]`, `{}` などの対応が一致していません。
- **推奨修正**: 対応する括弧を追加するか、余分な閉じ括弧を削除します。
- **例**:
  ```jv
  // 発生例
  val pattern = /\d{3}-[A-Z+/

  // 修正後
  val pattern = /\d{3}-[A-Z]+/
  ```

### JV_REGEX_E203 — サポートされない正規表現エスケープです
- **重大度**: Error / **カテゴリ**: `regex.literal.escape`
- **概要**: `\q` など Java の `Pattern` が理解しないエスケープ、または末尾の単独 `\` を検出しました。
- **推奨修正**: Java 互換のエスケープ（`\\n`, `\\t`, `\\\\` など）へ置換するか、末尾の `\` を削除します。
- **例**:
  ```jv
  // 発生例: 未対応エスケープ
  val pattern = /\qword/

  // 修正後
  val pattern = /\\Qword\\E/
  ```

### JV_REGEX_E204 — 正規表現リテラルに許可されない文字が含まれています
- **重大度**: Error / **カテゴリ**: `regex.literal.structure`
- **概要**: 制御文字や生のタブなど、リテラルとして扱えない文字が含まれています。
- **推奨修正**: 対象文字を削除するか `\t` などのエスケープに置き換えます。
- **例**:
  ```jv
  // 発生例 (TAB 文字を直接含む)
  val pattern = /foo	bar/

  // 修正後
  val pattern = /foo\tbar/
  ```

### JV_REGEX_W301 — Java 互換性が不確かな正規表現です
- **重大度**: Warning / **カテゴリ**: `regex.literal.structure`
- **概要**: Kotlin/JV 拡張構文など、Java への変換後に意味が変わる恐れがあるときに発生します。
- **推奨対応**: Java 側で `Pattern.compile` を実行し構文互換性を確認するか、保守的な書き方へリライトします。

### JV_REGEX_I401 — 正規表現検証が遅延しています
- **重大度**: Information / **カテゴリ**: `regex.performance`
- **概要**: 静的検証が 10ms を超えると情報診断として報告されます。
- **推奨対応**: パターンを分割する、量指定子を制限するなどして複雑性を下げてください。

### JV_REGEX_W001 — Optional 左辺には null ガードが必要です
- **重大度**: Warning / **カテゴリ**: なし（オプショナル評価時の一般注意）
- **概要**: `subject? is /pat/` といった Optional 型の判定ではコンパイラが暗黙の null チェックを挿入します。
- **推奨対応**: `subject != null && subject is /pat/` のように意図を明示すると読みやすさが向上します。

### JV_REGEX_E002 — `is /pattern/` 左辺の型が互換性を満たしません
- **重大度**: Error / **カテゴリ**: なし（型システム）
- **概要**: 左辺が `CharSequence` 互換型ではありません。
- **推奨修正**: `subject.toString()` で文字列化するか、`CharSequence` を実装する型を利用してください。
- **例**:
  ```jv
  struct Point(x: Int y: Int)

  val point = Point(1 2)
  val result = point is /\d+/      // ❌ JV_REGEX_E002
  val fixed  = point.toString() is /\d+/ // ✅
  ```

### JV_REGEX_E003 — `is` 右辺は Pattern 型でなければなりません
- **重大度**: Error
- **概要**: 右辺が `java.util.regex.Pattern` にならない式です。
- **推奨修正**: 右辺を `/.../` リテラルまたは `Pattern.compile(...)` の戻り値に置き換えます。

### JV_REGEX_E210 — Pattern 型が期待と一致しません
- **重大度**: Error
- **概要**: 型注釈が `Pattern` を要求している箇所に別型を渡しています。
- **推奨修正**: 宣言側の型注釈を `Pattern` に合わせるか、式を `Pattern` へ変換してください。

### JV_REGEX_E220 — 定数正規表現の静的検証に失敗しました
- **重大度**: Error
- **概要**: コンパイル時に `Pattern.compile` 同等のチェックでエラーが発生しました。
- **推奨修正**: パターンを Java 互換の書き方へ修正するか、動的評価へ切り替えます。
- **例**:
  ```jv
  // 発生例: Java が認識しないエスケープ
  const pattern = /\p{UnicodeInvalid}/ // ❌ JV_REGEX_E220
  ```

### JV_REGEX_I001 — 正規表現リテラルのモードとフラグが混同されています
- **重大度**: Information / **カテゴリ**: `regex.mode`
- **概要**: 短縮モード `m/.../` とフラグ `m` (MULTILINE) が混在しています。
- **Quick Fix**: `regex.mode.normalize` — `[match]` を明示する、または `m` フラグを削除します。
- **例**:
  ```jv
  val matched = m/subject/^\d+$/m   // ❌
  val fixed   = m/subject/^\d+$/    // ɪ-mode のみ
  val alt     = /[match] subject /^\d+$/m
  ```

### JV_REGEX_I002 — 正規表現リテラルのモードがあいまいです
- **重大度**: Information / **カテゴリ**: `regex.mode`
- **概要**: 置換か判定かがコードから読み取れない場合に通知されます。
- **Quick Fix**: `regex.mode.explicit` — `/[match] subject /pattern/` のように明示するか、置換であれば `//` 区画を追加します。

### JV_REGEX_I003 — 正規表現リテラル結果を大規模入力で配列化しています
- **重大度**: Information
- **概要**: `as List` などで大量データを一括展開するとメモリ圧迫が発生する可能性があります。
- **推奨対応**: `Stream` のまま処理するか `limit` を併用してください。

### JV_REGEX_I004 — 小規模な正規表現リテラル結果はリスト化できます
- **重大度**: Information
- **概要**: 少数の結果を繰り返し利用する場合はリスト化が推奨されます。
- **推奨対応**: `as List` で結果を固定し再利用コストを下げます。

### JV_REGEX_I005 — `Stream<MatchResult>` は再利用できません
- **重大度**: Information
- **概要**: `Stream` を複数回走査すると結果が失われるため警告します。
- **推奨対応**: 結果を `as List` で素材化してから再利用してください。

### JV_REGEX_E101 — 正規表現リテラルで未知のフラグが指定されました
- **重大度**: Error / **カテゴリ**: `regex.flag`
- **概要**: `i`, `m`, `s`, `u`, `d`, `x`, `l`, `c` 以外のフラグを使用しました。
- **Quick Fix**: `regex.flag.remove` — 認識できないフラグを削除します。
- **例**:
  ```jv
  val expr = /text/zg        // ❌ JV_REGEX_E101 (z)
  val fixed = /text/g        // ✅
  ```

### JV_REGEX_E102 — 正規表現リテラルのラムダ戻り値が String ではありません
- **重大度**: Error / **カテゴリ**: `regex.replacement`
- **概要**: 置換ラムダが `String` 以外を返しています。
- **Quick Fix**: `regex.replacement.string` — `toString()` などで文字列化してください。
- **例**:
  ```jv
  val rebuilt = a/input/\d+/ { match ->
      match.group(0).length      // ❌ Int を返している
  }

  val fixed = a/input/\d+/ { match ->
      match.group(0).length.toString()
  }
  ```

### JV_REGEX_E103 — 排他的なフラグが同時指定されています
- **重大度**: Error / **カテゴリ**: `regex.flag`
- **概要**: `l` (Literal) と他フラグのように共存できない組み合わせが存在します。
- **Quick Fix**: `regex.flag.resolve` — どちらか片方のみ残します。
- **例**:
  ```jv
  val bad   = /subject/flags=lc/      // ❌
  val fixed = /subject/flags=c/       // ✅
  ```

### JV_REGEX_W003 — 正規表現リテラル置換ラムダが外側の変数を変更しています
- **重大度**: Warning
- **概要**: 置換ラムダ内で外部変数を変更すると並行処理で破綻する恐れがあります。
- **推奨対応**: 局所変数に書き込む、もしくは同期化してください。

### JV_REGEX_S001 — 正規表現リテラルの結果を外部 API へ直接渡しています
- **重大度**: Warning
- **概要**: サニタイズなしで外部システムへ渡すとセキュリティ事故につながる可能性があります。
- **推奨対応**: 出力前にエスケープ・検証処理を挟んでください。

## 補足

- ここに記載の Quick Fix ID は CLI/LSP でフィルタリングに利用できます。
- `RegexCommandIssue` 由来の診断は `regex.flag` / `regex.mode` / `regex.replacement` を利用しており、IDE で分類表示が可能です。
- ドキュメントへの改善提案や追記は `.spec-workflow/specs/regex-java-integration-error-handling/tasks.md` のタスク進捗に従って実施してください。
