# 生文字列リテラル

## 概要

- シングルクォート (`'...'`) と三重シングルクォート (`'''...'''`) で、エスケープ不要の文字列を表現できる。
- `${name}` や `\` をそのまま記述でき、設定ファイル断片や正規表現、Windowsパスの記述が容易になる。
- 既存のダブルクォート文字列と相互運用し、型推論・コード生成・LSP診断の各モジュールと整合するよう実装されている。

## 構文バリエーション

- `val path = 'C:\Users\jv'`
  - バックスラッシュはリテラルとして保持され、Java生成時には `C:\\Users\\jv` のように自動エスケープされる。
- `val regex = '''^\\d{4}-\\d{2}-\\d{2}$'''`
  - 改行や波括弧を含む複雑なパターンをそのまま記述できる。
- `val literal = '${value}'`
  - 生文字列では補間が無効化され、`${value}` という5文字の `String` として扱われる。

## 型判定と推論

- 文字列長が1文字の生文字列は `Char` として推論される。例: `val newline: Char = '\n'`
- 2文字以上の生文字列、または複雑なコードポイントを含む場合は `String` となる。
- null安全やジェネリクスの制約解決は `docs/design/java-aware-type-inference.md` の方針を継承し、暗黙のBoxing/Unboxingや`String.valueOf`変換と連携する。

## Javaコード生成

- 単一行は通常のダブルクォート文字列へ変換され、必要な箇所のみエスケープが挿入される。
- 三重シングルクォートはText Block (`"""`) に変換される。最終行が改行で終わらない場合は `\` を末尾に追加し、JavaのText Block規則を満たす。
- 1文字の生文字列は `char` リテラルとしてレンダリングされ、文脈で `String` が必要な場合は `String.valueOf` が挿入される。
- Java 21/25 の双方で `--release` オプションによる `javac` 検証が成功するよう、生成物は両バージョンに対応している。

## 診断とハイライト

- 字句解析段階で未終端の生文字列を検出し、`UnterminatedRawString` エラーとして位置情報付きで報告する。
- `'''` を閉じ忘れた場合は開始位置を指し示し、LSP診断でも同じエラーコードが利用される。
- LSPのシンタックスハイライトでは `RawSingle` / `RawTriple` を専用の分類として扱い、既存のテーマ設定と共存する。

## 利用例

```jv
val rawChar = 'A'
val rawPath = 'C:\Users\jv'
val query = '''
SELECT *
FROM logs
WHERE level = 'ERROR'
'''

fun describe(input: String): String {
    return rawChar.toString() + ":" + input + query
}
```

生成されるJava 25コード (抜粋):

```java
final char rawChar = 'A';
final String rawPath = "C\\\\Users\\jv";
final String query = """
SELECT *
FROM logs
WHERE level = 'ERROR'
""";
```

`javac --release 21/25` でコンパイル可能であることを `jv/tests/integration/raw_string_literals.rs` の統合テストで確認済み。
