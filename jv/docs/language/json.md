# JSON リテラルとPOJO生成

**日本語** | [English](json-en.md)

jv では Kotlin ライクな JSON リテラルをサポートし、パイプライン全体を通じて以下の特性を提供します。

- **コメント付き JSONC**: `//` と `/* */` の両コメントを保持し、AST から JavaDoc まで伝播。
- **インライン POJO 自動生成**: `val payload = { ... }` のようなリテラルを Java 25 の `record` として生成。`payload.user.name` のアクセスは型安全に補完されます。
- **スキーマ推論**: ネスト構造、Optional、`List<T>` を推論して `PayloadSample` / `PayloadUserSample` などの記述子を作成。
- **外部サンプルとの整合**: `@Sample("payload.json")` で得られる POJO と同じ名前付け・フィールド整列を採用。

## 例: インライン JSON → Java

```jv
val payload = {
  "user": {
    "name": "Alice",
    "age": 30
  },
  "tags": ["admin", "core"],
  // ランタイムには展開されないが、診断では保持される(コメントも書けます)
  "status": "active"
}
```

上記をビルドすると、以下の Java 25 コードが生成されます。

```java
public record PayloadSample(java.util.List<String> tags, PayloadUserSample user, String status) {}

public record PayloadUserSample(int age, String name) {}

public class PayloadSampleData {
    private static final String RAW_JSON = "{...}";
    public static final PayloadSample PAYLOAD = new PayloadSample(
        java.util.List.of("admin", "core"),
        new PayloadUserSample(30, "Alice"),
        "active"
    );
}
```

## ヒント

- JSON リテラルは `SUM(1 2 3)` のような空白区切り引数とも共存できます。
- コメントは本ガイドのサンプルのように、POJO 生成後の JavaDoc に転写されます。
- 型注釈は不要です。推論とキャッシュにより、同一リテラルは O(1) で再利用されます。
