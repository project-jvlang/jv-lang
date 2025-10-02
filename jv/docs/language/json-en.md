# JSON Literals & POJO Generation

**English** | [日本語](json.md)

jv supports Kotlin-style JSON literal blocks and preserves the full pipeline experience.

- **JSONC comments**: both `//` and `/* */` comments are preserved and flow from the AST all the way to generated JavaDoc.
- **Inline POJO generation**: literals such as `val payload = { ... }` become Java 25 `record`s, providing type-safe completions like `payload.user.name`.
- **Schema inference**: nested objects, optional fields, and `List<T>` structures are inferred to produce descriptors such as `PayloadSample` / `PayloadUserSample`.
- **Parity with external samples**: inline literals follow the same naming and field ordering rules as `@Sample("payload.json")` declarations.

## Example: Inline JSON → Java

```jv
val payload = {
  "user": {
    "name": "Alice",
    "age": 30
  },
  "tags": ["admin", "core"],
  // Comments remain available for diagnostics (and JavaDoc)
  "status": "active"
}
```

Building the snippet above produces the following Java 25 code:

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

## Tips

- JSON literals happily coexist with whitespace-delimited calls such as `SUM(1 2 3)`.
- Comments, as illustrated above, are emitted verbatim in the generated JavaDoc to aid documentation.
- Type annotations are unnecessary; inference and caching allow identical literals to be reused in O(1).
