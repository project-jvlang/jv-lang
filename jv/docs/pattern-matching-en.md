# Pattern Matching Guide

**English** | [日本語](pattern-matching.md)

The `when` expression in jv offers zero-runtime pattern matching that targets Java 25 natively while providing Java 21 fallbacks. This guide covers the supported pattern syntax, exhaustiveness diagnostics, code generation strategies, and how the tooling surface (CLI/LSP/telemetry) keeps everything aligned.

## Table of Contents

1. [Structure of when Expressions](#structure-of-when-expressions)
2. [Supported Pattern Forms](#supported-pattern-forms)
3. [Exhaustiveness and Diagnostics](#exhaustiveness-and-diagnostics)
4. [Null Safety and Smart Casts](#null-safety-and-smart-casts)
5. [Java Code Generation Strategies](#java-code-generation-strategies)
6. [Tooling Integration and Telemetry](#tooling-integration-and-telemetry)
7. [Further Reading](#further-reading)

## Structure of when Expressions

`when` unifies conditional branching and value production. It can return values or be used for side effects only.

```jv
val result = when (value) {
    0 -> "zero"
    in 1..9 -> "small"
    is Int && value > 100 -> "large"
    else -> "unknown"
}
```

- The canonical form is `when (<subject>) { pattern -> expression }`, but you may omit the subject (`when { ... }`) to express guard-only DSL flows.
- Patterns can be comma-separated; the first matching branch is evaluated.
- Bindings introduced inside a branch are type-safe within that branch.

When you care only about side effects, just execute statements inside each arm without capturing a result. The following snippet mutates counters and prints diagnostics without returning a value.

```jv
data class Login(val username: String)
data class Logout(val username: String)

var auditBalance = 0
val event = Login("alice")

when (event) {
    is Login -> auditBalance += 1
    is Logout -> auditBalance -= 1
    else -> println("ignored: ${event}")
}
```

`when` is still an expression, so you can explicitly discard its value via `val _ = when (...) { ... }` if you prefer, but in most cases a bare `when` is concise and clear for side-effecting logic.

## Supported Pattern Forms

### Literal Patterns

Match on concrete values such as strings, numbers, or enum constants.

```jv
when (status) {
    "OK" -> println("success")
    "ERROR" -> println("retry")
    else -> println("unknown")
}
```

### Type Patterns

Use `is Type` along with `val` / `var` bindings to smart-cast.

```jv
when (payload) {
    is User -> println("Hello, ${payload.name}")
    is Error(val code, val message) -> println("[$code] $message")
    else -> println("unsupported")
}
```

### Destructuring Patterns

Deconstruct records/data classes and expose nested bindings (up to depth 2).

```jv
when (event) {
    is UserEvent.Login(val user, val device) -> logAudit(user.id, device.ip)
    is UserEvent.Logout -> cleanup()
    else -> notifyOps(event)
}
```

### Range Patterns

Express inclusive or exclusive intervals with `in start..end` or `in start..<end`.

```jv
when (score) {
    in 0..49 -> Grade.FAIL
    in 50..79 -> Grade.PASS
    in 80..100 -> Grade.DISTINCTION
    else -> Grade.INVALID
}
```

### Guarded Patterns

Append `&&` conditions to refine matches. Guards can reference bindings from the same branch.

```jv
when (order) {
    is Order(val total) && total > 10_000 -> flagHighValue(order)
    is Order && order.items.isEmpty() -> flagEmpty(order)
    else -> accept(order)
}
```

### Subjectless when

A DSL-like form that evaluates the first branch whose guard is `true`.

```jv
when {
    user.isAnonymous -> promptLogin()
    user.role == Role.Admin -> showAdminDashboard()
    else -> showHome()
}
```

## Exhaustiveness and Diagnostics

The compiler analyses every `when` for coverage, conflicts, and Java 21 compatibility. Key diagnostic codes include:

| Code | Meaning | Suggested Action |
|------|---------|------------------|
| `JV3100` | Missing cases for sealed/enum hierarchies | Apply the `MissingCase` suggestion to add the required branches |
| `JV3101` | Overlapping or empty ranges | Adjust the ranges to remove overlaps |
| `JV3103` | Raw `if` expression detected | Use the Quick Fix to rewrite it as `when` |
| `JV3105` | Construct cannot be lowered to Java 21 | Simplify guards/depth or target Java 25 |
| `JV3199` | Unsupported depth or syntax | Follow the roadmap in the diagnostic message |

The CLI and LSP display bilingual text and Quick Fix payloads. Run `jv check --telemetry` to inspect diagnostic counts and Quick Fix adoption.

## Null Safety and Smart Casts

Pattern matches feed directly into the Null Safety bridge, so downstream code receives refined types.

```jv
when (payload) {
    is User(val email?) -> sendSummary(email)
    else -> logWarn("missing email")
}

// `email` is treated as non-null only inside the matching branch.
```

- Nullable bindings such as `val email?` remain safe in guards and subsequent expressions.
- Telemetry fields `pattern_cache_hits`, `pattern_cache_misses`, and `pattern_bridge_ms` are shared with the Null Safety subsystem; SLA breaches raise `JV3110`.

## Java Code Generation Strategies

Output differs per target JDK. The following example uses record patterns and guards.

```jv
val outcome = when (message) {
    is Message.Error(val code, val detail) && code >= 500 -> "retry"
    is Message.Error -> "alert"
    is Message.Info -> "ok"
    else -> "ignore"
}
```

### Java 25 Output

```java
final String outcome = switch (message) {
    case Message.Error(var code, var detail) when code >= 500 -> "retry";
    case Message.Error(var code, var detail) -> "alert";
    case Message.Info(var info) -> "ok";
    default -> "ignore";
};
```

- Guards become `case ... when` clauses, and destructuring maps to record patterns.
- A `strategy_description` comment annotates the switch to document the lowering plan.

### Java 21 Fallback

```java
final String outcome = (() -> {
    if (message instanceof Message.Error error && error.code() >= 500) {
        return "retry";
    }
    if (message instanceof Message.Error error) {
        return "alert";
    }
    if (message instanceof Message.Info info) {
        return "ok";
    }
    return "ignore";
})();
```

- Java 21 relies on `instanceof` chains with pattern bindings.
- Deeply nested patterns (depth ≥ 3) or certain guards cannot be expressed; `JV3105` guides remediation.

## Tooling Integration and Telemetry

- `jv check` / `jv lsp` surface `pattern_cache_hits`, `pattern_cache_misses`, `pattern_bridge_ms`, and `when_strategy` via the `--telemetry` flag.
- Quick Fix panels offer automatic rewrites (`if` → `when`), `MissingCase` inserts, and guidance for fallback limitations.
- Golden fixtures in `jv/crates/jv_cli/tests/fixtures/pattern` lock both Java 25 and Java 21 renderings (`.expected_java25` / `.expected_java21`) to prevent regressions.

## Verification Guide

The health of the pattern-matching pipeline is sustained through regular measurement and fixture verification. Review the latest performance budget checks using the file listed below whenever you prototype optimizations or publish a performance report, ensuring targets such as “1000 `when` cases within 50 ms” remain satisfied.

- [benchmarks/pattern-switch/history.md](../../benchmarks/pattern-switch/history.md)

To keep the documented scenarios aligned with the generator output, run the command shown below. It compares the Java 25 / Java 21 golden outputs and diagnostics, surfacing any drift. Trigger this verification at major milestones—spec updates, benchmark refreshes, or tooling releases—and record the findings in your engineering notes.

```bash
cargo test -p jv_cli -- --test-mode fixture --fixture-dir jv/crates/jv_cli/tests/fixtures/pattern
```

## Notes

The verification command and all embedded code examples are cross-checked by `jv/crates/jv_docs/tests/validate_examples.rs`. Whenever you change a snippet, rerun the test suite and update the expected snippet in the test file if it no longer matches.
