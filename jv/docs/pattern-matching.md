# パターンマッチングガイド

**日本語** | [English](pattern-matching-en.md)

jv の `when` 式は、Java 25 のパターンマッチングと Java 21 のフォールバック生成を両立するゼロランタイム構文です。本ガイドでは、サポートされるパターン構文、網羅性診断、コード生成の仕組み、CLI/LSP/テレメトリとの連携を紹介します。

## 目次

1. [when式の基本構造](#when式の基本構造)
2. [サポートされるパターン](#サポートされるパターン)
3. [網羅性と診断コード](#網羅性と診断コード)
4. [Null Safety とスマートキャスト](#null-safety-とスマートキャスト)
5. [Javaコード生成戦略](#javaコード生成戦略)
6. [ツール連携とテレメトリ](#ツール連携とテレメトリ)
7. [関連資料](#関連資料)

## when式の基本構造

`when` 式は Java の `switch` を拡張した構文で、条件分岐と値の返却を統一します。式として値を返すほか、副作用のみの利用も可能です。

```jv
val result = when (value) {
    0 -> "zero"
    in 1..9 -> "small"
    is Int && value > 100 -> "large"
    else -> "unknown"
}
```

- `when (<subject>) { pattern -> expression }` 形式を基本とし、`when { ... }` のように主題を省略してガード式のみで分岐することもできます。
- パターンは複数の `,` 区切りで並べられ、最初に一致した分岐が実行されます。
- 各分岐内で宣言されたバインディングは、対応するブロック内で型安全に利用できます。

副作用のみが目的の場合は、戻り値を受け取らずに分岐ごとの処理を記述できます。下記の例では監査ログを更新するだけで値を返しません。

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

`when` が式であることに変わりはないため、必要なら `val _ = when (...) { ... }` のように結果を明示的に破棄できますが、多くの場合は上記のように単独で記述すれば十分です。

## サポートされるパターン

### リテラルパターン

文字列、数値、列挙値などのリテラル一致を行います。

```jv
when (status) {
    "OK" -> println("success")
    "ERROR" -> println("retry")
    else -> println("unknown")
}
```

### 型パターン

`is Type` 形式で型を判定し、`val` または `var` でスマートキャストされたバインディングを宣言できます。

```jv
when (payload) {
    is User -> println("Hello, ${payload.name}")
    is Error(val code, val message) -> println("[$code] $message")
    else -> println("unsupported")
}
```

### 分解パターン

レコード/データクラスを分解し、ネストした構造を 2 段まで取り出せます。

```jv
when (event) {
    is UserEvent.Login(val user, val device) -> logAudit(user.id, device.ip)
    is UserEvent.Logout -> cleanup()
    else -> notifyOps(event)
}
```

### 範囲パターン

`in start..end`（両端含む）または `in start..<end`（上限除外）で範囲マッチングを記述します。

```jv
when (score) {
    in 0..49 -> Grade.FAIL
    in 50..79 -> Grade.PASS
    in 80..100 -> Grade.DISTINCTION
    else -> Grade.INVALID
}
```

### ガード付きパターン

パターンの後ろに `&&` で条件を追加し、分岐をさらに絞り込みます。ガードは同じ分岐内のバインディングを参照できます。

```jv
when (order) {
    is Order(val total) && total > 10_000 -> flagHighValue(order)
    is Order && order.items.isEmpty() -> flagEmpty(order)
    else -> accept(order)
}
```

### 主題なし when

パターンだけで分岐する DSL 風構文です。条件が真になった最初のブロックが評価されます。

```jv
when {
    user.isAnonymous -> promptLogin()
    user.role == Role.Admin -> showAdminDashboard()
    else -> showHome()
}
```

## 網羅性と診断コード

`when` 式は解析時に網羅性、矛盾、フォールバック可否を検査します。主な診断コードは以下の通りです。

| コード | 概要 | 対応策 |
|--------|------|--------|
| `JV3100` | sealed/enum の網羅性が不足 | `MissingCase` 提案を適用し不足分岐を追加 |
| `JV3101` | 範囲が重複または空集合になる | 範囲を見直し、重複を解消 |
| `JV3103` | `if` 構文を検出 | Quick Fix で `when` へ変換 |
| `JV3105` | Java 21 フォールバック不可の構文 | ガードや深度を調整、またはターゲットを Java 25 にする |
| `JV3199` | 未サポートの深度/構文 | メッセージ内のサポート予定を確認し回避 |

LSP/CLI では日英バイリンガルなメッセージと Quick Fix を表示します。`jv check --telemetry` を利用すると診断発生回数と Quick Fix 適用率を確認できます。

## Null Safety とスマートキャスト

`when` 分岐で得られた型情報は Null Safety ブリッジに連携され、同じスコープ内の後続コードに型が伝播します。

```jv
when (payload) {
    is User(val email?) -> sendSummary(email)
    else -> logWarn("missing email")
}

// payload が User だった場合のみ email は非nullとして扱われる
```

- `val email?` のような nullable バインディングは、ガードや後続式で安全に利用できます。
- `pattern_cache_hits` / `pattern_cache_misses` / `pattern_bridge_ms` のメトリクスは `NullSafety` と共有され、SLA 超過時は `JV3110` を発行します。

## Javaコード生成戦略

`when` 式はターゲット JDK に応じて異なるコードを生成します。以下はレコードとガードを含む例です。

```jv
val outcome = when (message) {
    is Message.Error(val code, val detail) && code >= 500 -> "retry"
    is Message.Error -> "alert"
    is Message.Info -> "ok"
    else -> "ignore"
}
```

### Java 25 の出力

```java
final String outcome = switch (message) {
    case Message.Error(var code, var detail) when code >= 500 -> "retry";
    case Message.Error(var code, var detail) -> "alert";
    case Message.Info(var info) -> "ok";
    default -> "ignore";
};
```

- `when` ガードは `case ... when` に変換され、分解パターンはレコードパターンとして出力されます。
- `strategy_description` コメントが付与され、最適化方針を可読化します。

### Java 21 フォールバック

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

- Java 21 では `instanceof` + 分解バインディングを組み合わせた if チェーンを生成します。
- 深度 3 以上のパターンや guard の一部は Java 21 でサポートされないため、`JV3105` 診断を通じてガイドします。

## 代表的なケースと Quick Fix フロー

### Example 1 — 型スイッチとフォールバック

```jv
val length = when (value) {
    is String -> value.length
    is Int -> value * 2
    else -> 0
}
```

- **Java 25:** `switch` + レコードパターン。`strategy=Switch arms=3 guards=0 default=true` がコメント出力されます。
- **Java 21:** `(() -> { ... })()` ブロック内で `instanceof` チェーンを生成し、分岐ごとに `return` します。
- **Quick Fix:** 未網羅時は `MissingCase` 提案が日英ラベル付きで提示されます。

### Example 2 — 範囲とハイブリッド戦略

```jv
val label = when (score) {
    in 0..<60 -> "Fail"
    in 60..<80 -> "Pass"
    in 80..100 -> "Excellent"
    else -> "Invalid"
}
```

- Java 25 は `case int i when ...` を生成。
- Java 21 は範囲比較を AND 連結した if チェーンへ展開し、テレメトリに `strategy=Hybrid arms=4 guards=3` が記録されます。
- NEG フィクスチャ `NEG-005` は範囲重複時の `JV3101` を検証します。

### Example 3 — 深度 2 の分解

```jv
val summary = when (event) {
    is Audit(val actor, val payload: Audit.Payload(val id, val amount)) && amount > 10_000 -> "flag"
    is Audit -> "ok"
    else -> "skip"
}
```

- 深度 2 までサポート（最大深度 10）。深度 11 以上で `JV3199` が発火し `--explain JV3199` が誘導されます。
- Java 21 ではネストした `instanceof` と一時変数を組み合わせて展開されます。
- NEG フィクスチャ `NEG-013`〜`NEG-015` が閾値超過時の診断文言を固定化します。

### Quick Fix ステップ

1. `jv check` または LSP で診断発生（例: `JV3103 if expressions are not supported; use when instead`）。
2. Quick Fix 一覧に日英メッセージとテンプレートコードが表示される。
3. 適用すると `if` ブロックが `when` へ置換され、必要な `else` 分岐が自動補完される。
4. 適用後に `jv check --telemetry` を実行すると `when_strategies` と `pattern_cache_hits` の値が更新され、変換結果の挙動を追跡できる。

## ツール連携とテレメトリ

- `jv check` / `jv lsp` は `pattern_cache_hits`, `pattern_cache_misses`, `pattern_bridge_ms`, `when_strategy` を計測し、`--telemetry` で表示します。
- Quick Fix パネルでは `if` → `when` の自動変換、`MissingCase` の自動挿入、フォールバック指針へのリンクを提示します。
- ゴールデンフィクスチャ（`tests/fixtures/pattern`）は Java 25 / 21 のそれぞれの出力を `.expected_java25` / `.expected_java21` に固定し、回帰を防ぎます。

## 検証ガイド

パターンマッチング実装の健康状態は、定期的な計測とフィクスチャ検証で維持します。以下のファイルで最新の計測結果を確認し、新しい最適化や性能レポート更新時に既存の予算（例: 1000 when ≤50ms）が守られているかをチェックしてください。

- [benchmarks/pattern-switch/history.md](../../benchmarks/pattern-switch/history.md)

また、ドキュメントに掲載した代表的な `when` ケースが最新のコード生成と一致しているかどうかは、以下のコマンドで検証します。このテストは Java 25 / 21 のゴールデン出力と診断を比較し、差分があれば原因調査を促します。ベンチマーク履歴を更新したタイミングや仕様変更後など、節目ごとに実行して結果を記録してください。

```bash
cargo test -p jv_cli -- --test-mode fixture --fixture-dir jv/crates/jv_cli/tests/fixtures/pattern
```

## 注意事項

上記の検証コマンドとドキュメント内のコード例は `jv/crates/jv_docs/tests/validate_examples.rs` で自動照合されます。コードスニペットを更新する場合は必ずテストも実行し、差分があればテスト側の期待値を同期してください。
