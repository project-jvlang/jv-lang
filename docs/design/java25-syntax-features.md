# java25-syntax-features: Java 25 構文機能対応設計書

## 概要

本ドキュメントは Java 25 (2025年9月リリース、LTS) で導入された主要な構文機能と、jv 言語における対応戦略を定義します。Java 25 は18の新機能 (permanent, preview, incubator, experimental) を含む大規模アップデートであり、パターンマッチング、コンストラクタ柔軟化、ソースファイル簡素化など、jv のデザインゴールと高い親和性を持つ機能が含まれています。

## デザインゴール

- Java 25 の新構文を jv から自然に利用可能にする
- preview/incubator 機能については明示的なオプトイン機構を提供
- jv の既存シンタックスシュガーと競合しない形で統合
- コード生成時に適切な Java コンパイラフラグ (`--enable-preview`) を自動管理
- 下位互換性: Java 21/22/23/24 へのフォールバック戦略を保持

## Java 25 主要機能一覧

### 確定機能 (Finalized)

| JEP | 機能名 | 影響範囲 | jv 対応優先度 |
|-----|--------|---------|--------------|
| JEP 506 | Scoped Values | Runtime API | Medium |
| JEP 510 | Key Derivation Functions API | Security API | Low |

### プレビュー機能 (Preview)

| JEP | 機能名 | 影響範囲 | jv 対応優先度 |
|-----|--------|---------|--------------|
| JEP 507 | Primitive Types in Patterns, instanceof, and switch (3rd Preview) | 構文・パターンマッチング | **High** |
| JEP 511 | Module Import Declarations | モジュールシステム | Medium |
| JEP 512 | Compact Source Files and Instance Main Methods | エントリーポイント簡素化 | **High** |
| JEP 513 | Flexible Constructor Bodies | コンストラクタ構文 | **High** |
| JEP 505 | Structured Concurrency (5th Preview) | 並行処理 API | Medium |

### 実験的機能 (Experimental)

| JEP | 機能名 | 影響範囲 | jv 対応優先度 |
|-----|--------|---------|--------------|
| JEP 509 | JFR CPU-Time Profiling | Profiling API | Low |
| JEP 514 | Ahead-of-Time Class Loading & Linking | AOT 最適化 | Low |
| JEP 515 | Preserve JVM Method Profiling Across Runs | AOT 最適化 | Low |

### その他の変更

| JEP | 機能名 | 影響範囲 | jv 対応優先度 |
|-----|--------|---------|--------------|
| JEP 503 | Removal of 32-bit x86 Support | プラットフォーム | N/A |
| - | Compact Object Headers | ヒープ最適化 | N/A |

---

## 優先対応機能詳細

### 1. JEP 507: Primitive Types in Patterns, instanceof, and switch (3rd Preview)

#### 機能概要

プリミティブ型に対するパターンマッチングを `instanceof`、`switch` で利用可能にします。これにより参照型とプリミティブ型の統一的な取り扱いが可能になります。

#### Java 25 構文例

```java
// instanceof with primitive types
int i = 42;
if (i instanceof int) {
    System.out.println("i is an int");
}

// Widening check (safe cast)
if (i instanceof double) {
    // Every int can be cast to double safely
}

// Narrowing check (unsafe cast - returns false)
if (i instanceof byte) {
    // Won't match - int is not safely convertible to byte
}

// Switch with primitive type patterns
int value = 100_000;
switch (value) {
    case short v -> System.out.println("Short: " + v);
    default -> System.out.println("Not in short range: " + value);
}

// Switch with wrapper types (Long, Float, Double, Boolean now allowed)
Double d = 20.0d;
switch (d) {
    case 20.0d -> System.out.println("double is 20");
    default -> System.out.println("wrong number: " + d);
}

// Record pattern with primitives
record Value(int i) {}
Value v = new Value(20);
switch (v) {
    case Value(int i) when i < 20 -> System.out.println("Too low: " + i);
    case Value(int i) -> System.out.println("Perfect: " + i);
}
```

#### jv での対応戦略

**シンタックス**: jv の `when` 式は既に Java の `switch` へトランスパイルされており、追加の構文変更は不要です。

**型推論統合**:
- `jv_checker::inference::compatibility` に `PrimitivePatternCompatibility` を追加
- widening/narrowing の安全性チェックを `CompatibilityChecker` に統合
- プリミティブパターンマッチング時の exhaustiveness チェック強化

**コード生成**:
- `jv_codegen_java` で `when` 式生成時、プリミティブ型パターンを含む場合は Java 25 `switch` の該当構文を出力
- `jv.toml` の `java.target_version = "25"` かつ `java.enable_preview = true` の場合のみ有効化
- フォールバック: Java 21-24 では従来の `instanceof` + キャスト + `switch` の組み合わせで代替

**実装タスク**:
1. `jv_parser` に `when` 式のプリミティブパターン対応追加 (構文解析は既存で対応済み)
2. `jv_checker` で型互換性チェック拡張 (`PrimitivePatternCompatibility`)
3. `jv_codegen_java` で Java 25 構文生成 (`SwitchExpressionGenerator::generate_primitive_pattern`)
4. フィーチャーフラグ `java.enable_preview` の管理機構追加
5. テスト: `jv_codegen_java/tests/primitive_patterns.rs` 新設

---

### 2. JEP 513: Flexible Constructor Bodies

#### 機能概要

派生クラスのコンストラクタで `super()` 呼び出し前にステートメントを実行可能になります。ただし、`this` への参照 (インスタンスメンバーアクセス) は制限されます。

#### Java 25 構文例

```java
// Previously invalid; now allowed
public class DerivedClass extends BaseClass {
    public DerivedClass(String value) {
        // Validation before super() call
        String validated = validateInput(value);
        super(validated);

        // Instance member access is now allowed
        this.initialize();
    }

    private static String validateInput(String value) {
        if (value == null || value.isEmpty()) {
            throw new IllegalArgumentException("Invalid value");
        }
        return value.trim();
    }
}
```

**制限事項**:
- `super()` 前では `this` へのアクセス不可
- static メソッド呼び出しやローカル変数操作のみ許可
- `super()` は最初の「インスタンス生成ポイント」として機能

#### jv での対応戦略

**シンタックス**: jv の `class` 定義で `init` ブロックまたはプライマリコンストラクタを使用する際、検証ロジックを自然に配置可能にします。

**AST 拡張**:
- `jv_ast::ClassDecl` の `constructor` フィールドに `pre_super_stmts: Vec<Statement>` を追加
- パーサーは `super()` 呼び出し前のステートメントを `pre_super_stmts` に格納

**チェッカー検証**:
- `jv_checker::constructor_validator` モジュール新設
- `pre_super_stmts` 内で `this` 参照がないことを静的検証
- インスタンスメンバーへのアクセスを検出した場合はコンパイルエラー

**コード生成**:
- `jv_codegen_java::ConstructorGenerator` が `pre_super_stmts` を `super()` の前に配置
- Java 25 未満ではヘルパーメソッドへ抽出して回避

**実装タスク**:
1. `jv_ast` に `PreSuperStatement` のサポート追加
2. `jv_parser` で `init` ブロック内の検証ロジック解析
3. `jv_checker::constructor_validator` 新設
4. `jv_codegen_java` でコンストラクタ生成ロジック拡張
5. テスト: `jv_codegen_java/tests/flexible_constructors.rs` 新設

---

### 3. JEP 512: Compact Source Files and Instance Main Methods

#### 機能概要

Java プログラムのエントリーポイントを簡素化し、初心者にも優しい記述を可能にします。暗黙的クラス宣言と `void main()` の簡易構文が導入されます。

#### Java 25 構文例

```java
// Traditional approach
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}

// Java 25 compact file
void main() {
    IO.println("Hello, World!");
}

// Convenience methods available
void main() {
    String name = IO.readln("Enter your name: ");
    IO.println("Hello, " + name + "!");
}
```

**新しい IO メソッド**:
- `IO.println(Object)` - `System.out.println` の簡易版
- `IO.readln(String)` - コンソール入力の簡易版

#### jv での対応戦略

**シンタックス**: jv のトップレベル関数はすでにユーティリティクラスへ変換されていますが、エントリーポイント (`main`) については特別扱いを追加します。

**AST 拡張**:
- `jv_ast::FunctionDecl` に `is_main_entry: bool` フラグ追加
- パーサーはトップレベル `fun main()` を検出時にフラグをセット

**コード生成**:
- `is_main_entry == true` かつ Java 25 preview モード時、`void main()` を直接出力
- Java 25 未満では従来の `public static void main(String[] args)` を生成
- `IO.println`/`IO.readln` の使用を検出した場合、Java 25 の `java.io.IO` を利用

**実装タスク**:
1. `jv_parser` でトップレベル `main` 関数の検出ロジック追加
2. `jv_ast` に `is_main_entry` フラグ追加
3. `jv_codegen_java::EntryPointGenerator` 新設
4. `jv_build` で `--enable-preview` フラグ管理
5. テスト: `jv_cli/tests/compact_main.rs` 新設

---

### 4. JEP 511: Module Import Declarations

#### 機能概要

モジュール単位でのインポート宣言を導入し、個別クラスインポートを集約します。

#### Java 25 構文例

```java
// Individual imports
import java.util.List;
import java.util.Map;
import java.util.Set;

// Module import (Java 25)
import module java.base;
```

#### jv での対応戦略

**シンタックス**: jv の `import` 文に `module` キーワードを追加します。

```jv
// jv syntax
import module java.base

// Transpiles to Java 25
import module java.base;
```

**パーサー拡張**:
- `jv_lexer` に `MODULE` キーワード追加
- `jv_parser::ImportDecl` に `is_module_import: bool` フラグ追加

**コード生成**:
- Java 25 モードで `is_module_import == true` の場合、`import module` を出力
- Java 25 未満では `import module` をワイルドカードインポートへ展開

**実装タスク**:
1. `jv_lexer` に `MODULE` キーワード追加
2. `jv_parser` で `import module` 構文解析
3. `jv_codegen_java` でモジュールインポート生成
4. テスト: `jv_codegen_java/tests/module_imports.rs` 新設

---

## 中優先度機能

### JEP 506: Scoped Values (Finalized)

スレッドローカル変数の代替として、仮想スレッド対応のスコープ値を提供します。

**jv での対応**: `jv_stdlib` に `ScopedValue` ラッパーを追加し、`spawn {}` ブロック内で利用可能にします。

### JEP 505: Structured Concurrency (5th Preview)

構造化並行処理を提供する `StructuredTaskScope` API。

**jv での対応**: `spawn {}` ブロックの内部実装で `StructuredTaskScope` を利用し、よりエレガントなエラーハンドリングとキャンセルを実現します。

---

## フィーチャーフラグ管理

### jv.toml 設定

```toml
[java]
target_version = "25"        # Target Java version
enable_preview = true        # Enable preview features
enable_incubator = false     # Enable incubator modules

[java.features]
primitive_patterns = true    # JEP 507
flexible_constructors = true # JEP 513
compact_source = true        # JEP 512
module_imports = true        # JEP 511
```

### コンパイラフラグ生成

`jv_build` は以下のフラグを `javac` に渡します:

```bash
javac --release 25 --enable-preview --source 25 <files>
```

### フォールバック戦略

| Java Version | Fallback Strategy |
|--------------|-------------------|
| 21-24 | プリミティブパターン → 従来の `instanceof` + `switch` |
| 21-24 | Flexible Constructor → static ヘルパーメソッド抽出 |
| 21-24 | Compact Source → 完全な `class` 宣言生成 |
| 21-24 | Module Import → ワイルドカードインポート展開 |

---

## テスト戦略

### ユニットテスト
- `jv_parser/tests/java25_syntax.rs` - 新構文のパース検証
- `jv_checker/tests/java25_validation.rs` - 型チェック・制約検証

### 統合テスト
- `jv_codegen_java/tests/java25_codegen.rs` - Java 25 コード生成の正確性
- `jv_cli/tests/java25_integration.rs` - E2E コンパイル・実行テスト

### ゴールデンテスト
- `jv/tests/fixtures/java25/` - スナップショット比較用の jv ソースと期待される Java 出力

### 互換性テスト
- `jv_build/tests/version_compatibility.rs` - Java 21/22/23/24 へのフォールバック検証

---

## 実装スケジュール

| Phase | 機能 | 期間 | 依存関係 |
|-------|------|------|---------|
| Phase 1 | JEP 507 (Primitive Patterns) | Week 1-2 | 型推論システム完成 |
| Phase 2 | JEP 513 (Flexible Constructors) | Week 3 | AST 拡張完了 |
| Phase 3 | JEP 512 (Compact Source) | Week 4 | コード生成基盤完成 |
| Phase 4 | JEP 511 (Module Imports) | Week 5 | パーサー拡張完了 |
| Phase 5 | フォールバック実装 | Week 6 | 全機能完了 |
| Phase 6 | 統合テスト・ドキュメント | Week 7 | 全フェーズ完了 |

---

## パフォーマンス考慮事項

- **コンパイル時間**: preview 機能の有効化により javac の処理時間が 5-10% 増加する可能性
- **ランタイム影響**: JEP 507/513 は純粋な構文機能でありランタイムオーバーヘッドなし
- **JEP 512**: `IO.println` は内部で `System.out.println` を呼び出すため実質同一
- **キャッシュ戦略**: `jv_build` でプレビュー機能の有効/無効に応じたビルドキャッシュを分離

---

## セキュリティ考慮事項

- **JEP 513**: `super()` 前の検証ロジックが例外をスローする場合、オブジェクト初期化が中断されることを文書化
- **プレビュー機能**: 本番環境での使用には `--enable-preview` の明示的なオプトインが必要であることを警告

---

## 参照

- [JEP 507: Primitive Types in Patterns, instanceof, and switch (3rd Preview)](https://openjdk.org/jeps/507)
- [JEP 513: Flexible Constructor Bodies](https://openjdk.org/jeps/513)
- [JEP 512: Compact Source Files and Instance Main Methods](https://openjdk.org/jeps/512)
- [JEP 511: Module Import Declarations](https://openjdk.org/jeps/511)
- [JEP 506: Scoped Values](https://openjdk.org/jeps/506)
- [JEP 505: Structured Concurrency (5th Preview)](https://openjdk.org/jeps/505)
- [JDK 25 Release Notes](https://jdk.java.net/25/release-notes)
- [docs/design/type-inference-system.md](type-inference-system.md)
- [docs/design/java-aware-type-inference.md](java-aware-type-inference.md)
