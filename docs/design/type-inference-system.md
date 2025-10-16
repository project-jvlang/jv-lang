# type-inference-system: jv言語の型推論システム設計書

## 概要

jvの型推論システムは、Kotlin風の型アノテーション省略記法を提供しつつ、Java 25の静的型システムとの完全な互換性を保証します。本ドキュメントでは、制約ベース型推論アルゴリズムの実装方針と各コンポーネントの役割を整理します。

## アーキテクチャ概要

型推論システムは3層構造で実装されています:

```
AST (jv_ast)
  ↓
制約生成 (jv_checker/inference/constraint)
  ↓
制約解決 (jv_checker/inference/unify)
  ↓
型環境・スキーマ (jv_checker/inference/environment)
```

### コアクレート

- **jv_ast**: 型アノテーション構造とASTノード定義
- **jv_checker/inference**: 型推論エンジンのコア実装
- **jv_ir**: 脱糖後の中間表現（型情報を保持）
- **jv_codegen_java**: Java 25コード生成時の型具体化

## 型推論アルゴリズム

### 基本方針

jvの型推論は **Hindley-Milner型システム** を基礎とし、以下の拡張を施しています:

1. **制約ベース推論**: 型変数と型制約の集合を生成し、単一化アルゴリズムで解決
2. **Null安全統合**: Optional型を制約に組み込み、Null安全演算子との整合性を保証
3. **関数シグネチャ一般化**: 自由型変数を量化し、多相的な関数型を導出
4. **曖昧性検出**: 解決後にUnknown型が残る場合はエラーとして報告

### 推論フロー

```
┌─────────────────────────────────────────────────────────┐
│ 1. ConstraintGenerator: ASTから制約集合を生成          │
│    - 変数宣言: 初期化式と型アノテーションを制約化      │
│    - 式評価: 演算子・関数呼び出しから型関係を抽出      │
│    - パターンマッチ: when式のnarrowing情報を統合        │
└─────────────────────────────────────────────────────────┘
                         ↓
┌─────────────────────────────────────────────────────────┐
│ 2. ConstraintSolver: Robinson単一化で型変数を解決      │
│    - 等価制約: Equal(t1, t2) を単一化                  │
│    - 代入制約: Assign(id, ty) で型変数を束縛           │
│    - Optional型: 内包型を再帰的に単一化して伝播        │
│    - Occurs Check: 無限型の検出と防止                  │
└─────────────────────────────────────────────────────────┘
                         ↓
┌─────────────────────────────────────────────────────────┐
│ 3. InferenceEngine: 関数シグネチャの一般化と検証       │
│    - 代入マップ構築: TypeId → TypeKind                 │
│    - 自由型変数抽出: 量化すべき型変数を収集            │
│    - TypeScheme生成: 多相型として環境に登録            │
│    - 曖昧性検出: Unknown残存時はエラー報告             │
└─────────────────────────────────────────────────────────┘
```

## コンポーネント詳細

### 1. 型表現 (`types.rs`)

#### TypeKind

推論中に扱う主な型表現:

```rust
pub enum TypeKind {
    Primitive(&'static str),           // Int, String, Boolean等
    Reference(String),                 // ユーザー定義型
    Optional(Box<TypeKind>),           // Null許容型 (T?)
    Variable(TypeId),                  // 型変数 (推論中)
    Function(Vec<TypeKind>, Box<TypeKind>), // 関数型
    Unknown,                           // 未決定型
}
```

**設計原則**:
- `Primitive`: Java標準型へ直接マッピング
- `Optional`: Null安全機構との統合点
- `Variable`: 推論中の型プレースホルダ
- `Unknown`: 曖昧性検出のマーカー

#### TypeBinding

型変数と具体型の束縛を表現:

```rust
pub struct TypeBinding {
    pub variable: TypeVariable,
    pub ty: TypeKind,
}
```

### 2. 制約生成 (`constraint/generator.rs`)

#### ConstraintGenerator

AST走査時に制約を生成するビジター実装。スコープ管理は`TypeEnvironment`に委譲し、以下の制約を収集:

##### 主要な制約生成パターン

| AST構造 | 生成制約 | 例 |
|---------|----------|-----|
| `val x = 42` | `Equal(x_var, Int)` | 初期化式の型を変数に伝播 |
| `val y: String = "hi"` | `Equal(y_var, String)`<br>`Equal(y_var, String)` | アノテーション+初期化の両方を検証 |
| `x + y` | `Equal(x_ty, y_ty)` | 算術演算子のオペランド型一致 |
| `f(arg)` | `Equal(f_ty, (arg_ty) -> result)` | 関数呼び出しと引数型の整合 |
| `x ?: default` | `Equal(x_ty, Optional(T))`<br>`Equal(T, default_ty)` | Elvis演算子とOptional展開 |

##### 特殊ケース

- **when式**: PatternMatchServiceと連携してnarrowingを適用
  ```rust
  when (maybe) {
      is String -> maybe  // maybeはString型にnarrow
      else -> ""
  }
  ```

- **拡張関数**: ExtensionRegistryで候補を検索し、曖昧性を検出
  ```rust
  "hello".length  // String型の拡張メソッド解決
  ```

- **ループ分類**: LoopClassificationで数値範囲/Iterableを判定
  ```rust
  for (i in 0..10)      // NumericRange: iはInt型
  for (x in list)       // Iterable: xは要素型
  ```

### 3. 制約解決 (`unify.rs`)

#### ConstraintSolver

Robinson単一化アルゴリズムを実装し、制約集合を解決:

```rust
pub struct ConstraintSolver {
    substitutions: HashMap<TypeId, TypeKind>,
    parallel_config: ParallelInferenceConfig,
}
```

##### 単一化ルール

| 左辺 | 右辺 | 処理 | 結果 |
|------|------|------|------|
| `Variable(a)` | `Variable(b)` | a == b なら成功 | `Variable(a)` |
| `Variable(a)` | `T` | Occurs Check後に束縛 | `T` |
| `Primitive(p1)` | `Primitive(p2)` | 名前一致確認 | `Primitive(p1)` |
| `Optional(T1)` | `Optional(T2)` | 内包型を再帰単一化 | `Optional(unified)` |
| `Optional(T)` | `U` | TとUを単一化 | `Optional(unified)` |
| `Function(P1, R1)` | `Function(P2, R2)` | パラメータと戻り値を個別に単一化 | `Function(...)` |
| `Unknown` | `T` | Tを優先 | `T` |

##### エラー条件

- **TypeMismatch**: 互換性のない型 (例: `Int` vs `String`)
- **OccursCheck**: 無限型の検出 (例: `t = Optional(t)`)
- **Placeholder**: 未実装制約への遭遇

### 4. 型環境 (`environment.rs`)

#### TypeEnvironment

スコープごとに変数→型スキーマのマッピングを管理:

```rust
pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, TypeScheme>>,
    type_id_counter: u32,
}
```

**主要操作**:
- `enter_scope() / leave_scope()`: ネストスコープ管理
- `define_scheme(name, scheme)`: 変数束縛の登録
- `lookup(name) -> Option<TypeScheme>`: スコープチェインでの探索
- `fresh_type_variable() -> TypeKind`: 新規型変数の生成
- `generalize(ty) -> TypeScheme`: 自由型変数の量化
- `instantiate(scheme) -> TypeKind`: 量化された型の具体化

#### TypeScheme

多相型を表現 (Hindley-Milner型推論の型スキーマ):

```rust
pub struct TypeScheme {
    pub quantifiers: Vec<TypeId>,  // 量化された型変数
    pub ty: TypeKind,              // 型本体
}
```

**例**:
```jv
fun identity(x) = x
// TypeScheme: ∀t. (t) -> t
// quantifiers: [t0]
// ty: Function([Variable(t0)], Variable(t0))
```

### 5. 推論エンジン (`engine.rs`)

#### InferenceEngine

制約生成・解決を統合し、型推論全体を調整:

```rust
pub struct InferenceEngine {
    environment: TypeEnvironment,
    bindings: Vec<TypeBinding>,
    function_schemes: HashMap<String, TypeScheme>,
    telemetry: InferenceTelemetry,
    // ...
}
```

##### 推論実行フロー

```rust
pub fn infer_program(&mut self, program: &Program) -> InferenceResult<()> {
    // 1. Preludeのインストール (標準型・拡張関数)
    let extensions = prelude::install_prelude(&mut environment);

    // 2. 制約生成
    let generator = ConstraintGenerator::new(&mut environment, &extensions, imports);
    let constraints = generator.generate(program);

    // 3. 制約解決
    let solve_result = ConstraintSolver::with_config(parallel_config).solve(constraints)?;
    let substitutions = build_substitution_map(&solve_result.bindings);

    // 4. 関数シグネチャの一般化
    for name in collect_function_names(program) {
        let scheme = environment.lookup(&name)?;
        let resolved = resolve_type(&scheme.ty, &substitutions);

        // 曖昧性検出
        if resolved.contains_unknown() {
            return Err(InferenceError::AmbiguousFunction { name });
        }

        // 自由型変数を量化
        let quantifiers = resolved.free_type_vars();
        let final_scheme = TypeScheme::new(quantifiers, resolved);
        function_schemes.insert(name, final_scheme);
    }

    // 5. テレメトリ記録
    self.telemetry = InferenceTelemetry { constraints_emitted, bindings_resolved, ... };
    Ok(())
}
```

## 拡張機能との統合

### 1. Null安全との連携

- `Optional(T)` 型を制約に直接組み込み
- `?.` / `?:` 演算子を制約生成時に展開
- when式のnarrowingで `Optional(T)` → `T` への型精緻化

### 2. パターンマッチング統合

- `PatternMatchService` がナロー情報を提供
- ブランチごとにスコープを分岐し、narrowed bindingを適用
- when式の全ブランチ型を統一して結果型を決定

### 3. 拡張関数解決

- `ExtensionRegistry` でレシーバ型→メソッド名のマッピングを管理
- 型変数がレシーバの場合は候補を列挙し、使用頻度で曖昧性判定
- 解決後にレシーバ型への制約を追加

### 4. ループ分類

- `LoopClassification` で数値範囲・Iterable・LazySequenceを判別
- 数値範囲: 開始・終了の型一致制約
- Iterable: `expression_can_yield_iterable` で妥当性検証

### 5. Import解決

- `ImportRegistry` が外部シンボルの型情報を提供
- `SymbolIndex` から型スキーマを復元
- 識別子解決時にローカル環境→インポート順で探索

## パフォーマンス最適化

### 並列推論設定

```rust
pub struct ParallelInferenceConfig {
    pub enabled: bool,
    pub constraint_batching: usize,
    pub thread_count: usize,
}
```

- `constraint_batching`: 制約をバッチ処理してキャッシュ効率を向上
- 型変数の使用頻度を記録し、拡張関数解決の優先度判定に利用

### テレメトリ

```rust
pub struct InferenceTelemetry {
    pub constraints_emitted: usize,
    pub bindings_resolved: usize,
    pub inference_duration_ms: f64,
}
```

推論性能の計測とボトルネック特定に活用。

## エラー処理

### InferenceError

```rust
pub enum InferenceError {
    SolveFailure(SolveError),
    AmbiguousFunction { name: String },
}
```

- `SolveFailure`: 制約解決失敗 (型不一致・Occurs Check等)
- `AmbiguousFunction`: 関数シグネチャにUnknown型が残存

### SolveError

```rust
pub enum SolveError {
    Placeholder { placeholder: &'static str, note: Option<String> },
    TypeMismatch { left: TypeKind, right: TypeKind, note: Option<String> },
    OccursCheck { id: TypeId, ty: TypeKind, note: Option<String> },
}
```

各エラーに補足情報(`note`)を付与し、診断メッセージの品質を向上。

## Java 25との対応

### 型マッピング

| jv型 | Java 25型 | 備考 |
|------|-----------|------|
| `Int` | `int` / `Integer` | プリミティブ優先、null許容時はボクシング |
| `String` | `java.lang.String` | 常に参照型 |
| `Boolean` | `boolean` / `Boolean` | プリミティブ優先 |
| `Double` | `double` / `Double` | 浮動小数点数 |
| `T?` | `@Nullable T` (参照型) | Null許容アノテーション付与 |
| `(A) -> B` | `Function<A, B>` | java.util.function系への変換 |

### コード生成時の型具体化

- `jv_codegen_java` は `InferenceEngine` の結果を利用
- `TypeKind` → Javaソースコード型表記への変換
- Raw型警告と防御コード挿入 ([raw-type-defensive-strategy.md](raw-type-defensive-strategy.md)参照)

## テスト戦略

### 単体テスト (`jv_checker/src/inference/*/tests.rs`)

- **制約生成**: リテラル・二項演算・when式の制約正当性
- **単一化**: Variable-Primitive、Optional伝播、Occurs Check検出
- **エンジン**: 関数一般化、デフォルト値特殊化、曖昧性報告

### 統合テスト (`jv_checker/tests/inference.rs`)

- 実際の `.jv` コード片をパースして推論実行
- 期待される型スキーマとの比較
- エラーケースの網羅的検証

### ゴールデンテスト (`jv_codegen_java/tests/generics.rs`)

- 推論結果を含むJavaコード生成のスナップショット比較
- 型アノテーション省略コードが正しいJavaに変換されることを保証

## 今後の拡張計画

### フェーズ2: ジェネリクス完全対応

- `TypeKind::Generic` の導入
- 型引数の制約生成と変位チェック
- where句述語の制約統合

### フェーズ3: 高度な型推論

- 型クラス制約 (Capability要求)
- 高階カインド推論
- 依存型的な const パラメータ解決

### フェーズ4: インクリメンタル推論

- モジュール境界での型情報キャッシュ
- 変更影響範囲の限定再推論
- LSPとの統合によるリアルタイム型情報提供

## まとめ

jvの型推論システムは、制約ベース推論とRobinson単一化を基盤とし、Null安全・パターンマッチング・拡張関数といったモダンな言語機能と統合されています。InferenceEngineを中心としたモジュール構成により、段階的な機能拡張と保守性を両立しています。

## 参照

- [raw-type-defensive-strategy.md](raw-type-defensive-strategy.md) - Raw型防御コード挿入方針
- `jv_checker/src/inference/` - 実装ソースコード
- `jv_ast/src/types.rs` - 型アノテーション定義
