# java-aware-type-inference: 型推論エンジンのJava特化改修指示書

## 概要

現在の型推論システムは汎用的なHindley-Milner型推論を基礎としていますが、Java 25との相互運用性を高めるために、Javaの型システム固有の特性を認識する必要があります。本文書では、プリミティブ・ボックス型の区別、型変換メソッドの自動検出、widening conversion等、Java型システムへの対応を段階的に実装するための改修指示をまとめます。

## 現状の課題

### 1. プリミティブとボックス型の未区別

**問題**:
- `TypeKind::Primitive("Int")` として抽象的に表現
- Javaの `int` (プリミティブ) と `Integer` (ボックス型) を区別していない
- Null許容文脈（`Int?`）ではボックス型必須だが、推論エンジンが認識しない

**影響**:
```jv
val x: Int = 42        // int? Integer? 判断不可
val y: Int? = null     // これはInteger必須だが型推論が追跡しない
val z: Int = y ?: 0    // アンボクシングが必要だが検出されない
```

### 2. JDK変換メソッドの未認識

**問題**:
- `toString()`, `valueOf()`, `parse*()` などの変換メソッドが型環境に存在しない
- String ↔ Int, Double ↔ String などの相互変換の考慮がない
- 型互換性判定が厳密な等価性のみ

**影響**:
```jv
val num = 123
val text = num.toString()  // toString()が型環境に未登録

val str = "456"
val parsed = Int.parse(str)  // 推論システムに未登録
```

### 3. Java型変換規則の未実装

**問題**:
- Widening primitive conversion（`int` → `long`, `float` → `double`）未サポート
- Boxing/Unboxing conversion の自動挿入なし
- String conversion の特別扱いなし

**影響**:
```jv
val x: Int = 42
val y: Long = x          // int → long は Java で合法だが推論が追跡しない

val list: List<Int> = [1, 2, 3]  // int → Integer の自動変換を期待
```

### 4. SymbolIndexの活用不足

**問題**:
- JDK型シグネチャを読み取る `SymbolIndex` は存在
- しかし、メソッドシグネチャを型互換性判定に活用していない
- インスタンスメソッドは名前のみ記録（シグネチャなし）

## 改修方針

### フェーズ構成

- **Phase 1**: 型表現の拡張とプリミティブ・ボックス型の区別
- **Phase 2**: Java型変換規則の実装（boxing/unboxing, widening）
- **Phase 3**: 変換メソッドの自動検出とカタログ化
- **Phase 4**: 型互換性判定の高度化と制約生成への統合

## Architecture

Java 固有のロジックを `jv_checker::java` モジュールへ集約し、型推論層との依存関係を明示的に切り分けた。

- `jv_checker::java::primitive` — `JavaPrimitive` 列挙と `JavaBoxingTable`、`JavaNullabilityPolicy` を提供し、プリミティブ名称解決・boxing/unboxing・null 許容判定を一元管理する。
- `jv_checker::inference::types` — `JavaPrimitive` を `PrimitiveType` として利用し、推論層は `crate::java::*` 経由で Java 固有機能へアクセスする。
- `jv_checker::inference::type_factory` — 型注釈解析時に `JavaPrimitive`/`JavaBoxingTable` を参照し、Java 固有知識を型生成ユーティリティから排除する。

この分離により、将来的な Java 仕様変更や JVM バージョン追加時には `jv_checker::java` を中心に更新すればよく、推論アルゴリズムの汎用性と保守性が向上する。

## Phase 1: 型表現の拡張

### 目的

プリミティブ型とボックス型を明確に区別し、Null安全との整合性を確保する。

### 実装内容

#### 1.1 TypeKind の拡張

**ファイル**: `jv/crates/jv_checker/src/inference/types.rs`

```rust
/// 型推論で用いる主な型表現（拡張版）
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Primitive(PrimitiveType),      // int, boolean, double等
    Boxed(PrimitiveType),           // Integer, Boolean, Double等
    Reference(String),              // ユーザー定義型・JDKクラス
    Optional(Box<TypeKind>),        // Null許容型 (T?)
    Variable(TypeId),               // 型変数 (推論中)
    Function(Vec<TypeKind>, Box<TypeKind>), // 関数型
    Array(Box<TypeKind>, usize),    // 配列型
    Unknown,                        // 未決定型
}

/// Javaプリミティブ型の列挙
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Int,
    Long,
    Short,
    Byte,
    Float,
    Double,
    Boolean,
    Char,
}

impl PrimitiveType {
    /// プリミティブ型名を取得
    pub fn java_name(&self) -> &'static str {
        match self {
            Self::Int => "int",
            Self::Long => "long",
            Self::Short => "short",
            Self::Byte => "byte",
            Self::Float => "float",
            Self::Double => "double",
            Self::Boolean => "boolean",
            Self::Char => "char",
        }
    }

    /// 対応するボックス型の完全修飾名を取得
    pub fn boxed_fqcn(&self) -> &'static str {
        match self {
            Self::Int => "java.lang.Integer",
            Self::Long => "java.lang.Long",
            Self::Short => "java.lang.Short",
            Self::Byte => "java.lang.Byte",
            Self::Float => "java.lang.Float",
            Self::Double => "java.lang.Double",
            Self::Boolean => "java.lang.Boolean",
            Self::Char => "java.lang.Character",
        }
    }

    /// 文字列からのパース
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "int" => Some(Self::Int),
            "long" => Some(Self::Long),
            "short" => Some(Self::Short),
            "byte" => Some(Self::Byte),
            "float" => Some(Self::Float),
            "double" => Some(Self::Double),
            "boolean" => Some(Self::Boolean),
            "char" => Some(Self::Char),
            _ => None,
        }
    }
}

impl TypeKind {
    /// プリミティブ型を生成
    pub fn primitive(prim: PrimitiveType) -> Self {
        TypeKind::Primitive(prim)
    }

    /// ボックス型を生成
    pub fn boxed(prim: PrimitiveType) -> Self {
        TypeKind::Boxed(prim)
    }

    /// プリミティブ型かどうか判定
    pub fn is_primitive(&self) -> bool {
        matches!(self, TypeKind::Primitive(_))
    }

    /// ボックス型かどうか判定
    pub fn is_boxed(&self) -> bool {
        matches!(self, TypeKind::Boxed(_))
    }

    /// Null許容可能な型かどうか判定
    pub fn is_nullable(&self) -> bool {
        !self.is_primitive()
    }
}
```

#### 1.2 prelude の更新

**ファイル**: `jv/crates/jv_checker/src/inference/prelude.rs`

```rust
// 定数の更新
const INT: PrimitiveType = PrimitiveType::Int;
const LONG: PrimitiveType = PrimitiveType::Long;
const BOOLEAN: PrimitiveType = PrimitiveType::Boolean;
const DOUBLE: PrimitiveType = PrimitiveType::Double;

fn primitive(prim: PrimitiveType) -> TypeKind {
    TypeKind::Primitive(prim)
}

fn boxed(prim: PrimitiveType) -> TypeKind {
    TypeKind::Boxed(prim)
}

// 使用例
let take_sig = TypeScheme::monotype(function(
    vec![primitive(INT)],
    primitive(SEQUENCE_CORE)
));
```

#### 1.3 単一化ルールの更新

**ファイル**: `jv/crates/jv_checker/src/inference/unify.rs`

```rust
impl ConstraintSolver {
    fn unify(&mut self, left: TypeKind, right: TypeKind, note: Option<String>)
        -> Result<TypeKind, SolveError>
    {
        let left = self.prune(left);
        let right = self.prune(right.clone());

        match (left.clone(), right.clone()) {
            // プリミティブ型同士
            (TypeKind::Primitive(a), TypeKind::Primitive(b)) => {
                if a == b {
                    Ok(TypeKind::Primitive(a))
                } else {
                    Err(SolveError::TypeMismatch { left, right, note })
                }
            }

            // ボックス型同士
            (TypeKind::Boxed(a), TypeKind::Boxed(b)) => {
                if a == b {
                    Ok(TypeKind::Boxed(a))
                } else {
                    Err(SolveError::TypeMismatch { left, right, note })
                }
            }

            // プリミティブ vs ボックス（エラー: 明示的変換必要）
            (TypeKind::Primitive(a), TypeKind::Boxed(b)) |
            (TypeKind::Boxed(b), TypeKind::Primitive(a)) => {
                Err(SolveError::BoxingRequired {
                    primitive: a,
                    boxed: b,
                    note,
                })
            }

            // Optional型とプリミティブ（エラー: プリミティブはnull不可）
            (TypeKind::Optional(_), TypeKind::Primitive(prim)) |
            (TypeKind::Primitive(prim), TypeKind::Optional(_)) => {
                Err(SolveError::PrimitiveCannotBeNull {
                    primitive: prim,
                    note,
                })
            }

            // Optional型とボックス型（ボックス型を優先）
            (TypeKind::Optional(inner), TypeKind::Boxed(prim)) |
            (TypeKind::Boxed(prim), TypeKind::Optional(inner)) => {
                // Optional<Boxed(Int)> と Boxed(Int) を統一
                match *inner {
                    TypeKind::Boxed(inner_prim) if inner_prim == prim => {
                        Ok(TypeKind::Optional(Box::new(TypeKind::Boxed(prim))))
                    }
                    _ => self.unify(*inner, TypeKind::Boxed(prim), note)
                }
            }

            // 既存のルールは維持...
            _ => { /* 既存の処理 */ }
        }
    }
}

// 新しいエラー型
#[derive(Debug, PartialEq)]
pub enum SolveError {
    // 既存のエラー
    Placeholder { placeholder: &'static str, note: Option<String> },
    TypeMismatch { left: TypeKind, right: TypeKind, note: Option<String> },
    OccursCheck { id: TypeId, ty: TypeKind, note: Option<String> },

    // 新規追加
    BoxingRequired { primitive: PrimitiveType, boxed: PrimitiveType, note: Option<String> },
    PrimitiveCannotBeNull { primitive: PrimitiveType, note: Option<String> },
    ConversionRequired { from: TypeKind, to: TypeKind, note: Option<String> },
}
```

#### 1.4 ImportRegistry の更新

**ファイル**: `jv/crates/jv_checker/src/inference/imports.rs`

```rust
fn java_type_to_type_kind(java_type: &JavaType) -> TypeKind {
    match java_type {
        JavaType::Primitive(name) => {
            if let Some(prim) = PrimitiveType::from_str(name) {
                TypeKind::Primitive(prim)
            } else {
                TypeKind::Unknown
            }
        }
        JavaType::Reference { name, .. } => {
            // ボックス型として認識
            if let Some(prim) = detect_boxed_type(name) {
                TypeKind::Boxed(prim)
            } else {
                TypeKind::Reference(name.clone())
            }
        }
        // 既存の処理は維持...
    }
}

fn detect_boxed_type(fqcn: &str) -> Option<PrimitiveType> {
    match fqcn {
        "java.lang.Integer" => Some(PrimitiveType::Int),
        "java.lang.Long" => Some(PrimitiveType::Long),
        "java.lang.Short" => Some(PrimitiveType::Short),
        "java.lang.Byte" => Some(PrimitiveType::Byte),
        "java.lang.Float" => Some(PrimitiveType::Float),
        "java.lang.Double" => Some(PrimitiveType::Double),
        "java.lang.Boolean" => Some(PrimitiveType::Boolean),
        "java.lang.Character" => Some(PrimitiveType::Char),
        _ => None,
    }
}
```

### テスト追加

**ファイル**: `jv/crates/jv_checker/tests/inference_primitives.rs`

```rust
#[test]
fn primitive_vs_boxed_type_mismatch() {
    let program = Parser::parse("val x: Int = Integer.valueOf(42)").unwrap();
    let mut engine = InferenceEngine::new();

    let result = engine.infer_program(&program);
    assert!(matches!(result, Err(InferenceError::SolveFailure(
        SolveError::BoxingRequired { .. }
    ))));
}

#[test]
fn null_assignment_to_primitive_rejected() {
    let program = Parser::parse("val x: Int = null").unwrap();
    let mut engine = InferenceEngine::new();

    let result = engine.infer_program(&program);
    assert!(matches!(result, Err(InferenceError::SolveFailure(
        SolveError::PrimitiveCannotBeNull { .. }
    ))));
}

#[test]
fn nullable_int_requires_boxed_type() {
    let program = Parser::parse("val x: Int? = 42").unwrap();
    let mut engine = InferenceEngine::new();

    let result = engine.infer_program(&program);
    assert!(result.is_ok());

    let scheme = engine.lookup_variable("x").unwrap();
    assert!(matches!(scheme.ty, TypeKind::Optional(
        box TypeKind::Boxed(PrimitiveType::Int)
    )));
}
```

### 実装優先度

1. **高**: TypeKind, PrimitiveType の定義
2. **高**: 単一化ルールの基本実装
3. **中**: ImportRegistry の更新
4. **中**: prelude の更新
5. **低**: エラーメッセージの多言語化

## Phase 2: Java型変換規則の実装

### 目的

Java言語仕様で定義された型変換規則（JLS Chapter 5）を推論エンジンに実装する。

### 実装内容

#### 2.1 型互換性判定システム

**新規ファイル**: `jv/crates/jv_checker/src/inference/compatibility.rs`

```rust
use crate::inference::types::{PrimitiveType, TypeKind};

/// 型変換の種類（JLS Chapter 5）
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConversionKind {
    /// 完全一致（変換不要）
    Identity,

    /// Widening primitive conversion (JLS §5.1.2)
    WideningPrimitive,

    /// Narrowing primitive conversion (JLS §5.1.3)
    NarrowingPrimitive,

    /// Boxing conversion (JLS §5.1.7)
    Boxing,

    /// Unboxing conversion (JLS §5.1.8)
    Unboxing,

    /// String conversion (JLS §5.1.11)
    StringConversion,

    /// Method invocation conversion (JLS §5.3)
    MethodInvocation,

    /// 変換不可
    Incompatible,
}

/// 型変換のコスト（低いほど優先）
impl ConversionKind {
    pub fn cost(&self) -> u32 {
        match self {
            Self::Identity => 0,
            Self::WideningPrimitive => 1,
            Self::Boxing => 2,
            Self::Unboxing => 2,
            Self::MethodInvocation => 3,
            Self::StringConversion => 10,
            Self::NarrowingPrimitive => 100,  // 明示的キャスト推奨
            Self::Incompatible => u32::MAX,
        }
    }
}

/// 型互換性判定結果
#[derive(Debug, Clone)]
pub struct CompatibilityResult {
    pub kind: ConversionKind,
    pub conversion_path: Vec<ConversionStep>,
}

#[derive(Debug, Clone)]
pub struct ConversionStep {
    pub from: TypeKind,
    pub to: TypeKind,
    pub method: Option<String>,  // 変換メソッド名
}

/// 型互換性判定エンジン
pub struct CompatibilityChecker;

impl CompatibilityChecker {
    /// 型 `from` を型 `to` に変換可能か判定
    pub fn check_compatibility(from: &TypeKind, to: &TypeKind) -> CompatibilityResult {
        // 完全一致
        if from == to {
            return CompatibilityResult {
                kind: ConversionKind::Identity,
                conversion_path: vec![],
            };
        }

        match (from, to) {
            // Widening primitive conversion
            (TypeKind::Primitive(from_prim), TypeKind::Primitive(to_prim)) => {
                Self::check_primitive_widening(*from_prim, *to_prim)
            }

            // Boxing conversion
            (TypeKind::Primitive(prim), TypeKind::Boxed(boxed_prim)) if prim == boxed_prim => {
                CompatibilityResult {
                    kind: ConversionKind::Boxing,
                    conversion_path: vec![ConversionStep {
                        from: from.clone(),
                        to: to.clone(),
                        method: Some(format!("{}.valueOf", boxed_prim.boxed_fqcn())),
                    }],
                }
            }

            // Unboxing conversion
            (TypeKind::Boxed(boxed_prim), TypeKind::Primitive(prim)) if prim == boxed_prim => {
                CompatibilityResult {
                    kind: ConversionKind::Unboxing,
                    conversion_path: vec![ConversionStep {
                        from: from.clone(),
                        to: to.clone(),
                        method: Some(format!("{}Value", prim.java_name())),
                    }],
                }
            }

            // String conversion (任意の型 → String)
            (_, TypeKind::Reference(name)) if name == "java.lang.String" => {
                CompatibilityResult {
                    kind: ConversionKind::StringConversion,
                    conversion_path: vec![ConversionStep {
                        from: from.clone(),
                        to: to.clone(),
                        method: Some("toString".to_string()),
                    }],
                }
            }

            // Optional unwrapping
            (TypeKind::Optional(inner), target) => {
                let inner_compat = Self::check_compatibility(inner, target);
                if inner_compat.kind != ConversionKind::Incompatible {
                    // Optional<T> → T への暗黙unwrapping（Null安全検証必要）
                    return inner_compat;
                }
                CompatibilityResult {
                    kind: ConversionKind::Incompatible,
                    conversion_path: vec![],
                }
            }

            _ => CompatibilityResult {
                kind: ConversionKind::Incompatible,
                conversion_path: vec![],
            },
        }
    }

    fn check_primitive_widening(
        from: PrimitiveType,
        to: PrimitiveType
    ) -> CompatibilityResult {
        use PrimitiveType::*;

        let is_widening = matches!(
            (from, to),
            // byte → short, int, long, float, double
            (Byte, Short | Int | Long | Float | Double) |
            // short → int, long, float, double
            (Short, Int | Long | Float | Double) |
            // char → int, long, float, double
            (Char, Int | Long | Float | Double) |
            // int → long, float, double
            (Int, Long | Float | Double) |
            // long → float, double
            (Long, Float | Double) |
            // float → double
            (Float, Double)
        );

        if is_widening {
            CompatibilityResult {
                kind: ConversionKind::WideningPrimitive,
                conversion_path: vec![ConversionStep {
                    from: TypeKind::Primitive(from),
                    to: TypeKind::Primitive(to),
                    method: None,  // 暗黙変換
                }],
            }
        } else {
            CompatibilityResult {
                kind: ConversionKind::Incompatible,
                conversion_path: vec![],
            }
        }
    }
}
```

#### 2.2 制約生成への統合

**ファイル**: `jv/crates/jv_checker/src/inference/constraint/generator.rs`

```rust
use crate::inference::compatibility::{CompatibilityChecker, ConversionKind};

impl ConstraintGenerator {
    fn infer_assignment(&mut self, target_ty: TypeKind, value_ty: TypeKind, span: &Span) {
        // 互換性チェック
        let compat = CompatibilityChecker::check_compatibility(&value_ty, &target_ty);

        match compat.kind {
            ConversionKind::Identity => {
                // 完全一致 - 通常の等価制約
                self.push_constraint(
                    ConstraintKind::Equal(target_ty, value_ty),
                    Some("assignment type must match")
                );
            }

            ConversionKind::WideningPrimitive |
            ConversionKind::Boxing |
            ConversionKind::Unboxing => {
                // 暗黙変換可能 - 変換制約を生成
                self.push_constraint(
                    ConstraintKind::Convertible {
                        from: value_ty,
                        to: target_ty,
                        conversion: compat,
                    },
                    Some("implicit conversion required")
                );
            }

            ConversionKind::StringConversion |
            ConversionKind::MethodInvocation => {
                // 明示的変換推奨 - 警告付き制約
                self.push_constraint(
                    ConstraintKind::ConvertibleWithWarning {
                        from: value_ty,
                        to: target_ty,
                        conversion: compat,
                    },
                    Some("explicit conversion recommended")
                );
            }

            ConversionKind::NarrowingPrimitive => {
                // Narrowing conversion - エラー（明示的キャスト必要）
                self.diagnostics.push(Diagnostic::error(
                    "E_TYPE_001",
                    format!("narrowing conversion requires explicit cast"),
                    span.clone(),
                ));
            }

            ConversionKind::Incompatible => {
                // 変換不可 - 型不一致エラー
                self.diagnostics.push(Diagnostic::error(
                    "E_TYPE_002",
                    format!("incompatible types: cannot convert {} to {}",
                        value_ty.display(), target_ty.display()),
                    span.clone(),
                ));
            }
        }
    }
}

// 制約の種類を拡張
pub enum ConstraintKind {
    Equal(TypeKind, TypeKind),
    Assign(TypeId, TypeKind),

    // 新規追加
    Convertible {
        from: TypeKind,
        to: TypeKind,
        conversion: CompatibilityResult,
    },
    ConvertibleWithWarning {
        from: TypeKind,
        to: TypeKind,
        conversion: CompatibilityResult,
    },

    Placeholder(&'static str),
}
```

#### 2.3 単一化への統合

**ファイル**: `jv/crates/jv_checker/src/inference/unify.rs`

```rust
impl ConstraintSolver {
    fn process_constraint(&mut self, constraint: &Constraint) -> Result<(), SolveError> {
        match &constraint.kind {
            ConstraintKind::Equal(left, right) => {
                self.unify(left.clone(), right.clone(), constraint.note.clone())?;
            }

            ConstraintKind::Convertible { from, to, conversion } => {
                // 変換可能な制約を処理
                self.apply_conversion(from.clone(), to.clone(), conversion)?;
            }

            ConstraintKind::ConvertibleWithWarning { from, to, conversion } => {
                // 警告付き変換
                self.telemetry.add_warning(format!(
                    "implicit conversion from {} to {} via {}",
                    from.display(),
                    to.display(),
                    conversion.kind.name()
                ));
                self.apply_conversion(from.clone(), to.clone(), conversion)?;
            }

            // 既存の処理...
        }
        Ok(())
    }

    fn apply_conversion(
        &mut self,
        from: TypeKind,
        to: TypeKind,
        conversion: &CompatibilityResult,
    ) -> Result<(), SolveError> {
        // 変換パスを記録（コード生成時に使用）
        self.conversions.push((from.clone(), to.clone(), conversion.clone()));

        // 型変数がある場合は束縛
        match from {
            TypeKind::Variable(id) => {
                self.bind_variable(id, to, None)?;
            }
            _ => {
                // 具体型同士の変換は単に記録
            }
        }

        Ok(())
    }
}
```

### テスト追加

**ファイル**: `jv/crates/jv_checker/tests/inference_conversions.rs`

```rust
#[test]
fn widening_primitive_conversion_allowed() {
    let program = Parser::parse("val x: Long = 42").unwrap();
    let mut engine = InferenceEngine::new();

    let result = engine.infer_program(&program);
    assert!(result.is_ok());

    // 変換記録を確認
    let conversions = engine.telemetry().conversions;
    assert_eq!(conversions.len(), 1);
    assert!(matches!(conversions[0].kind, ConversionKind::WideningPrimitive));
}

#[test]
fn boxing_conversion_in_collection() {
    let program = Parser::parse("val list: List<Int> = listOf(1, 2, 3)").unwrap();
    let mut engine = InferenceEngine::new();

    let result = engine.infer_program(&program);
    assert!(result.is_ok());

    // int → Integer への boxing を検出
    let conversions = engine.telemetry().conversions;
    assert!(conversions.iter().any(|c|
        matches!(c.kind, ConversionKind::Boxing)
    ));
}

#[test]
fn narrowing_conversion_rejected() {
    let program = Parser::parse("val x: Int = 42L").unwrap();
    let mut engine = InferenceEngine::new();

    let result = engine.infer_program(&program);
    assert!(result.is_err());
}
```

### 実装優先度

1. **高**: CompatibilityChecker の基本実装
2. **高**: Widening primitive conversion
3. **高**: Boxing/Unboxing conversion
4. **中**: 制約生成への統合
5. **低**: String conversion

## Phase 3: 変換メソッドの自動検出

### 目的

JDK APIの変換メソッドを命名規則から自動検出し、型互換性判定に活用する。

### 実装内容

#### 3.1 変換検出システム

**新規ファイル**: `jv/crates/jv_build/src/metadata/conversion_detector.rs`

```rust
use crate::metadata::{JavaMethodSignature, TypeEntry, StaticMemberKind};
use jv_ir::types::JavaType;

/// 変換メソッドの種類
#[derive(Debug, Clone, PartialEq)]
pub enum ConversionMethodKind {
    /// to* メソッド（直接変換）
    DirectConversion,
    /// valueOf/parse* （静的ファクトリ）
    StaticFactory,
    /// *Value メソッド（アンボクシング）
    Unboxing,
    /// stream/iterator 系
    CollectionTransform,
    /// map/flatMap 系
    FunctionalTransform,
}

/// 変換メソッドのシグネチャ
#[derive(Debug, Clone)]
pub struct ConversionSignature {
    pub kind: ConversionMethodKind,
    pub source_type: JavaType,
    pub target_type: JavaType,
    pub method_name: String,
    pub is_static: bool,
    pub confidence: f32,  // 0.0-1.0
}

/// 変換検出ルール
pub trait DetectionRule: Send + Sync {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature>;

    fn check_static_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature>;
}

/// Rule 1: to* パターン
pub struct ToMethodRule;

impl DetectionRule for ToMethodRule {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        if !method_name.starts_with("to") {
            return None;
        }

        // toString は特別扱い
        if method_name == "toString" {
            return Some(ConversionSignature {
                kind: ConversionMethodKind::DirectConversion,
                source_type: JavaType::Reference {
                    name: owner_type.to_string(),
                    type_args: vec![],
                },
                target_type: JavaType::Reference {
                    name: "java.lang.String".to_string(),
                    type_args: vec![],
                },
                method_name: method_name.to_string(),
                is_static: false,
                confidence: 1.0,
            });
        }

        // 戻り値型がレシーバと異なることを確認
        if signature.parameters.len() <= 1
            && type_differs_from_owner(owner_type, &signature.return_type)
        {
            return Some(ConversionSignature {
                kind: ConversionMethodKind::DirectConversion,
                source_type: parse_owner_type(owner_type),
                target_type: signature.return_type.clone(),
                method_name: method_name.to_string(),
                is_static: false,
                confidence: 0.9,
            });
        }

        None
    }

    fn check_static_method(&self, ..) -> Option<ConversionSignature> {
        None
    }
}

/// Rule 2: valueOf/parse* パターン
pub struct StaticFactoryRule;

impl DetectionRule for StaticFactoryRule {
    fn check_instance_method(&self, ..) -> Option<ConversionSignature> {
        None
    }

    fn check_static_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        let is_factory = method_name == "valueOf"
            || method_name.starts_with("parse")
            || method_name == "of"
            || method_name == "from";

        if !is_factory || signature.parameters.len() != 1 {
            return None;
        }

        let param_type = &signature.parameters[0];
        let return_type = &signature.return_type;

        if return_type_matches_owner(owner_type, return_type) {
            return Some(ConversionSignature {
                kind: ConversionMethodKind::StaticFactory,
                source_type: param_type.clone(),
                target_type: return_type.clone(),
                method_name: format!("{}.{}", owner_type, method_name),
                is_static: true,
                confidence: 0.95,
            });
        }

        None
    }
}

/// Rule 3: *Value パターン
pub struct UnboxingRule;

impl DetectionRule for UnboxingRule {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        if !method_name.ends_with("Value") || signature.parameters.len() != 0 {
            return None;
        }

        if let JavaType::Primitive(prim_name) = &signature.return_type {
            let expected_owner = format!("java.lang.{}", capitalize(prim_name));

            if owner_type.ends_with(&expected_owner) {
                return Some(ConversionSignature {
                    kind: ConversionMethodKind::Unboxing,
                    source_type: parse_owner_type(owner_type),
                    target_type: signature.return_type.clone(),
                    method_name: method_name.to_string(),
                    is_static: false,
                    confidence: 1.0,
                });
            }
        }

        None
    }

    fn check_static_method(&self, ..) -> Option<ConversionSignature> {
        None
    }
}

/// Rule 4: コレクション変換
pub struct CollectionTransformRule;

impl DetectionRule for CollectionTransformRule {
    fn check_instance_method(
        &self,
        owner_type: &str,
        method_name: &str,
        signature: &JavaMethodSignature,
    ) -> Option<ConversionSignature> {
        let known_transforms = [
            "stream", "parallelStream", "iterator",
            "spliterator", "toArray", "entrySet", "keySet", "values"
        ];

        if !known_transforms.contains(&method_name) || signature.parameters.len() > 1 {
            return None;
        }

        Some(ConversionSignature {
            kind: ConversionMethodKind::CollectionTransform,
            source_type: parse_owner_type(owner_type),
            target_type: signature.return_type.clone(),
            method_name: method_name.to_string(),
            is_static: false,
            confidence: 0.95,
        })
    }

    fn check_static_method(&self, ..) -> Option<ConversionSignature> {
        None
    }
}

/// 変換検出エンジン
pub struct ConversionDetector {
    rules: Vec<Box<dyn DetectionRule>>,
}

impl ConversionDetector {
    pub fn new() -> Self {
        Self {
            rules: vec![
                Box::new(ToMethodRule),
                Box::new(StaticFactoryRule),
                Box::new(UnboxingRule),
                Box::new(CollectionTransformRule),
            ],
        }
    }

    pub fn detect_conversions(&self, type_entry: &TypeEntry) -> Vec<ConversionSignature> {
        let mut conversions = Vec::new();

        // インスタンスメソッドを検査
        for (method_name, signature) in &type_entry.instance_methods {
            for rule in &self.rules {
                if let Some(conv) = rule.check_instance_method(
                    &type_entry.fqcn,
                    method_name,
                    signature,
                ) {
                    conversions.push(conv);
                }
            }
        }

        // 静的メソッドを検査
        for (method_name, member) in &type_entry.static_fields {
            if let StaticMemberKind::Method { signature } = &member.kind {
                for rule in &self.rules {
                    if let Some(conv) = rule.check_static_method(
                        &type_entry.fqcn,
                        method_name,
                        signature,
                    ) {
                        conversions.push(conv);
                    }
                }
            }
        }

        conversions
    }
}

// ヘルパー関数
fn type_differs_from_owner(owner_type: &str, return_type: &JavaType) -> bool {
    match return_type {
        JavaType::Reference { name, .. } => name != owner_type,
        JavaType::Primitive(_) => true,
        _ => false,
    }
}

fn return_type_matches_owner(owner_type: &str, return_type: &JavaType) -> bool {
    match return_type {
        JavaType::Reference { name, .. } => name == owner_type,
        JavaType::Primitive(prim) => {
            let expected_wrapper = format!("java.lang.{}", capitalize(prim));
            owner_type.ends_with(&expected_wrapper)
        }
        _ => false,
    }
}

fn parse_owner_type(owner_type: &str) -> JavaType {
    JavaType::Reference {
        name: owner_type.to_string(),
        type_args: vec![],
    }
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().chain(chars).collect(),
        None => String::new(),
    }
}
```

#### 3.2 SymbolIndex の拡張

**ファイル**: `jv/crates/jv_build/src/metadata/index.rs`

```rust
use crate::metadata::conversion_detector::{ConversionDetector, ConversionSignature};

impl SymbolIndex {
    /// JDK全体から変換メソッドを抽出
    pub fn build_conversion_catalog(&self) -> ConversionCatalog {
        let detector = ConversionDetector::new();
        let mut catalog = ConversionCatalog::new();

        for (fqcn, type_entry) in &self.types {
            let conversions = detector.detect_conversions(type_entry);

            for conv in conversions {
                // 高信頼度の変換のみカタログ化
                if conv.confidence >= 0.8 {
                    catalog.add_conversion(conv);
                }
            }
        }

        catalog
    }
}

/// 変換メソッドのカタログ
#[derive(Debug, Clone, Default)]
pub struct ConversionCatalog {
    /// ソース型 → 変換リスト
    conversions: HashMap<String, Vec<ConversionSignature>>,
}

impl ConversionCatalog {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_conversion(&mut self, conv: ConversionSignature) {
        let source_key = type_to_key(&conv.source_type);
        self.conversions
            .entry(source_key)
            .or_default()
            .push(conv);
    }

    /// ソース型からターゲット型への変換を検索
    pub fn find_conversions(
        &self,
        from: &JavaType,
        to: &JavaType,
    ) -> Vec<ConversionSignature> {
        let source_key = type_to_key(from);

        self.conversions
            .get(&source_key)
            .map(|conversions| {
                conversions
                    .iter()
                    .filter(|conv| types_match(&conv.target_type, to))
                    .cloned()
                    .collect()
            })
            .unwrap_or_default()
    }
}

fn type_to_key(ty: &JavaType) -> String {
    match ty {
        JavaType::Primitive(name) => name.clone(),
        JavaType::Reference { name, .. } => name.clone(),
        _ => "Unknown".to_string(),
    }
}

fn types_match(a: &JavaType, b: &JavaType) -> bool {
    // 簡易比較（実際にはジェネリクス考慮が必要）
    type_to_key(a) == type_to_key(b)
}
```

#### 3.3 型推論への統合

**ファイル**: `jv/crates/jv_checker/src/inference/imports.rs`

```rust
use jv_build::metadata::ConversionCatalog;

impl ImportRegistry {
    pub fn register_conversions(
        &mut self,
        env: &mut TypeEnvironment,
        catalog: &ConversionCatalog,
    ) {
        // カタログから変換を型環境に登録
        for (source, conversions) in catalog.iter() {
            for conv in conversions {
                if conv.confidence >= 0.85 {
                    env.register_conversion(
                        conv.source_type.clone(),
                        conv.target_type.clone(),
                        conv.method_name.clone(),
                        conv.confidence,
                    );
                }
            }
        }
    }
}
```

**ファイル**: `jv/crates/jv_checker/src/inference/environment.rs`

```rust
#[derive(Debug)]
pub struct TypeEnvironment {
    scopes: Vec<HashMap<String, TypeScheme>>,
    type_id_counter: u32,

    // 新規追加
    conversions: HashMap<(String, String), Vec<ConversionInfo>>,
}

#[derive(Debug, Clone)]
pub struct ConversionInfo {
    pub method_name: String,
    pub confidence: f32,
}

impl TypeEnvironment {
    pub fn register_conversion(
        &mut self,
        from: JavaType,
        to: JavaType,
        method_name: String,
        confidence: f32,
    ) {
        let key = (type_to_string(&from), type_to_string(&to));
        self.conversions
            .entry(key)
            .or_default()
            .push(ConversionInfo {
                method_name,
                confidence,
            });
    }

    pub fn lookup_conversions(&self, from: &JavaType, to: &JavaType) -> Vec<ConversionInfo> {
        let key = (type_to_string(from), type_to_string(to));
        self.conversions
            .get(&key)
            .cloned()
            .unwrap_or_default()
    }
}

fn type_to_string(ty: &JavaType) -> String {
    match ty {
        JavaType::Primitive(name) => name.clone(),
        JavaType::Reference { name, .. } => name.clone(),
        _ => "Unknown".to_string(),
    }
}
```

#### 3.4 互換性判定への統合

**ファイル**: `jv/crates/jv_checker/src/inference/compatibility.rs`

```rust
impl CompatibilityChecker {
    pub fn check_compatibility_with_catalog(
        from: &TypeKind,
        to: &TypeKind,
        env: &TypeEnvironment,
    ) -> CompatibilityResult {
        // まず標準変換をチェック
        let standard_result = Self::check_compatibility(from, to);

        if standard_result.kind != ConversionKind::Incompatible {
            return standard_result;
        }

        // カタログから変換メソッドを検索
        let java_from = type_kind_to_java_type(from);
        let java_to = type_kind_to_java_type(to);

        let conversions = env.lookup_conversions(&java_from, &java_to);

        if let Some(best_conversion) = conversions.first() {
            return CompatibilityResult {
                kind: ConversionKind::MethodInvocation,
                conversion_path: vec![ConversionStep {
                    from: from.clone(),
                    to: to.clone(),
                    method: Some(best_conversion.method_name.clone()),
                }],
            };
        }

        // 変換不可
        CompatibilityResult {
            kind: ConversionKind::Incompatible,
            conversion_path: vec![],
        }
    }
}

fn type_kind_to_java_type(kind: &TypeKind) -> JavaType {
    match kind {
        TypeKind::Primitive(prim) => JavaType::Primitive(prim.java_name().to_string()),
        TypeKind::Boxed(prim) => JavaType::Reference {
            name: prim.boxed_fqcn().to_string(),
            type_args: vec![],
        },
        TypeKind::Reference(name) => JavaType::Reference {
            name: name.clone(),
            type_args: vec![],
        },
        _ => JavaType::Unknown,
    }
}
```

### テスト追加

**ファイル**: `jv/crates/jv_build/tests/conversion_detector.rs`

```rust
use jv_build::metadata::conversion_detector::*;

#[test]
fn detects_to_string_conversion() {
    let rule = ToMethodRule;
    let signature = JavaMethodSignature {
        parameters: vec![],
        return_type: JavaType::Reference {
            name: "java.lang.String".to_string(),
            type_args: vec![],
        },
    };

    let result = rule.check_instance_method(
        "java.lang.Integer",
        "toString",
        &signature,
    );

    assert!(result.is_some());
    let conv = result.unwrap();
    assert_eq!(conv.confidence, 1.0);
    assert_eq!(conv.kind, ConversionMethodKind::DirectConversion);
}

#[test]
fn detects_value_of_factory() {
    let rule = StaticFactoryRule;
    let signature = JavaMethodSignature {
        parameters: vec![JavaType::Reference {
            name: "java.lang.String".to_string(),
            type_args: vec![],
        }],
        return_type: JavaType::Reference {
            name: "java.lang.Integer".to_string(),
            type_args: vec![],
        },
    };

    let result = rule.check_static_method(
        "java.lang.Integer",
        "valueOf",
        &signature,
    );

    assert!(result.is_some());
    let conv = result.unwrap();
    assert_eq!(conv.kind, ConversionMethodKind::StaticFactory);
    assert!(conv.is_static);
}

#[test]
fn detects_int_value_unboxing() {
    let rule = UnboxingRule;
    let signature = JavaMethodSignature {
        parameters: vec![],
        return_type: JavaType::Primitive("int".to_string()),
    };

    let result = rule.check_instance_method(
        "java.lang.Integer",
        "intValue",
        &signature,
    );

    assert!(result.is_some());
    let conv = result.unwrap();
    assert_eq!(conv.confidence, 1.0);
    assert_eq!(conv.kind, ConversionMethodKind::Unboxing);
}
```

### 実装優先度

1. **高**: ToMethodRule, StaticFactoryRule, UnboxingRule
2. **高**: ConversionDetector 基本実装
3. **高**: SymbolIndex::build_conversion_catalog
4. **中**: TypeEnvironment への統合
5. **中**: CompatibilityChecker への統合
6. **低**: CollectionTransformRule, FunctionalTransformRule

## Phase 4: 高度な統合と最適化

### 目的

変換推論を完全に型推論エンジンに統合し、コード生成への連携を確立する。

### 実装内容

#### 4.1 推論テレメトリの拡張

**ファイル**: `jv/crates/jv_inference/src/lib.rs`

```rust
/// 推論結果のテレメトリ（拡張版）
#[derive(Debug, Clone, Default)]
pub struct InferenceTelemetry {
    pub constraints_emitted: usize,
    pub bindings_resolved: usize,
    pub inference_duration_ms: f64,

    // 新規追加
    pub conversions_applied: Vec<AppliedConversion>,
    pub boxing_count: usize,
    pub unboxing_count: usize,
    pub widening_count: usize,
}

#[derive(Debug, Clone)]
pub struct AppliedConversion {
    pub from: String,
    pub to: String,
    pub kind: String,
    pub method: Option<String>,
    pub span: Option<Span>,
}
```

#### 4.2 IR への変換情報の記録

**ファイル**: `jv/crates/jv_mapper/src/lib.rs`

```rust
use jv_inference::AppliedConversion;
use jv_ir::types::ConversionMetadata;

impl Mapper {
    pub fn apply_conversions(
        &mut self,
        ir_program: &mut IrProgram,
        conversions: &[AppliedConversion],
    ) {
        for conv in conversions {
            // IR ノードに変換メタデータを付与
            if let Some(node) = self.find_node_at_span(&conv.span) {
                node.add_conversion_metadata(ConversionMetadata {
                    from_type: conv.from.clone(),
                    to_type: conv.to.clone(),
                    conversion_method: conv.method.clone(),
                });
            }
        }
    }
}
```

#### 4.3 コード生成への反映

**ファイル**: `jv/crates/jv_codegen_java/src/generator/expressions.rs`

```rust
impl JavaCodeGenerator {
    fn generate_expression_with_conversion(
        &mut self,
        expr: &IrExpression,
    ) -> Result<String, CodeGenError> {
        let base_expr = self.generate_expression(expr)?;

        // 変換メタデータを確認
        if let Some(metadata) = expr.conversion_metadata() {
            return self.wrap_with_conversion(base_expr, metadata);
        }

        Ok(base_expr)
    }

    fn wrap_with_conversion(
        &mut self,
        expr: String,
        metadata: &ConversionMetadata,
    ) -> Result<String, CodeGenError> {
        match metadata.conversion_method.as_deref() {
            Some("Integer.valueOf") => {
                Ok(format!("Integer.valueOf({})", expr))
            }
            Some("intValue") => {
                Ok(format!("{}.intValue()", expr))
            }
            Some("toString") => {
                Ok(format!("{}.toString()", expr))
            }
            None => {
                // 暗黙変換（widening等）- そのまま出力
                Ok(expr)
            }
            Some(method) => {
                // 一般的な変換メソッド
                Ok(format!("{}.{}({})",
                    self.extract_method_owner(method),
                    self.extract_method_name(method),
                    expr
                ))
            }
        }
    }
}
```

#### 4.4 診断メッセージの改善

**ファイル**: `jv/crates/jv_checker/src/diagnostics.rs`

```rust
pub static CONVERSION_DIAGNOSTICS: &[DiagnosticDescriptor] = &[
    DiagnosticDescriptor {
        code: "JV_TYPE_001",
        title: "Implicit boxing conversion applied",
        help: "The primitive type was automatically boxed. Consider making this explicit for clarity.",
        severity: DiagnosticSeverity::Information,
    },
    DiagnosticDescriptor {
        code: "JV_TYPE_002",
        title: "Widening primitive conversion applied",
        help: "The numeric type was widened according to Java's type promotion rules.",
        severity: DiagnosticSeverity::Information,
    },
    DiagnosticDescriptor {
        code: "JV_TYPE_003",
        title: "Conversion method invocation required",
        help: "An explicit conversion method will be called. Review the generated Java code.",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV_TYPE_004",
        title: "Narrowing conversion requires explicit cast",
        help: "Use an explicit cast expression to perform this narrowing conversion.",
        severity: DiagnosticSeverity::Error,
    },
];
```

### テスト追加

**ファイル**: `jv/crates/jv_codegen_java/tests/conversions.rs`

```rust
#[test]
fn generates_boxing_conversion() {
    let source = r#"
        val list: List<Int> = listOf(1, 2, 3)
    "#;

    let java = compile_to_java(source).unwrap();
    assert!(java.contains("Integer.valueOf(1)"));
    assert!(java.contains("Integer.valueOf(2)"));
    assert!(java.contains("Integer.valueOf(3)"));
}

#[test]
fn generates_widening_conversion() {
    let source = r#"
        val x: Long = 42
    "#;

    let java = compile_to_java(source).unwrap();
    // wideningは暗黙的なのでキャストなし
    assert!(java.contains("long x = 42"));
}

#[test]
fn generates_to_string_conversion() {
    let source = r#"
        val num = 123
        val text = num.toString()
    "#;

    let java = compile_to_java(source).unwrap();
    assert!(java.contains("Integer.toString(num)") || java.contains("num.toString()"));
}
```

### 実装優先度

1. **高**: InferenceTelemetry の拡張
2. **高**: IR への変換情報記録
3. **中**: コード生成への反映
4. **中**: 診断メッセージの追加
5. **低**: パフォーマンス最適化

## 実装ロードマップ

### タイムライン

| Phase | 期間 | 主要成果物 |
|-------|------|------------|
| Phase 1 | 2週間 | TypeKind拡張、プリミティブ・ボックス型の区別 |
| Phase 2 | 2週間 | Java型変換規則の実装 |
| Phase 3 | 3週間 | 変換メソッド自動検出システム |
| Phase 4 | 2週間 | 統合とコード生成への反映 |
| **合計** | **9週間** | Java特化型推論エンジン完成 |

### マイルストーン

**M1 (2週間後)**: Phase 1完了
- プリミティブとボックス型を明確に区別
- Null許容文脈でボックス型を強制
- 基本的な単一化ルールが動作

**M2 (4週間後)**: Phase 2完了
- Widening/narrowing conversion実装
- Boxing/unboxing conversion実装
- 制約生成と単一化に統合

**M3 (7週間後)**: Phase 3完了
- JDK APIから変換メソッドを自動検出
- 型環境に変換カタログを統合
- 互換性判定に変換メソッドを活用

**M4 (9週間後)**: Phase 4完了
- コード生成に変換情報を反映
- 診断メッセージ完備
- 統合テスト完了

## リスクと対策

### リスク1: ジェネリクス型引数の扱い

**問題**: `List<T>.stream()` → `Stream<T>` の型パラメータ伝播が複雑

**対策**:
- Phase 3では単純型のみ対応
- ジェネリクス完全対応は別タスクで実施

### リスク2: 推論性能の劣化

**問題**: 変換探索で制約解決が遅延

**対策**:
- 変換カタログを事前計算してキャッシュ
- 信頼度閾値で候補を絞る
- 並列推論の活用

### リスク3: Java仕様との齟齬

**問題**: JLS準拠が不完全

**対策**:
- JLS Chapter 5を詳細にレビュー
- OpenJDKのjavacと出力を比較
- エッジケースを網羅的にテスト

## 検証基準

### Phase 1検証

- [ ] `Int` と `Int?` が異なる型として認識される
- [ ] `Int?` は `Boxed(Int)` にマッピングされる
- [ ] プリミティブ型への `null` 代入がエラーになる
- [ ] 100件以上のテストケースが通過

### Phase 2検証

- [ ] `int` → `long` のwidening conversionが動作
- [ ] `int` ↔ `Integer` のboxing/unboxingが検出される
- [ ] 任意の型 → `String` 変換が動作
- [ ] 150件以上のテストケースが通過

### Phase 3検証

- [ ] `Integer.valueOf(String)` が自動検出される
- [ ] `toString()` が変換メソッドとして登録される
- [ ] JDK主要クラス（50以上）から変換が抽出される
- [ ] 200件以上のテストケースが通過

### Phase 4検証

- [ ] 変換情報がJavaコードに正しく反映される
- [ ] 診断メッセージが適切に表示される
- [ ] 既存のjvサンプルコードが全て正常にコンパイルされる
- [ ] パフォーマンスが従来比で20%以内の劣化に収まる

## 参考資料

- **JLS Chapter 5**: Type Conversions (https://docs.oracle.com/javase/specs/jls/se25/html/jls-5.html)
- **Hindley-Milner型推論**: 既存ドキュメント `type-inference-system.md`
- **Null安全機構**: `jv_checker/src/null_safety/`
- **SymbolIndex**: `jv_build/src/metadata/index.rs`
- **既存の型推論実装**: `jv_checker/src/inference/`

## まとめ

本改修により、型推論エンジンはJava型システムの以下の特性を完全に理解するようになります:

1. ✅ プリミティブとボックス型の明確な区別
2. ✅ Boxing/unboxing conversionの自動処理
3. ✅ Widening/narrowing conversionの認識
4. ✅ JDK変換メソッドの自動検出と活用
5. ✅ Java仕様準拠の型互換性判定

これにより、jv言語とJava 25の相互運用性が大幅に向上し、より自然で直感的な型推論が可能になります。
