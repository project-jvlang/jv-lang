# type-inference-system: jv言語 Java対応型推論エンジン設計書

## 概要

jv の型推論エンジンは Hindley-Milner を基盤にした制約ベース推論を採用しつつ、Java 25 の型システムに特化した互換性レイヤを備えています。`docs/design/java-aware-type-inference.md` で定義した仕様は本ドキュメントで体系化され、プリミティブ/ボックス型の区別、JLS に準拠した型変換、JDK 由来のヘルパーメソッド活用を含む総合的な推論フローを提供します。

## デザインゴール

- Java 25 の型変換規則 (JLS Chapter 5) を違反なく適用する
- Null 安全機構と Boxing/Unboxing の推論を破綻させない
- 型注釈省略時も Java 側で期待される型に収束させる
- SymbolIndex と連携して jv/Java 双方のシグネチャを一元管理する
- テレメトリと診断を通じて暗黙変換の可視化とトレーサビリティを確保する

## システムアーキテクチャ

### レイヤ構造

```
AST (jv_ast)
  ↓
ConstraintEmitter (jv_checker::inference::constraint)
  ↓
ConstraintSolver & CompatibilityChecker (jv_checker::inference::{solver,compatibility})
  ↘ java::primitive / java::conversion / java::catalog
  ↓
TypeEnvironment & Schema Generalizer (jv_checker::inference::environment)
  ↓
IR (jv_ir) → Java Codegen (jv_codegen_java)
```

### 主なクレート/モジュール

- `jv_ast`: 型注釈を含む AST ノード定義と型構築ユーティリティ
- `jv_checker::inference`: 制約生成・解決・型環境・互換性判定の中核実装
- `jv_checker::java`: Java 固有のプリミティブ定義、boxing テーブル、null 許容ポリシー、変換カタログ
- `jv_build::metadata`: `SymbolIndex` と JDK メタデータを提供し、変換メソッドを自動抽出
- `jv_codegen_java`: 推論結果を Java 25 の型表記へ具体化

## 型モデル

### TypeKind 拡張

Java 固有情報を埋め込むために型表現を以下のように拡張しています。

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Primitive(PrimitiveType),      // int, long, double など
    Boxed(PrimitiveType),          // java.lang.Integer 等
    Reference(String),             // ユーザー定義型・JDK クラス
    Optional(Box<TypeKind>),       // Null 許容型 (T?)
    Variable(TypeId),              // 推論中の型変数
    Function(Vec<TypeKind>, Box<TypeKind>),
    Array(Box<TypeKind>, usize),
    Unknown,
}
```

`PrimitiveType` は `JavaPrimitive` を内包し、`java_name()`・`boxed_fqcn()` 等のユーティリティを提供します。プリミティブとボックス型の相互変換は `jv_checker::java::primitive::JavaBoxingTable` が担当し、Null 許容時は自動的に `Boxed` へエスカレートします。

### Nullability ポリシー

- `JavaNullabilityPolicy` が Optional 化・アンボクシング時のガード挿入を制御
- `NullableGuard` と `NullabilityGuardPlanner` が制約解決結果から null 安全なガード生成をスケジュール
- Optional の内側は常に非 null とみなし、アンボクシング時にガードが足りない場合は診断を発行

### 型スキーマと一般化

- let 束縛では自由型変数を量化して `TypeScheme` を生成
- 一般化対象外 (プリミティブ/ボックス) は実行コストを抑えるために置換せず保持
- `TypeFactory` が AST 上の型注釈を `TypeKind` へ変換し、暗黙の Boxing を必要に応じて挿入

## 制約生成

`ConstraintEmitter` は AST を走査し、以下の情報を制約グラフに投影します。

- 変数宣言・関数定義: 型スキーマと型変数の生成
- 演算子適用: 左右オペランドの互換性制約と期待型処理 (`expected_type`)
- 呼び出し式: 引数ごとに Compatibility チェックを要求する `CallConstraint`
- Null 合体/安全呼び出し: `Optional` との整合を保証する制約とガード要求

制約は `ConstraintSet` に蓄積され、生成段階で Java 固有の型情報には触れず、変換は解決フェーズへ委譲します。

## 制約解決と互換性判定

`ConstraintSolver` は単一化をベースに解決を行い、最終的な型代入 `Substitution` を構築します。制約解決の際、互換性の判定は `CompatibilityChecker` を経由して Java 変換ルールを適用します。

### ConversionKind とコストモデル

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConversionKind {
    Identity,
    WideningPrimitive,
    NarrowingPrimitive,
    Boxing,
    Unboxing,
    StringConversion,
    MethodInvocation,
    Incompatible,
}
```

- `ConversionKind::cost()` で変換優先度を数値化し、最小コストのパスを採用
- Boxing/Unboxing は Null 安全ポリシーと連動し、必要であれば `NullableGuard` を挿入
- Widening では `PrimitiveWideningTable` を参照し、JLS §5.1.2 に沿った伝播を実現

### 互換性チェックの段階

1. **直接互換**: `TypeKind` が一致していれば `Identity`
2. **プリミティブ階層**: Widening/Narrowing を判定
3. **Boxing/Unboxing**: `JavaBoxingTable` を参照
4. **文字列/汎用化**: 任意型→`java.lang.String` への `StringConversion`
5. **メソッド誘導変換**: `ConversionCatalog` から候補を引き当てて `MethodInvocation` を生成
6. **不一致**: `Incompatible` を返し、ConstraintSolver が型エラーを報告

`CompatibilityResult` には `conversion_path` が含まれ、各段階で適用されたメソッド (`HelperSpec`) を追跡できます。これをテレメトリと診断に反映し、暗黙変換の可視化を行います。

## 変換カタログと SymbolIndex 連携

- `SymbolIndex::build_conversion_catalog` が JDK シグネチャを走査し、`ConversionDetector` で高信頼の変換メソッドを抽出
- `ConversionCatalog::find_conversions` がソース型→ターゲット型のメソッド候補を返却
- 推論中は `MethodInvocation` が選択された場合、`HelperSpec` に変換メソッド名 (`java.lang.Integer#valueOf` 等) を記録
- カタログ命中率 (`conversion_catalog_hits`/`misses`) をメトリクス化し、カバレッジの継続的改善に利用

## Null 安全統合

- Optional 推論は `OptionalConstraint` により最外層だけでなくネストにも適用
- アンボクシングが推論された場合、`NullabilityGuardPlanner` が ?: や `if (obj != null)` などの保護条件を要求
- Null 許容フィールドへの代入では Boxing 変換と Null ガードを同時に挿入し、Java コード生成時に `@Nullable` を付与

## テレメトリと診断

`InferenceTelemetry` は暗黙変換とガード挿入を定量化します。

```rust
pub struct InferenceTelemetry {
    pub constraints_emitted: usize,
    pub bindings_resolved: usize,
    pub inference_duration_ms: f64,
    pub widening_conversions: usize,
    pub boxing_conversions: usize,
    pub unboxing_conversions: usize,
    pub string_conversions: usize,
    pub method_invocation_conversions: usize,
    pub nullable_guards_generated: usize,
    pub conversion_catalog_hits: u64,
    pub conversion_catalog_misses: u64,
    pub conversion_events: Vec<AppliedConversion>,
    pub nullable_guards: Vec<NullableGuard>,
}
```

- CLI/LSP では「Conversion telemetry」「Helper recommendations」「Implicit conversion diagnostics」の 3 セクションを表示
- `AppliedConversion` は各変換の `ConversionKind`、適用メソッド、ソース/Java の Span を保持
- `HelperSpec` を用いた推奨メッセージ生成で、暗黙的に挿入された Java ヘルパーを開発者に提示

## テスト戦略

- ユニットテスト: `jv_checker/src/inference/**/*tests.rs` で制約生成・互換性判定・単一化を個別検証
- 統合テスト: `jv_checker/tests/inference_java_compat.rs` (新設) で Boxing/Widening/メソッド変換シナリオを網羅
- ゴールデンテスト: `jv_codegen_java/tests/implicit_conversions.rs` で暗黙変換を含む Java 出力をスナップショット比較
- パフォーマンステスト: `inference_bench.rs` の ignored テストで 10% 以内の性能維持を保証

## 今後の拡張

- ジェネリクス完全対応 (`TypeKind::Generic` と変位チェック) の導入
- Capability ベースの制約追加による型クラス的表現力の向上
- 境界キャッシュを用いたインクリメンタル推論と LSP へのリアルタイム提供
- ConversionCatalog におけるジェネリクス/配列対応の強化

## 参照

- [docs/design/java-aware-type-inference.md](java-aware-type-inference.md)
- `jv_checker/src/inference/`
- `jv_checker/src/java/`
- `jv_build/src/metadata/`
