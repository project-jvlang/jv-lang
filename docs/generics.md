# ジェネリック制約とコード生成アップデート

## where句の制約伝搬

- `where` 句に記述した境界は `ConstraintSolution` に集約され、IR の `IrTypeParameter.bounds` に格納される。
- Java 生成時は `extends`/`implements` としてシグネチャに反映される。
- 例: `fun <T> max(a: T, b: T): T where T: Comparable<T>` → `class Foo<T extends Comparable<T>> { ... }`
- Nullable な境界 (`T: Comparable<T?>` など) は `NullabilitySummary` に記録され、将来的な `@Nullable` 付与に利用される。

## 能力束縛 (Capability) の診断

- 能力が見つからない場合 (`CapabilityResolutionError::NotFound`) は、テンプレート化されたエラーメッセージを生成する。
- 曖昧な能力 (`CapabilityResolutionError::Ambiguous`) は候補一覧付きで警告する。
- テンプレートは `jv/resources/diagnostics_ja.toml` と `jv/resources/diagnostics_en.toml` で管理し、日本語/英語を切り替えられる。

## ロケール切り替え

- `GenericDiagnostics::with_locale(LocaleCode::En)` で英語を選択可能。デフォルトは日本語。
- `LocaleCode` は `jv_support::i18n` で定義され、テンプレートは `once_cell` で遅延ロードされる。

## テスト

- `jv_codegen_java/tests/generics.rs` に `class_signature_includes_generic_bounds` を追加し、`extends` 出力を検証。
- `jv_cli` の `where_constraints_flow_into_ir_bounds` で `ConstraintSolution`→IR→コード生成の流れを通しテスト。
- 診断メッセージは日本語/英語双方のテンプレート適用をユニットテスト (`translate_conflicting_argument_english_locale`) で確認。
