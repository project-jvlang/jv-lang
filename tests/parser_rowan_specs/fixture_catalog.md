# Rowan Verification Harness Fixture Catalog (Task 4.1.1)

このカタログは `jv/tests` 配下の `.jv` フィクスチャを、Rowan 検証ハーネスで優先的に整備するカテゴリに分類したものです。各カテゴリはハーネスで確認すべき構文領域を示し、優先度が高いものから順番に取り組みます。

## P0: declaration-constructs
- カバレッジ: package/import、val/var 宣言、data/class、アノテーション
- 対象フィクスチャ:
  - `jv/tests/fixtures/data_class.jv`
  - `jv/tests/fixtures/java_annotations/pass_through.jv`
  - `jv/tests/fixtures/package/package_single_segment.jv`
  - `jv/tests/fixtures/package/package_with_class.jv`
  - `jv/tests/fixtures/package/nested_package.jv`
  - `jv/tests/fixtures/package/complex_stdlib_pattern.jv`
  - `jv/tests/lang/imports/smart_imports.jv`

既存のハーネスで検証済み: `hello_world.jv`, `02-variables.jv`, `package/simple_package.jv`, `sequence/primitive_sum_specialization.jv`

## P0: control-flow-when-positive
- カバレッジ: when/pattern マッチングの正常系（戻り値、ガード、複数アーム）
- 対象フィクスチャ（30件）:
  - `jv/tests/fixtures/pattern/example1.jv`
  - `jv/tests/fixtures/pattern/example3.jv`
  - `jv/tests/fixtures/pattern/example4.jv`
  - `jv/tests/fixtures/pattern/example5.jv`
  - `jv/tests/fixtures/pattern/example6.jv`
  - `jv/tests/fixtures/pattern/example7.jv`
  - `jv/tests/fixtures/pattern/example8.jv`
  - `jv/tests/fixtures/pattern/example9.jv`
  - `jv/tests/fixtures/pattern/example10.jv`
  - `jv/tests/fixtures/pattern/example11.jv`
  - `jv/tests/fixtures/pattern/example12.jv`
  - `jv/tests/fixtures/pattern/example13.jv`
  - `jv/tests/fixtures/pattern/example14.jv`
  - `jv/tests/fixtures/pattern/example15.jv`
  - `jv/tests/fixtures/pattern/example16.jv`
  - `jv/tests/fixtures/pattern/example17.jv`
  - `jv/tests/fixtures/pattern/example18.jv`
  - `jv/tests/fixtures/pattern/example19.jv`
  - `jv/tests/fixtures/pattern/example20.jv`
  - `jv/tests/fixtures/pattern/example21.jv`
  - `jv/tests/fixtures/pattern/example22.jv`
  - `jv/tests/fixtures/pattern/example23.jv`
  - `jv/tests/fixtures/pattern/example24.jv`
  - `jv/tests/fixtures/pattern/example25.jv`
  - `jv/tests/fixtures/pattern/example26.jv`
  - `jv/tests/fixtures/pattern/example27.jv`
  - `jv/tests/fixtures/pattern/example28.jv`
  - `jv/tests/fixtures/pattern/example29.jv`
  - `jv/tests/fixtures/pattern/example30.jv`

既存のハーネスで検証済み: `pattern/example2.jv`

## P1: control-flow-when-negative
- カバレッジ: when/pattern マッチングのエラーケースと診断
- 対象フィクスチャ（15件）:
  - `jv/tests/fixtures/pattern/neg-001.jv`
  - `jv/tests/fixtures/pattern/neg-002.jv`
  - `jv/tests/fixtures/pattern/neg-003.jv`
  - `jv/tests/fixtures/pattern/neg-004.jv`
  - `jv/tests/fixtures/pattern/neg-005.jv`
  - `jv/tests/fixtures/pattern/neg-006.jv`
  - `jv/tests/fixtures/pattern/neg-007.jv`
  - `jv/tests/fixtures/pattern/neg-008.jv`
  - `jv/tests/fixtures/pattern/neg-009.jv`
  - `jv/tests/fixtures/pattern/neg-010.jv`
  - `jv/tests/fixtures/pattern/neg-011.jv`
  - `jv/tests/fixtures/pattern/neg-012.jv`
  - `jv/tests/fixtures/pattern/neg-013.jv`
  - `jv/tests/fixtures/pattern/neg-014.jv`
  - `jv/tests/fixtures/pattern/neg-015.jv`

## P1: dsl-and-sequences
- カバレッジ: 空白区切り配列 DSL、シーケンス専用構文、文字列補間 DSL
- 対象フィクスチャ:
  - `jv/tests/fixtures/sequence/list_literal_sequences.jv`
  - `jv/tests/fixtures/sequence/sequence_chain.jv`
  - `jv/tests/fixtures/sequence/java21_compat.jv`
  - `jv/tests/lang/strings/sequence_interpolation.jv`
  - `jv/tests/performance/phase1.jv`

既存のハーネスで検証済み: `sequence/primitive_sum_specialization.jv`
