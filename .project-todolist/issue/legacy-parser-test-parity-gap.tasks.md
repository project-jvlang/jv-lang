# Task: Legacy Parser Test Parity Gap - Implicit Val Declaration Scope Coverage

**Task ID**: `legacy-parser-test-parity-gap`
**Related Issue**: `implicit-val-scope-limitation`
**Status**: In Progress
**Priority**: High
**Component**: `jv_parser_rowan`, `jv_ir`, `jv_codegen_java`
**Created**: 2025-11-08

---

## Overview

レガシーパーサー(`jv_parser`)からの移行時に、暗黙val宣言（`val`キーワードを省略した不変宣言）のスコープ対応テストが不足していることが判明。
現在の実装はトップレベルのみ対応しており、関数内・ブロック内などの任意のスコープでの動作が検証されていない。

**言語仕様**: `val identifier = expression` という明示宣言も、キーワードを省略した `identifier = expression` という暗黙宣言も不変変数として扱われ、最終的にJavaの `final` にコンパイルされます。

本タスクでは `val` キーワードの有無にかかわらず同じ不変性が保証されることを前提とし、明示宣言と暗黙宣言の双方がすべてのスコープで正しくローワリングされることを確認する。

**問題**: スコープの深さに依存するロジック(`is_top_level`チェック)が存在し、関数内やブロック内の暗黙val宣言が`Assignment`として誤って扱われる。

---

## Phase 1: jv_parser_rowan - パーサーレイヤー

### タスク 1.1: 包括的テストケースの追加 ✅

**ファイル**: `jv/crates/jv_parser_rowan/src/tests/val_declaration_scope_coverage.rs`

**テスト対象**:
- [x] トップレベル: `val x = 0`, `x = 0`, `val x: Int = 0`, `x: Int = 0`
- [x] 関数内: 上記4パターン
- [x] クラスメンバ: `val x = 0`（プロパティとして）
- [x] forループ内: 上記4パターン
- [x] when分岐内: `val x = 0`, `x = 0`
- [x] ラムダ内: `val x = 0`, `x = 0`
- [x] 深くネストした構造: 関数→for→when→ラムダ内での`x = 0`
- [x] 複数のネストレベルで異なる宣言パターン

**期待される結果**:
```rust
// 全てのスコープで
val x = 0      → ValDeclaration { origin: ExplicitKeyword, type_annotation: None }
x = 0          → ValDeclaration { origin: Implicit, type_annotation: None }
val x: Int = 0 → ValDeclaration { origin: ExplicitKeyword, type_annotation: Some("Int") }
x: Int = 0     → ValDeclaration { origin: ImplicitTyped, type_annotation: Some("Int") }
```

**実装**: ✅ 完了 (2025-11-08)

---

### タスク 1.2: テスト実行と失敗箇所の特定 ✅

**コマンド**:
```bash
cd jv
cargo test --lib -p jv_parser_rowan val_declaration_scope_coverage
```

**実行結果** (2025-11-08):
- ❌ 関数内の`x = 0`テスト（`is_top_level`チェックにより`Assignment`として扱われる） - **修正済み** ✅
- ❌ forループ内の`x = 0`テスト - **修正済み** ✅
- ❌ when分岐内の`x = 0`テスト - **パーサー問題（BindingPattern未生成）** 🔴
- ❌ ラムダ内の`x = 0`テスト - **パーサー問題（BindingPattern未生成）** 🔴
- ❌ 深くネストした構造内の`x = 0`テスト - **テストナビゲーション問題** 🟡

**成功したテスト**:
- ✅ トップレベルの全パターン（4件）
- ✅ 関数内の全パターン（4件） - **修正により成功** ✅
- ✅ クラスメンバ（1件）
- ✅ forループ内の全パターン（4件） - **修正により成功** ✅
- ✅ when分岐内の`val x = 0`（1件）
- ✅ ラムダ内の`val x = 0`（1件）
- ✅ 複数ネストレベル（1件）

**最終結果**: 16/19 tests passing (84.2%)

**タスク**:
1. ✅ テストを実行
2. ✅ 失敗するテストケースを記録
3. ✅ 失敗理由を分析

---

### タスク 1.3: lower_assignment関数の修正 ✅

**ファイル**: `jv/crates/jv_parser_rowan/src/lowering/statements.rs`

**修正完了** (2025-11-08)

**修正前の問題コード** (484-507行):
```rust
let is_top_level = node.parent().map_or(false, |parent| {
    if parent.kind() != SyntaxKind::StatementList {
        return false;
    }
    parent.parent()
        .map(|grand| grand.kind() == SyntaxKind::Root)
        .unwrap_or(false)
});

if is_top_level {  // ← この条件が不要
    if let BindingPatternKind::Identifier { name, .. } = &pattern {
        return Ok(Statement::ValDeclaration {
            origin: ValBindingOrigin::Implicit,
            ...
        });
    }
}
```

**修正後のコード** (484-497行):
```rust
// 暗黙val宣言: `x = 0` の形式（型注釈なし、valキーワードなし）
// binding_patternが存在する場合は、スコープに関係なく暗黙valとして扱う
if let BindingPatternKind::Identifier { name, .. } = &pattern {
    let modifiers = Modifiers::default();
    return Ok(Statement::ValDeclaration {
        name: name.clone(),
        binding: Some(pattern),
        type_annotation: None,
        initializer: value,
        modifiers,
        origin: ValBindingOrigin::Implicit,
        span,
    });
}
```

**変更内容**:
- ✅ `is_top_level`チェックロジック（484-492行）を完全に削除
- ✅ スコープ依存の条件分岐を削除
- ✅ バインディングパターンの存在のみで判定するように修正
- ✅ 日本語コメントで意図を明確化

**修正理由の詳細**:

元のコード想定:
```rust
if let Some(pattern) = binding_pattern.clone() {
    // 型注釈あり: x: String = "hello"
    if let Some(type_node) = child_node(&target_node, SyntaxKind::TypeAnnotation) {
        if let Some(annotation) = lower_type_annotation_container(...) {
            if let Some(name) = pattern.first_identifier() {
                return Ok(Statement::ValDeclaration {
                    origin: ValBindingOrigin::ImplicitTyped,
                    ...
                });
            }
        }
    }

    // 型注釈なし: x = 1
    // スコープに関係なく暗黙val宣言として扱う
    if let BindingPatternKind::Identifier { name, .. } = &pattern {
        return Ok(Statement::ValDeclaration {
            name: name.clone(),
            binding: Some(pattern),
            type_annotation: None,
            initializer: value,
            modifiers: Modifiers::default(),
            origin: ValBindingOrigin::Implicit,
            span,
        });
    }
}

// ここに到達するのは binding_pattern が None の場合のみ
// つまり、obj.field = 1 や arr[0] = 1 などの本当の代入
Ok(Statement::Assignment { ... })
```

**判定基準**:
- ✅ バインディングパターンが存在 → ValDeclaration
- ❌ バインディングパターンが存在しない（プロパティアクセスなど） → Assignment

**修正手順**:
1. 484-492行の`is_top_level`判定ロジックを削除
2. 494-507行の`if is_top_level`条件を削除
3. バインディングパターンが存在する場合は無条件で`ValDeclaration`を返すように修正
4. コメントを追加して意図を明確化

---

### タスク 1.4: テストの再実行と検証 🔄

**コマンド**:
```bash
cd jv
cargo test --lib -p jv_parser_rowan val_declaration_scope_coverage
```

**実行結果** (2025-11-08):
- ✅ **16/19 tests passing** (84.2%)
- ✅ 主要なval宣言（明示・暗黙）の問題は修正済み（関数内、forループ内）
- 🔴 **3 tests failing** - パーサーレベルの問題

**失敗テストの詳細**:

1. **when分岐内_x_equals_0** 🔴
   - **問題**: `binding_pattern: None`
   - **原因**: パーサー（syntax layer）がwhen arm内で`BindingPattern`ノードを生成していない
   - **エラー**: `Assignment { binding_pattern: None, ... }` (should be `ValDeclaration`)

2. **ラムダ内_x_equals_0** 🔴
   - **問題**: `binding_pattern: None`
   - **原因**: パーサー（syntax layer）がlambda body内で`BindingPattern`ノードを生成していない
   - **エラー**: `Assignment { binding_pattern: None, ... }` (should be `ValDeclaration`)

3. **深くネストした関数内_x_equals_0** 🟡
   - **問題**: テストナビゲーションロジックエラー
   - **原因**: テストコードがネスト構造を正しく探索できていない

**パーサーレベルの問題**:

when armとlambda bodyで`x = 0`を書いたときに、CST(Concrete Syntax Tree)レベルで`BindingPattern`ノードが生成されていない。これにより、loweringレイヤーでは`binding_pattern: None`となり、暗黙valとして扱えない。

**追加タスク**: タスク1.5として分離（パーサー修正）

**追加検証**:
```bash
# 全パーサーテストの実行
cargo test --lib -p jv_parser_rowan

# 特に既存のval宣言関連テスト
cargo test --lib -p jv_parser_rowan 暗黙val宣言を代入構文から復元する
cargo test --lib -p jv_parser_rowan 暗黙型付きval宣言のメタデータを保持する
```

---

### タスク 1.5: パーサー修正（BindingPattern生成） ✅ (2025-11-10 完了)

**優先度**: Medium (whenとlambdaは使用頻度が高い)

**ステータス**: val宣言完了 (2025-11-10)

**修正前の症状**:
- when分岐およびlambda本文での `x = 0` が `Assignment { binding_pattern: None }` となり、`ValDeclaration` に変換されない
- 深くネストしたケースではテスト側のナビゲーションが `Lambda` 経路を追跡できず失敗

**最終的な原因**:
- Rowan層では`BindingPattern`ノードが生成されているものの、Expression Parser (`parse_lambda_statement`) が代入式を再構築する際に binding 情報を破棄し、常に `binding_pattern: None` を割り当てていた
- when arm の本文が `Lambda` で表現されるケースをテストコードが考慮していなかった

**対応内容の概要**:
- `parse_lambda_statement` に `try_parse_implicit_val_statement` を追加し、左辺が単純識別子（任意で型注釈付き）の `=` 文を暗黙/暗黙型付き `ValDeclaration` として生成
- 型注釈付き `x: Int = 0` を `ValBindingOrigin::ImplicitTyped` で保持するよう対応
- `深くネストした関数内_x_equals_0` テストを when arm が `Lambda` を返す経路にも対応するよう調整し、ASTのトラバースを安定化
- 今後: `var` 宣言（明示、型注釈付き）の同等ケースを検証し、必要に応じてローワリングを拡張する

---

## 調査記録 (2025-11-09)

### 📚 参考ドキュメント

**重要**: [`docs/design/rowan-parser-pipeline.md`](../../../docs/design/rowan-parser-pipeline.md) - Rowan Parser Pipeline 技術仕様

このドキュメントには、パーサーの内部構造とフローが詳細に記載されています：

#### パイプライン全体構造
- **5ステージ構成** (14-23行目): 字句解析→プリプロセス→Rowan解析→ローワリング→セマンティクス
- **Rowan解析ステージ** (20行目): `ParserContext` と戦略レジストリが `ParseEvent` 列を生成
- **ローワリングステージ** (21行目): `SyntaxNode` を `jv_ast::Statement` 群へ写像

#### パーサー内部構造（タスク1.5に直接関連）
- **ParserContext** (40-46行目):
  - `cursor` と `tokens`: 現在位置と解析対象トークン列
  - `events`: `ParseEvent` を逐次蓄積（後段の `ParseBuilder` が再生）
  - `block_depth` と `ExpressionState` スタック: **when ブロック、ラムダ、波括弧の入れ子を追跡**

- **ステートメント戦略レジストリ** (48-50行目):
  - `parser::strategies::registry()` が静的配列を返す
  - 適用順序: `package`→`import`→`val/var`→`fun`→`class`→制御構造→リソース管理→**代入**→式
  - **最初にマッチした戦略が `parse` を実行**

- **式解析と同期戦略** (52-59行目):
  - `ParserContext::parse_expression_until` が式解析の中心
  - `ExpressionState` が **`when` ブロック検出と `else` 同期判断を担う**
  - `when` の `{` で専用ブロックを開始し、`}` でスタックを戻す

- **ParseBuilder と Green ツリー構築** (68-74行目):
  - `ParseEvent` は `StartNode` / `FinishNode` / `Token` / `Error` の4種類
  - イベント列を順に適用し `rowan::GreenNode` を生成

#### ローワリング層（タスク1.5の失敗箇所）
- **LoweringContext** (77-82行目):
  - `TokenStore` を内部に持ち、Rowan トークンと元の `jv_lexer::Token` を対応付け
  - `tokens_for(node)` でノード配下のトークン列を取得
  - `span_for(node)` が `Span` を合成

- **ステートメント抽出フロー** (83-89行目):
  - `lower_program` は `collect_statements_from_children` を通じて `SyntaxKind::StatementList` を走査
  - **`is_top_level_statement` が対象ノードをフィルタリング** ← 潜在的な問題箇所？
  - `process_candidate` が `lower_single_statement` を呼ぶ

#### タスク1.5との関連性

1. **ExpressionState と when ブロック追跡** (52-59行目):
   - when分岐内のブロックが `ExpressionState` によって特別扱いされている可能性
   - `when` の `{` で専用ブロックを開始 → **式コンテキストとして扱われる可能性**

2. **ステートメント戦略の適用順序** (48-50行目):
   - 代入戦略は制御構造の後に適用される
   - when分岐内で別の戦略が先にマッチする可能性

3. **is_top_level_statement フィルタリング** (83-89行目):
   - ステートメント抽出時のフィルタリングロジック
   - when分岐内のステートメントが除外される可能性

4. **ParseEvent のバランス** (68-74行目):
   - `StartNode` / `FinishNode` のバランスが崩れると構造が異常になる
   - when分岐内で `BindingPattern` の `StartNode` が欠落している可能性

#### 調査への示唆

このドキュメントから、以下の調査方向が有力：

1. **式コンテキスト問題**: when分岐のボディが `ExpressionState` によって式として扱われ、その中の代入文が異なる方法でパースされている
2. **イベント生成問題**: when分岐内で `AssignmentStrategy.parse()` が `BindingPattern` の `StartNode`/`FinishNode` イベントを正しく生成していない
3. **lowering フィルタリング問題**: `is_top_level_statement` がwhen分岐内のステートメントを誤って扱っている

---

### 調査したコンポーネント

#### 1. AssignmentStrategy (`jv/crates/jv_parser_rowan/src/parser/strategies/assignment.rs`)

**実装状況**: ✅ 正常
- `parse_assignment_target()` (63-98行目): 識別子の場合に`ctx.parse_binding_pattern()`を正しく呼び出している
- ロジック:
  ```rust
  Some(TokenKind::Identifier) => {
      if ctx.peek_significant_kind_n(1) == Some(TokenKind::Dot) {
          // プロパティアクセス: obj.field
          // BindingPatternを生成しない
      } else {
          // 単純な識別子: x
          ctx.parse_binding_pattern();  // ← 88行目
      }
  }
  ```
- **結論**: AssignmentStrategyの実装に問題なし

#### 2. Whenブランチのパース処理 (`jv/crates/jv_parser_rowan/src/parser/strategies/control.rs`)

**実装状況**: ✅ 正常
- `parse_when_branch_body()` (109-145行目): `{}`で始まる場合に`ctx.parse_block()`を呼び出している
- ロジック:
  ```rust
  match ctx.peek_significant_kind() {
      Some(TokenKind::LeftBrace) => {
          ctx.parse_block();  // ← 113行目
      }
      Some(_) => {
          ctx.parse_expression_until(...);
      }
  }
  ```
- **結論**: when分岐のブロックは正しくステートメントリストとしてパースされている

#### 3. Block解析フロー (`jv/crates/jv_parser_rowan/src/parser/context.rs`)

**実装状況**: ✅ 正常
- `parse_block()` (221行目) → `parse_braced_statements()` (230行目) → `parse_statement_list()` (130行目)
- `parse_statement_list()`: ストラテジーレジストリをループして適切なストラテジーを選択
- **結論**: ブロック内のステートメント解析フローに問題なし

#### 4. Lowering層 (`jv/crates/jv_parser_rowan/src/lowering/statements.rs`)

**実装状況**: ✅ 正常（タスク1.3で修正済み）
- `lower_assignment()` (426-506行目): `BindingPattern`ノードの存在をチェック
- ロジック:
  ```rust
  let (target, binding_pattern) =
      if let Some(pattern_node) = child_node(&target_node, SyntaxKind::BindingPattern) {
          // BindingPatternが存在する場合
          (expr, Some(pattern))
      } else {
          // BindingPatternが存在しない場合
          (target, None)  // ← ここでNoneになる
      };
  ```
- **結論**: loweringロジックは正しく、問題はCST構造にある

### 重要な発見

#### 🟢 サブタスク1.5.1: CST構造のダンプ結果 (2025-11-09)

**デバッグテストの追加**:
- ファイル: `jv/crates/jv_parser_rowan/src/tests/val_declaration_scope_coverage.rs`
- 関数: `dump_cst_structure()`, `dump_node()`, デバッグテスト3件

**CST構造の比較結果**:

1. **関数内 `x = 0` (成功ケース)**:
```
kind: AssignmentStatement
  kind: AssignmentTarget
    kind: BindingPattern  // ← 存在する！
      text: "x"
  kind: Expression
    text: "0"
```

2. **when分岐内 `{ x = 0 }` (失敗ケース)**:
```
kind: WhenBranch
  kind: Block
    kind: StatementList
      kind: AssignmentStatement
        kind: AssignmentTarget
          kind: BindingPattern  // ← 存在する！
            text: "x"
        kind: Expression
          text: "0"
```

3. **lambda内 `x = 0` (失敗ケース)**:
```
kind: ValDeclaration
  kind: InitializerClause
    kind: Expression
      text: "{"  // ← ここで切れている！
```

**重要な発見**:
- ✅ **when分岐内のCST構造は完全で、`BindingPattern`ノードが正しく生成されている**
- ❌ **lambda内のCST構造が不完全**（lambda bodyの内容が欠落）

#### 🔴 サブタスク1.5.3: 根本原因の特定 (2025-11-09)

**調査ファイル**: `jv/crates/jv_parser_rowan/src/lowering/statements.rs`

**問題箇所**: `parse_lambda_statement` (1705-1753行)

```rust
fn parse_lambda_statement(
    &self,
    slice: &[&'a Token],
    absolute_start: usize,
) -> Result<Statement, ExpressionError> {
    // ...
    _ => {
        if let Some(assign_index) = Self::find_top_level_assign(slice) {
            let target_tokens = &slice[..assign_index];
            let value_tokens = &slice[assign_index + 1..];
            // ...
            let target = Self::parse_nested_expression(target_tokens)?.expr;
            let value = Self::parse_nested_expression(value_tokens)?.expr;
            Ok(Statement::Assignment {
                target,
                binding_pattern: None,  // ← 問題：強制的にNone
                value,
                span,
            })
        }
    }
}
```

**根本原因**:
- **Expression Parser**（statements.rs内のexpressionパーサー）が、lambda body内とwhen arm body内のstatementをパースする際、`binding_pattern: None`を強制的に設定している
- これは、expression parserレベルで`BindingPattern`の概念を処理していないため

**なぜwhen分岐内でもBindingPatternがNoneになるのか**:
- CST構造では`BindingPattern`ノードが存在するが、when armのbodyがexpression parserで処理される際（2070-2098行: `parse_lambda_body_as_block`）、`parse_lambda_statement`が呼ばれて`binding_pattern: None`が設定される

**二重パース問題**:
1. **Syntax Layer** (Rowan CST): `BindingPattern`ノードを正しく生成 ✅
2. **Expression Parser** (Lowering層内): CST構造を無視して独自にASTを生成し、`binding_pattern: None`を強制 ❌

### テスト結果

```bash
cargo test --lib -p jv_parser_rowan val_declaration_scope_coverage
```

**結果**: 16/19 tests passing (84.2%)

**失敗テスト**:
1. ❌ `when分岐内_x_equals_0`: `binding_pattern: None`
2. ❌ `ラムダ内_x_equals_0`: `binding_pattern: None`
3. 🟡 `深くネストした関数内_x_equals_0`: テストナビゲーションエラー

**成功テスト例**:
- ✅ `関数内_x_equals_0`: 正常動作
- ✅ `forループ内_x_equals_0`: 正常動作
- ✅ `トップレベル_x_equals_0`: 正常動作

---

## 次のステップ（サブタスク）

### サブタスク 1.5.1: CST構造のダンプと比較 🔍

**目的**: 関数内とwhen分岐内のCST構造を比較し、`BindingPattern`ノードが欠落する箇所を特定する

**実施内容**:
1. テストコードにデバッグ出力を追加して、CST構造を完全にダンプ
2. 成功ケース（関数内）と失敗ケース（when分岐内）のCST構造を比較
3. `AssignmentTarget`ノードの子要素を詳細に確認
4. `BindingPattern`ノードが生成されるべき箇所を特定

**必要な作業**:
```rust
// テストコードに追加
let parse_result = parse_source(r#"
when (value) {
    1 -> {
        x = 0
    }
}
"#);
eprintln!("=== CST Structure ===");
eprintln!("{:#?}", parse_result.green_node);
```

**期待される成果**: CST構造の違いを可視化し、問題箇所を正確に特定

---

### サブタスク 1.5.2: パーサーデバッグ出力の追加 ✅ (2025-11-10)

**ステータス**: 完了（デバッグ用イベントダンプを追加し、when分岐およびラムダ式の挙動を確認）

**実施内容**:
- `jv/crates/jv_parser_rowan/src/tests/val_declaration_scope_coverage.rs` に`dump_parser_events`ヘルパーを追加
- `debug_parser_events_function_success` / `debug_parser_events_when_failure` / `debug_parser_events_lambda_failure`の3つのデバッグテスト（`#[ignore]`）を追加し、`--ignored --nocapture`で実行可能にした
- 解析イベント（`ParseEvent`）とトークン列を詳細にダンプし、BindingPatternイベントの有無を確認

**取得ログの要約**:
- ✅ 関数内 `x = 0`: `AssignmentStatement`→`AssignmentTarget`→`BindingPattern`イベントが生成され、期待通り
- ✅ when分岐内 `{ x = 0 }`: `BindingPattern`イベントが存在し、Rowanレイヤーでは情報が失われていないことを確認
- ❌ ラムダ内 `{ -> x = 0 }`: `ParseEvent`列が単なるトークン列として扱われ、`AssignmentStatement`/`BindingPattern`が生成されない。Expression parserがラムダボディを式として処理し、暗黙valへの変換ロジックに到達しないことを再現

**検証コマンド**:
```bash
cd jv
cargo test --lib -p jv_parser_rowan debug_parser_events_when_failure -- --ignored --nocapture
cargo test --lib -p jv_parser_rowan debug_parser_events_lambda_failure -- --ignored --nocapture
```

**結論**:
- when分岐ではRowanパーサー段階で`BindingPattern`が生成されており、問題は後段（expression parser）の再構築過程にある
- ラムダ式ではRowanパーサー自体が`AssignmentStatement`を構築しておらず、暗黙val宣言の認識が欠落していることを確認

**成果物**: デバッグ出力により、RowanイベントとExpression Parserのギャップが定量的に把握できる状態になった。タスク1.5.3の分析結果の裏付けとして活用可能

---

### サブタスク 1.5.3: When式とLambda式のLowering詳細調査 ✅ (完了)

**調査完了日**: 2025-11-09

**調査結果**: 上記「根本原因の特定」セクションを参照

---

## 追加の仮説（検証済み）

### 仮説1: Expression Context vs Statement Context ✅ 確認
when分岐のボディは**式コンテキスト**でパースされており、ブロック式として扱われる。
内部のステートメントは`parse_lambda_body_as_block`→`parse_lambda_statement`で処理され、expression parserが独自にASTを生成する。

**検証結果**: 仮説1が正しいことを確認（2025-11-09）

### 仮説2: WhenブランチのCST構造の特殊性 ❌ 棄却
`WhenBranch`ノード内の`Block`ノードは正常で、`BindingPattern`ノードも正しく生成されている。

**検証結果**: CST構造に問題なし（サブタスク1.5.1で確認）

### 仮説3: Loweringレイヤーでのパス分岐 ✅ 確認
when式とlambda式のlowering時に、`parse_lambda_body_as_block`が呼ばれ、その中で`parse_lambda_statement`がCST構造を無視して独自にASTを生成する。

**検証結果**: 仮説3が正しいことを確認（2025-11-09）

---

## 修正実施 (2025-11-10)

### 実装概要
- `parse_lambda_statement` の代入分岐に新規ヘルパー `try_parse_implicit_val_statement` を導入し、左辺が識別子（任意で型注釈付き）の場合は `ValDeclaration` を生成
- 型注釈が存在する場合は `lower_type_annotation_from_tokens` を再利用して `ValBindingOrigin::ImplicitTyped` を設定、型エラーは `ExpressionError` 経由で報告
- 通常の代入（プロパティアクセスや添字付き）はこれまで通り `Statement::Assignment` として処理
- `深くネストした関数内_x_equals_0` テストで when arm が `Lambda` を返す経路にも対応するように分岐を刷新

### 変更ファイル
- `jv/crates/jv_parser_rowan/src/lowering/statements.rs`
  - `parse_lambda_statement` から暗黙val判定を抽出し、`Modifiers::default()` と `ValBindingOrigin` を適切に設定
  - 型注釈付きの暗黙val (`x: Int = 0`) を `ImplicitTyped` として扱う分岐を追加
- `jv/crates/jv_parser_rowan/src/tests/val_declaration_scope_coverage.rs`
  - 深いネストの検証で when arm の本文が `Lambda` でも `Block` でも成立するようトラバースを柔軟化

### テスト結果
```bash
cd jv
cargo test --lib -p jv_parser_rowan val_declaration_scope_coverage
```
→ 19/19 テスト成功（失敗していた when 分岐・ラムダ内・深いネストのケースが全て緑化）

**実装日時**: 2025-11-10  
**実装者**: AI Agent (Codex)

---

## Phase 2: jv_ir - IRトランスフォーメーションレイヤー

### タスク 2.1: IR変換テストケースの追加 📝

**ファイル**: `jv/crates/jv_ir/src/tests/val_declaration_transform_coverage.rs`（新規作成）

**テスト対象**:
- [x] トップレベルval宣言（暗黙/明示）のIR変換
- [x] トップレベルvar宣言（`var`キーワード使用）のIR変換
- [x] 関数内のval（暗黙/明示）およびvar宣言のIR変換
- [x] ネストしたスコープでのval/var変換
- [x] valが不変、varが可変として正しく表現されることの検証

**対応内容**:
- `jv/crates/jv_ir/src/tests/val_declaration_transform_coverage.rs` を新規作成し、トップレベル・関数スコープ・`when`ブロック内の暗黙/明示 `val` および `var` 変換を網羅的に検証。
- 変換結果の `IrStatement::VariableDeclaration` を再帰的に探索するヘルパーを追加し、各スコープで `is_final` と `IrModifiers::is_final` の整合性を確認。

**期待される変換**:
```jv
x = 1          // jv source
↓ AST
ValDeclaration { origin: Implicit, ... }
↓ IR Transform
// IRで不変性が保持されている
```

**テストケース例**:
```rust
#[test]
fn 関数内暗黙val宣言がIRで保持される() {
    let ast = create_ast("fun test() { x = 1 }");
    let ir = transform_to_ir(ast);

    // IRレベルで暗黙val宣言が保持されていることを検証
    // ValBindingOrigin::Implicitが維持されている
    assert_val_declaration_in_ir(ir, "x", ValBindingOrigin::Implicit);
}
```

---

### タスク 2.2: IR変換の実行と検証 🔄

**コマンド**:
```bash
cd jv
cargo test --lib -p jv_ir val_declaration_transform_coverage
```

**検証項目**:
- [x] AST→IR変換時に`ValBindingOrigin`および可変性フラグが保持される
- [x] IRレベルでスコープに関係なく val/var 宣言が正しく扱われる
- [x] IR変換後も不変性・可変性情報が失われない

**実行結果 (2025-11-08)**:
- `cargo test --lib -p jv_ir val_declaration_transform_coverage` を実行し、追加した3ケースが全て成功（トップレベル/関数/ネストスコープ）。

**想定される問題**:
- IR変換レイヤーでスコープ依存の処理があるか確認
- `ValBindingOrigin::Implicit`が正しく伝播されるか確認

---

### タスク 2.3: IR変換の修正（必要な場合） 📝

**ファイル**: `jv/crates/jv_ir/src/transform/*.rs`

**確認事項**:
- [x] `ValBindingOrigin`と可変性がIR変換で正しく保持される
- [x] スコープに依存する不正なロジックが存在しないか
- [x] val/var宣言の不変性・可変性がIRレベルで表現できているか

**結論**:
- 新規テストにより既存の `desugar_val_declaration`/`desugar_var_declaration` 実装が全スコープで不変性・可変性を正しく保持していることを確認。追加修正は不要。

**修正が必要な場合**:
1. スコープ依存のロジックを削除
2. `ValBindingOrigin`情報を正しく伝播
3. テストで検証

---

## Phase 3: jv_codegen_java - Javaコード生成レイヤー

### タスク 3.1: Java生成テストケースの追加 📝

**ファイル**: `jv/crates/jv_codegen_java/src/tests/val_declaration_codegen_coverage.rs`（新規作成）

**テスト対象**:
- [ ] トップレベル暗黙val → Java `final`変数
- [ ] トップレベルvar → Java 可変ローカル/フィールド
- [ ] 関数内の暗黙val/明示var → 適切なローカル変数宣言
- [ ] ネストしたスコープ内のval/var → 適切なJavaコード生成
- [ ] 型推論されたJava型および可変性修飾が正しいか

**期待されるJava生成**:
```jv
// jv source
fun test() {
    x = 1
    name: String = "hello"
    var y = 2
    var total: Int = 3
}
```

```java
// Generated Java
public void test() {
    final int x = 1;           // Implicitからfinal付き
    final String name = "hello"; // ImplicitTypedからfinal付き
    int y = 2;                  // varは非final
    int total = 3;              // 型付きvar
}
```

**テストケース例**:
```rust
#[test]
fn 関数内暗黙val宣言がfinalローカル変数として生成される() {
    let jv_source = r#"
fun test() {
    x = 1
}
"#;
    let java_code = compile_to_java(jv_source);

    assert!(java_code.contains("final int x = 1"));
    assert!(!java_code.contains("int x = 1;")); // finalなしは不可
}

#[test]
fn 深くネストした暗黙val宣言もfinalになる() {
    let jv_source = r#"
fun test() {
    for (i in 0..10) {
        x = i * 2
    }
}
"#;
    let java_code = compile_to_java(jv_source);

    assert!(java_code.contains("final int x = i * 2"));
}
```

---

### タスク 3.2: Java生成の実行と検証 🔄

**コマンド**:
```bash
cd jv
cargo test --lib -p jv_codegen_java val_declaration_codegen_coverage
```

**検証項目**:
- [ ] `ValBindingOrigin::Implicit` → Java `final`変数
- [ ] `ValBindingOrigin::ImplicitTyped` → Java `final`変数（型注釈あり）
- [ ] `ValBindingOrigin::ExplicitKeyword` → Java `final`変数
- [ ] スコープに関係なく全て`final`が付与される

**想定される問題**:
- コード生成でスコープ依存のロジックがあるか
- `ValBindingOrigin`から`final`修飾子への変換が正しいか
- 型推論がスコープに関係なく動作するか

---

### タスク 3.3: コード生成の修正（必要な場合） 📝

**ファイル**: `jv/crates/jv_codegen_java/src/*.rs`

**確認事項**:
- [ ] `ValDeclaration`は全て`final`変数として生成される
- [ ] `ValBindingOrigin`の種類に関わらず不変性が保証される
- [ ] スコープ依存の不正なロジックが存在しないか

**修正が必要な場合**:
1. `ValDeclaration`を`final`付きJava変数として生成
2. スコープ依存ロジックを削除
3. 型推論結果を正しくJava型に変換
4. テストで検証

---

## Phase 4: 統合テスト

### タスク 4.1: エンドツーエンド統合テスト 📝

**ファイル**: `jv/crates/jv_cli/tests/integration/implicit_val_scope_coverage.rs`（新規作成）

**テスト対象**:
- [ ] jv source → AST → IR → Java生成の全フロー
- [ ] 生成されたJavaコードのコンパイル可能性
- [ ] 実行時の動作確認

**テストケース例**:
```rust
#[test]
fn エンドツーエンド_関数内暗黙val宣言() {
    let jv_source = r#"
fun main() {
    x = 42
    println(x)
}
"#;

    // jvコンパイル → Java生成
    let output = Command::new("jv-minimal")
        .args(&["build", "test.jv", "--preview"])
        .output()
        .expect("failed to execute jv build");

    let java_code = String::from_utf8(output.stdout).unwrap();

    // 期待されるJavaコード
    assert!(java_code.contains("final int x = 42"));

    // Javaコンパイル確認（javac）
    assert!(compile_java_code(&java_code).is_ok());
}
```

---

### タスク 4.2: リグレッションテスト 🔄

**既存テストの全実行**:
```bash
cd jv

# 全クレートのテスト
cargo test --workspace

# 特に重要なクレート
cargo test -p jv_parser_rowan
cargo test -p jv_ir
cargo test -p jv_codegen_java
cargo test -p jv_checker
```

**確認項目**:
- [ ] 既存の全テストが引き続き成功
- [ ] 新しいテストが全て成功
- [ ] パフォーマンスの劣化がない

---

## 完了基準（Definition of Done）

### jv_parser_rowan
- [x] テストケース追加完了 (2025-11-08)
- [x] `is_top_level`チェックが削除されている (2025-11-08)
- [x] 関数内・forループ内の暗黙val宣言が動作する (2025-11-08)
- [x] 16/19 tests passing (84.2%) - 主要バグ修正済み
- [x] when/lambda内のBindingPattern再構築問題を修正（タスク1.5）
- [x] 全テストが成功 (19/19)
- [ ] when/lambdaを含む全スコープでの `var` 宣言（明示、型注釈付き）の挙動が検証・修正済み
- [ ] 既存テストに影響がない

### jv_ir
- [ ] テストケース追加完了（val/var両対応）
- [ ] 全テストが成功
- [ ] `ValBindingOrigin`と可変性が正しく保持される
- [ ] 既存テストに影響がない

### jv_codegen_java
- [ ] テストケース追加完了（val/var両対応）
- [ ] 全テストが成功
- [ ] 全スコープでvalは`final`、varは可変として生成される
- [ ] 既存テストに影響がない

### 統合テスト
- [ ] エンドツーエンドテスト追加完了
- [ ] 生成Javaコードがコンパイル可能
- [ ] 全ワークスペーステストが成功

### ドキュメント
- [ ] 課題文書の更新（`.project-todolist/issue/implicit-val-declaration-missing-implementation.md`）
- [ ] 実装の意図をコメントで明記
- [ ] CHANGELOGへの記載

---

## 参照

- **言語仕様**: `jv/docs/language-guide-en.md`
- **AST定義**: `jv/crates/jv_ast/src/statement.rs`
- **パーサー実装**: `jv/crates/jv_parser_rowan/src/lowering/statements.rs`
- **関連コミット**: [75d5e78](https://github.com/project-jvlang/jv-lang/commit/75d5e782) (トップレベルのみ対応)
- **課題文書**: `.project-todolist/issue/implicit-val-declaration-missing-implementation.md`

---

**最終更新**: 2025-11-10
**担当**: AI Agent
**ステータス**: Phase 1 実装完了（タスク1.1-1.5）

## 現状サマリー (2025-11-10)

### ✅ 完了済み
- **タスク1.1**: 包括的テストケース追加（19件）
- **タスク1.2**: テスト実行と失敗箇所特定
- **タスク1.3**: `lower_assignment`関数修正（`is_top_level`チェック削除）
- **タスク1.4**: 失敗ケースの切り出しと暫定改善（84.2% → 後続タスクで解決）
- **タスク1.5**: when/lambda 内の暗黙valローワリングを修正し 19/19 テストをパス (2025-11-10)
  - **サブタスク1.5.1**: CST構造のダンプと比較 ✅
  - **サブタスク1.5.2**: パーサーデバッグ出力の追加 ✅
  - **サブタスク1.5.3**: When式とLambda式のLowering詳細調査 ✅
  - **実装**: `parse_lambda_statement` に暗黙val検出ロジックを追加、テストを更新

### 🔜 次のステップ
- **Task 1.5 追跡**: when / lambda など全スコープでの `var` 宣言（明示、型注釈付き）を検証し、必要なローワリング修正を実施
- **Phase 2 (jv_ir)**: 暗黙val／明示varのIR変換テスト追加と検証
- **Phase 3 (jv_codegen_java)**: Javaコード生成での `final`（val）および可変（var）表現の確認
- **ドキュメント反映**: `.project-todolist/issue/implicit-val-declaration-missing-implementation.md` および CHANGELOG の更新

### 📊 調査成果
- **根本原因特定**: Expression Parserが`binding_pattern: None`を強制設定
- **CST構造検証**: when分岐内のCST構造は正常（`BindingPattern`ノード存在）
- **二重パース問題発見**: Syntax LayerとExpression Parserで異なるAST生成
- **実装完了**: `try_parse_implicit_val_statement`による暗黙val検出で課題を解消
- **追加検証要件**: `var` 宣言についても同様の検証・ローワリング拡張が必要

### 🔬 技術的発見
- **二重パースアーキテクチャ**:
  - Syntax Layer (Rowan CST): 正しく`BindingPattern`を生成
  - Expression Parser (Lowering層): 一部コンテキストで再構築が必要
- **対応箇所**: `parse_lambda_statement` (statements.rs、型注釈対応を含む)
- **影響範囲**: when arm body、lambda body、else arm body（val系は修正済み、var系は要対応）
