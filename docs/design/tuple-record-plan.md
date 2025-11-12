# タプルリテラル内部設計ノート

このドキュメントはタプルリテラルと多値返却に関する内部アーキテクチャを開発者向けにまとめたものです。言語仕様の使い方は `docs/language/tuple.md` を参照してください。

## 字句解析: コメントからフィールド名メタ情報へ

- `jv_lexer` は行末コメント (`// label`) とブロックコメント (`/* label */`) のうち 1 語の識別子を抽出し、`FieldNameLabel` トークンを生成する。
- コメントが有効な識別子でない場合は診断を発行し、通常コメントとして扱う。
- 抽出したラベルは `primary` / `secondary` / `identifier_hint` として優先順位を持たせたうえで、直前の式との距離を 1 トークン以内に制約する。

## 構文解析: `TupleFieldMeta`

- `jv_parser` は `(expr expr …)` 形式を `Expr::Tuple` として構築し、各要素に `TupleFieldMeta` を付与する。
- `TupleFieldMeta` には以下の情報を保持する。
  - `primary_label`: コメントから得た最優先のフィールド名。
  - `secondary_labels`: 同じ式に複数コメントが付いた場合の候補群。
  - `identifier_hint`: `val foo = ...` のように識別子から抽出した候補。
  - `fallback_index`: コメントや識別子が無い場合に `_i` として採用する番号。
- 空タプルや要素数 1 の括弧は構文エラーとして診断される。

## 型推論と `TupleRecordPlan`

- `jv_checker` は AST から `TupleRecordPlan` を生成してタプル使用箇所を集約する。
- `TupleRecordPlan` には以下が含まれる。
  - `strategy`: Specific（関数戻り値専用レコード）または Generic（共有レコード）。
  - `specific_name`: `Divmod_Result` のような PascalCase 名（必要に応じて生成）。
  - `generic_name`: `Tuple{arity}_{type hints}` 形式（例: `Tuple3_Int_Int_Int`）。
  - `fields`: `TupleFieldMeta` の正規化結果。
  - `type_hints`: それぞれの要素に対する型推論ヒント（未知は `Unknown`）。
  - `usage_sites`: 参照されたソース位置とコンテキスト (`FunctionReturn`, `BindingInitializer` など)。
- Specific 戦略は「単一関数の戻り値でのみタプルが使われている場合」に採用される。複数箇所で共有されている場合は Generic になる。
- `TupleTypeDescriptor` による注釈 `(Int String)` などは `type_hints` を上書きし、Java 型決定を安定させる。

## IR 変換と分割代入展開

- `TransformContext` は `TupleRecordPlan` と `TypeFacts` を取り込み、IR へ変換する際にタプル情報を参照できるようにする。
- 分割代入 (`val (x y) = tuple`) は IR で以下のステップに展開される。
  1. タプル式を一時変数に束縛。
  2. `tuple_plan` に基づいてフィールドアクセス (`_1()`, `quotient()`) を構築。
  3. 各変数へ代入するステートメントに展開。
- タプル→レコード型のマッピングは `TransformContext::register_tuple_plan_usage` で管理される。

## Java コード生成

- `jv_codegen_java::record::collect_tuple_records` が `TupleRecordPlan` からレコード定義を 1 度だけ生成する。
- フィールド名は以下の優先度で決定される。
  1. コメント。空白や複合語はトリムされ、空なら次へ。
  2. 識別子ヒント (`identifier_hint`)。
  3. フォールバック番号 (`_1`, `_2`, …)。
- Specific 戦略では PascalCase のレコード名とコメントベースのフィールド名をそのまま使う。Generic 戦略では `_1`, `_2` などに固定。
- `JavaCodeGenerator` は IR で展開された `FieldAccess` を Java レコードのアクセサ呼び出しに変換し、コメントラベルの優先順位を尊重する。

## テスト

- ゴールデンテスト: `jv/tests/integration/tuple_literal.rs` が `tests/data/tuple/expected/java21|java25` を検証。
- `jv_codegen_java/tests/tuple.rs` では `TupleRecordPlan` のフィールド名解決やリテラル生成をユニットテストしている。
- `jv_checker/tests/tuple.rs` は Specific/Generic 戦略の分岐や `type_hints` の反映をカバーする。

## 今後の拡張ポイント

- `_` ワイルドカードや部分的分割代入のさらなるパターン対応。
- `TupleRecordPlan` の差分アナライザを追加し、大規模コードベースでの重複レコード検出を最適化。
- Java 21 ターゲット時に `_1()` のようなアクセサを補助するユーティリティの自動生成。

実装の詳細は各 crate のソース（`jv_lexer`, `jv_parser`, `jv_checker`, `jv_codegen_java`）を参照してください。
