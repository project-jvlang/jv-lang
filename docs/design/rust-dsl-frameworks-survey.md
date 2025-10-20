# Rust DSLベースフレームワーク調査報告

## 調査概要

**調査日**: 2025-10-20
**調査対象**: RustにおけるDSL/ルールエンジン/パーサー関連フレームワーク
**目的**: jv言語プロジェクトにおける技術選定のための情報収集

本報告書では、以下の5つのRustフレームワークについて調査を実施しました：

1. **rule-rs** - JSONベースルールエンジン
2. **rhai** - 組み込みスクリプト言語
3. **pest** - PEGパーサージェネレーター
4. **tree-sitter** - インクリメンタルパーサー
5. **shy** - Shunting Yardルールエンジン

---

## 1. rule-rs

### 概要
**GitHub**: https://github.com/tclh123/rule-rs
**カテゴリ**: JSONベースルールエンジン
**作者**: tclh123

rule-rsは、JSONまたはRustオブジェクトを使用してルールを定義し評価するRust製のルールエンジンです。

### 主要機能

#### 1.1 ルール表現形式
- **リスト式構造**: `[op, arg0, arg1, ..., argn]`
  - `op`: 演算子
  - `arg0..n`: 引数（ネスト可能）
- **コンテキスト解決**: 第一引数を自動的にコンテキストパラメータとして解決
- **明示的変数参照**: `var`演算子による明示的な変数指定が可能

#### 1.2 サポート演算子

**基本演算子**:
```json
["=", "a", 1]                    // フィールド"a"が1と等しい
["=", ["var", "a"], 1]           // var演算子を明示的に使用
["=", "world", "hello"]          // 文字列比較
[">", "age", 18]                 // 数値比較
["<", "score", 100]              // 数値比較
[">=", "level", 5]               // 以上
["<=", "count", 10]              // 以下
["!=", "status", "inactive"]     // 不等号
```

**論理演算子**:
```json
["and", ["=", "type", "user"], [">", "age", 18]]           // AND条件
["or", ["=", "role", "admin"], ["=", "role", "moderator"]] // OR条件
["not", ["=", "deleted", true]]                             // NOT条件
```

**コレクション演算子**:
```json
["in", 1, 1, 2, 3]                          // 1がリストに含まれるか
["in", "admin", ["var", "roles"]]           // 配列に要素が含まれるか
["startswith", "hello", "he"]               // 文字列のプレフィックス一致
["startswith", "arr", "foo", "bar"]         // 配列の先頭要素一致
["endswith", "arr", "bar", "baz"]           // 配列の末尾要素一致
["contains", "message", "error"]            // 文字列に部分文字列を含む
```

#### 1.3 jv言語での適用例

**例1: jv_checkerでのnull安全性チェック**

```json
// .jv/rules/null_safety.json
{
  "rules": [
    {
      "name": "nullable_without_check",
      "description": "Nullable型の変数がnullチェックなしで使用されている",
      "severity": "error",
      "condition": [
        "and",
        ["=", ["var", "node_type"], "variable_access"],
        ["=", ["var", "type_nullable"], true],
        ["not", ["in", ["var", "node_id"], ["var", "null_checked_nodes"]]]
      ]
    },
    {
      "name": "redundant_null_check",
      "description": "Non-nullable型への不要なnullチェック",
      "severity": "warning",
      "condition": [
        "and",
        ["=", ["var", "node_type"], "null_check"],
        ["=", ["var", "target_nullable"], false]
      ]
    }
  ]
}
```

**例2: jv_pmでの依存関係バリデーション**

```json
// .jv/rules/dependency_rules.json
{
  "rules": [
    {
      "name": "outdated_dependency",
      "description": "依存関係が古すぎる",
      "severity": "warning",
      "condition": [
        "and",
        [">", ["var", "days_since_update"], 365],
        ["in", ["var", "category"], ["security", "critical"]]
      ]
    },
    {
      "name": "conflicting_versions",
      "description": "バージョンコンフリクト",
      "severity": "error",
      "condition": [
        "and",
        [">", ["var", "required_version_count"], 1],
        ["not", ["var", "all_compatible"]]
      ]
    }
  ]
}
```

**Rust実装例**:
```rust
use rule::Rule;
use serde_json::json;

// ルールの読み込みと評価
let rule_json = json!([
    "and",
    ["=", ["var", "node_type"], "variable_access"],
    ["=", ["var", "type_nullable"], true]
]);

let rule = Rule::from_json(&rule_json)?;

// コンテキストデータ
let context = json!({
    "node_type": "variable_access",
    "type_nullable": true,
    "node_id": 42,
    "null_checked_nodes": [10, 20, 30]
});

// ルール評価
let result = rule.check(&context)?; // false (nullチェックされていない)
```

### 特徴

**長所**:
- JSONベースで読みやすく、外部システムとの連携が容易
- シンプルなAPI設計
- 動的なルール評価が可能
- Pythonフォークも存在

**短所**:
- パフォーマンスベンチマークの公開情報が少ない
- ドキュメントが限定的
- 複雑な論理表現には記述が冗長になる可能性

### 適用シナリオ

- 動的なビジネスルール評価
- 外部システムからのルール定義受け入れ
- シンプルな条件判定エンジン
- JSON APIとの統合

---

## 2. rhai

### 概要
**公式サイト**: https://rhai.rs/
**GitHub**: https://github.com/rhaiscript/rhai
**ドキュメント**: https://rhai.rs/book/
**カテゴリ**: 組み込みスクリプト言語

rhaiは、Rustアプリケーションに組み込み可能な、安全で高速なスクリプト言語および評価エンジンです。

### 主要機能

#### 2.1 言語特性
- **構文**: JavaScript + Rust風の動的型付け言語
- **型システム**: 動的型付け（ただしRust側で型安全性を保証）
- **評価モデル**: インタープリタ型

#### 2.2 セキュリティ機能

**"Don't Panic" 保証**:
- ライブラリがホストシステムをパニックさせない設計
- あらゆるパニックはバグとして扱われる

**サンドボックス機能**:
- 不変宣言時、エンジンは外部環境を変更不可
- 明示的に許可しない限り環境の変更を防止
- 悪意ある攻撃からの保護：
  - スタックオーバーフロー防止
  - 過大データ処理の制限
  - 暴走スクリプトの検出と停止

#### 2.3 Rust統合機能

**緊密な統合**:
- ネイティブRust関数・型との統合
- ゲッター/セッター、メソッド、インデクサーのサポート
- 関数オーバーロード・演算子オーバーロード
- 関数ポインタによる動的ディスパッチ
- カリー化対応のクロージャ
- 共有値のキャプチャ

**カスタマイズ機能**:
- カスタム演算子の定義
- カスタム構文による言語拡張
- ループなどの機能の無効化
- キーワード・演算子の外科的な無効化

#### 2.4 プラットフォームサポート

- `no-std`環境対応
- WASM対応
- 主要なターゲットプラットフォームで動作
- オンラインPlayground提供（WebAssembly駆動）

### 特徴

**長所**:
- Rust型システムとの高い親和性
- セキュアなサンドボックス実行環境
- パフォーマンス最適化（Cloudflareの高速インタープリタ技術活用）
- 充実したドキュメントとコミュニティ
- 実戦投入実績（Apollo Router、vSMTPなど）

**短所**:
- 動的型付けのためコンパイル時型チェックの恩恵が限定的
- 大規模な計算にはネイティブRustに劣る
- 学習コスト（新しい言語習得）

### 適用シナリオ

- ゲームのスクリプトシステム
- 設定ファイルの高度なカスタマイズ
- プラグインシステムの実装
- ビジネスロジックの動的変更
- ルールエンジンのスクリプト層（vSMTP rule engine）
- IoTエッジデバイスのロジック制御

### 実装例

#### 2.5 Apollo GraphOS Routerでの使用例

Apollo GraphQLのRouterでは、rhaiスクリプトを使用してリクエスト/レスポンスのカスタマイズを実現：

```rhai
fn router_service(service) {
    const request_callback = Fn("process_request");
    service.map_request(request_callback);
}

fn process_request(request) {
    // リクエストのカスタマイズロジック
    request.headers["x-custom-header"] = "value";
}
```

#### 2.6 jv言語での適用例

**例1: ビルドフックスクリプト**

```rhai
// pre_build.rhai - ビルド前処理
fn pre_build(ctx) {
    print(`🔨 Building ${ctx.project_name} v${ctx.version}`);

    // 環境変数のバリデーション
    if !ctx.env.contains("JAVA_HOME") {
        error("JAVA_HOME is not set");
    }

    // カスタムコード生成
    if ctx.features.contains("reflection") {
        print("⚙️  Generating reflection metadata...");
        generate_reflection_metadata(ctx);
    }

    // ソースコードの前処理
    for file in ctx.source_files {
        if file.ends_with(".jv.template") {
            expand_template(file, ctx.template_vars);
        }
    }
}

fn generate_reflection_metadata(ctx) {
    let metadata = #{
        classes: [],
        methods: [],
        fields: []
    };

    // ASTを走査してメタデータを収集
    for ast_node in ctx.ast_nodes {
        if ast_node.kind == "class_declaration" {
            metadata.classes.push(#{
                name: ast_node.name,
                package: ast_node.package,
                annotations: ast_node.annotations
            });
        }
    }

    // メタデータをJSONファイルとして出力
    ctx.write_file(
        ctx.output_dir + "/reflection_metadata.json",
        to_json(metadata)
    );
}
```

**例2: カスタムリントルール**

```rhai
// lint/naming_conventions.rhai - 命名規則チェック
fn check_class_name(node, ctx) {
    let name = node.name;

    // クラス名はPascalCase
    if !is_pascal_case(name) {
        ctx.report_error(
            node.span,
            `Class name '${name}' should be in PascalCase`,
            "naming_convention"
        );
    }

    // インターフェースは"I"で始まらない（Kotlinスタイル）
    if node.is_interface && name.starts_with("I") && is_upper(name[1]) {
        ctx.report_warning(
            node.span,
            `Interface name '${name}' should not use 'I' prefix`,
            "kotlin_style"
        );
    }
}

fn check_method_name(node, ctx) {
    let name = node.name;

    // メソッド名はcamelCase
    if !is_camel_case(name) {
        ctx.report_error(
            node.span,
            `Method name '${name}' should be in camelCase`,
            "naming_convention"
        );
    }

    // テストメソッドは"test"で始まる、またはバッククォートで囲まれている
    if node.has_annotation("Test") {
        if !name.starts_with("test") && !name.starts_with("`") {
            ctx.report_warning(
                node.span,
                "Test method should start with 'test' or use backtick names",
                "test_naming"
            );
        }
    }
}

// ヘルパー関数
fn is_pascal_case(s) {
    if s.is_empty() { return false; }
    return is_upper(s[0]) && !s.contains("_");
}

fn is_camel_case(s) {
    if s.is_empty() { return false; }
    return is_lower(s[0]) && !s.contains("_");
}
```

**例3: パッケージライフサイクルフック**

```rhai
// hooks/post_install.rhai - パッケージインストール後処理
fn post_install(pkg, ctx) {
    print(`📦 Installed ${pkg.name} v${pkg.version}`);

    // ネイティブライブラリの設定
    if pkg.has_native_libs {
        setup_native_libs(pkg, ctx);
    }

    // コード生成が必要なパッケージ
    if pkg.metadata.contains("codegen") {
        run_codegen(pkg, ctx);
    }

    // セキュリティチェック
    if pkg.source == "external" {
        verify_signatures(pkg, ctx);
        scan_dependencies(pkg, ctx);
    }
}

fn setup_native_libs(pkg, ctx) {
    let platform = ctx.platform;
    let lib_dir = `${ctx.project_root}/.jv/libs/${platform}`;

    // プラットフォーム固有のネイティブライブラリを配置
    for lib in pkg.native_libs {
        if lib.platform == platform {
            ctx.copy_file(lib.source, `${lib_dir}/${lib.name}`);
            print(`  ✓ Installed native library: ${lib.name}`);
        }
    }
}

fn verify_signatures(pkg, ctx) {
    print(`  🔐 Verifying package signature...`);

    let public_key = ctx.registry.get_public_key(pkg.publisher);
    let signature = pkg.metadata["signature"];

    if !ctx.crypto.verify(pkg.content_hash, signature, public_key) {
        error(`Signature verification failed for ${pkg.name}`);
    }

    print(`  ✓ Signature verified`);
}
```

**例4: カスタムコード変換（マクロ的な使用）**

```rhai
// transforms/async_transform.rhai - async/await糖衣構文の変換
fn transform_async_function(ast_node, ctx) {
    // async fn foo() -> T を CompletableFuture<T> に変換

    let return_type = ast_node.return_type;
    let body = ast_node.body;

    // 新しい返り値型
    let new_return_type = `CompletableFuture<${return_type}>`;

    // メソッド本体を CompletableFuture.supplyAsync() でラップ
    let transformed_body = `
        return CompletableFuture.supplyAsync(() -> {
            ${transform_await_calls(body)}
        });
    `;

    return #{
        name: ast_node.name,
        params: ast_node.params,
        return_type: new_return_type,
        body: transformed_body,
        annotations: ast_node.annotations
    };
}

fn transform_await_calls(body) {
    let result = "";

    for stmt in body.statements {
        if stmt.kind == "await_expression" {
            // .await() を .join() または .get() に変換
            result += transform_await_to_join(stmt);
        } else {
            result += stmt.to_string();
        }
        result += "\n";
    }

    return result;
}
```

**例5: 統合ビルドワークフロー**

```rhai
// workflow/ci_build.rhai - CI/CD統合スクリプト
fn ci_build(ctx) {
    print("=== JV CI/CD Build Pipeline ===\n");

    // Stage 1: Validation
    run_stage("Validation", || {
        validate_dependencies(ctx);
        check_code_formatting(ctx);
        run_linter(ctx);
    });

    // Stage 2: Build
    run_stage("Build", || {
        compile_sources(ctx);
        generate_docs(ctx);
    });

    // Stage 3: Test
    run_stage("Test", || {
        run_unit_tests(ctx);
        run_integration_tests(ctx);
        check_coverage(ctx, 80); // 80%カバレッジ必須
    });

    // Stage 4: Package
    run_stage("Package", || {
        create_jar(ctx);
        create_sources_jar(ctx);
        create_javadoc_jar(ctx);
    });

    // Stage 5: Publish (mainブランチのみ)
    if ctx.branch == "main" && ctx.is_release {
        run_stage("Publish", || {
            publish_to_registry(ctx);
            create_github_release(ctx);
        });
    }

    print("\n✅ Build completed successfully!");
}

fn run_stage(name, action) {
    print(`\n📍 Stage: ${name}`);
    print("─".repeat(50));

    let start_time = timestamp();

    try {
        action.call();
        let duration = timestamp() - start_time;
        print(`✓ ${name} completed in ${duration}ms`);
    } catch (e) {
        print(`✗ ${name} failed: ${e}`);
        exit(1);
    }
}

fn check_coverage(ctx, min_coverage) {
    let coverage = ctx.test_results.coverage_percent;

    if coverage < min_coverage {
        error(`Coverage ${coverage}% is below minimum ${min_coverage}%`);
    }

    print(`  Code coverage: ${coverage}%`);
}
```

**Rust統合例**:

```rust
use rhai::{Engine, Scope, Dynamic};
use std::sync::Arc;

pub struct BuildContext {
    pub project_name: String,
    pub version: String,
    pub source_files: Vec<String>,
    // ... その他のフィールド
}

impl BuildContext {
    pub fn to_rhai_map(&self) -> rhai::Map {
        let mut map = rhai::Map::new();
        map.insert("project_name".into(), self.project_name.clone().into());
        map.insert("version".into(), self.version.clone().into());
        // ... その他のフィールド
        map
    }
}

pub fn run_build_hook(
    script_path: &str,
    context: &BuildContext
) -> Result<(), Box<dyn std::error::Error>> {
    let mut engine = Engine::new();

    // カスタム関数を登録
    engine.register_fn("error", |msg: &str| {
        panic!("Script error: {}", msg);
    });

    engine.register_fn("print", |msg: &str| {
        println!("{}", msg);
    });

    // コンテキストをスコープに追加
    let mut scope = Scope::new();
    scope.push("ctx", context.to_rhai_map());

    // スクリプト実行
    let script = std::fs::read_to_string(script_path)?;
    engine.eval_with_scope::<()>(&mut scope, &script)?;

    Ok(())
}
```

---

## 3. pest

### 概要
**公式サイト**: https://pest.rs/
**GitHub**: https://github.com/pest-parser/pest
**ドキュメント**: https://pest.rs/book/
**カテゴリ**: PEG（Parsing Expression Grammar）パーサージェネレーター

pestは、アクセシビリティ、正確性、パフォーマンスに重点を置いたRust製の汎用パーサーです。

### 主要機能

#### 3.1 文法定義

**PEG形式**:
- 正規表現に似た精神を持つがより表現力豊か
- 複雑な言語のパースに必要な拡張性を提供

**文法ファイル**:
- `.pest`ファイルに文法を定義
- プロシージャルコードとの分離
- 常に最新の形式化された言語仕様を維持

#### 3.2 文法例

**基本例（識別子リスト）**:
```pest
alpha = { 'a'..'z' | 'A'..'Z' }
digit = { '0'..'9' }
ident = { !digit ~ (alpha | digit)+ }
ident_list = _{ ident ~ (" " ~ ident)* }
```

**計算機文法**:
```pest
num = @{ int ~ ("." ~ ASCII_DIGIT*)? ~ (^"e" ~ int)? }
int = { ("+" | "-")? ~ ASCII_DIGIT+ }

operation = _{ add | subtract | multiply | divide | power }
add = { "+" }
subtract = { "-" }
multiply = { "*" }
divide = { "/" }
power = { "^" }

expr = { term ~ (operation ~ term)* }
term = _{ num | "(" ~ expr ~ ")" }
calculation = _{ SOI ~ expr ~ EOI }

WHITESPACE = _{ " " | "\t" }
```

#### 3.3 ルール修飾子

**アトミックルール**:
- `@`: アトミック＋サイレント
- `$`: アトミック＋トークン生成

**使用例**:
```pest
// 通常のルール
normal_rule = { "a" ~ "b" }

// アトミックルール（空白をスキップしない）
atomic_rule = @{ "a" ~ "b" }

// サイレントルール（トークンを生成しない）
silent_rule = _{ "a" ~ "b" }
```

#### 3.4 パーサー生成

```rust
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct MyParser;

fn main() {
    let pairs = MyParser::parse(Rule::calculation, "1 + 2 * 3")
        .unwrap_or_else(|e| panic!("{}", e));

    // パース結果の処理
    for pair in pairs {
        process_pair(pair);
    }
}

fn process_pair(pair: pest::iterators::Pair<Rule>) {
    match pair.as_rule() {
        Rule::expr => {
            println!("Expression: {}", pair.as_str());
        }
        Rule::num => {
            let num: f64 = pair.as_str().parse().unwrap();
            println!("Number: {}", num);
        }
        _ => {}
    }
}
```

#### 3.5 jv言語への適用例

**例1: jv言語の簡略版パーサー**

```pest
// jv.pest - jv言語文法定義

WHITESPACE = _{ " " | "\t" | "\r" | "\n" }
COMMENT = _{ "//" ~ (!"\n" ~ ANY)* | "/*" ~ (!"*/" ~ ANY)* ~ "*/" }

// キーワード
kw_val = { "val" }
kw_var = { "var" }
kw_fun = { "fun" }
kw_class = { "class" }
kw_data = { "data" }
kw_when = { "when" }
kw_if = { "if" }
kw_else = { "else" }
kw_return = { "return" }
kw_null = { "null" }

keyword = {
    kw_val | kw_var | kw_fun | kw_class | kw_data |
    kw_when | kw_if | kw_else | kw_return | kw_null
}

// 識別子
identifier = @{ !keyword ~ ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "_")* }

// リテラル
integer_literal = @{ ("+" | "-")? ~ ASCII_DIGIT+ }
float_literal = @{
    ("+" | "-")? ~ ASCII_DIGIT+ ~ "." ~ ASCII_DIGIT* ~
    (("e" | "E") ~ ("+" | "-")? ~ ASCII_DIGIT+)?
}
string_literal = @{ "\"" ~ (!"\"" ~ ANY)* ~ "\"" }
bool_literal = { "true" | "false" }
null_literal = { "null" }

literal = {
    float_literal | integer_literal | string_literal |
    bool_literal | null_literal
}

// 型
type_name = { identifier ~ ("." ~ identifier)* }
nullable_type = { type_name ~ "?" }
type_annotation = { nullable_type | type_name }
generic_type = { type_name ~ "<" ~ type_list ~ ">" }
type_list = { type_annotation ~ ("," ~ type_annotation)* }

// 変数宣言
val_declaration = {
    kw_val ~ identifier ~ (":" ~ type_annotation)? ~ "=" ~ expression
}

var_declaration = {
    kw_var ~ identifier ~ (":" ~ type_annotation)? ~ "=" ~ expression
}

// 関数定義
parameter = { identifier ~ ":" ~ type_annotation ~ ("=" ~ expression)? }
parameter_list = { parameter ~ ("," ~ parameter)* }
function_signature = {
    kw_fun ~ identifier ~ "(" ~ parameter_list? ~ ")" ~
    (":" ~ type_annotation)?
}
function_body = { "{" ~ statement* ~ "}" }
function_declaration = { function_signature ~ function_body }

// クラス定義
class_modifier = { "public" | "private" | "protected" | "abstract" | "final" }
class_header = {
    class_modifier* ~ kw_class ~ identifier ~ generic_type? ~
    ("(" ~ parameter_list? ~ ")")?
}
class_body = { "{" ~ class_member* ~ "}" }
class_member = { function_declaration | val_declaration | var_declaration }
class_declaration = { class_header ~ class_body }

// データクラス
data_class_declaration = {
    class_modifier* ~ kw_data ~ kw_class ~ identifier ~
    "(" ~ parameter_list ~ ")" ~ class_body?
}

// when式
when_entry = { expression ~ "->" ~ (expression | block) }
when_expression = {
    kw_when ~ "(" ~ expression ~ ")" ~ "{" ~ when_entry+ ~ "}"
}

// 式
primary_expression = {
    literal |
    identifier |
    "(" ~ expression ~ ")" |
    when_expression
}

// Null安全演算子
safe_call = { primary_expression ~ ("?." ~ identifier ~ call_suffix?)+ }
elvis_operator = { expression ~ "?:" ~ expression }

call_suffix = { "(" ~ argument_list? ~ ")" }
argument_list = { expression ~ ("," ~ expression)* }

postfix_expression = {
    primary_expression ~
    (call_suffix | safe_call)*
}

// 二項演算子（優先順位順）
multiplicative_op = { "*" | "/" | "%" }
additive_op = { "+" | "-" }
comparison_op = { "<=" | ">=" | "<" | ">" }
equality_op = { "==" | "!=" }
logical_and_op = { "&&" }
logical_or_op = { "||" }

multiplicative_expr = {
    postfix_expression ~ (multiplicative_op ~ postfix_expression)*
}

additive_expr = {
    multiplicative_expr ~ (additive_op ~ multiplicative_expr)*
}

comparison_expr = {
    additive_expr ~ (comparison_op ~ additive_expr)*
}

equality_expr = {
    comparison_expr ~ (equality_op ~ comparison_expr)*
}

logical_and_expr = {
    equality_expr ~ (logical_and_op ~ equality_expr)*
}

logical_or_expr = {
    logical_and_expr ~ (logical_or_op ~ logical_and_expr)*
}

expression = { elvis_operator | logical_or_expr }

// 文
expression_statement = { expression ~ ";" }
return_statement = { kw_return ~ expression? ~ ";" }
block = { "{" ~ statement* ~ "}" }
if_statement = {
    kw_if ~ "(" ~ expression ~ ")" ~ block ~
    (kw_else ~ (if_statement | block))?
}

statement = {
    val_declaration ~ ";" |
    var_declaration ~ ";" |
    function_declaration |
    class_declaration |
    data_class_declaration |
    if_statement |
    return_statement |
    expression_statement |
    block
}

// ファイル
source_file = { SOI ~ statement* ~ EOI }
```

**例2: Rustパーサー実装**

```rust
use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "jv.pest"]
pub struct JvParser;

#[derive(Debug, Clone)]
pub enum JvType {
    Simple(String),
    Nullable(Box<JvType>),
    Generic(String, Vec<JvType>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    SafeCall {
        receiver: Box<Expr>,
        method: String,
        args: Option<Vec<Expr>>,
    },
    Elvis {
        value: Box<Expr>,
        default: Box<Expr>,
    },
    When {
        scrutinee: Box<Expr>,
        branches: Vec<(Expr, Expr)>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Lt, Le, Gt, Ge,
    Eq, Ne,
    And, Or,
}

pub fn parse_jv_file(input: &str) -> Result<Vec<Statement>, pest::error::Error<Rule>> {
    let pairs = JvParser::parse(Rule::source_file, input)?;

    let mut statements = Vec::new();
    for pair in pairs {
        if pair.as_rule() == Rule::source_file {
            for inner in pair.into_inner() {
                if inner.as_rule() == Rule::statement {
                    statements.push(parse_statement(inner)?);
                }
            }
        }
    }

    Ok(statements)
}

fn parse_statement(pair: Pair<Rule>) -> Result<Statement, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::val_declaration => parse_val_declaration(inner),
        Rule::var_declaration => parse_var_declaration(inner),
        Rule::function_declaration => parse_function_declaration(inner),
        Rule::class_declaration => parse_class_declaration(inner),
        Rule::expression_statement => {
            let expr = parse_expression(inner.into_inner().next().unwrap())?;
            Ok(Statement::Expression(expr))
        }
        _ => unimplemented!("Statement: {:?}", inner.as_rule()),
    }
}

fn parse_expression(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    match pair.as_rule() {
        Rule::literal => parse_literal(pair),
        Rule::identifier => Ok(Expr::Identifier(pair.as_str().to_string())),
        Rule::elvis_operator => {
            let mut inner = pair.into_inner();
            let value = parse_expression(inner.next().unwrap())?;
            let default = parse_expression(inner.next().unwrap())?;
            Ok(Expr::Elvis {
                value: Box::new(value),
                default: Box::new(default),
            })
        }
        Rule::safe_call => parse_safe_call(pair),
        Rule::when_expression => parse_when_expression(pair),
        Rule::additive_expr => parse_binary_expr(pair, parse_additive_op),
        Rule::multiplicative_expr => parse_binary_expr(pair, parse_multiplicative_op),
        // ... 他の式
        _ => unimplemented!("Expression: {:?}", pair.as_rule()),
    }
}

fn parse_literal(pair: Pair<Rule>) -> Result<Expr, pest::error::Error<Rule>> {
    let inner = pair.into_inner().next().unwrap();

    let lit = match inner.as_rule() {
        Rule::integer_literal => {
            Literal::Integer(inner.as_str().parse().unwrap())
        }
        Rule::float_literal => {
            Literal::Float(inner.as_str().parse().unwrap())
        }
        Rule::string_literal => {
            let s = inner.as_str();
            Literal::String(s[1..s.len()-1].to_string()) // Remove quotes
        }
        Rule::bool_literal => {
            Literal::Bool(inner.as_str() == "true")
        }
        Rule::null_literal => Literal::Null,
        _ => unreachable!(),
    };

    Ok(Expr::Literal(lit))
}

// 使用例
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
        val name: String = "jv-lang"
        val version: String? = null
        val fullName = name ?: "unknown"

        data class Person(val name: String, val age: Int)

        fun greet(person: Person): String {
            return "Hello, ${person.name}!"
        }

        val result = when (version) {
            null -> "No version"
            else -> version
        }
    "#;

    let statements = parse_jv_file(source)?;

    for stmt in statements {
        println!("{:#?}", stmt);
    }

    Ok(())
}
```

**例3: エラーハンドリングと診断**

```rust
use pest::error::{Error, ErrorVariant};
use pest::Span;

pub struct JvDiagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: (usize, usize),
    pub suggestion: Option<String>,
}

pub enum Severity {
    Error,
    Warning,
    Info,
}

pub fn parse_with_diagnostics(
    input: &str
) -> Result<Vec<Statement>, Vec<JvDiagnostic>> {
    match JvParser::parse(Rule::source_file, input) {
        Ok(pairs) => {
            // パース成功、ASTを構築
            let statements = build_ast(pairs);
            Ok(statements)
        }
        Err(e) => {
            // パースエラーを診断情報に変換
            let diagnostic = convert_pest_error_to_diagnostic(&e, input);
            Err(vec![diagnostic])
        }
    }
}

fn convert_pest_error_to_diagnostic(
    error: &Error<Rule>,
    source: &str
) -> JvDiagnostic {
    let (line, col) = match error.line_col {
        pest::error::LineColLocation::Pos((line, col)) => (line, col),
        pest::error::LineColLocation::Span((line, col), _) => (line, col),
    };

    let message = match &error.variant {
        ErrorVariant::ParsingError {
            positives,
            negatives,
        } => {
            format!(
                "Unexpected token at line {}, column {}. Expected: {:?}",
                line, col, positives
            )
        }
        ErrorVariant::CustomError { message } => message.clone(),
    };

    // エラー位置からサジェスションを生成
    let suggestion = generate_suggestion(&error, source);

    JvDiagnostic {
        severity: Severity::Error,
        message,
        span: error.location,
        suggestion,
    }
}

fn generate_suggestion(error: &Error<Rule>, source: &str) -> Option<String> {
    // よくあるエラーパターンに対するサジェスション
    match &error.variant {
        ErrorVariant::ParsingError { positives, .. } => {
            if positives.contains(&Rule::type_annotation) {
                Some("型アノテーションが必要です。例: `val x: Int = 42`".to_string())
            } else if positives.contains(&Rule::kw_else) {
                Some("if文のブロックを閉じてください".to_string())
            } else {
                None
            }
        }
        _ => None,
    }
}
```

### パフォーマンス

#### 3.5 faster-pest最適化

**faster-pest**プロジェクト（2024年）:
- pest比で**705%**の速度
- nom比で**137%**の速度
- `serde_json`と同等のパフォーマンス

**最適化技術**:
- 低レベル最適化トリック
- AST認識プロセスのオーバーヘッド最小化
- Rustのゼロコスト抽象化を活用

### 特徴

**長所**:
- 宣言的で読みやすい文法定義
- 手書きパーサーより保守性が高い
- 文法と実装コードの分離
- 正確性の保証
- 充実したツールチェーン（オンラインfiddleエディタ）
- 高いパフォーマンス

**短所**:
- パース結果は型なしツリー（追加の型付けコードが必要）
- 学習曲線（PEG文法の理解）
- 複雑な文法では最適化が必要な場合がある

### 適用シナリオ

- プログラミング言語のパーサー実装
- 設定ファイルフォーマットのパーサー
- ドメイン固有言語（DSL）の実装
- データフォーマットのパース
- テンプレートエンジン

### pestとの比較 - 他のツール

**pest vs nom**:
- nom: 最終的なRust型を直接生成
- pest: 中間ツリーから型への変換が必要

**pest vs tree-sitter**:
- 両方とも型なしDOMツリーを生成
- tree-sitter: インクリメンタルパース特化
- pest: 一般的なパース用途

---

## 4. tree-sitter

### 概要
**公式サイト**: https://tree-sitter.github.io/tree-sitter/
**GitHub**: https://github.com/tree-sitter/tree-sitter
**カテゴリ**: インクリメンタルパーサー生成ツール

tree-sitterは、ソースファイルの具象構文木を構築し、ファイル編集時に効率的に構文木を更新できるインクリメンタルパーシングライブラリです。

### 主要機能

#### 4.1 インクリメンタルパーシング

**コア技術**:
- **アルゴリズム**: Tim Wagnerの論文「Efficient and Flexible Incremental Parsing」に基づくsentential-form incremental LRパーシング
- **メモリ効率**: 編集されていない部分は古いツリーを共有
- **高速更新**: 編集反映が1ミリ秒未満（通常）

**パフォーマンス特性**:
- **初期パース**: rustcの手書きパーサーの2〜3倍の時間
- **更新パース**: 1ミリ秒未満（キーストロークごとのパースが可能）
- **リアルタイム性**: エディタでのキー入力ごとのパースに最適化

#### 4.2 エラーリカバリ

**自動エラー処理**:
- エラーの開始と終了を特定
- エラー箇所でも動作する構文木を返却
- 部分的なエラーが残りのファイルのASTに影響しない

**エラーノード**:
- `ERROR`ノード: 無効なトークンをスキップして復帰
- `MISSING`ノード: 存在しないノードを仮定して復帰

**制限事項**（2024年時点）:
- リカバリロジックがブラックボックス
- 稀に壊滅的に悪い選択をする場合がある
- 文法作者によるリカバリ制御が限定的

#### 4.3 Rust統合

**バインディング機能**:
```rust
use tree_sitter::{Parser, Language};

// パーサーの初期化
let mut parser = Parser::new();
parser.set_language(language)?;

// 初期パース
let tree = parser.parse(source_code, None)?;

// インクリメンタル更新
let edit = InputEdit {
    start_byte,
    old_end_byte,
    new_end_byte,
    start_position,
    old_end_position,
    new_end_position,
};
tree.edit(&edit);
let new_tree = parser.parse(new_source_code, Some(&tree))?;
```

#### 4.4 文法DSL

**tree-sitter文法例**:
```javascript
module.exports = grammar({
  name: 'mylang',

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.expression_statement,
      $.declaration,
    ),

    expression_statement: $ => seq(
      $.expression,
      ';'
    ),

    expression: $ => choice(
      $.identifier,
      $.number,
      $.binary_expression,
    ),

    binary_expression: $ => prec.left(seq(
      $.expression,
      choice('+', '-', '*', '/'),
      $.expression
    )),

    identifier: $ => /[a-zA-Z_]\w*/,
    number: $ => /\d+/,
  }
});
```

#### 4.6 jv言語での適用例

**例1: jv言語のtree-sitter文法（抜粋）**

```javascript
// grammar.js - jv言語用tree-sitter文法
module.exports = grammar({
  name: 'jv',

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  word: $ => $.identifier,

  rules: {
    source_file: $ => repeat($._statement),

    // コメント
    line_comment: $ => token(seq('//', /.*/)),
    block_comment: $ => token(seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')),

    // 変数宣言
    val_declaration: $ => seq(
      'val',
      field('name', $.identifier),
      optional(seq(':', field('type', $.type))),
      '=',
      field('value', $._expression)
    ),

    var_declaration: $ => seq(
      'var',
      field('name', $.identifier),
      optional(seq(':', field('type', $.type))),
      '=',
      field('value', $._expression)
    ),

    // Null安全型
    type: $ => choice(
      $.simple_type,
      $.nullable_type,
      $.generic_type,
    ),

    simple_type: $ => $.identifier,

    nullable_type: $ => seq(
      field('base', $.simple_type),
      '?'
    ),

    generic_type: $ => seq(
      field('name', $.identifier),
      '<',
      sep1($.type, ','),
      '>'
    ),

    // 式
    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.elvis_expression,
      $.safe_call_expression,
      $.when_expression,
      $.call_expression,
      $.member_expression,
      $._primary_expression,
    ),

    // Elvis演算子
    elvis_expression: $ => prec.left(1, seq(
      field('value', $._expression),
      '?:',
      field('default', $._expression)
    )),

    // Safe call演算子
    safe_call_expression: $ => prec.left(10, seq(
      field('receiver', $._expression),
      '?.',
      field('method', $.identifier),
      optional(field('arguments', $.argument_list))
    )),

    // when式
    when_expression: $ => seq(
      'when',
      '(',
      field('scrutinee', $._expression),
      ')',
      '{',
      repeat1($.when_entry),
      '}'
    ),

    when_entry: $ => seq(
      field('condition', choice($._expression, 'else')),
      '->',
      field('body', choice($._expression, $.block))
    ),

    // データクラス
    data_class_declaration: $ => seq(
      repeat($.modifier),
      'data',
      'class',
      field('name', $.identifier),
      field('parameters', $.parameter_list),
      optional(field('body', $.class_body))
    ),

    // 文
    _statement: $ => choice(
      $.val_declaration,
      $.var_declaration,
      $.function_declaration,
      $.class_declaration,
      $.data_class_declaration,
      $.when_expression,
      $.expression_statement,
    ),

    expression_statement: $ => seq($._expression, ';'),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    _primary_expression: $ => choice(
      $.identifier,
      $.literal,
      $.parenthesized_expression,
    ),

    literal: $ => choice(
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.boolean_literal,
      $.null_literal,
    ),

    integer_literal: $ => /[0-9]+/,
    float_literal: $ => /[0-9]+\.[0-9]+/,
    string_literal: $ => /"([^"\\]|\\.)*"/,
    boolean_literal: $ => choice('true', 'false'),
    null_literal: $ => 'null',
  }
});

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
```

**例2: Rust統合コード（jv_lsp用）**

```rust
use tree_sitter::{Language, Parser, Query, QueryCursor};

extern "C" { fn tree_sitter_jv() -> Language; }

pub struct JvTreeSitter {
    parser: Parser,
    language: Language,
}

impl JvTreeSitter {
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let language = unsafe { tree_sitter_jv() };
        let mut parser = Parser::new();
        parser.set_language(language)?;

        Ok(Self { parser, language })
    }

    // インクリメンタルパース
    pub fn parse_incremental(
        &mut self,
        source: &str,
        old_tree: Option<&tree_sitter::Tree>,
    ) -> Option<tree_sitter::Tree> {
        self.parser.parse(source, old_tree)
    }

    // 構文ハイライト用クエリ
    pub fn get_highlights(&self, tree: &tree_sitter::Tree, source: &str) -> Vec<Highlight> {
        let query_source = r#"
            (val_declaration name: (identifier) @variable)
            (var_declaration name: (identifier) @variable.mutable)
            (function_declaration name: (identifier) @function)
            (class_declaration name: (identifier) @type)
            (data_class_declaration name: (identifier) @type)
            (type name: (identifier) @type)
            (call_expression function: (identifier) @function.call)
            (string_literal) @string
            (integer_literal) @number
            (float_literal) @number
            (boolean_literal) @constant.builtin
            (null_literal) @constant.builtin
            (line_comment) @comment
            (block_comment) @comment
            ["val" "var" "fun" "class" "data" "when" "if" "else"] @keyword
        "#;

        let query = Query::new(self.language, query_source).unwrap();
        let mut cursor = QueryCursor::new();
        let matches = cursor.matches(&query, tree.root_node(), source.as_bytes());

        let mut highlights = Vec::new();
        for m in matches {
            for capture in m.captures {
                let name = &query.capture_names()[capture.index as usize];
                highlights.push(Highlight {
                    range: capture.node.range(),
                    kind: name.to_string(),
                });
            }
        }

        highlights
    }

    // Null安全性チェック用クエリ
    pub fn find_nullable_usages(
        &self,
        tree: &tree_sitter::Tree,
        source: &str
    ) -> Vec<NullableUsage> {
        let query_source = r#"
            (val_declaration
              type: (nullable_type) @nullable_type
              name: (identifier) @var_name)

            (identifier) @usage
              (#eq? @usage @var_name)
              (#not-has-ancestor? @usage safe_call_expression)
              (#not-has-ancestor? @usage elvis_expression)
        "#;

        // クエリを実行してnullチェックなしの使用箇所を検出
        // ...
    }
}

#[derive(Debug)]
pub struct Highlight {
    pub range: tree_sitter::Range,
    pub kind: String,
}
```

**例3: LSPサーバーでの活用**

```rust
use tower_lsp::{LspService, Server};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

struct JvLanguageServer {
    tree_sitter: Arc<Mutex<JvTreeSitter>>,
    documents: Arc<Mutex<HashMap<Url, DocumentState>>>,
}

struct DocumentState {
    text: String,
    tree: Option<tree_sitter::Tree>,
    version: i32,
}

#[tower_lsp::async_trait]
impl LanguageServer for JvLanguageServer {
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;

        // 初期パース
        let tree = {
            let mut ts = self.tree_sitter.lock().await;
            ts.parse_incremental(&text, None)
        };

        // ドキュメント状態を保存
        self.documents.lock().await.insert(uri.clone(), DocumentState {
            text: text.clone(),
            tree,
            version: params.text_document.version,
        });

        // 構文ハイライトを送信
        self.send_semantic_tokens(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let mut docs = self.documents.lock().await;

        if let Some(doc) = docs.get_mut(&uri) {
            // インクリメンタル更新
            for change in params.content_changes {
                // 編集情報をtree-sitterに伝える
                if let Some(ref mut tree) = doc.tree {
                    if let Some(range) = change.range {
                        let edit = self.lsp_range_to_tree_sitter_edit(
                            range,
                            &doc.text,
                            &change.text
                        );
                        tree.edit(&edit);
                    }
                }

                // テキスト更新
                doc.text = change.text;
            }

            // 再パース（インクリメンタル）
            let old_tree = doc.tree.as_ref();
            let mut ts = self.tree_sitter.lock().await;
            doc.tree = ts.parse_incremental(&doc.text, old_tree);
            doc.version = params.text_document.version;

            // 構文ハイライト更新
            drop(docs);
            drop(ts);
            self.send_semantic_tokens(&uri).await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let docs = self.documents.lock().await;

        if let Some(doc) = docs.get(&uri) {
            if let Some(ref tree) = doc.tree {
                let ts = self.tree_sitter.lock().await;
                let highlights = ts.get_highlights(tree, &doc.text);

                // ハイライト情報をLSPトークンに変換
                let tokens = self.highlights_to_semantic_tokens(highlights);
                return Ok(Some(SemanticTokensResult::Tokens(
                    SemanticTokens {
                        result_id: None,
                        data: tokens,
                    }
                )));
            }
        }

        Ok(None)
    }
}
```

### エディタ統合

#### 4.5 採用実績（2024年）

**主要エディタ**:
- **VS Code**: 構文ハイライト、コードフォールディング
- **Neovim**: ネイティブtree-sitterサポート
- **Emacs 29**: ネイティブtree-sitterサポート、主要モードの全面書き換え計画
- **その他**: Atom、その他多数のエディタ

**機能提供**:
- 構文ハイライト
- コードナビゲーション
- コードフォールディング
- 構文認識の選択範囲拡張
- インデント制御

### 特徴

**長所**:
- 極めて高速なインクリメンタルパース
- エラーリカバリ機能
- 広範なエディタ統合実績
- 多言語サポート（100以上の言語文法）
- ストリーミング、レジューム可能なパース
- タイムアウト・キャンセル対応

**短所**:
- 文法定義がJavaScript DSL（Rust外）
- 初期パースは他のパーサーより遅い
- エラーリカバリの制御が限定的
- 型なしツリー（追加の型付けが必要）

### 適用シナリオ

- **最適**: コードエディタ/IDE
- **適**: 構文ハイライト、リアルタイムコード分析
- **適**: インクリメンタルな構文チェック
- **不適**: バッチコンパイラ（初期パース遅延）
- **不適**: 高度なエラーリカバリ制御が必要な場合

### rust-sitter

**高度なRust統合**:
- tree-sitter文法をRustコードで定義
- 型安全な文法記述
- Rustエコシステムとの統合強化

---

## 5. shy

### 概要
**GitHub**: https://github.com/paulchernoch/shy
**作者**: Paul Chernoch
**カテゴリ**: Shunting Yardアルゴリズムベースルールエンジン

shyは、Edsger Dijkstraが発見したShunting Yardアルゴリズムを使用して式を実行に適した形式にコンパイルするルールエンジンです。

### 主要機能

#### 5.1 アーキテクチャ

**3層構造**:

1. **Lexer（字句解析器）**
   - テキストを数値リテラル、変数名、演算子などのトークンストリームに前処理

2. **Parser（構文解析器）**
   - `ShuntingYard`構造体がトークンを受け取る
   - Shunting Yardアルゴリズムを適用してトークンを後置記法に並べ替え
   - `Expression`構造体を生成

3. **Evaluator（評価器）**
   - コンパイル済み`Expression`を実行
   - ビジネスオブジェクトの変数に読み書き可能

**数式変換例**:
- 中置記法: `1 + 2`
- 後置記法: `1`, `2`, `+`

#### 5.2 パフォーマンス特性

**リアルタイム性能**:
- ガベージコレクタ不使用による低遅延
- メモリ使用量が少ない（IoTエッジデバイス向け）

**キャッシング戦略**:
- パース処理が実行時間の約90%を占める
- `ApproximateLRUCache`でコンパイル済み式をキャッシュ
- 文字列式 → コンパイル済み`Expression`のマッピング

#### 5.3 演算子サポート

**豊富な演算子セット**（多くの言語と同じ優先順位・結合性）:

**基本演算子**:
- `!`: 論理NOT（前置）またはファクトリアル（後置）
- `^`: べき乗
- 上付き数字（¹ ² ³等）: べき乗の代替記法
- `+`, `-`, `*`, `/`: 四則演算
- `%`: 剰余
- `<`, `<=`, `>`, `>=`: 比較演算子
- `==`, `!=`: 等価演算子
- `&&`, `||`: 論理演算子

**特殊演算子**:
- `,`: カンマ演算子（関数用のリスト構築）
- `;`: セミコロン（部分式の分離、変数代入を許可）
- `~`: マッチ演算子（正規表現パターン）

**正規表現サポート**:
- Rust regex構文に従った文字列パターン
- ダブルクォート間に記述

#### 5.6 jv言語での適用例

**例1: ビルドルール評価エンジン**

```rust
use shy::{ShuntingYard, Expression, Context};

// jv_buildでのビルド条件評価
pub struct BuildRuleEngine {
    cache: ApproximateLRUCache<String, Expression>,
}

impl BuildRuleEngine {
    pub fn new() -> Self {
        Self {
            cache: ApproximateLRUCache::new(100),
        }
    }

    // ビルド条件の評価
    pub fn should_rebuild(&mut self, rule: &str, ctx: &BuildContext) -> bool {
        // キャッシュから取得または新規パース
        let expr = self.cache.get_or_insert(rule, || {
            ShuntingYard::parse(rule).unwrap()
        });

        // コンテキストを設定して評価
        let mut context = Context::new();
        context.set("source_modified", ctx.source_modified_time);
        context.set("target_modified", ctx.target_modified_time);
        context.set("dependencies_changed", ctx.dependencies_changed);

        expr.eval(&context).unwrap().as_bool()
    }
}

// 使用例
fn main() {
    let mut engine = BuildRuleEngine::new();

    let ctx = BuildContext {
        source_modified_time: 1000,
        target_modified_time: 500,
        dependencies_changed: false,
    };

    // ビルドルール
    let rules = vec![
        "source_modified > target_modified",  // ソースが新しい
        "dependencies_changed",                // 依存関係が変更
        "source_modified > target_modified || dependencies_changed",
    ];

    for rule in rules {
        if engine.should_rebuild(rule, &ctx) {
            println!("Rebuild required: {}", rule);
        }
    }
}
```

**例2: パフォーマンス計測ルール**

```rust
// jv_buildでのパフォーマンス閾値チェック
pub fn check_performance_thresholds(
    metrics: &PerformanceMetrics
) -> Vec<ThresholdViolation> {
    let mut violations = Vec::new();
    let mut engine = ShuntingYard::new();

    // 閾値ルール定義
    let rules = vec![
        ("compile_time", "compile_time_ms < 5000"),
        ("binary_size", "binary_size_kb < 50000"),
        ("startup_time", "startup_time_ms < 500"),
        ("memory_usage", "memory_usage_mb < 512"),
    ];

    for (name, rule) in rules {
        let expr = engine.parse(rule).unwrap();

        let mut ctx = Context::new();
        ctx.set("compile_time_ms", metrics.compile_time_ms);
        ctx.set("binary_size_kb", metrics.binary_size_kb);
        ctx.set("startup_time_ms", metrics.startup_time_ms);
        ctx.set("memory_usage_mb", metrics.memory_usage_mb);

        if !expr.eval(&ctx).unwrap().as_bool() {
            violations.push(ThresholdViolation {
                metric: name.to_string(),
                rule: rule.to_string(),
                actual_value: ctx.get(name).unwrap(),
            });
        }
    }

    violations
}
```

**例3: 依存関係解決ルール**

```rust
// jv_pmでの依存関係バージョン互換性チェック
pub struct DependencyResolver {
    rules: HashMap<String, Expression>,
}

impl DependencyResolver {
    pub fn check_compatibility(
        &self,
        dep_name: &str,
        version: &semver::Version,
        constraints: &str
    ) -> bool {
        let rule = format!(
            "version_major == {} && version_minor >= {} && version_patch >= {}",
            version.major, version.minor, version.patch
        );

        let expr = ShuntingYard::parse(&rule).unwrap();

        // バージョン制約をパース
        let constraint_expr = ShuntingYard::parse(constraints).unwrap();

        let mut ctx = Context::new();
        ctx.set("version_major", version.major as f64);
        ctx.set("version_minor", version.minor as f64);
        ctx.set("version_patch", version.patch as f64);

        constraint_expr.eval(&ctx).unwrap().as_bool()
    }
}

// 使用例
fn main() {
    let resolver = DependencyResolver::new();

    let version = semver::Version::parse("2.5.1").unwrap();

    // 互換性チェック
    let constraints = vec![
        "version_major == 2 && version_minor >= 5",  // OK
        "version_major == 2 && version_minor >= 6",  // NG
        "version_major >= 2",                         // OK
    ];

    for constraint in constraints {
        let compatible = resolver.check_compatibility(
            "some-lib",
            &version,
            constraint
        );
        println!("{}: {}", constraint, compatible);
    }
}
```

**例4: IoTエッジデバイス用センサールール**

```rust
// エッジデバイスでのセンサーデータフィルタリング
pub struct SensorRuleEngine {
    rules: Vec<(String, Expression)>,
    stats: StreamingStats,
}

impl SensorRuleEngine {
    pub fn process_sensor_data(&mut self, data: &SensorData) -> Option<Alert> {
        let mut ctx = Context::new();
        ctx.set("temperature", data.temperature);
        ctx.set("humidity", data.humidity);
        ctx.set("pressure", data.pressure);

        // ストリーミング統計を更新
        self.stats.update(data.temperature);
        ctx.set("temp_avg", self.stats.mean());
        ctx.set("temp_max", self.stats.max());
        ctx.set("temp_min", self.stats.min());

        // ルール評価
        for (name, expr) in &self.rules {
            if expr.eval(&ctx).unwrap().as_bool() {
                return Some(Alert {
                    rule_name: name.clone(),
                    sensor_data: data.clone(),
                    triggered_at: std::time::SystemTime::now(),
                });
            }
        }

        None
    }

    pub fn add_rule(&mut self, name: &str, rule: &str) {
        let expr = ShuntingYard::parse(rule).unwrap();
        self.rules.push((name.to_string(), expr));
    }
}

// 使用例（メモリ制約のあるエッジデバイス）
fn main() {
    let mut engine = SensorRuleEngine::new();

    // アラートルールを追加
    engine.add_rule(
        "high_temperature",
        "temperature > 80 && temperature > temp_avg + 10"
    );
    engine.add_rule(
        "abnormal_humidity",
        "humidity < 20 || humidity > 90"
    );
    engine.add_rule(
        "pressure_anomaly",
        "pressure < 950 || pressure > 1050"
    );

    // センサーデータストリームを処理
    loop {
        let data = read_sensor_data();

        if let Some(alert) = engine.process_sensor_data(&data) {
            send_alert_to_cloud(alert);
        }

        std::thread::sleep(std::time::Duration::from_secs(1));
    }
}
```

**例5: 正規表現マッチング（ログフィルタ）**

```rust
// jv_buildでのビルドログフィルタリング
pub struct LogFilter {
    error_pattern: Expression,
    warning_pattern: Expression,
}

impl LogFilter {
    pub fn new() -> Self {
        // 正規表現パターンをshyでコンパイル
        Self {
            error_pattern: ShuntingYard::parse(
                r#"message ~ "error|ERROR|Error""#
            ).unwrap(),
            warning_pattern: ShuntingYard::parse(
                r#"message ~ "warning|WARNING|Warning""#
            ).unwrap(),
        }
    }

    pub fn categorize(&self, log_line: &str) -> LogLevel {
        let mut ctx = Context::new();
        ctx.set_string("message", log_line);

        if self.error_pattern.eval(&ctx).unwrap().as_bool() {
            LogLevel::Error
        } else if self.warning_pattern.eval(&ctx).unwrap().as_bool() {
            LogLevel::Warning
        } else {
            LogLevel::Info
        }
    }
}

enum LogLevel {
    Error,
    Warning,
    Info,
}
```

#### 5.4 ストリーミング統計

**フルーガルアルゴリズム**:
- メモリ効率的な統計計算
- 対応する統計値：
  - 平均（mean）
  - 中央値（median）
  - 最小値・最大値（min/max）
  - 分位数（quantiles）
  - 標準偏差（standard deviation）

**ユースケース**:
- センサーデータのリアルタイム統計
- IoTエッジデバイスでのデータ集約

#### 5.5 エッジコンピューティング機能

**リモート更新**:
- エッジデバイスで動作中のアプリケーションへの新ルール配信
- ルールのホットスワップ機能

**REST サービス**:
- RESTサービスとして実行可能
- プリコンパイル済みルールのキャッシング

**制約環境対応**:
- センサーの断続的なインターネット接続を想定
- 数日分のデータを保存するメモリがない環境での動作

### 特徴

**長所**:
- 低メモリフットプリント
- リアルタイム性能保証（GC不使用）
- エッジデバイスに最適化
- ストリーミングデータ処理
- 豊富な演算子サポート
- RESTサービス化可能

**短所**:
- ドキュメントが限定的
- コミュニティが小規模
- 数学的/論理的演算に特化（汎用DSLとしては制限的）

### 適用シナリオ

- **最適**: IoTエッジデバイスのルールエンジン
- **最適**: リアルタイムセンサーデータ処理
- **適**: 数式評価エンジン
- **適**: 統計計算パイプライン
- **不適**: 複雑なビジネスロジック
- **不適**: 高度なDSL実装

---

## 比較分析

### 6.1 カテゴリ別分類

| フレームワーク | カテゴリ | 主用途 |
|---|---|---|
| **rule-rs** | ルールエンジン | ビジネスルール評価 |
| **rhai** | スクリプト言語 | 組み込みスクリプティング |
| **pest** | パーサージェネレーター | DSL/言語実装 |
| **tree-sitter** | インクリメンタルパーサー | エディタ統合 |
| **shy** | ルールエンジン | エッジコンピューティング |

### 6.2 パフォーマンス比較

| 項目 | rule-rs | rhai | pest | tree-sitter | shy |
|---|---|---|---|---|---|
| 初期パース速度 | 中 | 高 | 高 | 中 | 高 |
| インクリメンタル更新 | - | - | - | **極高** | - |
| メモリ使用量 | 中 | 中 | 中 | 低 | **極低** |
| リアルタイム性 | 中 | 高 | 高 | **極高** | **極高** |

### 6.3 機能比較

| 機能 | rule-rs | rhai | pest | tree-sitter | shy |
|---|---|---|---|---|---|
| DSL定義 | JSON | Rhai言語 | PEG文法 | JS DSL | 数式 |
| エラーリカバリ | ❌ | ✅ | 限定的 | ✅ | ❌ |
| サンドボックス | ❌ | ✅ | - | - | ❌ |
| Rust統合 | 良 | **優秀** | 良 | 良 | 良 |
| カスタマイズ性 | 低 | **高** | 中 | 中 | 低 |
| 型安全性 | 動的 | 動的 | 型なしツリー | 型なしツリー | 動的 |

### 6.4 学習曲線

| フレームワーク | 学習難易度 | 理由 |
|---|---|---|
| **rule-rs** | 低 | JSONベース、シンプルな構造 |
| **rhai** | 中 | JavaScript風構文、充実したドキュメント |
| **pest** | 中〜高 | PEG文法の理解が必要 |
| **tree-sitter** | 高 | 複雑なアルゴリズム、JS DSL |
| **shy** | 低〜中 | 数式評価に限定、シンプル |

### 6.5 エコシステム成熟度

| フレームワーク | コミュニティ | ドキュメント | 採用実績 | 総合評価 |
|---|---|---|---|---|
| **rule-rs** | 小 | 限定的 | 少 | ★★☆☆☆ |
| **rhai** | 大 | **充実** | **多数** | ★★★★★ |
| **pest** | 大 | 充実 | 多数 | ★★★★☆ |
| **tree-sitter** | **最大** | **充実** | **最多** | ★★★★★ |
| **shy** | 小 | 限定的 | 少 | ★★☆☆☆ |

---

## jvプロジェクトへの適用検討

### 7.1 パーサー実装の選択肢

#### Option A: pest
**推奨度**: ★★★★☆

**メリット**:
- PEG文法で複雑な構文を宣言的に記述可能
- jv文法を`.pest`ファイルで保守しやすい形で管理
- パフォーマンスが高い（faster-pestで更に向上）
- 豊富な実装例とコミュニティサポート

**デメリット**:
- 型なしツリーからjv ASTへの変換が必要
- インクリメンタルパースは未サポート

**適用領域**:
- `jv_parser` クレート
- コンパイラのパーサーフロントエンド

#### Option B: tree-sitter
**推奨度**: ★★★☆☆

**メリット**:
- LSP実装で極めて有利（インクリメンタルパース）
- エディタ統合の実績が豊富
- エラーリカバリ機能

**デメリット**:
- 文法定義がJavaScript DSL（開発フローの分断）
- 初期コンパイルには不向き
- 複雑な統合が必要

**適用領域**:
- `jv_lsp` クレート（特に構文ハイライト、リアルタイム診断）
- IDEプラグイン

#### Option C: 手書きパーサー（現状）
**推奨度**: ★★★★☆（継続推奨）

**メリット**:
- 最高のパフォーマンス
- エラーメッセージの完全制御
- jv特有の構文に最適化可能

**デメリット**:
- 保守コストが高い
- 文法変更時の更新が大変

**判断基準**:
- 現在chumskyを使用している場合は継続推奨
- 大幅な文法拡張時にpestへの移行を検討

### 7.2 ルールエンジン/スクリプティングの選択肢

#### Option D: rhai（ビルドスクリプト、カスタムルール）
**推奨度**: ★★★★☆

**適用例**:
```rust
// jv.tomlでのビルドフック
// [build]
// pre_build = "pre_build.rhai"
// post_build = "post_build.rhai"

// pre_build.rhai
fn pre_build(context) {
    print(`Building ${context.project_name}...`);

    // カスタムコード生成
    if context.features.contains("codegen") {
        generate_code(context.src_dir);
    }
}
```

**メリット**:
- ユーザーがビルドプロセスをカスタマイズ可能
- サンドボックス環境で安全に実行
- 学習コストが低い（JavaScript風）

**適用領域**:
- `jv_build` クレート - ビルドフック
- `jv_pm` クレート - パッケージライフサイクルスクリプト
- カスタムリンター/チェッカールール

#### Option E: rule-rs（静的解析ルール）
**推奨度**: ★★☆☆☆

**適用例**:
```json
// .jv/lint_rules.json
{
  "null_safety_violation": [
    "and",
    ["=", ["var", "type"], "nullable"],
    ["not", ["has", ["var", "checks"], "null_check"]]
  ]
}
```

**メリット**:
- 外部ファイルでルール定義
- JSONベースで理解しやすい

**デメリット**:
- 表現力が限定的
- 複雑なルールには不向き

**適用領域**:
- `jv_checker` クレート - カスタムリントルール（シンプルなもの）

### 7.3 推奨構成

```
jv_parser    : chumsky（現状維持）または pest（移行検討）
jv_lsp       : tree-sitter（構文ハイライト）+ chumsky/pest（セマンティック解析）
jv_build     : rhai（ビルドフック）
jv_pm        : rhai（パッケージスクリプト）
jv_checker   : rhai（カスタムルール定義）
```

---

## 結論

### 主要な発見

1. **パーサー実装**:
   - **pest**: バランスが良く、DSL実装に最適
   - **tree-sitter**: エディタ統合には不可欠だが、コンパイラには過剰

2. **スクリプティング**:
   - **rhai**: 成熟度、セキュリティ、パフォーマンスのバランスが優秀
   - ビルドシステムのカスタマイズに最適

3. **ルールエンジン**:
   - **rule-rs**: シンプルだが限定的
   - **shy**: エッジ特化、汎用性低い
   - **rhai**: 柔軟性が高く、ルールエンジンとしても使用可能

### 推奨事項

#### 短期（Phase 1-4）
- 現在の`chumsky`ベースのパーサーを継続
- LSPでの構文ハイライトに`tree-sitter`文法を追加検討

#### 中期（Phase 5-8）
- ビルドフックに`rhai`統合を実験
- カスタムリントルールで`rhai`スクリプティングをサポート

#### 長期（Phase 9-13）
- 文法が安定したら`pest`への移行を検討
- パッケージマネージャに`rhai`スクリプト機能を本格統合

### 最終評価

| 用途 | 第1候補 | 第2候補 | 第3候補 |
|---|---|---|---|
| **コンパイラパーサー** | chumsky/pest | 手書き | tree-sitter |
| **LSP構文解析** | tree-sitter | pest | chumsky |
| **ビルドスクリプト** | **rhai** | - | - |
| **カスタムルール** | **rhai** | rule-rs | - |
| **エッジ組み込み** | shy | rhai | - |

---

## 参考資料

### 公式ドキュメント
- rhai: https://rhai.rs/book/
- pest: https://pest.rs/book/
- tree-sitter: https://tree-sitter.github.io/tree-sitter/

### リポジトリ
- rule-rs: https://github.com/tclh123/rule-rs
- rhai: https://github.com/rhaiscript/rhai
- pest: https://github.com/pest-parser/pest
- tree-sitter: https://github.com/tree-sitter/tree-sitter
- shy: https://github.com/paulchernoch/shy

### 関連記事
- [Building fast interpreters in Rust - Cloudflare](https://blog.cloudflare.com/building-fast-interpreters-in-rust/)
- [Building a Rust parser using Pest and PEG - LogRocket](https://blog.logrocket.com/building-rust-parser-pest-peg/)
- [Tree-sitter: Revolutionizing Parsing](https://www.deusinmachina.net/p/tree-sitter-revolutionizing-parsing)

---

**調査実施者**: Claude (Anthropic)
**調査完了日**: 2025-10-20
