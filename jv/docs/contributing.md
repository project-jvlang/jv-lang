# jvへの貢献

**日本語** | [English](contributing-en.md)

jvへの貢献に関心をお持ちいただき、ありがとうございます！このガイドは、開発と貢献ワークフローの開始に役立ちます。

## 目次

1. [はじめに](#はじめに)
2. [開発環境セットアップ](#開発環境セットアップ)
3. [プロジェクト構造](#プロジェクト構造)
4. [開発ワークフロー](#開発ワークフロー)
5. [テスト](#テスト)
6. [コードスタイル](#コードスタイル)
7. [プルリクエストプロセス](#プルリクエストプロセス)
8. [イシューガイドライン](#イシューガイドライン)
9. [コミュニティ](#コミュニティ)

## はじめに

### 前提条件

- **Rust 1.70+**: [rustup](https://rustup.rs/)経由でインストール
- **Java 25+**: 生成されたコードのテストに必要
- **Git**: バージョン管理用
- **IDE**: rust-analyzer付きVS CodeまたはRustプラグイン付きIntelliJ

### クイックセットアップ

```bash
# リポジトリをクローン
git clone https://github.com/project-jvlang/jv-lang.git
cd jv-lang

# プロジェクトをビルド
cargo build

# テストを実行
cargo test

# pre-commitフックをインストール（オプションですが推奨）
cargo install --git https://github.com/project-jvlang/jv-tools pre-commit
pre-commit install
```

## 開発環境セットアップ

### 開発依存関係

```bash
# 追加の開発ツールをインストール
cargo install cargo-watch         # 変更時の自動再ビルド
cargo install cargo-audit         # セキュリティ監査
cargo install cargo-tarpaulin     # コードカバレッジ
cargo install cargo-deny          # 依存関係チェック
```

### IDE設定

#### VS Code

これらの拡張機能をインストール：
- **rust-analyzer**: Rust言語サポート
- **Error Lens**: インラインエラー表示
- **GitLens**: Git統合
- **Better TOML**: TOMLファイルサポート

推奨設定（`.vscode/settings.json`）：
```json
{
    "rust-analyzer.checkOnSave.command": "clippy",
    "rust-analyzer.cargo.features": "all",
    "editor.formatOnSave": true,
    "files.trimTrailingWhitespace": true
}
```

#### IntelliJ IDEA

- **Rust**プラグインをインストール
- コードフォーマット用に**Rustfmt**を設定
- リンティング用に**Clippy**を有効化

### 環境変数

```bash
export RUST_LOG=debug              # デバッグログを有効化
export JV_TEST_DATA=/path/to/test  # カスタムテストデータディレクトリ
export JAVA_HOME=/path/to/java25   # Java 25インストール
```

## プロジェクト構造

```
jv/
├── crates/                    # Rustワークスペースクレート
│   ├── jv_lexer/             # 字句解析
│   ├── jv_parser/            # 構文解析
│   ├── jv_ast/               # AST定義
│   ├── jv_ir/                # 中間表現
│   ├── jv_codegen_java/      # Javaコード生成
│   ├── jv_checker/           # 静的解析
│   ├── jv_fmt/               # コードフォーマット
│   ├── jv_build/             # ビルドシステム
│   ├── jv_mapper/            # ソースマッピング
│   ├── jv_pm/                # パッケージマネージャー
│   ├── jv_lsp/               # 言語サーバー
│   └── jv_cli/               # コマンドラインインターフェース
├── docs/                     # ドキュメント
├── examples/                 # jvプログラム例
├── tests/                    # 統合テスト
├── tools/                    # 開発ツール
├── Cargo.toml               # ワークスペース設定
└── README.md
```

### クレートの責任

- **jv_lexer**: jvソースコードのトークン化
- **jv_parser**: トークンを抽象構文木（AST）に解析
- **jv_ast**: ASTノード定義とユーティリティ
- **jv_ir**: 中間表現と変換
- **jv_codegen_java**: IRからJavaソースコード生成
- **jv_checker**: 型チェックと静的解析
- **jv_fmt**: ソースコードフォーマット
- **jv_build**: ビルドシステム統合とjavac相互作用
- **jv_mapper**: デバッグ用ソースマップ生成
- **jv_pm**: パッケージ管理と依存関係解決
- **jv_lsp**: 言語サーバープロトコル実装
- **jv_cli**: コマンドラインインターフェースとメインエントリーポイント

## 開発ワークフロー

### ブランチ戦略

シンプルなGit Flowを使用：

- **main**: 安定リリースブランチ
- **develop**: フィーチャー統合ブランチ
- **feature/**: フィーチャー開発ブランチ
- **bugfix/**: バグ修正ブランチ
- **hotfix/**: mainに対する重要な修正

### フィーチャー開発

1. **フィーチャーブランチを作成**:
   ```bash
   git checkout develop
   git pull origin develop
   git checkout -b feature/my-new-feature
   ```

2. **頻繁なコミットで変更を実施**:
   ```bash
   git add .
   git commit -m "Add lexer support for new operator"
   ```

3. **ブランチを最新に保つ**:
   ```bash
   git fetch origin
   git rebase origin/develop
   ```

4. **プッシュしてプルリクエストを作成**:
   ```bash
   git push origin feature/my-new-feature
   # GitHubでPRを作成
   ```

### 言語フィーチャーの追加

新しいjv言語フィーチャーを追加する際は、この順序に従ってください：

1. **lexerを更新** (`jv_lexer`):
   - 必要に応じて新しいトークンタイプを追加
   - トークン化ロジックを更新

2. **parserを更新** (`jv_parser`):
   - 新しい構文を処理するため文法を拡張
   - 新しい構造のASTノードを追加

3. **AST定義を更新** (`jv_ast`):
   - 新しいノードタイプとビジターを追加
   - シリアライゼーション/デシリアライゼーションを更新

4. **型チェッカーを更新** (`jv_checker`):
   - セマンティック検証を追加
   - 新しい型ルールを処理

5. **IR変換を更新** (`jv_ir`):
   - 新しい構造の脱糖方法を定義
   - 変換パスを追加

6. **コードジェネレーターを更新** (`jv_codegen_java`):
   - Javaコード生成を実装
   - 慣用的な出力を保証

7. **フォーマッターを更新** (`jv_fmt`):
   - 新しい構文のフォーマットルールを追加

8. **包括的なテストを追加**:
   - 各コンポーネントの単体テスト
   - 完全パイプラインの統合テスト
   - エラーケーステスト

9. **ドキュメントを更新**:
   - 言語ガイドの例
   - 仕様の更新

### デバッグ

#### コンパイラデバッグ

詳細ログを有効化：
```bash
RUST_LOG=debug cargo run -- build test.jv
```

中間表現を表示：
```bash
# 生成されたASTを表示
cargo run -- build --debug-ast test.jv

# 生成されたIRを表示
cargo run -- build --debug-ir test.jv

# 生成されたJava（コンパイル前）を表示
cargo run -- build --preview test.jv
```

#### テストデバッグ

特定のテストを実行：
```bash
# 特定のクレートのテストを実行
cargo test -p jv_parser

# 特定のテストを実行
cargo test test_string_interpolation

# 出力付きで実行
cargo test -- --nocapture
```

## テスト

### テストカテゴリ

#### 単体テスト
各クレートは`src/`ディレクトリに単体テストを持ちます：

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_string_literal() {
        let input = r#""Hello, world!""#;
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();

        assert_eq!(token.kind, TokenKind::StringLiteral("Hello, world!".to_string()));
    }
}
```

#### 統合テスト
`tests/`の完全コンパイルパイプラインテスト：

```rust
#[test]
fn test_compile_hello_world() {
    let jv_source = r#"
        fun main() {
            println("Hello, world!")
        }
    "#;

    let result = compile_jv_to_java(jv_source);
    assert!(result.is_ok());

    let java_code = result.unwrap();
    assert!(java_code.contains("System.out.println"));
}
```

#### ゴールデンテスト
生成出力を期待ファイルと比較：

```rust
#[test]
fn test_codegen_golden() {
    let jv_file = "tests/golden/input.jv";
    let expected_java = "tests/golden/expected.java";

    let actual_java = compile_file(jv_file).unwrap();
    let expected = fs::read_to_string(expected_java).unwrap();

    assert_eq!(normalize_whitespace(&actual_java), normalize_whitespace(&expected));
}
```

### テストの実行

```bash
# すべてのテストを実行
cargo test

# カバレッジ付きテスト実行
cargo tarpaulin --out html

# 特定のクレートのテストを実行
cargo test -p jv_parser

# 統合テストのみ実行
cargo test --test integration

# 詳細出力付き実行
cargo test -- --nocapture

# パターンマッチングテスト実行
cargo test string_interpolation
```

### テストデータ

テストファイルは`tests/data/`に整理されています：
```
tests/data/
├── valid/              # 有効なjvプログラム
│   ├── basic/         # 基本言語フィーチャー
│   ├── advanced/      # 高度なフィーチャー
│   └── edge_cases/    # エッジケースとコーナーケース
├── invalid/           # 無効なプログラム（エラーテスト用）
├── golden/            # ゴールデンテストファイル
│   ├── input.jv
│   └── expected.java
└── performance/       # パフォーマンスベンチマーク
```

### 良いテストの書き方

1. **テスト命名**: 説明的な名前を使用
   ```rust
   #[test]
   fn test_null_safe_member_access_with_chaining() { ... }
   ```

2. **Arrange-Act-Assertパターン**:
   ```rust
   #[test]
   fn test_parser_function_declaration() {
       // Arrange
       let input = "fun add(a: Int, b: Int): Int = a + b";
       let mut parser = Parser::new(input);

       // Act
       let result = parser.parse_function();

       // Assert
       assert!(result.is_ok());
       let func = result.unwrap();
       assert_eq!(func.name, "add");
       assert_eq!(func.parameters.len(), 2);
   }
   ```

3. **エラーケースのテスト**:
   ```rust
   #[test]
   fn test_parser_missing_semicolon_error() {
       let input = "val x = 42"; // セミコロンが欠如
       let result = Parser::new(input).parse();

       assert!(result.is_err());
       let error = result.unwrap_err();
       assert!(error.message.contains("Expected ';'"));
   }
   ```

## コードスタイル

### Rustコードスタイル

標準Rust規約に従う：

#### フォーマット
```bash
# すべてのコードをフォーマット
cargo fmt

# フォーマットをチェック
cargo fmt -- --check
```

#### リンティング
```bash
# clippyを実行
cargo clippy

# すべてのフィーチャー付きclippy
cargo clippy --all-features -- -D warnings
```

#### 命名規則

- **型**: `PascalCase`
- **関数**: `snake_case`
- **変数**: `snake_case`
- **定数**: `SCREAMING_SNAKE_CASE`
- **モジュール**: `snake_case`

#### ドキュメント

Rustドキュメントコメントを使用：
```rust
/// jv式をASTノードに解析します。
///
/// # 引数
///
/// * `input` - 解析するjvソースコード
/// * `options` - 解析オプションと設定
///
/// # 戻り値
///
/// 解析が成功すれば`Ok(Expression)`、失敗すれば`Err(ParseError)`を返します。
///
/// # 例
///
/// ```
/// use jv_parser::Parser;
///
/// let expr = Parser::new("1 + 2").parse_expression().unwrap();
/// assert_eq!(expr.kind, ExpressionKind::BinaryOperation);
/// ```
pub fn parse_expression(input: &str, options: ParseOptions) -> Result<Expression, ParseError> {
    // 実装...
}
```

#### エラーハンドリング

`Result`型を一貫して使用：

```rust
use anyhow::{Result, Context};

pub fn compile_file(path: &Path) -> Result<String> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("Failed to read file: {}", path.display()))?;

    let ast = parse(&source)
        .context("Failed to parse jv source")?;

    let java_code = generate_java(&ast)
        .context("Failed to generate Java code")?;

    Ok(java_code)
}
```

### Gitコミットメッセージ

慣用的コミット形式に従う：

```
<type>(<scope>): <description>

[optional body]

[optional footer]
```

**タイプ:**
- `feat`: 新機能
- `fix`: バグ修正
- `docs`: ドキュメント変更
- `style`: コードスタイル変更
- `refactor`: コードリファクタリング
- `test`: テストの追加または更新
- `chore`: ビルドプロセスまたは補助ツール変更

**例:**
```
feat(parser): when式のサポートを追加

パターンマッチングとガード句を含むwhen式の解析を実装。
式と文の両方の形式をサポート。

Closes #123
```

```
fix(codegen): メソッドチェーンでのnull安全性を処理

以前は、nullable型でのチェーンメソッド呼び出しが不正な
Javaコードを生成していました。今はnull安全な呼び出し
チェーンを適切に生成します。

Fixes #456
```

## プルリクエストプロセス

### 提出前

1. **テストが通ることを確認**:
   ```bash
   cargo test
   cargo clippy -- -D warnings
   cargo fmt -- --check
   ```

2. 必要に応じて**ドキュメントを更新**

3. 新機能に**テストを追加**

4. **統合テストを実行**:
   ```bash
   cargo test --test integration
   ```

### プルリクエストテンプレート

PRを作成する際は、以下を含めてください：

```markdown
## 説明
変更の簡潔な説明

## 変更の種類
- [ ] バグ修正（既存機能を壊さない問題修正）
- [ ] 新機能（既存機能を壊さない機能追加）
- [ ] 破壊的変更（既存機能が期待通りに動作しなくなる修正や機能）
- [ ] ドキュメント更新

## テスト
- [ ] 単体テストを追加/更新
- [ ] 統合テストを追加/更新
- [ ] すべてのテストがローカルで通る

## チェックリスト
- [ ] コードがスタイルガイドラインに従っている
- [ ] セルフレビューを完了
- [ ] ドキュメントを更新
- [ ] 破壊的変更なし（または明確に文書化）
```

### レビュープロセス

1. **自動チェック**が通る必要がある（CI/CD）
2. メンテナーによる**コードレビュー**
3. **議論**とフィードバック
4. **承認**とマージ

### フィードバックへの対応

```bash
# フィードバックに基づいて変更
git add .
git commit -m "Address PR feedback: improve error messages"
git push origin feature/my-feature
```

## イシューガイドライン

### バグ報告

バグ報告テンプレートを使用：

```markdown
## バグの説明
バグの明確な説明

## 再現手順
1. 内容でファイル`test.jv`を作成: ...
2. `jv build test.jv`を実行
3. エラーを観察: ...

## 期待される動作
何が起こるべきか

## 実際の動作
実際に何が起こるか

## 環境
- jvバージョン: `jv version`
- OS: Linux/macOS/Windows
- Javaバージョン: `java -version`

## 追加コンテキスト
その他の情報
```

### 機能リクエスト

```markdown
## 機能の説明
提案する機能の明確な説明

## 使用例
なぜこの機能が必要か？

## 提案する解決策
この機能はどのように動作すべきか？

## 検討した代替案
他にどのような解決策を検討したか？

## 追加コンテキスト
例、モックアップなど
```

### ラベル

- `bug`: 何かが動作していない
- `enhancement`: 新機能またはリクエスト
- `documentation`: ドキュメントの改善または追加
- `good-first-issue`: 新人に適している
- `help-wanted`: 特別な注意が必要
- `priority-high`: 高優先度の問題
- `component-parser`: パーサー関連の問題
- `component-codegen`: コード生成の問題

## コミュニティ

### コミュニケーションチャンネル

- **GitHub Issues**: バグ報告と機能リクエスト
- **GitHub Discussions**: 一般的な質問と議論
- **Discord**: リアルタイムチャット（READMEの招待）
- **Reddit**: r/jv_langコミュニティ議論

### 行動規範

私たちは[Contributor Covenant](https://www.contributor-covenant.org/)に従います：

- **敬意**を持ち包括的である
- フィードバックは**建設的**である
- **協力的**で助けになる
- 人ではなく**コードに焦点**を当てる

### ヘルプの取得

- **ドキュメント**: まずドキュメントをチェック
- **イシューを検索**: 同じ問題を抱えた人がいるかもしれません
- **質問する**: GitHub Discussionsを質問に使用
- **Discordに参加**: コミュニティからのリアルタイムヘルプ

### 認識

貢献者は以下で認識されます：
- **CONTRIBUTORS.md**ファイル
- 重要な貢献に対する**リリースノート**
- **GitHub contributors**ページ
- **Discord contributor**ロール

jvへの貢献をありがとうございます！🎉