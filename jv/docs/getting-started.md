# jvを始めよう

**日本語** | [English](getting-started-en.md)

このガイドは、jvのインストールと最初のjvプログラムの作成を支援します。

## インストール

### 前提条件

- **Java 25+**: jvはJava 25 LTSをターゲットとします
- **Rustツールチェーン**（ソースからビルドする場合）

### ソースからインストール

現在、jvはソースからビルドする必要があります：

```bash
# リポジトリをクローン
git clone https://github.com/jv-lang/jv.git
cd jv

# コンパイラをビルド
cargo build --release

# jvバイナリはtarget/release/jvにあります
./target/release/jv --help
```

### インストールの確認

```bash
jv version
# 出力: jv 0.1.0 - Java Sugar Language compiler
```

## 最初のjvプログラム

### 1. プロジェクトを初期化

```bash
mkdir my-jv-project
cd my-jv-project
jv init
```

これにより作成されます：
```
my-jv-project/
├── jv.toml          # プロジェクト設定
└── src/
    └── main.jv      # メインソースファイル
```

### 2. コードを書く

`src/main.jv`を編集：

```jv
val greeting = "Hello, jv!"

fun main() {
    println(greeting)

    val numbers = listOf(1, 2, 3, 4, 5)
    val doubled = numbers.map { it * 2 }

    println("Original: $numbers")
    println("Doubled: $doubled")
}
```

### 3. ビルドと実行

```bash
# プロジェクトをビルド（Javaコードを生成してコンパイル）
jv build

# コンパイル済みプログラムを実行
jv run
```

出力：
```
Hello, jv!
Original: [1, 2, 3, 4, 5]
Doubled: [2, 4, 6, 8, 10]
```

## ビルドプロセスの理解

`jv build`を実行すると、以下が実行されます：

1. **字句解析**: jvソースがトークン化されます
2. **構文解析**: トークンが抽象構文木（AST）に解析されます
3. **IR変換**: ASTが中間表現（IR）に変換されます
4. **Java生成**: IRが読みやすいJava 25ソースコードに変換されます
5. **Javaコンパイル**: 生成されたJavaが`javac`でコンパイルされます

生成されたJavaコードは`out/`ディレクトリで確認できます：

```bash
# 生成されたJavaを表示
cat out/Main.java
```

## 次のステップ

- [言語ガイド](language-guide.md)を読んでjv構文を学習
- [CLIリファレンス](cli-reference.md)で利用可能なすべてのコマンドを確認
- より複雑なプログラムについては[例](../examples/README.md)をチェック
- 大規模プロジェクトについては[プロジェクト構造](project-structure.md)を学習

## ヘルプの取得

- **ドキュメント**: このディレクトリのドキュメントを参照
- **イシュー**: [GitHub Issues](https://github.com/jv-lang/jv/issues)でバグを報告
- **ディスカッション**: [GitHub Discussions](https://github.com/jv-lang/jv/discussions)で質問