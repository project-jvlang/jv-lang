# jvプロジェクト構造

**日本語** | [English](project-structure-en.md)

このガイドでは、最適な開発体験と保守性のためのjvプロジェクトの組織化と構造化方法を説明します。

## 目次

1. [基本プロジェクトレイアウト](#基本プロジェクトレイアウト)
2. [設定ファイル](#設定ファイル)
3. [ソース組織](#ソース組織)
4. [テスト構造](#テスト構造)
5. [ビルドアーティファクト](#ビルドアーティファクト)
6. [ベストプラクティス](#ベストプラクティス)
7. [マルチモジュールプロジェクト](#マルチモジュールプロジェクト)
8. [ビルドツールとの統合](#ビルドツールとの統合)

## 基本プロジェクトレイアウト

### シンプルなプロジェクト

```
my-jv-project/
├── jv.toml                 # プロジェクト設定
├── README.md               # プロジェクトドキュメント
├── .gitignore              # Git無視パターン
├── src/                    # ソースコード
│   ├── main.jv            # メインエントリーポイント
│   ├── utils.jv           # ユーティリティ関数
│   └── models/            # モデルクラス
│       ├── User.jv
│       └── Product.jv
├── test/                   # テストファイル
│   ├── main_test.jv       # main.jvのテスト
│   ├── utils_test.jv      # utils.jvのテスト
│   └── models/            # modelsのテスト
│       ├── User_test.jv
│       └── Product_test.jv
├── resources/              # 静的リソース
│   ├── config.properties
│   └── data/
│       └── sample.json
└── out/                    # ビルド出力（生成される）
    ├── *.java             # 生成されたJavaファイル
    ├── *.class            # コンパイル済みバイトコード
    └── *.jar              # パッケージ化されたJAR
```

### ライブラリプロジェクト

```
my-jv-library/
├── jv.toml
├── README.md
├── LICENSE
├── src/
│   ├── lib.jv             # ライブラリエントリーポイント
│   ├── core/              # コア機能
│   │   ├── Parser.jv
│   │   └── Validator.jv
│   ├── utils/             # ユーティリティモジュール
│   │   ├── StringUtils.jv
│   │   └── DateUtils.jv
│   └── examples/          # 使用例
│       └── BasicExample.jv
├── test/
│   ├── core/
│   └── utils/
├── docs/                   # ライブラリドキュメント
│   ├── api.md
│   └── examples.md
└── benchmarks/            # パフォーマンステスト
    └── ParsingBenchmark.jv
```

## 設定ファイル

### `jv.toml`

メインプロジェクト設定ファイル：

```toml
[project]
name = "my-project"
version = "1.0.0"
authors = ["Your Name <you@example.com>"]
description = "jvプロジェクト"
license = "MIT"
homepage = "https://github.com/user/my-project"
repository = "https://github.com/user/my-project"
keywords = ["utility", "parser", "tool"]
categories = ["command-line-utilities"]

[build]
jdk_version = "25"
optimization_level = 2
debug = false
target_dir = "out"
source_dir = "src"
test_dir = "test"
resource_dir = "resources"

# jvレジストリからのjv依存関係
[dependencies]
math-utils = "1.2.0"
json-parser = { version = "2.1.0", optional = true }

# Maven依存関係
[dependencies.maven]
"org.apache.commons:commons-lang3" = "3.12.0"
"com.google.guava:guava" = "32.1.0-jre"

# 開発依存関係（本番に含まれない）
[dev-dependencies]
junit = "5.9.0"
mockito = "4.6.0"

[dev-dependencies.maven]
"org.junit.jupiter:junit-jupiter" = "5.9.0"

# フィーチャーフラグ
[features]
default = ["json-support"]
json-support = ["json-parser"]
advanced-parsing = ["math-utils"]

# ツールチェーン設定
[toolchain]
jdk = "temurin-25"
prefer_system = false
auto_download = true

# コードフォーマット
[format]
line_width = 100
indent_size = 4
trailing_comma = true
import_organization = true

# 静的解析
[check]
strict = true
null_safety = true
unused_warnings = true
performance_warnings = true
security_warnings = false

# ビルドプロファイル
[profiles.dev]
debug = true
optimization_level = 0

[profiles.release]
debug = false
optimization_level = 3
strip_debug_info = true

[profiles.test]
debug = true
optimization_level = 1
test_coverage = true
```

### `.gitignore`

jvプロジェクト用の標準無視パターン：

```gitignore
# ビルドアーティファクト
/out/
/target/
*.class
*.jar

# IDEファイル
.vscode/
.idea/
*.iml
*.swp
*.swo

# OSファイル
.DS_Store
Thumbs.db

# 一時ファイル
*.tmp
*.temp
*~

# ログ
*.log

# jv固有
jv.lock     # gitignoreしたい場合のみ（通常はコミット）
```

## ソース組織

### パッケージ構造

機能別にソースファイルを整理：

```
src/
├── main.jv                 # アプリケーションエントリーポイント
├── app/                    # アプリケーションロジック
│   ├── App.jv             # メインアプリケーションクラス
│   ├── Config.jv          # 設定処理
│   └── cli/               # コマンドラインインターフェース
│       ├── Commands.jv
│       └── Arguments.jv
├── core/                   # コアビジネスロジック
│   ├── Parser.jv
│   ├── Processor.jv
│   └── Validator.jv
├── models/                 # データモデル
│   ├── User.jv
│   ├── Product.jv
│   └── Order.jv
├── services/              # サービス層
│   ├── UserService.jv
│   ├── ProductService.jv
│   └── EmailService.jv
├── utils/                 # ユーティリティ関数
│   ├── StringUtils.jv
│   ├── DateUtils.jv
│   └── FileUtils.jv
└── extensions/            # 拡張関数
    ├── StringExtensions.jv
    └── CollectionExtensions.jv
```

### 命名規則

#### ファイルとディレクトリ
- クラスファイルには**PascalCase**を使用: `UserService.jv`
- ディレクトリには**camelCase**を使用: `userManagement/`
- ユーティリティモジュールには**snake_case**を使用: `string_utils.jv`
- 関連機能をディレクトリにグループ化

#### コード組織
```jv
// パッケージとインポートを含むファイルヘッダー
package com.example.myproject

import java.util.List
import java.time.LocalDateTime
import com.example.utils.StringUtils

// パブリックAPIを最初に
class UserService {
    fun createUser(name: String): User { ... }
    fun updateUser(id: String, name: String): User { ... }
}

// プライベート実装を最後に
private fun validateUserName(name: String): Boolean { ... }
private fun sanitizeInput(input: String): String { ... }
```

## テスト構造

### テスト組織

テストディレクトリでソース構造をミラー：

```
test/
├── unit/                   # 単体テスト
│   ├── core/
│   │   ├── ParserTest.jv
│   │   └── ValidatorTest.jv
│   ├── services/
│   │   └── UserServiceTest.jv
│   └── utils/
│       └── StringUtilsTest.jv
├── integration/            # 統合テスト
│   ├── ApiIntegrationTest.jv
│   └── DatabaseTest.jv
├── e2e/                   # エンドツーエンドテスト
│   ├── UserWorkflowTest.jv
│   └── OrderProcessingTest.jv
├── performance/           # パフォーマンステスト
│   └── ParsingBenchmark.jv
├── fixtures/              # テストデータ
│   ├── sample_users.json
│   └── test_config.properties
└── helpers/               # テストユーティリティ
    └── TestHelpers.jv
```

### テスト命名規則

```jv
// テストクラス命名: [ClassName]Test
class UserServiceTest {

    // テストメソッド命名: test[Action][Scenario]
    fun testCreateUserWithValidData() { ... }
    fun testCreateUserWithInvalidDataThrowsException() { ... }
    fun testUpdateUserNotFoundReturnsNull() { ... }
}
```

### テストカテゴリ

コメントやアノテーションを使用してテストを分類：

```jv
class UserServiceTest {
    // @Category("unit")
    fun testCreateUserValidation() { ... }

    // @Category("integration")
    fun testCreateUserWithDatabase() { ... }

    // @Category("performance")
    fun testCreateUserPerformance() { ... }
}
```

## ビルドアーティファクト

### 生成される構造

ビルドプロセスは`out/`に以下の構造を作成：

```
out/
├── java/                   # 生成されたJavaソース
│   └── com/example/
│       ├── Main.java
│       ├── UserService.java
│       └── models/
│           └── User.java
├── classes/               # コンパイル済みバイトコード
│   └── com/example/
│       ├── Main.class
│       └── UserService.class
├── resources/             # コピーされたリソース
│   ├── config.properties
│   └── data/
├── maps/                  # ソースマップ
│   ├── Main.java.map
│   └── UserService.java.map
├── docs/                  # 生成されたドキュメント
│   └── api/
├── my-project-1.0.0.jar   # パッケージ化されたJAR
└── reports/               # 解析レポート
    ├── test-results.xml
    ├── coverage.html
    └── static-analysis.json
```

### ビルド設定

`jv.toml`でビルド出力を制御：

```toml
[build]
# 出力ディレクトリ
target_dir = "out"
java_dir = "target"
classes_dir = "out/classes"
resources_dir = "out/resources"

# JARパッケージング
jar_name = "${project.name}-${project.version}.jar"
main_class = "com.example.Main"
include_sources = true
include_docs = false

# ドキュメント生成
generate_docs = true
docs_dir = "out/docs"
```

## ベストプラクティス

### プロジェクト組織

1. **一貫した構造**: 確立されたパターンに従う
2. **論理的なグループ化**: 関連ファイルをまとめる
3. **明確な命名**: 説明的なファイルとディレクトリ名を使用
4. **最小限のネスト**: 深いディレクトリ階層を避ける
5. **関心の分離**: ビジネスロジック、データ、ユーティリティを分離

### ファイル組織

1. **単一責任**: ファイルごとに1つの主要概念
2. **適度なサイズ**: 可能な場合はファイルを500行未満に保つ
3. **明確な依存関係**: 循環依存を最小限に抑える
4. **パブリックAPIを最初に**: パブリックインターフェースを上部に配置

### 設定管理

1. **環境固有の設定**: 異なる環境用にプロファイルを使用
2. **秘密管理**: 秘密をバージョン管理にコミットしない
3. **デフォルト値**: 合理的なデフォルトを提供
4. **ドキュメント**: 設定オプションにコメント

### バージョン管理

1. **ビルドアーティファクトを無視**: 生成ファイルをコミットしない
2. **小さなコミット**: 焦点を絞った原子的なコミット
3. **説明的なメッセージ**: 明確なコミットメッセージを書く
4. **ブランチ戦略**: 開発にフィーチャーブランチを使用

## マルチモジュールプロジェクト

### ワークスペース構造

大規模プロジェクトには複数のモジュールを使用：

```
my-jv-workspace/
├── jv-workspace.toml       # ワークスペース設定
├── common/                 # 共有ユーティリティ
│   ├── jv.toml
│   └── src/
├── api/                    # REST APIモジュール
│   ├── jv.toml
│   └── src/
├── web/                    # Webインターフェース
│   ├── jv.toml
│   └── src/
├── cli/                    # コマンドラインツール
│   ├── jv.toml
│   └── src/
└── docs/                   # 共有ドキュメント
```

### ワークスペース設定

`jv-workspace.toml`:
```toml
[workspace]
members = [
    "common",
    "api",
    "web",
    "cli"
]

# 共有依存関係
[workspace.dependencies]
json-parser = "2.1.0"
logging = "1.5.0"

# 共有設定
[workspace.profile.dev]
debug = true
optimization_level = 0

[workspace.profile.release]
debug = false
optimization_level = 3
```

個別モジュール`jv.toml`:
```toml
[project]
name = "my-project-api"
version = "1.0.0"

[dependencies]
# ワークスペースバージョンを使用
json-parser = { workspace = true }
# モジュール固有の依存関係
http-server = "3.2.0"
# モジュール間依存関係
common = { path = "../common" }
```

## ビルドツールとの統合

### Gradle統合

`build.gradle.kts`:
```kotlin
plugins {
    id("jv-gradle-plugin") version "1.0.0"
    id("java")
    id("application")
}

jv {
    jdkVersion = "25"
    optimizationLevel = 2
    generateSourceMaps = true
}

dependencies {
    implementation("org.apache.commons:commons-lang3:3.12.0")
    testImplementation("org.junit.jupiter:junit-jupiter:5.9.0")
}

application {
    mainClass.set("com.example.MainKt")
}
```

### Maven統合

`pom.xml`:
```xml
<build>
    <plugins>
        <plugin>
            <groupId>org.jv-lang</groupId>
            <artifactId>jv-maven-plugin</artifactId>
            <version>1.0.0</version>
            <configuration>
                <jdkVersion>25</jdkVersion>
                <optimizationLevel>2</optimizationLevel>
            </configuration>
            <executions>
                <execution>
                    <goals>
                        <goal>compile</goal>
                    </goals>
                </execution>
            </executions>
        </plugin>
    </plugins>
</build>
```

### 継続的インテグレーション

`.github/workflows/ci.yml`:
```yaml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Setup Java 25
      uses: actions/setup-java@v3
      with:
        java-version: '25'
        distribution: 'temurin'

    - name: Install jv
      run: |
        curl -sSL https://install.jv-lang.org | sh
        echo "$HOME/.jv/bin" >> $GITHUB_PATH

    - name: Build
      run: jv build

    - name: Test
      run: jv test

    - name: Check formatting
      run: jv fmt --check
```

この構造は、一貫性とベストプラクティスを維持しながら、あらゆるサイズのjvプロジェクトに対して堅実な基盤を提供します。
