# jvpm Maven ラッパーモード ガイド

## 概要

**jvpm** は jv 言語のパッケージマネージャーですが、Maven ラッパーモードを搭載しており、既存の Maven プロジェクトでもそのまま使用できます。`pom.xml` があるディレクトリで `jvpm` コマンドを実行すると、自動的にラッパーモードで動作します。

### 主な機能

- **init**: 新しい Maven プロジェクトの初期化
- **install**: 依存解決 + Maven ビルドの実行
- **add/remove**: 依存関係の追加・削除
- **jv.lock による再現性**: チーム全体で同じ依存バージョンを保証

## クイックスタート

### 新規プロジェクトの作成

```bash
# JAR プロジェクトを作成
jvpm init my-app
cd my-app

# 依存関係を追加
jvpm add org.junit.jupiter:junit-jupiter-api:5.9.2
jvpm add com.google.guava:guava:32.1.3-jre

# ビルド
jvpm install
```

### 既存 Maven プロジェクトでの使用

```bash
cd existing-maven-project

# 依存関係を追加（pom.xml と jv.lock が自動更新）
jvpm add org.apache.commons:commons-lang3:3.14.0

# ビルド
jvpm install -DskipTests
```

## init コマンド

### 基本構文

```bash
jvpm init [OPTIONS] [DIRECTORY]
```

### オプション

| オプション | 短縮形 | 説明 | デフォルト |
|-----------|--------|------|-----------|
| `--group-id` | | Maven グループ ID | `com.example` |
| `--artifact-id` | | Maven アーティファクト ID | ディレクトリ名 |
| `--version` | | プロジェクトバージョン | `0.1.0-SNAPSHOT` |
| `--packaging` | | パッケージング形式 (jar/war/pom) | `jar` |
| `--java-version` | | Java バージョン (17/21/25) | `25` |
| `--non-interactive` | `-n` | プロンプトなしで実行 | `false` |

### 使用例

```bash
# 対話モードで作成（推奨）
jvpm init my-app

# JAR プロジェクト（デフォルト）
jvpm init --non-interactive my-jar-app

# WAR プロジェクト（Web アプリケーション）
jvpm init --packaging war --java-version 21 my-webapp

# 親 POM（マルチモジュールプロジェクト）
jvpm init --packaging pom my-parent

# すべてのオプションを指定
jvpm init \
  --group-id com.mycompany \
  --artifact-id awesome-app \
  --version 1.0.0 \
  --packaging jar \
  --java-version 21 \
  --non-interactive \
  awesome-app
```

### 生成されるファイル

#### JAR プロジェクト
```
my-app/
├── pom.xml
├── .gitignore
└── src/
    ├── main/
    │   ├── java/
    │   └── resources/
    └── test/
        ├── java/
        └── resources/
```

#### WAR プロジェクト
```
my-webapp/
├── pom.xml
├── .gitignore
└── src/
    ├── main/
    │   ├── java/
    │   ├── resources/
    │   └── webapp/
    │       └── WEB-INF/
    └── test/
        ├── java/
        └── resources/
```

#### POM プロジェクト（親 POM）
```
my-parent/
├── pom.xml        # <modules> セクション付き
└── .gitignore
```

## install コマンド

### 基本構文

```bash
jvpm install [MAVEN_OPTIONS...]
```

### 動作フロー

1. **プロジェクト検出**: `pom.xml` の存在を確認
2. **依存解決**: PubGrub アルゴリズムで依存関係を解決
3. **jv.lock 更新**: 解決結果をロックファイルに記録
4. **pom.xml 同期**: 依存関係を pom.xml に反映
5. **Maven パススルー**: `mvn install` を実行

### Maven オプション

`jvpm install` に渡されたオプションはすべて Maven にパススルーされます：

```bash
# テストをスキップ
jvpm install -DskipTests

# Javadoc 生成をスキップ
jvpm install -Dmaven.javadoc.skip=true

# オフラインモード
jvpm install -o

# スナップショット強制更新
jvpm install -U

# プロファイル指定
jvpm install -Pproduction

# カスタムローカルリポジトリ
jvpm install -Dmaven.repo.local=/tmp/my-m2

# 複数オプションの組み合わせ
jvpm install -DskipTests -o -Pproduction
```

## 依存管理

### 依存関係の追加

```bash
# バージョン指定
jvpm add org.junit.jupiter:junit-jupiter-api:5.9.2

# 最新バージョン（対話的に選択）
jvpm add org.junit.jupiter:junit-jupiter-api

# 非対話モード
jvpm add --non-interactive org.junit.jupiter:junit-jupiter-api
```

### 依存関係の削除

```bash
jvpm remove org.junit.jupiter:junit-jupiter-api
```

### 依存解決戦略

jvpm は複数の依存解決戦略をサポートしています：

```bash
# 戦略一覧
jvpm resolver list

# デフォルト: PubGrub（推奨）
jvpm add --resolver pubgrub org.example:lib

# Maven 互換モード
jvpm add --resolver maven org.example:lib
```

## jv.lock ファイル

`jv.lock` は依存関係のバージョンを固定するためのファイルです。

### 特徴

- **再現性**: チーム全体で同じ依存バージョンを保証
- **高速性**: 解決済みの依存情報をキャッシュ
- **Git 管理**: リポジトリにコミットして共有

### 形式

```toml
# jv.lock (自動生成 - 手動編集非推奨)
version = "1"
strategy = "pubgrub"

[[packages]]
name = "org.junit.jupiter:junit-jupiter-api"
version = "5.9.2"
source = "registry"
checksum = "sha256:..."

[[packages]]
name = "org.opentest4j:opentest4j"
version = "1.3.0"
source = "registry"
checksum = "sha256:..."
```

## ネイティブモードとの違い

| 機能 | ネイティブモード | ラッパーモード |
|-----|-----------------|---------------|
| 設定ファイル | `jv.toml` | `pom.xml` |
| ロックファイル | `jv.lock` | `jv.lock` |
| ビルド | `jv build` | `jvpm install` |
| 出力 | `.java` ファイル | Maven 成果物 |
| 対象 | jv プロジェクト | Maven プロジェクト |

## エラーハンドリング

### よくあるエラー

#### pom.xml が存在しない場合

```
エラー: このディレクトリには pom.xml がありません。
`jvpm init` で新規プロジェクトを作成するか、Maven プロジェクトのディレクトリで実行してください。
```

#### jv.toml と pom.xml が両方存在する場合

```
エラー: jv.toml と pom.xml が両方存在します。
jv ネイティブプロジェクトでは `jv pm` コマンドを使用してください。
```

#### init 時にディレクトリが空でない場合

```
エラー: destination `/path/to/dir` is not empty
空のディレクトリを指定するか、新しいディレクトリ名を使用してください。
```

## パフォーマンス

jvpm は Maven と比較して高速な依存解決を提供します：

| 操作 | Maven | jvpm |
|-----|-------|------|
| 依存追加 | ~7-8 秒 | ~0.02 秒 |
| 依存解決 | ~20 秒 | ~14 秒 |

※ 実測値は環境により異なります。詳細は [jvpm-maven-wrapper-comparison.md](jvpm-maven-wrapper-comparison.md) を参照。

## デモスクリプト

`scripts/demo-init-install.sh` で init/install コマンドの動作を確認できます：

```bash
./scripts/demo-init-install.sh
```

このスクリプトは以下をテストします：
- JAR/WAR/POM パッケージングでの init
- code-with-quarkus サンプルでの install
- エラーケース（既存 pom.xml、jv.toml 検出など）

## 関連ドキュメント

- [jv_pm Phase 2 利用ガイド](jv_pm.md)
- [Maven Wrapper との比較](jvpm-maven-wrapper-comparison.md)
- [CLI リファレンス](../jv/docs/cli-reference.md)
