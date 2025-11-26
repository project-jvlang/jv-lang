# jvpm Maven ラッパーモード ガイド

## 概要

**jvpm** は jv 言語のパッケージマネージャーですが、Maven ラッパーモードを搭載しており、既存の Maven プロジェクトでもそのまま使用できます。`pom.xml` があるディレクトリで `jvpm` コマンドを実行すると、自動的にラッパーモードで動作します。

### 主な機能

- **init**: 新しい Maven プロジェクトの初期化
- **install**: 依存解決 + Maven ビルドの実行
- **add/remove**: 依存関係の追加・削除
- **jv.lock による再現性**: チーム全体で同じ依存バージョンを保証
- **Maven パススルー**: 未定義サブコマンドは Maven に直接フォワード（`jvpm clean`, `jvpm test` など）

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

## Maven パススルー機能

### 概要

`jvpm install` は依存解決を行った後、実際のビルド処理を Maven に委譲します。これにより、jvpm の高速な依存解決と Maven の成熟したビルドエコシステムの両方を活用できます。

### パススルーの仕組み

```
jvpm install -DskipTests -Pproduction
       ↓
[1. 依存解決 (PubGrub)]
       ↓
[2. jv.lock 更新]
       ↓
[3. pom.xml 同期]
       ↓
[4. Maven 実行: mvn install -DskipTests -Pproduction]
       ↓
[5. 終了コードを透過]
```

### Maven バイナリの検索順序

jvpm は以下の順序で Maven バイナリを検索します：

1. **環境変数 `JVPM_MAVEN_BIN`**: 明示的に指定された Maven バイナリ
2. **環境変数 `MVN_HOME` / `MAVEN_HOME`**: Maven ホームディレクトリの `bin/mvn`
3. **toolchains ディレクトリ**: プロジェクトルートの `toolchains/maven/bin/mvn`
4. **PATH**: システムパスから `mvn` を検索

### 環境変数

| 変数名 | 説明 | 例 |
|--------|------|-----|
| `JVPM_MAVEN_BIN` | Maven バイナリの絶対パス | `/opt/maven/bin/mvn` |
| `MVN_HOME` | Maven ホームディレクトリ | `/opt/maven` |
| `MAVEN_HOME` | Maven ホームディレクトリ（代替） | `/opt/maven` |
| `JAVA_HOME` | Java ホームディレクトリ（Maven 用） | `/usr/lib/jvm/java-21` |

### 終了コードの透過

Maven の終了コードはそのまま jvpm の終了コードとして返されます：

```bash
# Maven が成功した場合
jvpm install
echo $?  # 0

# Maven がエラーで終了した場合
jvpm install
echo $?  # Maven の終了コード（例: 1）
```

これにより、CI/CD パイプラインでの使用が容易になります。

### 未定義サブコマンドの Maven フォワード

jvpm は `add`, `remove`, `init`, `install`, `resolver`, `repo` 以外のサブコマンドを **すべて Maven に直接フォワード** します。これにより、jvpm を Maven の完全な代替として使用できます：

```bash
# Maven ゴールを直接実行
jvpm clean                    # → mvn clean
jvpm compile                  # → mvn compile
jvpm test                     # → mvn test
jvpm package                  # → mvn package
jvpm deploy                   # → mvn deploy
jvpm site                     # → mvn site

# 複数ゴールの組み合わせ
jvpm clean install            # → mvn clean install
jvpm clean package -DskipTests # → mvn clean package -DskipTests

# Maven プラグインの実行
jvpm dependency:tree          # → mvn dependency:tree
jvpm versions:display-dependency-updates  # → mvn versions:display-dependency-updates
jvpm spring-boot:run          # → mvn spring-boot:run

# プロファイルとオプション
jvpm -Pproduction package     # → mvn -Pproduction package
jvpm -o test                  # → mvn -o test (オフラインモード)
jvpm -U compile               # → mvn -U compile (スナップショット更新)
```

### jvpm コマンド一覧

| コマンド | 動作 |
|---------|------|
| `jvpm add` | 依存関係を追加（jv.lock + pom.xml を更新） |
| `jvpm remove` | 依存関係を削除（jv.lock + pom.xml を更新） |
| `jvpm init` | 新規 Maven プロジェクトを初期化 |
| `jvpm install` | 依存解決 + ビルドを実行（特殊コマンド） |
| `jvpm resolver` | 依存解決戦略の確認（`jv resolver` を案内） |
| `jvpm repo` | リポジトリ管理（`jv repo` を案内） |
| その他すべて | Maven に直接フォワード |

> **Note**: `jvpm install` は jvpm の特殊コマンドで、依存解決を実行してから Maven を呼び出します。`jvpm clean install` や `jvpm test` などは Maven に直接フォワードされるため、jvpm の依存解決は行われません。依存解決と Maven ビルドを両方行いたい場合は `jvpm install` を使用してください。

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
| テスト | `jv test` | `jvpm test` |
| クリーン | `jv clean` | `jvpm clean` |
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

## CI/CD 統合

### GitHub Actions

```yaml
name: Build with jvpm

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Set up JDK 21
        uses: actions/setup-java@v4
        with:
          java-version: '21'
          distribution: 'temurin'

      - name: Set up Maven
        uses: stCarolas/setup-maven@v5
        with:
          maven-version: 3.9.9

      - name: Install jvpm
        run: cargo install jv-cli

      - name: Build
        run: jvpm clean install -DskipTests

      - name: Test
        run: jvpm test
```

### GitLab CI

```yaml
build:
  image: rust:latest
  before_script:
    - apt-get update && apt-get install -y openjdk-21-jdk maven
    - cargo install jv-cli
  script:
    - jvpm clean install -DskipTests
  artifacts:
    paths:
      - target/
      - jv.lock

test:
  extends: .build
  script:
    - jvpm test
```

### jv.lock のコミット

チームで依存バージョンを統一するために、`jv.lock` を Git にコミットすることを推奨します：

```bash
git add jv.lock
git commit -m "chore: update jv.lock"
```

## トラブルシューティング

### Maven が見つからない

```
エラー: Mavenコマンド (mvn) を検出できませんでした。toolchains/maven/bin/mvn を利用可能にするか、JVPM_MAVEN_BIN / MVN_HOME を設定してください。
```

**解決策:**
1. Maven をインストールして PATH に追加
2. `JVPM_MAVEN_BIN` 環境変数を設定
3. プロジェクトの `toolchains/maven/` に Maven を配置

### 依存解決に時間がかかる

初回実行時は Maven 中央リポジトリからの取得が発生するため時間がかかります。2回目以降は `~/.m2/repository` のキャッシュが利用されます。

```bash
# キャッシュを利用したオフラインビルド
jvpm install -o
```

### pom.xml の変更が反映されない

`jvpm add` で追加した依存関係は `pom.xml` に自動反映されますが、手動で `pom.xml` を編集した場合は `jv.lock` との同期が必要です：

```bash
# jv.lock を再生成
rm jv.lock
jvpm install
```

## 関連ドキュメント

- [jv_pm Phase 2 利用ガイド](jv_pm.md)
- [Maven Wrapper との比較](jvpm-maven-wrapper-comparison.md)
- [CLI リファレンス](../jv/docs/cli-reference.md)
