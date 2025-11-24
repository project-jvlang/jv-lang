#!/usr/bin/env bash
set -euo pipefail

# 軽量比較スクリプト: Maven dependency:resolve が取得する Jar セットと jvpm (wrapper デフォルト) の取得セットを突き合わせる。
# デフォルト依存: commons-lang3 3.14.0。REPOSITORY_URL を上書きすればローカル/スタブリポジトリでも実行できる。

REPO_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
RUN_ID=$(date -u +%Y%m%dT%H%M%SZ)
OUTPUT_ROOT="$REPO_ROOT/target/performance/jvpm-wrapper-compare"
RUN_DIR="$OUTPUT_ROOT/run-$RUN_ID"
LOGFILE="$RUN_DIR/compare.log"

DEPENDENCY=${1:-org.apache.commons:commons-lang3:3.14.0}
REPOSITORY_URL=${REPOSITORY_URL:-https://repo.maven.apache.org/maven2}

JAVA25_HOME="$REPO_ROOT/toolchains/jdk25"
JAVA21_HOME="$REPO_ROOT/toolchains/jdk21"
MVN_HOME="$REPO_ROOT/toolchains/maven"
MVN_BIN="$MVN_HOME/bin/mvn"
JVPM_BIN="${JVPM_BIN:-$REPO_ROOT/jv/target/debug/jvpm}"

export JAVA_HOME="$JAVA25_HOME"
export JAVA25_HOME
export JAVA21_HOME
export MAVEN_HOME="$MVN_HOME"
export PATH="$JAVA25_HOME/bin:$JAVA21_HOME/bin:$MVN_HOME/bin:$PATH"

mkdir -p "$RUN_DIR"/{maven,wrapper,logs}
MAVEN_REPO="$RUN_DIR/maven/repository"
MAVEN_PROJECT="$RUN_DIR/maven/project"
WRAPPER_PROJECT="$RUN_DIR/wrapper/project"
WRAPPER_HOME="$RUN_DIR/wrapper/home"
SETTINGS_PATH="$RUN_DIR/maven/settings.xml"

IFS=':' read -r DEP_GROUP DEP_ARTIFACT DEP_VERSION <<< "$DEPENDENCY"
if [[ -z "${DEP_GROUP:-}" || -z "${DEP_ARTIFACT:-}" || -z "${DEP_VERSION:-}" ]]; then
    echo "依存関係は group:artifact:version 形式で指定してください: $DEPENDENCY" >&2
    exit 1
fi

ensure_binary() {
    local bin_path="$1"
    local name="$2"
    if [[ -x "$bin_path" ]]; then
        return
    fi
    if [[ "$name" == "jvpm" ]]; then
        echo "jvpm バイナリをビルドします..."
        (cd "$REPO_ROOT/jv" && cargo build --package jv_pm --bin jvpm)
    else
        echo "$name バイナリが見つかりません: $bin_path" >&2
        exit 1
    fi
}

write_settings() {
    cat <<EOF > "$SETTINGS_PATH"
<settings>
  <mirrors>
    <mirror>
      <id>primary</id>
      <url>${REPOSITORY_URL}</url>
      <mirrorOf>*</mirrorOf>
    </mirror>
  </mirrors>
  <profiles>
    <profile>
      <id>primary</id>
      <repositories>
        <repository>
          <id>primary</id>
          <url>${REPOSITORY_URL}</url>
        </repository>
      </repositories>
    </profile>
  </profiles>
  <activeProfiles>
    <activeProfile>primary</activeProfile>
  </activeProfiles>
</settings>
EOF
}

write_dependency_pom() {
    mkdir -p "$MAVEN_PROJECT"
    cat <<EOF > "$MAVEN_PROJECT/pom.xml"
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                             http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>wrapper-compare</artifactId>
  <version>0.1.0</version>
  <packaging>jar</packaging>
  <dependencies>
    <dependency>
      <groupId>${DEP_GROUP}</groupId>
      <artifactId>${DEP_ARTIFACT}</artifactId>
      <version>${DEP_VERSION}</version>
    </dependency>
  </dependencies>
</project>
EOF
}

write_wrapper_pom() {
    mkdir -p "$WRAPPER_PROJECT"
    cat <<EOF > "$WRAPPER_PROJECT/pom.xml"
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                             http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>wrapper-compare</artifactId>
  <version>0.1.0</version>
  <packaging>jar</packaging>
</project>
EOF
}

write_wrapper_config() {
    mkdir -p "$WRAPPER_HOME/.jv"
    cat <<EOF > "$WRAPPER_HOME/.jv/config.toml"
repositories = [{ name = "compare", url = "${REPOSITORY_URL}", priority = 5 }]
EOF
}

collect_jars() {
    local root="$1"
    if [[ ! -d "$root" ]]; then
        return
    fi
    find "$root" -type f -name '*.jar' -printf '%P\n' | sort
}

compare_sets() {
    local expected_file="$1"
    local actual_file="$2"
    local missing extra
    missing=$(comm -23 "$expected_file" "$actual_file" || true)
    extra=$(comm -13 "$expected_file" "$actual_file" || true)
    if [[ -n "$missing" || -n "$extra" ]]; then
        echo "Jar セットが一致しません" | tee -a "$LOGFILE"
        [[ -n "$missing" ]] && echo "missing:"$'\n'"$missing" | tee -a "$LOGFILE"
        [[ -n "$extra" ]] && echo "extra:"$'\n'"$extra" | tee -a "$LOGFILE"
        exit 1
    fi
    echo "Maven と wrapper の Jar セットが一致しました" | tee -a "$LOGFILE"
}

ensure_binary "$MVN_BIN" "maven"
ensure_binary "$JVPM_BIN" "jvpm"
write_settings
write_dependency_pom
write_wrapper_pom
write_wrapper_config

{
    echo "=== jvpm vs Maven Jar セット比較 ==="
    echo "依存: ${DEPENDENCY}"
    echo "リポジトリ: ${REPOSITORY_URL}"
    echo "出力先: ${RUN_DIR}"
} | tee -a "$LOGFILE"

echo "[1/3] Maven dependency:resolve 実行中..." | tee -a "$LOGFILE"
(
    cd "$MAVEN_PROJECT"
    "$MVN_BIN" -B \
        -s "$SETTINGS_PATH" \
        -Dmaven.repo.local="$MAVEN_REPO" \
        dependency:resolve | tee -a "$LOGFILE"
)

echo "[2/3] jvpm add 実行中..." | tee -a "$LOGFILE"
(
    cd "$WRAPPER_PROJECT"
    HOME="$WRAPPER_HOME" "$JVPM_BIN" add "$DEPENDENCY" --non-interactive | tee -a "$LOGFILE"
)

MAVEN_JARS="$RUN_DIR/maven-jars.txt"
WRAPPER_JARS="$RUN_DIR/wrapper-jars.txt"
collect_jars "$MAVEN_REPO" > "$MAVEN_JARS"
collect_jars "$WRAPPER_PROJECT/.jv/repository" > "$WRAPPER_JARS"

echo "[3/3] Jar セット比較..." | tee -a "$LOGFILE"
compare_sets "$MAVEN_JARS" "$WRAPPER_JARS"
echo "完了: ログ $LOGFILE" | tee -a "$LOGFILE"
