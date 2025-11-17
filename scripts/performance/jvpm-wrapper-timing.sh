#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT=$(cd "$(dirname "$0")/.." && pwd)
PERF_ROOT="$REPO_ROOT/target/performance/jvpm-wrapper-timing"
TIMESTAMP=$(date -u +%Y%m%dT%H%M%SZ)
LOGFILE="$PERF_ROOT/measurements-$TIMESTAMP.log"

JAVA25_HOME="$REPO_ROOT/toolchains/jdk25"
JAVA21_HOME="$REPO_ROOT/toolchains/jdk21"
MVN_HOME="$REPO_ROOT/toolchains/maven"
MVN_BIN="$MVN_HOME/bin/mvn"
JVPM_BIN="$REPO_ROOT/target/debug/jvpm"
RUNS=${RUNS:-3}

export JAVA_HOME="$JAVA25_HOME"
export JAVA25_HOME
export JAVA21_HOME
export MAVEN_HOME="$MVN_HOME"
export PATH="$JAVA25_HOME/bin:$JAVA21_HOME/bin:$MVN_HOME/bin:$PATH"

DEPENDENCY=${1:-org.apache.commons:commons-lang3:3.14.0}
IFS=':' read -r DEP_GROUP DEP_ARTIFACT DEP_VERSION <<< "$DEPENDENCY"
if [[ -z "${DEP_GROUP:-}" || -z "${DEP_ARTIFACT:-}" || -z "${DEP_VERSION:-}" ]]; then
    echo "依存関係の指定は group:artifact:version 形式で行ってください" >&2
    exit 1
fi

PROJECT_GROUP="com.example.wrapper"
PROJECT_ARTIFACT="jvpm-wrapper-bench"
PROJECT_VERSION="0.1.0"

check_toolchain_dir() {
    local path="$1"
    if [[ ! -d "$path" ]]; then
        echo "toolchains ディレクトリが見つかりません: $path" >&2
        exit 1
    fi
}

for required in "$JAVA25_HOME" "$JAVA21_HOME" "$MVN_HOME"; do
    check_toolchain_dir "$required"
done

if [[ ! -x "$MVN_BIN" ]]; then
    echo "Maven 実行ファイルが見つかりません: $MVN_BIN" >&2
    exit 1
fi

if [[ ! -x "$JVPM_BIN" ]]; then
    echo "jvpm バイナリが見つかりません。ビルドを開始します..."
    (cd "$REPO_ROOT" && cargo build --package jv_pm --bin jvpm)
fi

run_and_log() {
    local description="$1"
    local workdir="$2"
    shift 2
    (
        cd "$workdir"
        printf "\n=== %s (%s) ===\n" "$description" "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
        /usr/bin/time -p "$@"
    ) 2>&1 | tee -a "$LOGFILE"
}

write_base_pom() {
    local target_dir="$1"
    cat <<EOF > "$target_dir/pom.xml"
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                             http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>${PROJECT_GROUP}</groupId>
  <artifactId>${PROJECT_ARTIFACT}</artifactId>
  <version>${PROJECT_VERSION}</version>
  <packaging>jar</packaging>
</project>
EOF
}

write_dependency_pom() {
    local target_dir="$1"
    cat <<EOF > "$target_dir/pom.xml"
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                             http://maven.apache.org/maven-v4_0_0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>${PROJECT_GROUP}</groupId>
  <artifactId>${PROJECT_ARTIFACT}</artifactId>
  <version>${PROJECT_VERSION}</version>
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

rm -rf "$PERF_ROOT"
mkdir -p "$PERF_ROOT"
MAVEN_PROJECTS="$PERF_ROOT/maven"
JVPM_PROJECTS="$PERF_ROOT/jvpm"
mkdir -p "$MAVEN_PROJECTS" "$JVPM_PROJECTS"

echo "測定ログ: $LOGFILE"
printf "依存関係: %s\n" "$DEPENDENCY" | tee -a "$LOGFILE"

for run in $(seq 1 "$RUNS"); do
    run_dir="$MAVEN_PROJECTS/run-$run"
    mkdir -p "$run_dir"
    write_dependency_pom "$run_dir"
    run_and_log "Maven dependency:resolve run #$run" "$run_dir" "$MVN_BIN" -B dependency:resolve
done

for run in $(seq 1 "$RUNS"); do
    run_dir="$JVPM_PROJECTS/run-$run"
    mkdir -p "$run_dir"
    write_base_pom "$run_dir"
    run_and_log "jvpm add $DEPENDENCY run #$run" "$run_dir" "$JVPM_BIN" add "$DEPENDENCY"
done

echo "測定が完了しました (ログ: $LOGFILE)"
