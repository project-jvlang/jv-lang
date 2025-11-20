#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
TARGET_DIR="$REPO_ROOT/target"
PERF_ROOT="$TARGET_DIR/performance/jvpm-wrapper-timing"
TIMESTAMP=$(date -u +%Y%m%dT%H%M%SZ)
LOGFILE="$PERF_ROOT/measurements-$TIMESTAMP.log"

MAVEN_CACHE_DIR=${MAVEN_CACHE_DIR:-"$HOME/.m2/repository"}
JV_CACHE_DIR=${JV_CACHE_DIR:-"$HOME/.jv/cache"}

JAVA25_HOME="$REPO_ROOT/toolchains/jdk25"
JAVA21_HOME="$REPO_ROOT/toolchains/jdk21"
MVN_HOME="$REPO_ROOT/toolchains/maven"
MVN_BIN="$MVN_HOME/bin/mvn"
JVPM_BIN="$TARGET_DIR/debug/jvpm"
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

clean_caches() {
    for dir in "$MAVEN_CACHE_DIR" "$JV_CACHE_DIR"; do
        if [[ -d "$dir" ]]; then
            echo "キャッシュを削除します: $dir"
            rm -rf "$dir"
        else
            echo "キャッシュディレクトリが見つかりません（スキップ）: $dir"
        fi
    done
}

clean_caches

if [[ ! -x "$MVN_BIN" ]]; then
    echo "Maven 実行ファイルが見つかりません: $MVN_BIN" >&2
    exit 1
fi

if [[ ! -x "$JVPM_BIN" ]]; then
    echo "jvpm バイナリが見つかりません。ビルドを開始します..."
    (cd "$REPO_ROOT/jv" && env CARGO_TARGET_DIR="$TARGET_DIR" cargo build --package jv_pm --bin jvpm)
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

validate_jvpm_pom() {
    local workdir="$1"
    local artifact="$2"
    local pom_path="$workdir/pom.xml"

    if [[ ! -f "$pom_path" ]]; then
        echo "pom.xml が見つかりません: $pom_path" >&2
        exit 1
    fi

    if ! grep -q "<dependency>" "$pom_path"; then
        echo "pom.xml に依存関係セクションが含まれていません: $pom_path" >&2
        exit 1
    fi

    if ! grep -q "<artifactId>${artifact}</artifactId>" "$pom_path"; then
        echo "期待する依存関係 ${artifact} が pom.xml に見つかりません: $pom_path" >&2
        exit 1
    fi

    echo "pom.xml (${workdir}) に ${artifact} が登録されていることを確認しました"
}

verify_downloaded_jar() {
    local group="$1"
    local artifact="$2"
    local version="$3"
    local run_dir="${4:-}"
    local group_path="${group//./\/}"
    local jar_path="$MAVEN_CACHE_DIR/$group_path/$artifact/$version/$artifact-$version.jar"

    if [[ -f "$jar_path" ]]; then
        echo "Jar を確認しました: $jar_path"
    else
        echo "ダウンロードされた Jar が見つかりません: $jar_path" >&2
        exit 1
    fi

    local actual_count
    actual_count=$(find "$MAVEN_CACHE_DIR" -type f -name '*.jar' | wc -l | tr -d ' ')

    if [[ -n "$run_dir" ]]; then
        local jar_list="$run_dir/maven-jars.txt"
        if [[ -f "$jar_list" ]]; then
            local expected_count
            expected_count=$(wc -l < "$jar_list" | tr -d ' ')
            if [[ "$actual_count" -ne "$expected_count" ]]; then
                echo "Maven の Jar 件数が一致しません (expected=${expected_count}, actual=${actual_count})" >&2
                exit 1
            fi
            echo "Maven Jar 件数を確認しました: ${actual_count} 件"
        fi
    fi
}

verify_wrapper_jar() {
    local workdir="$1"
    local group="$2"
    local artifact="$3"
    local version="$4"
    local group_path="${group//./\/}"
    local local_repo="$workdir/.jv/repository"
    local jar_path="$local_repo/$group_path/$artifact/$version/$artifact-$version.jar"

    if [[ -f "$jar_path" ]]; then
        echo "Wrapper ローカルリポジトリに Jar を確認しました: $jar_path"
    else
        echo "Wrapper ローカルリポジトリに依存 Jar が見つかりません: $jar_path" >&2
        exit 1
    fi
}

verify_wrapper_jar_removed() {
    local workdir="$1"
    local group="$2"
    local artifact="$3"
    local version="$4"
    local group_path="${group//./\/}"
    local local_repo="$workdir/.jv/repository"
    local version_dir="$local_repo/$group_path/$artifact/$version"

    if [[ -d "$version_dir" ]]; then
        echo "依存 Jar がまだ存在しています: $version_dir" >&2
        exit 1
    else
        echo "依存 Jar が正常に削除されました: $version_dir"
    fi
}

collect_maven_jar_list() {
    local run_dir="$1"
    local jar_list="$run_dir/maven-jars.txt"

    find "$MAVEN_CACHE_DIR" -type f -name '*.jar' -printf '%P\n' | sort > "$jar_list"
}

verify_wrapper_jars_from_list() {
    local run_dir="$1"
    local jar_list="$run_dir/maven-jars.txt"

    if [[ ! -f "$jar_list" ]]; then
        echo "Maven jar リストが見つかりません: $jar_list" >&2
        exit 1
    fi

    while IFS= read -r relative; do
        local jar_path="$run_dir/.jv/repository/$relative"
        if [[ ! -f "$jar_path" ]]; then
            echo "Wrapper のローカルリポジトリに Jar がありません: $jar_path" >&2
            exit 1
        fi
    done < "$jar_list"
    local wrapper_count
    wrapper_count=$(find "$run_dir/.jv/repository" -type f -name '*.jar' | wc -l | tr -d ' ')
    local expected_count
    expected_count=$(wc -l < "$jar_list" | tr -d ' ')
    if [[ "$wrapper_count" -ne "$expected_count" ]]; then
        echo "Wrapper ローカルリポジトリの Jar 件数が一致しません (expected=${expected_count}, actual=${wrapper_count})" >&2
        exit 1
    fi
    echo "Wrapper リポジトリに Maven 取得 Jar をすべて確認しました (${wrapper_count} 件)"
}

verify_wrapper_jars_removed_from_list() {
    local run_dir="$1"
    local jar_list="$run_dir/maven-jars.txt"

    if [[ ! -f "$jar_list" ]]; then
        echo "Maven jar リストが見つかりません: $jar_list" >&2
        exit 1
    fi

    while IFS= read -r relative; do
        local version_dir="$run_dir/.jv/repository/${relative%/*}"
        if [[ -d "$version_dir" ]]; then
            echo "依存 Jar が残っています: $version_dir" >&2
            exit 1
        fi
    done < "$jar_list"
    echo "Wrapper ローカルリポジトリから Maven Jar がすべて削除されていることを確認しました"
}

run_maven_with_verify() {
    local run="$1"
    local workdir="$2"

run_and_log "Maven dependency:resolve run #$run" "$workdir" "$MVN_BIN" -B -Dmaven.repo.local="$MAVEN_CACHE_DIR" dependency:resolve
    collect_maven_jar_list "$workdir"
    verify_downloaded_jar "$DEP_GROUP" "$DEP_ARTIFACT" "$DEP_VERSION" "$workdir"
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
    clean_caches
    write_dependency_pom "$run_dir"
    run_maven_with_verify "$run" "$run_dir"
done

for run in $(seq 1 "$RUNS"); do
    run_dir="$JVPM_PROJECTS/run-$run"
    maven_run_dir="$MAVEN_PROJECTS/run-$run"
    mkdir -p "$run_dir"
    clean_caches
    write_base_pom "$run_dir"
    run_and_log "jvpm add $DEPENDENCY run #$run" "$run_dir" "$JVPM_BIN" add "$DEPENDENCY"
    cp "$maven_run_dir/maven-jars.txt" "$run_dir/maven-jars.txt"
    verify_wrapper_jars_from_list "$run_dir" |& tee -a "$LOGFILE"
    verify_wrapper_jar "$run_dir" "$DEP_GROUP" "$DEP_ARTIFACT" "$DEP_VERSION" |& tee -a "$LOGFILE"
    validate_jvpm_pom "$run_dir" "$DEP_ARTIFACT" |& tee -a "$LOGFILE"
    run_and_log "jvpm remove $DEPENDENCY run #$run" "$run_dir" "$JVPM_BIN" remove "$DEPENDENCY"
    verify_wrapper_jar_removed "$run_dir" "$DEP_GROUP" "$DEP_ARTIFACT" "$DEP_VERSION" |& tee -a "$LOGFILE"
done

echo "測定が完了しました (ログ: $LOGFILE)"
