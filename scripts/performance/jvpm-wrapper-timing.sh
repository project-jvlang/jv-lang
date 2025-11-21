#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
# NOTE: ビルド/実行は jv/target 配下を使用し、ログなどの副産物はリポジトリ直下の target/performance に分離する。
TARGET_DIR="$REPO_ROOT/jv/target"
PERF_ROOT="$REPO_ROOT/target/performance/jvpm-wrapper-timing"
TIMESTAMP=$(date -u +%Y%m%dT%H%M%SZ)
LOGFILE="$PERF_ROOT/measurements-$TIMESTAMP.log"

MAVEN_CACHE_DIR=${MAVEN_CACHE_DIR:-"$HOME/.m2/repository"}
JV_CACHE_DIR=${JV_CACHE_DIR:-"$HOME/.jv/cache"}

JAVA25_HOME="$REPO_ROOT/toolchains/jdk25"
JAVA21_HOME="$REPO_ROOT/toolchains/jdk21"
MVN_HOME="$REPO_ROOT/toolchains/maven"
MVN_BIN="$MVN_HOME/bin/mvn"
JVPM_BIN="$TARGET_DIR/debug/jvpm"
JV_BIN="${JV_BIN:-$TARGET_DIR/debug/jv}"
RUNS=${RUNS:-3}

export JAVA_HOME="$JAVA25_HOME"
export JAVA25_HOME
export JAVA21_HOME
export MAVEN_HOME="$MVN_HOME"
export PATH="$JAVA25_HOME/bin:$JAVA21_HOME/bin:$MVN_HOME/bin:$PATH"

WRAPPER_DEP=${WRAPPER_DEP:-org.apache.commons:commons-lang3:3.14.0}
WRAPPER_ALT_DEP=${WRAPPER_ALT_DEP:-org.junit.jupiter:junit-jupiter:5.9.2}
JV_DEP=${JV_DEP:-$WRAPPER_DEP}
JV_ALT_DEP=${JV_ALT_DEP:-$WRAPPER_ALT_DEP}
REPOSITORY_URL=${REPOSITORY_URL:-https://repo.maven.apache.org/maven2}
DEPENDENCY=${1:-$WRAPPER_DEP}

set_dependency_vars() {
    local coordinate="$1"
    IFS=':' read -r DEP_GROUP DEP_ARTIFACT DEP_VERSION <<< "$coordinate"
    if [[ -z "${DEP_GROUP:-}" || -z "${DEP_ARTIFACT:-}" || -z "${DEP_VERSION:-}" ]]; then
        echo "依存関係の指定は group:artifact:version 形式で行ってください: $coordinate" >&2
        exit 1
    fi
    DEPENDENCY="$coordinate"
}

set_dependency_vars "$DEPENDENCY"

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

if [[ ! -x "$JV_BIN" ]]; then
    echo "jv バイナリが見つかりません。ビルドを開始します..."
    (cd "$REPO_ROOT/jv" && env CARGO_TARGET_DIR="$TARGET_DIR" cargo build --package jv_cli --bin jv)
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

    local -a missing=()
    while IFS= read -r relative; do
        local jar_path="$run_dir/.jv/repository/$relative"
        if [[ ! -f "$jar_path" ]]; then
            missing+=("$relative")
        fi
    done < "$jar_list"
    # 追加/欠落をフルリストで確認
    local tmp_wrapper_list
    tmp_wrapper_list=$(mktemp)
    find "$run_dir/.jv/repository" -type f -name '*.jar' -printf '%P\n' | sort > "$tmp_wrapper_list"
    local missing_file extra_file
    missing_file=$(mktemp)
    extra_file=$(mktemp)
    comm -23 "$jar_list" "$tmp_wrapper_list" > "$missing_file"
    comm -13 "$jar_list" "$tmp_wrapper_list" > "$extra_file"

    local wrapper_count expected_count missing_count extra_count
    wrapper_count=$(wc -l < "$tmp_wrapper_list" | tr -d ' ')
    expected_count=$(wc -l < "$jar_list" | tr -d ' ')
    missing_count=$(wc -l < "$missing_file" | tr -d ' ')
    extra_count=$(wc -l < "$extra_file" | tr -d ' ')

    if [[ "$missing_count" -gt 0 ]]; then
        echo "不足 Jar (${missing_count} 件):"
        cat "$missing_file"
    fi
    if [[ "$extra_count" -gt 0 ]]; then
        echo "過剰 Jar (${extra_count} 件):"
        cat "$extra_file"
    fi

    rm -f "$tmp_wrapper_list" "$missing_file" "$extra_file"

    if [[ "$missing_count" -gt 0 || "$extra_count" -gt 0 || "$wrapper_count" -ne "$expected_count" ]]; then
        echo "Wrapper ローカルリポジトリが Maven 基準と一致しません (expected=${expected_count}, actual=${wrapper_count}, missing=${missing_count}, extra=${extra_count})" >&2
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
    local label="${3:-}"
    local description="Maven dependency:resolve run #$run"
    if [[ -n "$label" ]]; then
        description="$label: Maven dependency:resolve run #$run"
    fi

    run_and_log "$description" "$workdir" "$MVN_BIN" -B -Dmaven.repo.local="$MAVEN_CACHE_DIR" dependency:resolve
    collect_maven_jar_list "$workdir"
    verify_downloaded_jar "$DEP_GROUP" "$DEP_ARTIFACT" "$DEP_VERSION" "$workdir"
}

rm -rf "$PERF_ROOT"
mkdir -p "$PERF_ROOT"
MAVEN_PROJECTS="$PERF_ROOT/maven"
JVPM_PROJECTS="$PERF_ROOT/jvpm"
JV_NATIVE_PROJECTS="$PERF_ROOT/jv-native"
mkdir -p "$MAVEN_PROJECTS" "$JVPM_PROJECTS" "$JV_NATIVE_PROJECTS"

echo "測定ログ: $LOGFILE"
printf "依存関係 (wrapper default): %s\n" "$WRAPPER_DEP" | tee -a "$LOGFILE"
printf "依存関係 (wrapper alt): %s\n" "$WRAPPER_ALT_DEP" | tee -a "$LOGFILE"
printf "依存関係 (jv default): %s\n" "$JV_DEP" | tee -a "$LOGFILE"
printf "依存関係 (jv alt): %s\n" "$JV_ALT_DEP" | tee -a "$LOGFILE"

run_wrapper_scenario() {
    local label="$1"
    local dependency="$2"
    local strategy="${3:-}"
    set_dependency_vars "$dependency"

    printf "\n--- Wrapper Scenario: %s (%s) ---\n" "$label" "$dependency" | tee -a "$LOGFILE"
    for run in $(seq 1 "$RUNS"); do
        local maven_run_dir="$MAVEN_PROJECTS/${label}-run-$run"
        local run_dir="$JVPM_PROJECTS/${label}-run-$run"
        mkdir -p "$maven_run_dir" "$run_dir"
        if [[ "$run" -eq 1 ]]; then clean_caches; fi
        write_dependency_pom "$maven_run_dir"
        run_maven_with_verify "$run" "$maven_run_dir" "$label"
        write_base_pom "$run_dir"
        if [[ -n "$strategy" ]]; then
            run_and_log "jvpm add $dependency ($label) run #$run" "$run_dir" "$JVPM_BIN" add "$dependency" --strategy "$strategy"
        else
            run_and_log "jvpm add $dependency ($label) run #$run" "$run_dir" "$JVPM_BIN" add "$dependency"
        fi
        cp "$maven_run_dir/maven-jars.txt" "$run_dir/maven-jars.txt"
        verify_wrapper_jars_from_list "$run_dir" |& tee -a "$LOGFILE"
        verify_wrapper_jar "$run_dir" "$DEP_GROUP" "$DEP_ARTIFACT" "$DEP_VERSION" |& tee -a "$LOGFILE"
        validate_jvpm_pom "$run_dir" "$DEP_ARTIFACT" |& tee -a "$LOGFILE"
        run_and_log "jvpm remove $dependency ($label) run #$run" "$run_dir" "$JVPM_BIN" remove "$dependency"
        verify_wrapper_jar_removed "$run_dir" "$DEP_GROUP" "$DEP_ARTIFACT" "$DEP_VERSION" |& tee -a "$LOGFILE"
    done
}

write_jv_manifest() {
    local target_dir="$1"
    cat <<EOF > "$target_dir/jv.toml"
[package]
name = "jv-wrapper-perf"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]

[project.output]
directory = "target"

[[repositories]]
name = "perf-repo"
url = "$REPOSITORY_URL"
priority = 5
EOF
    mkdir -p "$target_dir/src"
    cat <<'EOF' > "$target_dir/src/main.jv"
fun main() {
    println("benchmark")
}
EOF
}

run_jv_scenario() {
    local label="$1"
    local dependency="$2"
    local strategy="${3:-}"
    set_dependency_vars "$dependency"

    printf "\n--- JV Scenario: %s (%s) ---\n" "$label" "$dependency" | tee -a "$LOGFILE"
    for run in $(seq 1 "$RUNS"); do
        local maven_run_dir="$MAVEN_PROJECTS/${label}-run-$run"
        local run_dir="$JV_NATIVE_PROJECTS/${label}-run-$run"
        mkdir -p "$maven_run_dir" "$run_dir"
        if [[ "$run" -eq 1 ]]; then clean_caches; fi
        write_dependency_pom "$maven_run_dir"
        run_maven_with_verify "$run" "$maven_run_dir" "$label"
        write_jv_manifest "$run_dir"
        cp "$maven_run_dir/maven-jars.txt" "$run_dir/maven-jars.txt"
        if [[ -n "$strategy" ]]; then
            run_and_log "jv add $dependency ($label) run #$run" "$run_dir" env JVPM_BIN="$JVPM_BIN" "$JV_BIN" add --non-interactive --strategy "$strategy" "$dependency"
        else
            run_and_log "jv add $dependency ($label) run #$run" "$run_dir" env JVPM_BIN="$JVPM_BIN" "$JV_BIN" add --non-interactive "$dependency"
        fi
        verify_wrapper_jars_from_list "$run_dir" |& tee -a "$LOGFILE"
        verify_wrapper_jar "$run_dir" "$DEP_GROUP" "$DEP_ARTIFACT" "$DEP_VERSION" |& tee -a "$LOGFILE"
        if [[ ! -f "$run_dir/jv.lock" ]]; then
            echo "jv.lock が生成されていません: $run_dir/jv.lock" >&2
            exit 1
        fi
    done
}

run_wrapper_scenario "wrapper-default" "$WRAPPER_DEP"
run_wrapper_scenario "wrapper-pubgrub" "$WRAPPER_ALT_DEP" "pubgrub"
run_jv_scenario "jv-default" "$JV_DEP"
run_jv_scenario "jv-maven-compat" "$JV_ALT_DEP" "maven-compat"

echo "測定が完了しました (ログ: $LOGFILE)"
