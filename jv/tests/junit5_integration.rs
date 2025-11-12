use jv_pm::JavaTarget;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

const TEST_SUITE_SOURCE: &str = r#"
package fixtures.junit5

fun add(lhs: Int, rhs: Int): Int {
    lhs + rhs
}

test "simple equality" {
    val sum = add(40 2)
    sum == 42
}

test "labeled addition cases" [
    ["carry" 11 17 28]
    ["negative" -5 3 -2]
] (label: String, lhs: Int, rhs: Int, expected: Int) {
    val computed = add(lhs rhs)
    computed == expected
}
"#;

const POM_XML: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <groupId>dev.jv</groupId>
    <artifactId>junit5-integration</artifactId>
    <version>0.1.0</version>
    <properties>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <maven.compiler.release>21</maven.compiler.release>
        <jv.generated.tests>${project.build.directory}/generated-tests</jv.generated.tests>
    </properties>
    <dependencies>
        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter</artifactId>
            <version>5.10.2</version>
            <scope>test</scope>
        </dependency>
    </dependencies>
    <build>
        <plugins>
            <plugin>
                <groupId>org.codehaus.mojo</groupId>
                <artifactId>build-helper-maven-plugin</artifactId>
                <version>3.5.0</version>
                <executions>
                    <execution>
                        <id>add-generated-tests</id>
                        <phase>generate-test-sources</phase>
                        <goals>
                            <goal>add-test-source</goal>
                        </goals>
                        <configuration>
                            <sources>
                                <source>${jv.generated.tests}</source>
                            </sources>
                        </configuration>
                    </execution>
                </executions>
            </plugin>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-surefire-plugin</artifactId>
                <version>3.2.5</version>
                <configuration>
                    <useModulePath>false</useModulePath>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
"#;

struct TempDirGuard {
    path: PathBuf,
}

fn workspace_temp_root() -> PathBuf {
    static ROOT: OnceLock<PathBuf> = OnceLock::new();
    ROOT.get_or_init(|| {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let base = manifest_dir
            .ancestors()
            .find_map(|ancestor| {
                let candidate = ancestor.join("target");
                candidate.exists().then_some(candidate)
            })
            .unwrap_or_else(|| manifest_dir.join("target"));
        let temp_root = base.join("test-temp");
        let _ = fs::remove_dir_all(&temp_root);
        temp_root
    })
    .clone()
}

impl TempDirGuard {
    fn new(label: &str) -> std::io::Result<Self> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let base = workspace_temp_root();
        fs::create_dir_all(&base)?;
        let path = base.join(format!("junit5-cli-{label}-{timestamp}"));
        fs::create_dir_all(&path)?;
        Ok(Self { path })
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        // Keep artifacts for inspection on failure.
    }
}

fn toolchains_root() -> Option<PathBuf> {
    let mut current = Some(PathBuf::from(env!("CARGO_MANIFEST_DIR")));
    while let Some(dir) = current {
        let candidate = dir.join("toolchains");
        if candidate.is_dir() {
            return Some(candidate);
        }
        current = dir.parent().map(Path::to_path_buf);
    }
    None
}

fn toolchain_java_home(target: JavaTarget) -> Option<PathBuf> {
    let root = toolchains_root()?;
    let candidates: &[&str] = match target {
        JavaTarget::Java25 => &["jdk25", "java25"],
        JavaTarget::Java21 => &["jdk21", "java21"],
    };
    for dir in candidates {
        let candidate = root.join(dir);
        if candidate.join("bin").join(java_executable_name()).exists() {
            return Some(candidate);
        }
    }
    None
}

fn java_executable_name() -> &'static str {
    if cfg!(windows) {
        "java.exe"
    } else {
        "java"
    }
}

fn manifest_toml() -> String {
    r#"[package]
name = "junit5-integration-suite"
version = "0.1.0"

[project]
entrypoint = "tests/suite.jv"

[project.sources]
include = ["tests/**/*.jv"]
"#
    .to_string()
}

fn write_project_files(root: &Path) {
    fs::write(root.join("jv.toml"), manifest_toml()).expect("write manifest");

    let tests_dir = root.join("tests");
    fs::create_dir_all(&tests_dir).expect("create tests dir");
    fs::write(tests_dir.join("suite.jv"), TEST_SUITE_SOURCE).expect("write test suite");

    fs::write(root.join("pom.xml"), POM_XML).expect("write pom");
}

fn create_fake_maven_home(root: &Path, log_path: &Path) {
    let bin_dir = root.join("bin");
    fs::create_dir_all(&bin_dir).expect("create fake maven bin dir");
    let log = log_path.to_string_lossy().replace('"', "\\\"");

    let unix_script = format!(
        r#"#!/usr/bin/env bash
set -euo pipefail

LOG_FILE="{log}"

{{
  echo "TARGET=${{JV_TEST_TARGET:-}}"
  echo "JAVA_HOME=${{JAVA_HOME:-}}"
  echo "GENERATED=${{JV_GENERATED_TESTS:-}}"
  printf 'ARGS=%s\n' "$*"
  echo "---"
}} >> "$LOG_FILE"
"#
    );
    let unix_path = bin_dir.join("mvn");
    fs::write(&unix_path, unix_script).expect("write fake mvn");
    #[cfg(unix)]
    {
        let mut perms = fs::metadata(&unix_path)
            .expect("read fake mvn permissions")
            .permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&unix_path, perms).expect("chmod fake mvn");
    }

    let win_log = log_path.to_string_lossy().replace('"', "\"\"");
    let windows_script = format!(
        r#"@echo off
set LOG_FILE="{win_log}"
(
  echo TARGET=%JV_TEST_TARGET%
  echo JAVA_HOME=%JAVA_HOME%
  echo GENERATED=%JV_GENERATED_TESTS%
  echo ARGS=%*
  echo ---
)>>"%LOG_FILE%"
exit /b 0
"#
    );
    fs::write(bin_dir.join("mvn.cmd"), windows_script).expect("write fake mvn.cmd");
}

fn run_jv_test(
    cli_path: &Path,
    project_dir: &Path,
    target: JavaTarget,
    fake_maven_home: &Path,
    java21_home: &Path,
    java25_home: &Path,
) -> Vec<PathBuf> {
    let mut command = Command::new(cli_path);
    command.current_dir(project_dir);
    command.arg("test");
    command.arg("--target");
    command.arg(target.as_str());
    command.env("MAVEN_HOME", fake_maven_home);
    command.env("JAVA21_HOME", java21_home);
    command.env("JAVA25_HOME", java25_home);

    let output = command.output().expect("invoke jv test");
    if !output.status.success() {
        panic!(
            "jv test --target {} failed:\nstdout: {}\nstderr: {}",
            target.as_str(),
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let generated_dir = project_dir
        .join("target")
        .join("generated-tests")
        .join(format!("java{}", target.as_str()));
    let mut java_files = Vec::new();
    if generated_dir.exists() {
        for entry in fs::read_dir(&generated_dir).expect("read generated dir") {
            let entry = entry.expect("dir entry");
            if entry
                .path()
                .extension()
                .and_then(|ext| ext.to_str())
                == Some("java")
            {
                java_files.push(entry.path());
            }
        }
    }
    assert!(
        !java_files.is_empty(),
        "expected generated Java files under {}",
        generated_dir.display()
    );

    java_files
}

fn parse_maven_log(log_path: &Path) -> Vec<String> {
    if !log_path.exists() {
        return Vec::new();
    }
    let content = fs::read_to_string(log_path).expect("read fake maven log");
    content
        .split("\n---")
        .filter_map(|section| {
            let trimmed = section.trim();
            (!trimmed.is_empty()).then_some(trimmed.to_string())
        })
        .collect()
}

#[test]
fn jv_test_generates_junit_sources_and_invokes_maven_per_target() {
    let Some(cli_path) = std::env::var_os("CARGO_BIN_EXE_jv").map(PathBuf::from) else {
        eprintln!("Skipping jv test integration: CARGO_BIN_EXE_jv is not available");
        return;
    };

    let Some(java25_home) = toolchain_java_home(JavaTarget::Java25) else {
        eprintln!("Skipping jv test integration: toolchains/jdk25 not found");
        return;
    };
    let Some(java21_home) = toolchain_java_home(JavaTarget::Java21) else {
        eprintln!("Skipping jv test integration: toolchains/jdk21 not found");
        return;
    };

    let fixture = TempDirGuard::new("suite").expect("create temp project dir");
    write_project_files(fixture.path());

    let log_path = fixture.path().join("maven_invocations.log");
    let fake_maven_home = fixture.path().join("fake-maven");
    create_fake_maven_home(&fake_maven_home, &log_path);
    fs::write(&log_path, "").expect("truncate log file");

    let java_files_25 = run_jv_test(
        &cli_path,
        fixture.path(),
        JavaTarget::Java25,
        &fake_maven_home,
        &java21_home,
        &java25_home,
    );
    assert!(
        java_files_25
            .iter()
            .any(|file| fs::read_to_string(file).unwrap().contains("@Test")),
        "Java25 target should include @Test annotations:\n{:#?}",
        java_files_25
    );

    let java_files_21 = run_jv_test(
        &cli_path,
        fixture.path(),
        JavaTarget::Java21,
        &fake_maven_home,
        &java21_home,
        &java25_home,
    );
    assert!(
        java_files_21
            .iter()
            .any(|file| fs::read_to_string(file).unwrap().contains("@ParameterizedTest")),
        "Java21 target should include parameterized tests:\n{:#?}",
        java_files_21
    );

    let invocations = parse_maven_log(&log_path);
    assert_eq!(
        invocations.len(),
        2,
        "expected Maven to be invoked once per Java target, log:\n{}",
        fs::read_to_string(&log_path).unwrap_or_default()
    );
    for (section, expected_target) in invocations.iter().zip(["25", "21"]) {
        assert!(
            section.contains(&format!("TARGET={expected_target}")),
            "JV_TEST_TARGET should be passed to Maven stub: {section}"
        );
        let expected_home = if expected_target == "25" {
            java25_home.display().to_string()
        } else {
            java21_home.display().to_string()
        };
        assert!(
            section.contains(&expected_home),
            "JAVA_HOME should be propagated (expected {expected_home}): {section}"
        );
        assert!(
            section.contains("jv.generated.tests=") || section.contains("GENERATED="),
            "jv.generated.tests property should be forwarded: {section}"
        );
    }
}
