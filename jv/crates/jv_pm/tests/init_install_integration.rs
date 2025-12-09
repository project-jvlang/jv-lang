//! Integration tests for init and install commands.

use std::{env, fs, path::Path, sync::Mutex};

use once_cell::sync::Lazy;
use tempfile::tempdir;

use jv_pm::{
    cli::InitArgs,
    wrapper::{
        error::WrapperError,
        init::{InitConfig, JavaVersion, Packaging, ProjectInitializer},
    },
};

static ENV_LOCK: Lazy<Mutex<()>> = Lazy::new(|| Mutex::new(()));

fn run_in_directory<T>(path: &Path, test: impl FnOnce() -> T) -> T {
    let _guard = ENV_LOCK.lock().unwrap();
    let original = env::current_dir().expect("current dir");
    env::set_current_dir(path).expect("set cwd");
    let result = test();
    env::set_current_dir(original).expect("restore cwd");
    result
}

// =============================================================================
// Init command integration tests
// =============================================================================

#[test]
fn init_creates_jar_project_with_correct_structure() {
    let temp = tempdir().expect("temp");
    let project_dir = temp.path().join("my-jar-app");

    let config = InitConfig {
        group_id: "com.example".to_string(),
        artifact_id: "my-jar-app".to_string(),
        version: "1.0.0".to_string(),
        packaging: Packaging::Jar,
        java_version: JavaVersion::Java21,
        target_dir: project_dir.clone(),
    };

    let initializer = ProjectInitializer::new(config);
    let summary = initializer.generate().expect("generate");

    // Verify pom.xml
    assert!(summary.pom_path.exists());
    let pom = fs::read_to_string(&summary.pom_path).expect("read pom");
    assert!(pom.contains("<groupId>com.example</groupId>"));
    assert!(pom.contains("<artifactId>my-jar-app</artifactId>"));
    assert!(pom.contains("<version>1.0.0</version>"));
    assert!(pom.contains("<packaging>jar</packaging>"));
    assert!(pom.contains("<maven.compiler.source>21</maven.compiler.source>"));
    assert!(pom.contains("<maven.compiler.target>21</maven.compiler.target>"));

    // Verify directory structure
    assert!(project_dir.join("src/main/java").is_dir());
    assert!(project_dir.join("src/main/resources").is_dir());
    assert!(project_dir.join("src/test/java").is_dir());
    assert!(project_dir.join("src/test/resources").is_dir());

    // Verify .gitignore
    assert!(project_dir.join(".gitignore").exists());
}

#[test]
fn init_creates_war_project_with_webapp_directory() {
    let temp = tempdir().expect("temp");
    let project_dir = temp.path().join("my-war-app");

    let config = InitConfig {
        group_id: "com.example".to_string(),
        artifact_id: "my-war-app".to_string(),
        version: "0.1.0".to_string(),
        packaging: Packaging::War,
        java_version: JavaVersion::Java17,
        target_dir: project_dir.clone(),
    };

    let initializer = ProjectInitializer::new(config);
    let summary = initializer.generate().expect("generate");

    // Verify pom.xml
    let pom = fs::read_to_string(&summary.pom_path).expect("read pom");
    assert!(pom.contains("<packaging>war</packaging>"));
    assert!(pom.contains("<maven.compiler.source>17</maven.compiler.source>"));

    // Verify war-specific directories
    assert!(project_dir.join("src/main/java").is_dir());
    assert!(project_dir.join("src/main/webapp").is_dir());
    assert!(project_dir.join("src/main/webapp/WEB-INF").is_dir());
}

#[test]
fn init_creates_pom_project_with_modules_section() {
    let temp = tempdir().expect("temp");
    let project_dir = temp.path().join("my-parent");

    let config = InitConfig {
        group_id: "com.example".to_string(),
        artifact_id: "my-parent".to_string(),
        version: "1.0.0".to_string(),
        packaging: Packaging::Pom,
        java_version: JavaVersion::Java25,
        target_dir: project_dir.clone(),
    };

    let initializer = ProjectInitializer::new(config);
    let summary = initializer.generate().expect("generate");

    // Verify pom.xml has modules section
    let pom = fs::read_to_string(&summary.pom_path).expect("read pom");
    assert!(pom.contains("<packaging>pom</packaging>"));
    assert!(pom.contains("<modules>"));
    assert!(pom.contains("</modules>"));

    // Verify no source directories for pom packaging
    assert!(!project_dir.join("src").exists());
}

#[test]
fn init_non_interactive_uses_defaults() {
    let temp = tempdir().expect("temp");
    let project_dir = temp.path().join("default-project");

    let args = InitArgs {
        group_id: None,
        artifact_id: None,
        version: None,
        packaging: None,
        java_version: None,
        non_interactive: true,
        directory: Some(project_dir.clone()),
    };

    let config = InitConfig::from_args_non_interactive(&args).expect("config");
    assert_eq!(config.group_id, "com.example");
    assert_eq!(config.artifact_id, "default-project"); // derived from directory
    assert_eq!(config.version, "0.1.0-SNAPSHOT");
    assert_eq!(config.packaging, Packaging::Jar);
    assert_eq!(config.java_version, JavaVersion::Java25);
}

#[test]
fn init_fails_when_pom_exists() {
    let temp = tempdir().expect("temp");

    // Create existing pom.xml
    fs::write(temp.path().join("pom.xml"), "<project/>").expect("write");

    let config = InitConfig {
        group_id: "com.example".to_string(),
        artifact_id: "test".to_string(),
        version: "1.0.0".to_string(),
        packaging: Packaging::Jar,
        java_version: JavaVersion::Java25,
        target_dir: temp.path().to_path_buf(),
    };

    let initializer = ProjectInitializer::new(config);
    let err = initializer.validate().unwrap_err();
    assert!(matches!(err, WrapperError::ProjectAlreadyExists));
}

#[test]
fn init_fails_when_jv_toml_exists() {
    let temp = tempdir().expect("temp");

    // Create existing jv.toml
    fs::write(temp.path().join("jv.toml"), "[package]").expect("write");

    let config = InitConfig {
        group_id: "com.example".to_string(),
        artifact_id: "test".to_string(),
        version: "1.0.0".to_string(),
        packaging: Packaging::Jar,
        java_version: JavaVersion::Java25,
        target_dir: temp.path().to_path_buf(),
    };

    let initializer = ProjectInitializer::new(config);
    let err = initializer.validate().unwrap_err();
    assert!(matches!(err, WrapperError::NativeProjectDetected));
}

#[test]
fn init_fails_when_directory_not_empty() {
    let temp = tempdir().expect("temp");

    // Create some file (not pom.xml or jv.toml)
    fs::write(temp.path().join("README.md"), "# Test").expect("write");

    let config = InitConfig {
        group_id: "com.example".to_string(),
        artifact_id: "test".to_string(),
        version: "1.0.0".to_string(),
        packaging: Packaging::Jar,
        java_version: JavaVersion::Java25,
        target_dir: temp.path().to_path_buf(),
    };

    let initializer = ProjectInitializer::new(config);
    let err = initializer.validate().unwrap_err();
    assert!(matches!(err, WrapperError::DestinationNotEmpty(_)));
}

#[test]
fn init_creates_nested_directory_structure() {
    let temp = tempdir().expect("temp");
    let project_dir = temp.path().join("projects/java/my-app");

    let config = InitConfig {
        group_id: "com.example".to_string(),
        artifact_id: "my-app".to_string(),
        version: "1.0.0".to_string(),
        packaging: Packaging::Jar,
        java_version: JavaVersion::Java25,
        target_dir: project_dir.clone(),
    };

    let initializer = ProjectInitializer::new(config);
    let summary = initializer.generate().expect("generate");

    // Verify nested directories created
    assert!(project_dir.exists());
    assert!(summary.pom_path.exists());
}

// =============================================================================
// Init + WrapperContext integration tests
// =============================================================================

#[test]
fn init_followed_by_wrapper_context_detect() {
    use jv_pm::wrapper::context::WrapperContext;

    let temp = tempdir().expect("temp");
    let project_dir = temp.path().join("wrapper-test");

    // Create project with init
    let config = InitConfig {
        group_id: "com.example".to_string(),
        artifact_id: "wrapper-test".to_string(),
        version: "1.0.0".to_string(),
        packaging: Packaging::Jar,
        java_version: JavaVersion::Java25,
        target_dir: project_dir.clone(),
    };

    let initializer = ProjectInitializer::new(config);
    initializer.generate().expect("generate");

    // WrapperContext should detect the initialized project
    let context = run_in_directory(&project_dir, || WrapperContext::detect()).expect("detect");
    assert!(!context.template_generated); // pom.xml already exists from init
    assert!(context.pom_path.exists());
}

// =============================================================================
// InstallConfig tests (basic integration)
// =============================================================================

#[test]
fn install_config_parses_maven_arguments_correctly() {
    use jv_pm::cli::InstallArgs;
    use jv_pm::wrapper::install::InstallConfig;
    use std::ffi::OsString;

    let args = InstallArgs {
        maven_args: vec![
            OsString::from("-DskipTests"),
            OsString::from("-Dmaven.javadoc.skip=true"),
            OsString::from("-o"),
        ],
    };

    let config = InstallConfig::from_args(&args);
    assert_eq!(config.maven_args.len(), 3);
    assert_eq!(config.maven_args[0], OsString::from("-DskipTests"));
}

// =============================================================================
// Command filter integration tests
// =============================================================================

#[test]
fn init_command_allowed_in_wrapper_mode() {
    use jv_pm::cli::Commands;
    use jv_pm::wrapper::filter::{CliMode, WrapperCommandFilter};

    let command = Commands::Init(InitArgs {
        group_id: None,
        artifact_id: None,
        version: None,
        packaging: None,
        java_version: None,
        non_interactive: true,
        directory: None,
    });

    // init should be allowed in wrapper mode
    assert!(WrapperCommandFilter::validate(&command, CliMode::Wrapper).is_ok());
}

#[test]
fn install_command_allowed_in_wrapper_mode() {
    use jv_pm::cli::{Commands, InstallArgs};
    use jv_pm::wrapper::filter::{CliMode, WrapperCommandFilter};
    use std::ffi::OsString;

    let command = Commands::Install(InstallArgs {
        maven_args: vec![OsString::from("-DskipTests")],
    });

    // install should be allowed in wrapper mode
    assert!(WrapperCommandFilter::validate(&command, CliMode::Wrapper).is_ok());
}
