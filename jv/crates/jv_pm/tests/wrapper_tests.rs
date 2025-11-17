use std::{collections::HashMap, env, fs, path::Path, sync::Mutex};

use once_cell::sync::Lazy;
use tempfile::tempdir;

use jv_pm::{
    LoggingConfig, Manifest, MavenIntegrationConfig, MavenIntegrationDispatcher,
    MavenProjectMetadata, PackageInfo, ProjectSection, RepositorySection,
    cli::{Commands, ResolverCommand},
    lockfile::{LockedSource, LockfileService},
    resolver::{
        DependencyScope, ResolutionSource, ResolutionStats, ResolvedDependencies,
        ResolvedDependency, ResolverAlgorithmKind, VersionDecision,
    },
    wrapper::{
        context::WrapperContext,
        error::WrapperError,
        filter::{CliMode, WrapperCommandFilter},
        lockfile::WrapperLockfileWriter,
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

fn create_file(path: &Path, name: &str) {
    fs::write(path.join(name), "").expect("write file");
}

#[test]
fn detect_returns_error_for_jv_native_project() {
    let temp = tempdir().expect("temp");
    create_file(temp.path(), "jv.toml");

    let err = run_in_directory(temp.path(), || WrapperContext::detect()).unwrap_err();
    assert!(matches!(err, WrapperError::NativeProjectDetected));
}

#[test]
fn detect_returns_error_for_mixed_configuration() {
    let temp = tempdir().expect("temp");
    create_file(temp.path(), "jv.toml");
    create_file(temp.path(), "pom.xml");

    let err = run_in_directory(temp.path(), || WrapperContext::detect()).unwrap_err();
    assert!(matches!(err, WrapperError::MixedProjectConfiguration));
}

#[test]
fn detect_loads_context_for_maven_project() {
    let temp = tempdir().expect("temp");
    create_file(temp.path(), "pom.xml");

    let context = run_in_directory(temp.path(), || WrapperContext::detect()).expect("context");
    assert!(!context.template_generated);
    assert!(context.pom_path.exists());
    assert!(context.settings_path.exists());
}

#[test]
fn detect_generates_template_when_artifacts_missing() {
    let temp = tempdir().expect("temp");

    let context = run_in_directory(temp.path(), || WrapperContext::detect()).expect("context");
    assert!(context.template_generated);
    assert!(context.pom_path.exists());
    assert!(context.settings_path.exists());
    assert!(context.project_root.exists());
}

#[cfg(unix)]
#[test]
fn detect_handles_template_generation_failure() {
    let temp = tempdir().expect("temp");
    let _guard = ReadOnlyGuard::new(temp.path());

    let err = run_in_directory(temp.path(), || WrapperContext::detect()).unwrap_err();
    assert!(matches!(err, WrapperError::OperationFailed(_)));
}

#[cfg(unix)]
struct ReadOnlyGuard {
    path: std::path::PathBuf,
    original: fs::Permissions,
}

#[cfg(unix)]
impl ReadOnlyGuard {
    fn new(path: &Path) -> Self {
        use std::os::unix::fs::PermissionsExt;

        let original = fs::metadata(path).expect("metadata").permissions();
        fs::set_permissions(path, fs::Permissions::from_mode(0o555)).expect("set readonly");
        Self {
            path: path.to_path_buf(),
            original,
        }
    }
}

#[cfg(unix)]
impl Drop for ReadOnlyGuard {
    fn drop(&mut self) {
        let _ = fs::set_permissions(&self.path, self.original.clone());
    }
}

#[test]
fn wrapper_lockfile_writer_generates_sorted_lock_dependencies() {
    let resolved = ResolvedDependencies {
        strategy: "pubgrub".to_string(),
        algorithm: ResolverAlgorithmKind::PubGrub,
        dependencies: vec![
            ResolvedDependency {
                name: "org.zeta:core".to_string(),
                requested: "^1.0".to_string(),
                decision: VersionDecision::Exact("1.0.0".to_string()),
                scope: DependencyScope::Main,
                source: ResolutionSource::Registry,
            },
            ResolvedDependency {
                name: "org.alpha:util".to_string(),
                requested: "^2.0".to_string(),
                decision: VersionDecision::Exact("2.1.0".to_string()),
                scope: DependencyScope::Main,
                source: ResolutionSource::Registry,
            },
        ],
        diagnostics: Vec::new(),
        stats: ResolutionStats {
            elapsed_ms: 0,
            total_dependencies: 2,
            decided_dependencies: 2,
        },
    };

    let temp = tempdir().expect("temp");
    WrapperLockfileWriter::write_lockfile(temp.path(), &resolved).expect("write lockfile");

    let lockfile_path = temp.path().join("jv.lock");
    let lockfile = LockfileService::load(&lockfile_path).expect("load lockfile");
    let root_package = lockfile
        .packages
        .iter()
        .find(|package| package.source == LockedSource::Lockfile)
        .expect("root package");

    let ordered_names = root_package
        .dependencies
        .iter()
        .map(|dependency| dependency.name.clone())
        .collect::<Vec<_>>();
    assert_eq!(ordered_names, vec!["org.alpha:util", "org.zeta:core"]);
}

#[test]
fn wrapper_command_filter_rejects_jv_commands_in_wrapper_mode() {
    let command = Commands::Resolver(ResolverCommand::List { json: false });
    let err = WrapperCommandFilter::validate(&command, CliMode::Wrapper).unwrap_err();
    assert!(matches!(err, WrapperError::OperationFailed(_)));

    assert!(WrapperCommandFilter::validate(&command, CliMode::Native).is_ok());
}

#[test]
fn wrapper_integration_strategy_generates_maven_artifacts() {
    let manifest = sample_manifest();
    let resolved = sample_resolved();

    let temp = tempdir().expect("temp dir");
    let local_repo = temp.path().join("repository");
    fs::create_dir_all(&local_repo).expect("create local repo");

    let dispatcher = MavenIntegrationDispatcher::new();
    let config = MavenIntegrationConfig {
        manifest: Some(&manifest),
        resolved: &resolved,
        lockfile: None,
        repositories: &[],
        mirrors: &[],
        project_root: Path::new("."),
        local_repository: &local_repo,
    };

    let files = dispatcher
        .generate("wrapper-default", &config)
        .expect("wrapper integration");

    let pom = files
        .files
        .iter()
        .find(|(path, _)| path == Path::new("pom.xml"))
        .expect("pom.xml generated")
        .1
        .as_str();
    assert!(pom.contains("<artifactId>demo-app</artifactId>"));
    assert!(pom.contains("<dependency>"));

    let settings = files
        .files
        .iter()
        .find(|(path, _)| path == Path::new("settings.xml"))
        .expect("settings.xml generated")
        .1
        .as_str();
    assert!(settings.contains("<localRepository>"));
}

fn sample_manifest() -> Manifest {
    Manifest {
        package: PackageInfo {
            name: "demo-app".to_string(),
            version: "1.2.3".to_string(),
            description: Some("デモアプリ".to_string()),
            dependencies: HashMap::from([
                ("org.example:demo-lib".to_string(), "^1.0".to_string()),
                ("org.example:test-kit".to_string(), "^2.0".to_string()),
            ]),
        },
        project: ProjectSection::default(),
        repositories: RepositorySection::default(),
        mirrors: Vec::new(),
        build: None,
        logging: LoggingConfig::default(),
        maven: MavenProjectMetadata {
            group_id: "com.example".to_string(),
            artifact_id: Some("demo-app".to_string()),
            packaging: Some("jar".to_string()),
            description: None,
            url: Some("https://example.com".to_string()),
        },
    }
}

fn sample_resolved() -> ResolvedDependencies {
    ResolvedDependencies {
        strategy: "pubgrub".to_string(),
        algorithm: ResolverAlgorithmKind::PubGrub,
        dependencies: vec![
            ResolvedDependency {
                name: "org.example:demo-lib".to_string(),
                requested: "^1.0".to_string(),
                decision: VersionDecision::Exact("1.0.1".to_string()),
                scope: DependencyScope::Main,
                source: ResolutionSource::Registry,
            },
            ResolvedDependency {
                name: "org.example:test-kit".to_string(),
                requested: "^2.0".to_string(),
                decision: VersionDecision::Exact("2.1.0".to_string()),
                scope: DependencyScope::Dev,
                source: ResolutionSource::Registry,
            },
        ],
        diagnostics: Vec::new(),
        stats: ResolutionStats {
            elapsed_ms: 0,
            total_dependencies: 2,
            decided_dependencies: 2,
        },
    }
}
