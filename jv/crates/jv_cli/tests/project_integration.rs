use std::fs;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};

use jv_cli::pipeline::project::{
    layout::ProjectLayout, locator::ProjectLocator, manifest::ManifestLoader,
};
use jv_cli::pipeline::{BuildOptionsFactory, CliOverrides, OutputManager, compile};
use jv_pm::{JavaTarget, LoggingConfigLayer};

const SAMPLE_MAIN: &str = r#"
val message = "integration"
"#;

const SAMPLE_HELPER: &str = r#"
fun helper(value: Int): Int {
    value + 1
}
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
        let path = base.join(format!("jv-project-integration-{}-{}", label, timestamp));
        fs::create_dir_all(&path)?;
        Ok(Self { path })
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        // Keep build artifacts for post-test inspection.
    }
}

fn write_manifest(root: &Path) {
    fs::write(
        root.join("jv.toml"),
        r#"[package]
name = "cli-project-structure"
version = "0.1.0"

[package.dependencies]

[build]
java_version = "21"

[project]
entrypoint = "src/app/main.jv"

[project.sources]
include = ["src/**/*.jv", "features/**/*.jv"]
exclude = ["src/excluded/**/*.jv"]

[project.output]
directory = "dist"
clean = false
"#,
    )
    .expect("manifest should be written");
}

fn create_project_structure(root: &Path) {
    fs::create_dir_all(root.join("src/app")).expect("src/app directory");
    fs::create_dir_all(root.join("features/util")).expect("features/util directory");
    fs::create_dir_all(root.join("src/excluded")).expect("src/excluded directory");

    fs::write(root.join("src/app/main.jv"), SAMPLE_MAIN).expect("main source");
    fs::write(root.join("features/util/helper.jv"), SAMPLE_HELPER).expect("helper source");
    fs::write(root.join("src/excluded/ignore.jv"), SAMPLE_HELPER).expect("excluded source");
}

fn relative(path: &Path, root: &Path) -> String {
    path.strip_prefix(root)
        .map(|value| value.to_string_lossy().replace('\\', "/"))
        .unwrap_or_else(|_| path.to_string_lossy().into_owned())
}

#[test]
fn project_pipeline_supports_multi_target_builds() {
    let fixture = TempDirGuard::new("multi-target").expect("temp dir");
    let root = fixture.path();
    write_manifest(root);
    create_project_structure(root);

    let nested_start = root.join("src/app");
    let project_root = ProjectLocator::new()
        .locate(&nested_start)
        .expect("project root detected");

    let manifest_path = project_root.manifest_path().to_path_buf();
    let settings = ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = ProjectLayout::from_settings(&project_root, &settings).expect("layout builds");

    let sources: Vec<String> = layout
        .sources()
        .iter()
        .map(|path| relative(path, project_root.root_dir()))
        .collect();

    assert!(sources.contains(&"src/app/main.jv".to_string()));
    assert!(sources.contains(&"features/util/helper.jv".to_string()));
    assert!(!sources.iter().any(|path| path.contains("excluded")));
    assert_eq!(
        relative(layout.entrypoint(), project_root.root_dir()),
        "src/app/main.jv"
    );

    let output_base = project_root.join("dist");

    let overrides21 = CliOverrides {
        entrypoint: None,
        output: Some(output_base.clone()),
        java_only: true,
        check: true,
        format: false,
        target: Some(JavaTarget::Java21),
        clean: false,
        perf: false,
        emit_types: false,
        verbose: false,
        emit_telemetry: false,
        parallel_inference: false,
        inference_workers: None,
        constraint_batch: None,
        // APT defaults
        apt_enabled: false,
        apt_processors: None,
        apt_processorpath: None,
        apt_options: Vec::new(),
        logging_cli: LoggingConfigLayer::default(),
        logging_env: LoggingConfigLayer::default(),
    };

    let plan21 = BuildOptionsFactory::compose(
        project_root.clone(),
        settings.clone(),
        layout.clone(),
        overrides21,
    )
    .expect("plan for java21");

    let mut prepared21 = OutputManager::prepare(plan21).expect("output prepared");
    let artifacts21 = compile(prepared21.plan()).expect("compile java21 succeeds");
    let target_dir_21 = prepared21.target_dir().to_path_buf();
    prepared21.mark_success();

    assert!(target_dir_21.ends_with("java21"));
    assert!(!artifacts21.java_files.is_empty());
    for java in &artifacts21.java_files {
        assert!(java.starts_with(&target_dir_21));
    }
    let report21 = artifacts21.compatibility.expect("compatibility report");
    assert!(report21.json_path.starts_with(&target_dir_21));

    let overrides25 = CliOverrides {
        entrypoint: None,
        output: Some(output_base.clone()),
        java_only: true,
        check: true,
        format: true,
        target: Some(JavaTarget::Java25),
        clean: false,
        perf: false,
        emit_types: false,
        verbose: false,
        emit_telemetry: false,
        parallel_inference: false,
        inference_workers: None,
        constraint_batch: None,
        apt_enabled: false,
        apt_processors: None,
        apt_processorpath: None,
        apt_options: Vec::new(),
        logging_cli: LoggingConfigLayer::default(),
        logging_env: LoggingConfigLayer::default(),
    };

    let plan25 = BuildOptionsFactory::compose(project_root, settings, layout, overrides25)
        .expect("plan for java25");

    let mut prepared25 = OutputManager::prepare(plan25).expect("output prepared");
    let artifacts25 = compile(prepared25.plan()).expect("compile java25 succeeds");
    let target_dir_25 = prepared25.target_dir().to_path_buf();
    prepared25.mark_success();

    assert!(target_dir_25.ends_with("java25"));
    assert!(target_dir_21 != target_dir_25);
    assert!(target_dir_21.exists(), "java21 directory retained");
    assert!(target_dir_25.exists(), "java25 directory present");
    assert!(!artifacts25.java_files.is_empty());
    for java in &artifacts25.java_files {
        assert!(java.starts_with(&target_dir_25));
    }
}

#[test]
fn clean_option_removes_stale_target_artifacts() {
    let fixture = TempDirGuard::new("clean-target").expect("temp dir");
    let root = fixture.path();
    write_manifest(root);
    create_project_structure(root);

    let project_root = ProjectLocator::new()
        .locate(root.join("src/app"))
        .expect("project root detected");
    let manifest_path = project_root.manifest_path().to_path_buf();
    let settings = ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = ProjectLayout::from_settings(&project_root, &settings).expect("layout builds");
    let output_base = project_root.join("dist");

    let base_overrides = |clean: bool| CliOverrides {
        entrypoint: None,
        output: Some(output_base.clone()),
        java_only: true,
        check: true,
        format: false,
        target: Some(JavaTarget::Java21),
        clean,
        perf: false,
        emit_types: false,
        verbose: false,
        emit_telemetry: false,
        parallel_inference: false,
        inference_workers: None,
        constraint_batch: None,
        apt_enabled: false,
        apt_processors: None,
        apt_processorpath: None,
        apt_options: Vec::new(),
        logging_cli: LoggingConfigLayer::default(),
        logging_env: LoggingConfigLayer::default(),
    };

    let plan_initial = BuildOptionsFactory::compose(
        project_root.clone(),
        settings.clone(),
        layout.clone(),
        base_overrides(false),
    )
    .expect("initial plan succeeds");

    let mut prepared_initial = OutputManager::prepare(plan_initial).expect("prepare initial");
    let artifacts_initial = compile(prepared_initial.plan()).expect("initial compile");
    assert!(!artifacts_initial.java_files.is_empty());
    let target_dir = prepared_initial.target_dir().to_path_buf();
    fs::write(target_dir.join("stale.log"), "old").expect("stale marker");
    prepared_initial.mark_success();
    assert!(target_dir.join("stale.log").exists());

    let plan_clean =
        BuildOptionsFactory::compose(project_root, settings, layout, base_overrides(true))
            .expect("clean plan succeeds");

    let mut prepared_clean = OutputManager::prepare(plan_clean).expect("prepare clean");
    assert!(prepared_clean.clean_applied(), "clean flag should trigger");
    assert!(
        !prepared_clean.target_dir().join("stale.log").exists(),
        "stale artifacts removed"
    );

    let artifacts_clean = compile(prepared_clean.plan()).expect("clean compile");
    assert!(!artifacts_clean.java_files.is_empty());
    prepared_clean.mark_success();
}
mod jv_pm_phase2_e2e {
    include!("../../../../tests/e2e/jv_pm_phase2_e2e.rs");
}
