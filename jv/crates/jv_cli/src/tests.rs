use super::*;
use crate::commands::explain;
use crate::pipeline::compute_script_main_class;

use crate::pipeline::generics::{apply_type_facts, find_primitive_specialization_hint};
use jv_ast::types::{Kind, PrimitiveTypeName};
use jv_inference::constraint::{ConstraintGraph, ConstraintSolution, WhereConstraintResolver};
use jv_inference::service::{TypeFacts, TypeFactsBuilder, TypeLevelValue, TypeScheme};
use jv_inference::solver::Variance;
use jv_inference::types::{
    BoundConstraint, BoundPredicate, GenericBounds, PrimitiveBoundConstraint, SymbolId, TypeId,
    TypeKind, TypeVariant,
};
use jv_ir::{
    IrExpression, IrModifiers, IrParameter, IrProgram, IrStatement, IrTypeLevelValue,
    IrTypeParameter, IrVariance, JavaType, LoggingMetadata, PipelineShape,
    PrimitiveSpecializationHint, SequencePipeline, SequenceSource, SequenceTerminal,
    SequenceTerminalEvaluation, SequenceTerminalKind,
};
use jv_pm::LoggingConfigLayer;
use std::collections::HashMap;

mod compat;
mod project_layout;
mod project_locator;
mod project_output;

use jv_build::{BuildConfig, BuildSystem, JavaTarget};
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, MutexGuard, OnceLock};
use std::time::{SystemTime, UNIX_EPOCH};
use zip::write::FileOptions;

fn dummy_span() -> jv_ast::Span {
    jv_ast::Span::dummy()
}

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
    fn new(prefix: &str) -> Self {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let base = workspace_temp_root();
        fs::create_dir_all(&base).unwrap();
        let path = base.join(format!("jv-cli-{}-{}", prefix, timestamp));
        fs::create_dir_all(&path).unwrap();
        Self { path }
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

fn current_dir_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

fn lock_current_dir() -> MutexGuard<'static, ()> {
    current_dir_lock()
        .lock()
        .unwrap_or_else(|poison| poison.into_inner())
}

fn fixtures_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../tests/fixtures")
}

fn collect_fixture_files(dir: &Path, files: &mut Vec<PathBuf>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.file_name().is_some_and(|name| name == "target") {
                continue;
            }
            if path.is_dir() {
                collect_fixture_files(&path, files);
            } else if path.extension().is_some_and(|ext| ext == "jv") {
                files.push(path);
            }
        }
    }
}

#[test]
fn script_main_class_from_numeric_file_stem() {
    let path = Path::new("tests/fixtures/02-variables.jv");
    let class = compute_script_main_class("", path);
    assert_eq!(class, "ZeroTwoVariables");
}

#[test]
fn script_main_class_from_package_name_retains_main_suffix() {
    let path = Path::new("src/main.jv");
    let class = compute_script_main_class("analytics-dashboard", path);
    assert_eq!(class, "AnalyticsDashboardMain");
}

#[test]
fn script_main_class_converts_embedded_digits() {
    let path = Path::new("reports/monthly-2025.jv");
    let class = compute_script_main_class("", path);
    assert_eq!(class, "MonthlyTwoZeroTwoFive");
}

#[test]
fn test_cli_parsing() {
    // Test version command
    let version_args = vec!["jv", "version"];
    let cli = Cli::try_parse_from(version_args).unwrap();
    assert!(matches!(cli.command, Some(Commands::Version)));

    // Test init command
    let init_args = vec!["jv", "init", "my-project"];
    let cli = Cli::try_parse_from(init_args).unwrap();
    match cli.command {
        Some(Commands::Init { name }) => assert_eq!(name, "my-project"),
        _ => panic!("Expected Init command"),
    }
}

#[test]
fn test_build_command_parsing() {
    let build_args = vec![
        "jv", "build", "test.jv", "-o", "output", "--check", "--format",
    ];
    let cli = Cli::try_parse_from(build_args).unwrap();

    match cli.command {
        Some(Commands::Build {
            input,
            output,
            java_only,
            check,
            format,
            clean,
            perf,
            ..
        }) => {
            assert_eq!(input.as_deref(), Some("test.jv"));
            assert_eq!(output.as_deref(), Some("output"));
            assert!(!java_only);
            assert!(check);
            assert!(format);
            assert!(!clean);
            assert!(!perf);
        }
        _ => panic!("Expected Build command"),
    }
}

#[test]
fn test_build_command_parsing_with_apt() {
    let build_args = vec![
        "jv",
        "build",
        "test.jv",
        "--apt",
        "--processors",
        "org.example.Proc1,Proc2",
        "--processorpath",
        "libs/anno.jar",
        "--apt-option",
        "mapstruct.defaultComponentModel=spring",
        "--apt-option",
        "flag",
    ];
    let cli = Cli::try_parse_from(build_args).unwrap();

    match cli.command {
        Some(Commands::Build {
            apt,
            processors,
            processorpath,
            apt_options,
            ..
        }) => {
            assert!(apt, "--apt should enable annotation processing");
            assert_eq!(processors.as_deref(), Some("org.example.Proc1,Proc2"));
            assert_eq!(processorpath.as_deref(), Some("libs/anno.jar"));
            assert!(apt_options.iter().any(|o| o.contains("mapstruct")));
            assert!(apt_options.iter().any(|o| o == "flag"));
        }
        _ => panic!("Expected Build command"),
    }
}

#[test]
fn test_build_plan_applies_apt_overrides() {
    let _guard = lock_current_dir();
    let temp_dir = TempDirGuard::new("apt-plan");
    let root_path = temp_dir.path();
    let manifest_path = root_path.join("jv.toml");
    let src_dir = root_path.join("src");
    let entrypoint = src_dir.join("main.jv");
    fs::create_dir_all(&src_dir).expect("create src");
    let manifest = "[package]\nname = \"apt-plan\"\nversion = \"0.1.0\"\n\n[project]\nentrypoint = \"src/main.jv\"\n\n[project.sources]\ninclude = [\"src/**/*.jv\"]\n";
    fs::write(&manifest_path, manifest).expect("write manifest");
    fs::write(&entrypoint, "fun main() {}\n").expect("write entrypoint");

    let project_root = pipeline::project::locator::ProjectRoot::new(
        root_path.to_path_buf(),
        manifest_path.clone(),
    );
    let settings =
        pipeline::project::manifest::ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = pipeline::project::layout::ProjectLayout::from_settings(&project_root, &settings)
        .expect("layout resolves");

    let overrides = pipeline::CliOverrides {
        entrypoint: Some(entrypoint.clone()),
        output: Some(root_path.join("target")),
        java_only: true,
        check: false,
        format: false,
        target: None,
        clean: true,
        perf: false,
        emit_types: false,
        verbose: false,
        emit_telemetry: false,
        parallel_inference: false,
        inference_workers: None,
        constraint_batch: None,
        apt_enabled: true,
        apt_processors: Some("org.example.Proc1,Proc2".to_string()),
        apt_processorpath: Some("libs/anno.jar".to_string()),
        apt_options: vec![
            "mapstruct.defaultComponentModel=spring".to_string(),
            "flag".to_string(),
        ],
        logging_cli: LoggingConfigLayer::default(),
        logging_env: LoggingConfigLayer::default(),
    };

    let plan = pipeline::BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("compose");
    let apt = &plan.build_config.apt;
    assert!(apt.enabled);
    assert_eq!(
        apt.processors,
        vec!["org.example.Proc1".to_string(), "Proc2".to_string()]
    );
    assert_eq!(apt.processorpath, vec!["libs/anno.jar".to_string()]);
    assert!(apt.options.iter().any(|o| o.contains("mapstruct")));
    assert!(apt.options.iter().any(|o| o == "flag"));
}
#[test]
fn test_build_command_perf_flag() {
    let build_args = vec!["jv", "build", "test.jv", "--perf"];
    let cli = Cli::try_parse_from(build_args).unwrap();

    match cli.command {
        Some(Commands::Build { perf, .. }) => {
            assert!(perf);
        }
        _ => panic!("Expected Build command"),
    }
}

#[test]
fn test_run_command_parsing() {
    let run_args = vec!["jv", "run", "test.jv", "arg1", "arg2"];
    let cli = Cli::try_parse_from(run_args).unwrap();

    match cli.command {
        Some(Commands::Run { input, args }) => {
            assert_eq!(input, "test.jv");
            assert_eq!(args, vec!["arg1", "arg2"]);
        }
        _ => panic!("Expected Run command"),
    }
}

#[test]
fn test_fmt_command_parsing() {
    let fmt_args = vec!["jv", "fmt", "file1.jv", "file2.jv"];
    let cli = Cli::try_parse_from(fmt_args).unwrap();

    match cli.command {
        Some(Commands::Fmt { files }) => {
            assert_eq!(files, vec!["file1.jv", "file2.jv"]);
        }
        _ => panic!("Expected Fmt command"),
    }
}

#[test]
fn test_explain_command_parsing() {
    let explain_args = vec!["jv", "explain", "JV2001"];
    let cli = Cli::try_parse_from(explain_args).unwrap();

    match cli.command {
        Some(Commands::Explain { code }) => {
            assert_eq!(code, "JV2001");
        }
        _ => panic!("Expected Explain command"),
    }
}

#[test]
fn test_explain_render_known_code() {
    let rendered = explain::render_explanation("JV2002").expect("explanation should exist");
    assert!(rendered.contains("JV2002"));
    assert!(rendered.to_lowercase().contains("remediation"));
}

#[test]
fn test_check_command_parsing() {
    let check_args = vec!["jv", "check", "test.jv"];
    let cli = Cli::try_parse_from(check_args).unwrap();

    match cli.command {
        Some(Commands::Check { input }) => {
            assert_eq!(input, "test.jv");
        }
        _ => panic!("Expected Check command"),
    }
}

#[test]
fn test_no_command() {
    let no_command_args = vec!["jv"];
    let cli = Cli::try_parse_from(no_command_args).unwrap();
    assert!(cli.command.is_none());
}

#[test]
fn test_init_project_in_temp_dir() {
    let _guard = lock_current_dir();
    let temp_dir = TempDirGuard::new("init-temp");
    let temp_path = temp_dir.path().to_path_buf();

    // Change to temp directory
    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(&temp_path).unwrap();

    // Test init in current directory
    let result = init_project(".");
    assert!(result.is_ok());

    // Check that files were created
    assert!(temp_path.join("jv.toml").exists());
    assert!(temp_path.join("src").exists());
    assert!(temp_path.join("src/main.jv").exists());

    // Check jv.toml content
    let jv_toml_content = fs::read_to_string(temp_path.join("jv.toml")).unwrap();
    assert!(jv_toml_content.contains("[package]"));
    assert!(jv_toml_content.contains("java_version = \"25\""));

    // Check main.jv content
    let main_jv_content = fs::read_to_string(temp_path.join("src/main.jv")).unwrap();
    assert!(main_jv_content.contains("fun main()"));
    assert!(main_jv_content.contains("Hello, jv!"));
    assert!(main_jv_content.contains("greeting = \"Hello, jv!\""));
    assert!(main_jv_content.contains("[1 2 3]"));

    // Restore original directory
    std::env::set_current_dir(&original_dir).unwrap();
}

#[test]
fn test_init_project_with_name() {
    let _guard = lock_current_dir();
    let temp_dir = TempDirGuard::new("init-named");
    let temp_path = temp_dir.path().to_path_buf();

    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(&temp_path).unwrap();

    let project_name = "test-project";
    let result = init_project(project_name);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), project_name);

    let project_path = temp_path.join(project_name);
    assert!(project_path.exists());
    assert!(project_path.join("jv.toml").exists());
    assert!(project_path.join("src/main.jv").exists());

    std::env::set_current_dir(&original_dir).unwrap();
}

#[test]
fn test_validate_file_exists() {
    let temp_dir = TempDirGuard::new("validate");
    let temp_file = temp_dir.path().join("test.jv");

    // Test non-existent file
    let result = validate_file_exists(temp_file.to_str().unwrap());
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));

    // Create the file and test again
    fs::write(&temp_file, "val x = 42").unwrap();
    let result = validate_file_exists(temp_file.to_str().unwrap());
    assert!(result.is_ok());
}

#[test]
fn compile_tracks_binding_usage() {
    let temp_dir = TempDirGuard::new("binding-usage");
    let root_path = temp_dir.path();
    let manifest_path = root_path.join("jv.toml");

    fs::write(
        &manifest_path,
        r#"[package]
name = "binding-stats"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    )
    .expect("write manifest");

    let src_dir = root_path.join("src");
    fs::create_dir_all(&src_dir).expect("create src directory");
    let entrypoint = src_dir.join("main.jv");
    fs::write(
        &entrypoint,
        r#"result = 1
typed: Int = 2
val explicit = result + typed
var counter = 0
counter = counter + explicit
"#,
    )
    .expect("write source");

    let project_root = pipeline::project::locator::ProjectRoot::new(
        root_path.to_path_buf(),
        manifest_path.clone(),
    );
    let settings =
        pipeline::project::manifest::ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = pipeline::project::layout::ProjectLayout::from_settings(&project_root, &settings)
        .expect("layout resolves");

    let overrides = pipeline::CliOverrides {
        entrypoint: Some(entrypoint.clone()),
        output: Some(root_path.join("target")),
        java_only: true,
        check: false,
        format: false,
        target: None,
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

    let plan = pipeline::BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds");

    let artifacts = pipeline::compile(&plan).expect("program should compile");

    assert_eq!(artifacts.binding_usage.implicit, 1);
    assert_eq!(artifacts.binding_usage.implicit_typed, 1);
    assert_eq!(artifacts.binding_usage.explicit, 1);
    assert_eq!(artifacts.binding_usage.vars, 1);
}

#[test]
fn compile_reports_whitespace_diagnostic() {
    let temp_dir = TempDirGuard::new("diag");
    let project_root_path = temp_dir.path();
    let manifest_path = project_root_path.join("jv.toml");
    fs::write(
        &manifest_path,
        r#"[package]
name = "diag"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/whitespace_mix.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    )
    .unwrap();

    let src_dir = project_root_path.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    let input_path = src_dir.join("whitespace_mix.jv");
    fs::write(&input_path, "val numbers = [1, 2 3]").unwrap();

    let project_root = pipeline::project::locator::ProjectRoot::new(
        project_root_path.to_path_buf(),
        manifest_path.clone(),
    );
    let settings =
        pipeline::project::manifest::ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = pipeline::project::layout::ProjectLayout::from_settings(&project_root, &settings)
        .expect("layout resolves");

    let overrides = pipeline::CliOverrides {
        entrypoint: Some(input_path.clone()),
        output: Some(project_root.root_dir().join("out/diag")),
        java_only: true,
        check: false,
        format: false,
        target: None,
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

    let plan = pipeline::BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds");

    let result = pipeline::compile(&plan);
    assert!(result.is_err());

    let message = result.unwrap_err().to_string();
    assert!(message.contains("JV2101"));
    assert!(message.contains("カンマ") || message.contains("comma"));
}

#[test]
fn compile_without_parameter_type_annotation_uses_object() {
    let temp_dir = TempDirGuard::new("param-inference");
    let project_root_path = temp_dir.path();
    let manifest_path = project_root_path.join("jv.toml");
    fs::write(
        &manifest_path,
        r#"[package]
name = "inference-gap"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    )
    .unwrap();

    let src_dir = project_root_path.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    let input_path = src_dir.join("main.jv");
    fs::write(
        &input_path,
        r#"
            data Module(val name: String, val done: Boolean)

            fun render(item) {
                val status = when (item.done) {
                    true -> "complete"
                    false -> "pending"
                }
                return "${item.name} - ${status}"
            }

            fun main(): Unit {
                val plan = [
                    Module("lexer" true)
                    Module("parser" false)
                    Module("codegen" false)
                ]

                for (entry in plan) {
                    println(render(entry))
                }
            }
        "#,
    )
    .unwrap();

    let project_root = pipeline::project::locator::ProjectRoot::new(
        project_root_path.to_path_buf(),
        manifest_path.clone(),
    );
    let settings =
        pipeline::project::manifest::ManifestLoader::load(&manifest_path).expect("manifest loads");
    let layout = pipeline::project::layout::ProjectLayout::from_settings(&project_root, &settings)
        .expect("layout resolves");

    let overrides = pipeline::CliOverrides {
        entrypoint: Some(input_path.clone()),
        output: Some(project_root.root_dir().join("out/inference-gap")),
        java_only: true,
        check: false,
        format: false,
        target: None,
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

    let plan = pipeline::BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds");

    let artifacts = pipeline::compile(&plan).expect("program should compile");
    assert!(
        !artifacts.java_files.is_empty(),
        "expected java output to be generated"
    );

    let java_source = fs::read_to_string(&artifacts.java_files[0]).expect("read generated java");
    assert!(
        java_source.contains("render(Module item)"),
        "expected inferred record parameter type to flow into generated Java.\nGenerated Java:\n{java_source}"
    );
    assert!(
        !java_source.contains("render(Object item)"),
        "render parameter should no longer default to Object when TypeFacts supply record details.\nGenerated Java:\n{java_source}"
    );
}
