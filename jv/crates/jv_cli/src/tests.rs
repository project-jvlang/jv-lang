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
    IrTypeParameter, IrVariance, JavaType, PipelineShape, PrimitiveSpecializationHint,
    SequencePipeline, SequenceSource, SequenceTerminal, SequenceTerminalEvaluation,
    SequenceTerminalKind,
};
use std::collections::HashMap;

mod compat;
mod project_layout;
mod project_locator;
mod project_output;

use jv_build::{BuildConfig, BuildSystem, JavaTarget};
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

#[test]
fn compile_repository_fixtures_without_interpolation() {
    let _guard = lock_current_dir();
    let temp_root = TempDirGuard::new("cli-fixtures");
    let fixtures_root = fixtures_root();
    let mut files = Vec::new();
    collect_fixture_files(&fixtures_root, &mut files);
    assert!(
        !files.is_empty(),
        "expected to discover fixtures under {:?}",
        fixtures_root
    );

    let mut compiled = 0usize;
    let mut failures = Vec::new();
    for path in files {
        if path.to_string_lossy().contains("/pattern/") {
            continue;
        }
        if path.to_string_lossy().contains("/java_annotations/") {
            continue;
        }
        if path
            .to_string_lossy()
            .contains("package/complex_stdlib_pattern.jv")
        {
            continue;
        }
        if path.to_string_lossy().contains("/pattern/neg-") {
            continue;
        }
        let source = match fs::read_to_string(&path) {
            Ok(content) => content,
            Err(err) => {
                failures.push((path.display().to_string(), format!("read error: {err}")));
                continue;
            }
        };
        if source.contains("${") {
            // CLI still relies on ParserPipeline which lacks interpolation lowering.
            continue;
        }

        compiled += 1;
        let project_dir = temp_root.path().join(format!("fixture-{compiled}"));
        let manifest_path = project_dir.join("jv.toml");
        let src_dir = project_dir.join("src");
        let entrypoint = src_dir.join("main.jv");
        fs::create_dir_all(&src_dir).expect("create src directory");
        fs::write(
            &manifest_path,
            r#"[package]
name = "fixture"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
        )
        .expect("write manifest");
        fs::write(&entrypoint, source).expect("write fixture source");

        let project_root = pipeline::project::locator::ProjectRoot::new(
            project_dir.to_path_buf(),
            manifest_path.clone(),
        );
        let settings = pipeline::project::manifest::ManifestLoader::load(&manifest_path)
            .expect("manifest loads");
        let layout =
            pipeline::project::layout::ProjectLayout::from_settings(&project_root, &settings)
                .expect("layout resolves");

        let overrides = pipeline::CliOverrides {
            entrypoint: Some(entrypoint.clone()),
            output: Some(project_dir.join("target")),
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
        };

        let plan =
            pipeline::BuildOptionsFactory::compose(project_root, settings, layout, overrides)
                .expect("plan composition succeeds");

        match pipeline::compile(&plan) {
            Ok(artifacts) => {
                assert!(
                    !artifacts.java_files.is_empty(),
                    "fixture {} should generate at least one Java file",
                    path.display()
                );
            }
            Err(err) => {
                failures.push((path.display().to_string(), err.to_string()));
            }
        }
    }

    assert!(
        compiled > 0,
        "no eligible fixtures without interpolation were compiled"
    );

    if !failures.is_empty() {
        let details = failures
            .into_iter()
            .map(|(path, err)| format!("{path}: {err}"))
            .collect::<Vec<_>>()
            .join("\n");
        panic!("CLI failed to compile some fixtures:\n{details}");
    }
}

#[test]
fn primitive_int_family_sum_pipeline_executes_via_cli() {
    let _guard = lock_current_dir();
    let temp_dir = TempDirGuard::new("primitive-sum-specialization");
    let project_root_path = temp_dir.path();
    let manifest_path = project_root_path.join("jv.toml");
    fs::write(
        &manifest_path,
        r#"[package]
name = "primitive-sum"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]
"#,
    )
    .expect("write manifest");

    let src_dir = project_root_path.join("src");
    fs::create_dir_all(&src_dir).expect("create src directory");
    let entrypoint_path = src_dir.join("main.jv");
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../tests/fixtures/sequence/primitive_sum_specialization.jv");
    fs::copy(&fixture, &entrypoint_path).expect("copy fixture source");

    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        let project_root = pipeline::project::locator::ProjectRoot::new(
            project_root_path.to_path_buf(),
            manifest_path.clone(),
        );
        let settings = pipeline::project::manifest::ManifestLoader::load(&manifest_path)
            .expect("manifest loads");
        let layout =
            pipeline::project::layout::ProjectLayout::from_settings(&project_root, &settings)
                .expect("layout resolves");

        let output_dir = project_root_path.join(format!("target-{}", target.as_str()));

        let overrides = pipeline::CliOverrides {
            entrypoint: Some(entrypoint_path.clone()),
            output: Some(output_dir.clone()),
            java_only: true,
            check: false,
            format: false,
            target: Some(target),
            clean: false,
            perf: false,
            emit_types: false,
            verbose: false,
            emit_telemetry: false,
            parallel_inference: false,
            inference_workers: None,
            constraint_batch: None,
        };

        let plan =
            pipeline::BuildOptionsFactory::compose(project_root, settings, layout, overrides)
                .expect("plan composition succeeds");

        let artifacts =
            pipeline::compile(&plan).expect("primitive specialization fixture should compile");

        let mut java_source = String::new();
        for file in &artifacts.java_files {
            let content = fs::read_to_string(file).expect("read generated java source");
            java_source.push_str(&content);
        }

        assert!(
            java_source.contains(".mapToInt("),
            "expected mapToInt specialization for target {} in generated Java:\n{}",
            target.as_str(),
            java_source
        );
        assert!(
            java_source.contains(".mapToLong("),
            "expected mapToLong specialization for target {} in generated Java:\n{}",
            target.as_str(),
            java_source
        );
        assert!(
            java_source.contains("if (value instanceof Character)"),
            "expected Character handling branch in canonical adapter for target {}:\n{}",
            target.as_str(),
            java_source
        );
        assert!(
            java_source.contains("return ((Number) value).intValue();"),
            "expected Number fallback branch in Character handling lambda for target {}:\n{}",
            target.as_str(),
            java_source
        );
    }
}

#[test]
fn stdlib_flatmap_iterable_signature_is_generated_correctly() {
    let temp_dir = TempDirGuard::new("sequence-flatmap-signature");
    let root_path = temp_dir.path();
    let manifest_path = root_path.join("jv.toml");

    fs::write(
        &manifest_path,
        r#"[package]
name = "flatmap-signature"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]

[project.output]
directory = "target"
clean = false
"#,
    )
    .expect("write manifest");

    let src_dir = root_path.join("src");
    fs::create_dir_all(&src_dir).expect("create src directory");
    let entrypoint = src_dir.join("main.jv");
    fs::write(
        &entrypoint,
        r#"fun main() {
    val numbers = [1 2 3]
    val expanded = numbers.flatMap { value -> [value value + 10] }
    val result = expanded.toList()
    println(result)
}
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
    };

    let plan = pipeline::BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds");

    pipeline::compile(&plan).expect("program should compile");

    let sequence_java = plan.output_dir().join("jv/collections/Sequence.java");
    let sequence_contents =
        fs::read_to_string(&sequence_java).expect("Sequence.java should exist after compile");

    let sequence_lines: Vec<&str> = sequence_contents.lines().collect();

    let flat_map_iterable_signature = sequence_lines
        .iter()
        .find(|line| line.contains("flatMapIterable("))
        .copied()
        .expect("Sequence should expose flatMapIterable alias");

    assert!(
        flat_map_iterable_signature.contains("java.util.function.Function<T, Iterable<R>>"),
        "flatMapIterable should accept Function<T, Iterable<R>>: {flat_map_iterable_signature}"
    );

    let flat_map_stream_signature = sequence_lines
        .iter()
        .find(|line| line.contains("flatMapStream("))
        .copied()
        .expect("Sequence should expose flatMapStream alias");

    assert!(
        flat_map_stream_signature.contains("java.util.function.Function<T, Stream<R>>"),
        "flatMapStream should accept Function<T, Stream<R>>: {flat_map_stream_signature}"
    );
}

#[test]
fn compile_accepts_for_in_loops() {
    let temp_dir = TempDirGuard::new("compile-for-in");
    let root_path = temp_dir.path();
    let manifest_path = root_path.join("jv.toml");

    fs::write(
        &manifest_path,
        r#"[package]
name = "loop-test"
version = "0.1.0"

[package.dependencies]

[project]
entrypoint = "src/main.jv"

[project.sources]
include = ["src/**/*.jv"]

[project.output]
directory = "target"
clean = false
"#,
    )
    .expect("write manifest");

    let src_dir = root_path.join("src");
    fs::create_dir_all(&src_dir).expect("create src directory");
    let entrypoint = src_dir.join("main.jv");
    fs::write(
        &entrypoint,
        r#"fun main() {
    for (exclusive in 0..3) {
        val copy = exclusive
    }

    for (inclusive in 1..=3) {
        val echo = inclusive
    }
}
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
    };

    let plan = pipeline::BuildOptionsFactory::compose(project_root, settings, layout, overrides)
        .expect("plan composition succeeds");

    let _artifacts = pipeline::compile(&plan).expect("loop program should compile");
    assert!(
        plan.output_dir().exists(),
        "loop compilation should create the configured output directory"
    );
}

#[test]
fn preflight_blocks_incompatible_classpath() {
    let temp_dir = TempDirGuard::new("compat-preflight");
    let jar_path = temp_dir.path().join("requires25.jar");

    {
        let file = File::create(&jar_path).unwrap();
        let mut writer = zip::ZipWriter::new(file);
        let options = FileOptions::default();
        writer.start_file("META-INF/MANIFEST.MF", options).unwrap();
        writer
            .write_all(b"Manifest-Version: 1.0\nBuild-Jdk: 25.0.0\n")
            .unwrap();
        writer.finish().unwrap();
    }

    let mut build_config = BuildConfig::with_target(JavaTarget::Java21);
    build_config.classpath = vec![jar_path.to_string_lossy().into_owned()];
    let build_system = BuildSystem::new(build_config);

    let error = pipeline::compat::preflight(&build_system, Path::new("dummy.jv"))
        .expect_err("expected compatibility failure");

    let message = error.to_string();
    assert!(message.contains("JV2001"));
    assert!(message.contains("Java 25"));
}

#[test]
fn test_get_version() {
    let version = get_version();
    assert!(version.contains("jv"));
    assert!(version.contains("Java Sugar Language compiler"));
    assert!(version.contains(env!("CARGO_PKG_VERSION")));
}

#[test]
fn test_build_command_defaults() {
    let build_args = vec!["jv", "build", "test.jv"];
    let cli = Cli::try_parse_from(build_args).unwrap();

    match cli.command {
        Some(Commands::Build {
            input,
            output,
            java_only,
            check,
            format,
            clean,
            ..
        }) => {
            assert_eq!(input.as_deref(), Some("test.jv"));
            assert!(output.is_none());
            assert!(!java_only);
            assert!(!check);
            assert!(!format);
            assert!(!clean);
        }
        _ => panic!("Expected Build command"),
    }
}

#[test]
fn test_init_command_default() {
    let init_args = vec!["jv", "init"];
    let cli = Cli::try_parse_from(init_args).unwrap();

    match cli.command {
        Some(Commands::Init { name }) => assert_eq!(name, "."),
        _ => panic!("Expected Init command"),
    }
}

#[test]
fn test_invalid_command() {
    let invalid_args = vec!["jv", "invalid-command"];
    let result = Cli::try_parse_from(invalid_args);
    assert!(result.is_err());
}

#[test]
fn test_missing_required_args() {
    // Build without entrypoint should succeed and defer to manifest defaults
    let build_args = vec!["jv", "build"];
    let cli = Cli::try_parse_from(build_args).unwrap();
    match cli.command {
        Some(Commands::Build { input, .. }) => assert!(input.is_none()),
        _ => panic!("Expected Build command"),
    }

    // Test check without input file
    let invalid_check = vec!["jv", "check"];
    let result = Cli::try_parse_from(invalid_check);
    assert!(result.is_err());
}

#[test]
fn test_java_only_flag() {
    let build_args = vec!["jv", "build", "test.jv", "--java-only"];
    let cli = Cli::try_parse_from(build_args).unwrap();

    match cli.command {
        Some(Commands::Build { java_only, .. }) => assert!(java_only),
        _ => panic!("Expected Build command"),
    }
}

#[test]
fn test_build_command_target_override() {
    let build_args = vec!["jv", "build", "test.jv", "--target", "21"];
    let cli = Cli::try_parse_from(build_args).unwrap();

    match cli.command {
        Some(Commands::Build { target, .. }) => {
            assert_eq!(target, Some(JavaTarget::Java21));
        }
        _ => panic!("Expected Build command"),
    }
}

#[test]
fn test_build_command_clean_flag() {
    let build_args = vec!["jv", "build", "test.jv", "--clean"];
    let cli = Cli::try_parse_from(build_args).unwrap();

    match cli.command {
        Some(Commands::Build { clean, .. }) => assert!(clean),
        _ => panic!("Expected Build command"),
    }
}

#[test]
fn test_multiple_files_fmt() {
    let fmt_args = vec!["jv", "fmt", "a.jv", "b.jv", "c.jv"];
    let cli = Cli::try_parse_from(fmt_args).unwrap();

    match cli.command {
        Some(Commands::Fmt { files }) => {
            assert_eq!(files.len(), 3);
            assert!(files.contains(&"a.jv".to_string()));
            assert!(files.contains(&"b.jv".to_string()));
            assert!(files.contains(&"c.jv".to_string()));
        }
        _ => panic!("Expected Fmt command"),
    }
}

#[test]
fn apply_type_facts_enriches_class_metadata() {
    let span = dummy_span();
    let mut program = IrProgram {
        package: Some("demo".to_string()),
        imports: Vec::new(),
        type_declarations: vec![IrStatement::ClassDeclaration {
            name: "Box".to_string(),
            type_parameters: vec![IrTypeParameter::new("T", span.clone())],
            superclass: None,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            nested_classes: Vec::new(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let type_id = TypeId::new(7);
    let bounds = GenericBounds::new(vec![BoundConstraint::new(
        type_id,
        BoundPredicate::Interface("java::lang::Comparable".into()),
    )]);
    let mut scheme_bounds = HashMap::new();
    scheme_bounds.insert(type_id, bounds.clone());
    let mut scheme = TypeScheme::with_bounds(
        vec![type_id],
        TypeKind::new(TypeVariant::Primitive("demo::Box")),
        scheme_bounds,
    );
    scheme.set_variance(type_id, Variance::Covariant);

    let mut builder = TypeFactsBuilder::new();
    builder.add_scheme("demo::Box", scheme);
    builder.record_bounds(type_id, bounds);
    builder.record_variance(type_id, Variance::Covariant);
    builder.record_sealed_permits(
        type_id,
        vec![TypeKind::new(TypeVariant::Primitive("demo::Foo"))],
    );
    builder.record_kind_assignment(type_id, Kind::Star);
    builder.record_const_binding("demo::Box", "SIZE", TypeLevelValue::Int(32));
    builder.record_type_level_evaluation(
        "demo::Box",
        "dimension",
        TypeLevelValue::String("3D".to_string()),
    );

    let facts = builder.build();

    apply_type_facts(&mut program, &facts);

    let IrStatement::ClassDeclaration {
        type_parameters,
        modifiers,
        ..
    } = &program.type_declarations[0]
    else {
        panic!("expected class declaration");
    };

    assert_eq!(type_parameters.len(), 1);
    let param = &type_parameters[0];
    assert_eq!(param.variance, IrVariance::Covariant);
    assert_eq!(param.permits, vec!["demo.Foo".to_string()]);
    assert_eq!(param.kind, Some(Kind::Star));

    let bounds = &param.bounds;
    assert_eq!(bounds.len(), 1);
    match &bounds[0] {
        JavaType::Reference { name, generic_args } => {
            assert_eq!(name, "java.lang.Comparable");
            assert!(generic_args.is_empty());
        }
        other => panic!("unexpected bound: {:?}", other),
    }

    assert!(modifiers.is_sealed);
    assert_eq!(modifiers.permitted_types, vec!["demo.Foo".to_string()]);

    let metadata_entry = program
        .generic_metadata
        .get("demo::Box")
        .expect("generic metadata should be recorded");
    assert_eq!(
        metadata_entry.type_parameter_kinds.get("T"),
        Some(&Kind::Star)
    );
    assert!(matches!(
        metadata_entry.const_parameter_values.get("SIZE"),
        Some(IrTypeLevelValue::Int(32))
    ));
    match metadata_entry.type_level_bindings.get("dimension") {
        Some(IrTypeLevelValue::String(value)) => assert_eq!(value, "3D"),
        other => panic!("unexpected type-level binding: {other:?}"),
    }
}

#[test]
fn apply_type_facts_records_nested_metadata() {
    let span = dummy_span();
    let inner_span = span.clone();
    let inner_class = IrStatement::ClassDeclaration {
        name: "Inner".to_string(),
        type_parameters: vec![IrTypeParameter::new("V", inner_span.clone())],
        superclass: None,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers: IrModifiers::default(),
        span: inner_span,
    };

    let mut program = IrProgram {
        package: Some("demo".to_string()),
        imports: Vec::new(),
        type_declarations: vec![IrStatement::ClassDeclaration {
            name: "Outer".to_string(),
            type_parameters: Vec::new(),
            superclass: None,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            nested_classes: vec![inner_class],
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let inner_id = TypeId::new(99);
    let mut inner_scheme = TypeScheme::new(
        vec![inner_id],
        TypeKind::new(TypeVariant::Primitive("demo::Outer::Inner")),
    );
    inner_scheme.set_variance(inner_id, Variance::Invariant);

    let mut builder = TypeFactsBuilder::new();
    builder.add_scheme("demo::Outer::Inner", inner_scheme);
    builder.record_kind_assignment(inner_id, Kind::Star);
    builder.record_const_binding("demo::Outer::Inner", "CAPACITY", TypeLevelValue::Bool(true));

    let facts = builder.build();
    apply_type_facts(&mut program, &facts);

    let IrStatement::ClassDeclaration { nested_classes, .. } = &program.type_declarations[0] else {
        panic!("expected outer class");
    };
    let IrStatement::ClassDeclaration {
        type_parameters, ..
    } = &nested_classes[0]
    else {
        panic!("expected inner class");
    };
    assert_eq!(
        type_parameters.get(0).and_then(|param| param.kind.clone()),
        Some(Kind::Star)
    );

    let entry = program
        .generic_metadata
        .get("demo::Outer::Inner")
        .expect("nested metadata should be recorded");
    assert!(matches!(
        entry.const_parameter_values.get("CAPACITY"),
        Some(IrTypeLevelValue::Bool(true))
    ));
}

#[test]
fn apply_type_facts_records_metadata_without_generics() {
    let span = dummy_span();
    let class = IrStatement::ClassDeclaration {
        name: "Config".to_string(),
        type_parameters: Vec::new(),
        superclass: None,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: Vec::new(),
        nested_classes: Vec::new(),
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let mut program = IrProgram {
        package: Some("demo".to_string()),
        imports: Vec::new(),
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let mut builder = TypeFactsBuilder::new();
    let scheme = TypeScheme::new(
        Vec::new(),
        TypeKind::new(TypeVariant::Primitive("demo::Config")),
    );
    builder.add_scheme("demo::Config", scheme);
    builder.record_const_binding("demo::Config", "MAX_SIZE", TypeLevelValue::Int(128));
    builder.record_type_level_evaluation(
        "demo::Config",
        "descriptor",
        TypeLevelValue::String("Constants".to_string()),
    );

    let facts = builder.build();
    apply_type_facts(&mut program, &facts);

    let entry = program
        .generic_metadata
        .get("demo::Config")
        .expect("metadata entry for Config");
    assert!(entry.type_parameter_kinds.is_empty());
    assert!(matches!(
        entry.const_parameter_values.get("MAX_SIZE"),
        Some(IrTypeLevelValue::Int(128))
    ));
    match entry.type_level_bindings.get("descriptor") {
        Some(IrTypeLevelValue::String(value)) => assert_eq!(value, "Constants"),
        other => panic!("unexpected descriptor metadata: {other:?}"),
    }
}

#[test]
fn apply_type_facts_sets_sequence_specialization_hint() {
    let span = dummy_span();
    let type_param_span = span.clone();

    let mut pipeline = SequencePipeline {
        source: SequenceSource::Collection {
            expr: Box::new(IrExpression::Identifier {
                name: "values".to_string(),
                java_type: JavaType::Reference {
                    name: "jv.collections.Sequence".to_string(),
                    generic_args: Vec::new(),
                },
                span: span.clone(),
            }),
            element_hint: None,
        },
        stages: Vec::new(),
        terminal: Some(SequenceTerminal {
            kind: SequenceTerminalKind::Sum,
            evaluation: SequenceTerminalEvaluation::Aggregator,
            requires_non_empty_source: false,
            specialization_hint: None,
            canonical_adapter: None,
            span: span.clone(),
        }),
        lazy: false,
        span: span.clone(),
        shape: PipelineShape::default(),
        source_element_type: None,
        stage_element_types: Vec::new(),
    };
    pipeline.recompute_shape();

    let method_body = IrExpression::SequencePipeline {
        pipeline,
        java_type: JavaType::Primitive("int".to_string()),
        span: span.clone(),
    };

    let method_span = span.clone();
    let method = IrStatement::MethodDeclaration {
        name: "sum".to_string(),
        java_name: None,
        type_parameters: vec![IrTypeParameter::new("T", type_param_span)],
        parameters: vec![IrParameter {
            name: "values".to_string(),
            java_type: JavaType::Reference {
                name: "jv.collections.Sequence".to_string(),
                generic_args: Vec::new(),
            },
            modifiers: IrModifiers::default(),
            span: method_span.clone(),
        }],
        primitive_return: None,
        return_type: JavaType::Primitive("int".to_string()),
        body: Some(method_body),
        modifiers: IrModifiers::default(),
        throws: Vec::new(),
        span: method_span,
    };

    let class = IrStatement::ClassDeclaration {
        name: "Example".to_string(),
        type_parameters: Vec::new(),
        superclass: None,
        interfaces: Vec::new(),
        fields: Vec::new(),
        methods: vec![method],
        nested_classes: Vec::new(),
        modifiers: IrModifiers::default(),
        span: span.clone(),
    };

    let mut program = IrProgram {
        package: Some("example".to_string()),
        imports: Vec::new(),
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    };

    let type_id = TypeId::new(11);
    let primitive_constraint = PrimitiveBoundConstraint::new(PrimitiveTypeName::Int, Vec::new());
    let bounds = GenericBounds::new(vec![BoundConstraint::new(
        type_id,
        BoundPredicate::Primitive(primitive_constraint.clone()),
    )]);
    let mut scheme_bounds = HashMap::new();
    scheme_bounds.insert(type_id, bounds.clone());
    let scheme = TypeScheme::with_bounds(vec![type_id], TypeKind::default(), scheme_bounds);

    let mut builder = TypeFactsBuilder::new();
    builder.add_scheme("example::Example::sum", scheme.clone());
    builder.add_scheme("Example::sum", scheme.clone());
    builder.add_scheme("Example.sum", scheme.clone());
    builder.add_scheme("Example$sum", scheme.clone());
    builder.add_scheme("example.Example.sum", scheme.clone());
    builder.add_scheme("example$Example$sum", scheme.clone());
    builder.add_scheme("sum", scheme);
    builder.record_bounds(type_id, bounds);

    let facts = builder.build();
    assert!(
        TypeFacts::scheme_for(&facts, "sum").is_some(),
        "expected scheme for method symbol"
    );
    let scheme = TypeFacts::scheme_for(&facts, "sum").unwrap();
    let generics = scheme.generics();
    let mut primitive_bound_present = false;
    for type_id in generics {
        if let Some(bounds) = scheme.bounds_for(*type_id) {
            if bounds
                .constraints()
                .iter()
                .any(|constraint| matches!(constraint.predicate, BoundPredicate::Primitive(_)))
            {
                primitive_bound_present = true;
                break;
            }
        }
    }
    assert!(
        primitive_bound_present,
        "expected primitive bound in scheme"
    );
    let manual_hint = find_primitive_specialization_hint(
        Some("example"),
        &["Example".to_string(), "sum".to_string()],
        &[IrTypeParameter::new("T", jv_ast::Span::dummy())],
        &facts,
    );
    assert!(manual_hint.is_some(), "expected manual primitive hint");

    apply_type_facts(&mut program, &facts);

    let IrStatement::ClassDeclaration { methods, .. } = &program.type_declarations[0] else {
        panic!("expected class declaration");
    };
    let IrStatement::MethodDeclaration {
        body: Some(body),
        type_parameters: method_params,
        ..
    } = &methods[0]
    else {
        panic!("expected method body");
    };
    let post_hint = find_primitive_specialization_hint(
        Some("example"),
        &["Example".to_string(), "sum".to_string()],
        method_params.as_slice(),
        &facts,
    );
    assert!(
        post_hint.is_some(),
        "expected primitive hint after enrichment"
    );
    let IrExpression::SequencePipeline { pipeline, .. } = body else {
        panic!("expected sequence pipeline body");
    };
    let terminal = pipeline
        .terminal
        .as_ref()
        .expect("sequence terminal should exist");
    let hint = terminal
        .specialization_hint
        .as_ref()
        .expect("primitive specialization hint should be attached");

    assert_eq!(
        hint,
        &PrimitiveSpecializationHint {
            type_param: "T".to_string(),
            canonical: PrimitiveTypeName::Int,
            aliases: Vec::new(),
            span: jv_ast::Span::default(),
        }
    );
}

#[test]
fn where_constraints_flow_into_ir_bounds() {
    use jv_ast::types::{QualifiedName, TypeAnnotation, WhereClause, WherePredicate};
    use std::collections::HashMap;

    let span = dummy_span();
    let mut program = IrProgram {
        package: Some("demo".to_string()),
        imports: Vec::new(),
        type_declarations: vec![IrStatement::ClassDeclaration {
            name: "Box".to_string(),
            type_parameters: vec![IrTypeParameter::new("T", span.clone())],
            superclass: None,
            interfaces: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            nested_classes: Vec::new(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: span.clone(),
    };

    let mut params = HashMap::new();
    let type_id = TypeId::new(11);
    params.insert("T".to_string(), type_id);
    let clause = WhereClause {
        predicates: vec![WherePredicate::TraitBound {
            type_param: "T".into(),
            trait_name: QualifiedName::new(vec!["Comparable".into()], span.clone()),
            type_args: vec![TypeAnnotation::Simple("T".into())],
            span: span.clone(),
        }],
        primitive_bounds: Vec::new(),
        span: span.clone(),
    };

    let symbol = SymbolId::from("demo::Box");
    let resolver = WhereConstraintResolver::new(symbol.clone(), &params);
    let constraints = resolver.from_clause(&clause);
    let mut graph = ConstraintGraph::new();
    let summary = graph.add_where_constraints(&constraints);
    let null_summary = summary.into_nullability_summary();
    let solution = ConstraintSolution::from_generic_constraints(
        symbol.clone(),
        &constraints,
        null_summary,
        Vec::new(),
    );

    let mut scheme = TypeScheme::new(
        vec![type_id],
        TypeKind::new(TypeVariant::Primitive("demo::Box")),
    );
    scheme.set_variance(type_id, Variance::Invariant);

    let mut builder = TypeFactsBuilder::new();
    builder.add_scheme("demo::Box", scheme);
    builder.apply_constraint_solution(&solution);

    let facts = builder.build();
    apply_type_facts(&mut program, &facts);

    let IrStatement::ClassDeclaration {
        type_parameters, ..
    } = &program.type_declarations[0]
    else {
        panic!("expected class declaration");
    };

    assert_eq!(type_parameters.len(), 1);
    let param = &type_parameters[0];
    assert_eq!(param.bounds.len(), 1);
    match &param.bounds[0] {
        JavaType::Reference { name, .. } => assert_eq!(name, "Comparable"),
        other => panic!("unexpected bound: {other:?}"),
    }
}
