use super::*;
use crate::commands::explain;
use crate::pipeline::compute_script_main_class;

use crate::pipeline::generics::apply_type_facts;
use jv_inference::constraint::{
    ConstraintGraph, ConstraintSolution, NullabilitySummary, WhereConstraintResolver,
};
use jv_inference::service::{TypeFactsBuilder, TypeScheme};
use jv_inference::solver::Variance;
use jv_inference::types::{
    BoundConstraint, BoundPredicate, GenericBounds, SymbolId, TypeId, TypeKind, TypeVariant,
};
use jv_ir::{IrModifiers, IrProgram, IrStatement, IrTypeParameter, IrVariance, JavaType};
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
use std::sync::{Mutex, OnceLock};
use std::time::{SystemTime, UNIX_EPOCH};
use zip::write::FileOptions;

fn dummy_span() -> jv_ast::Span {
    jv_ast::Span::dummy()
}

struct TempDirGuard {
    path: PathBuf,
}

impl TempDirGuard {
    fn new(prefix: &str) -> Self {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let path = std::env::temp_dir().join(format!("jv-cli-{}-{}", prefix, timestamp));
        fs::create_dir_all(&path).unwrap();
        Self { path }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

fn current_dir_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
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
    let _guard = current_dir_lock().lock().unwrap();
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

    // Restore original directory
    std::env::set_current_dir(&original_dir).unwrap();
}

#[test]
fn test_init_project_with_name() {
    let _guard = current_dir_lock().lock().unwrap();
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
    assert!(message.contains("JV1007"));
    assert!(message.contains("配列"));
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
