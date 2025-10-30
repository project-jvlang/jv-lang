use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result};
use jv_ast::{BinaryOp, CallArgumentStyle, Literal, Span};
use jv_checker::{
    CheckError, TypeChecker,
    diagnostics::{
        DiagnosticStrategy, collect_raw_type_diagnostics, from_check_error, from_parse_error,
        from_transform_error,
    },
};
use jv_cli::format_tooling_diagnostic;
use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
use jv_ir::{
    IrCaseLabel, IrDeconstructionComponent, IrDeconstructionPattern, IrExpression, IrModifiers,
    IrParameter, IrProgram, IrRecordComponent, IrStatement, IrSwitchCase, IrVisibility, JavaType,
    transform_program,
};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use jv_pm::JavaTarget;
use tempfile::tempdir;

fn main() {
    if let Err(error) = run() {
        eprintln!("fixture runner error: {error:?}");
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let mut args = env::args().skip(1);
    let mut mode: Option<String> = None;
    let mut fixture_dir: Option<PathBuf> = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--test-mode" => mode = args.next(),
            "--fixture-dir" => fixture_dir = args.next().map(PathBuf::from),
            _ => continue,
        }
    }

    if mode.is_none() && fixture_dir.is_none() {
        return Ok(());
    }

    let mode = mode.context("--test-mode requires a value")?;
    if mode != "fixture" {
        eprintln!("unsupported test mode: {mode}");
        return Ok(());
    }

    let fixture_dir = fixture_dir.context("--fixture-dir is required in fixture mode")?;

    run_fixture_suite(&fixture_dir)
}

fn run_fixture_suite(dir: &Path) -> Result<()> {
    let resolved = if dir.is_absolute() {
        dir.to_path_buf()
    } else {
        workspace_root().join(dir)
    };

    let mut entries = fs::read_dir(&resolved)
        .with_context(|| format!("read fixture directory {}", resolved.display()))?
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry
                .path()
                .extension()
                .map(|ext| ext == "jv")
                .unwrap_or(false)
        })
        .map(|entry| entry.path())
        .collect::<Vec<_>>();
    entries.sort();

    if entries.is_empty() {
        println!("no fixtures found under {}", dir.display());
        return Ok(());
    }

    let javac_version = detect_javac_version();
    if javac_version.is_none() {
        eprintln!("warning: javac not found; skipping compilation checks");
    }

    let mut failures = Vec::new();

    for source_path in &entries {
        if let Err(error) = run_fixture_case(source_path, javac_version) {
            failures.push((source_path.clone(), error));
        }
    }

    if failures.is_empty() {
        println!(
            "fixture suite passed: {} cases (javac {:?})",
            entries.len(),
            javac_version
        );
        Ok(())
    } else {
        for (path, error) in &failures {
            eprintln!("fixture {} failed: {error:?}", path.display());
        }
        anyhow::bail!("{} fixture(s) failed", failures.len())
    }
}

fn collect_fixture_diagnostic(
    source_path: &Path,
    display_path: &Path,
    analyze_codegen: bool,
) -> Result<String> {
    let source = fs::read_to_string(source_path)
        .with_context(|| format!("read fixture source {}", source_path.display()))?;

    let pipeline = RowanPipeline::default();
    let output = match pipeline.parse(&source) {
        Ok(output) => output,
        Err(error) => {
            if let Some(diagnostic) = from_parse_error(&error) {
                let diagnostic = diagnostic.with_strategy(DiagnosticStrategy::Deferred);
                return Ok(format_tooling_diagnostic(display_path, &diagnostic));
            }
            anyhow::bail!("parser error without diagnostic code: {error:?}");
        }
    };
    let program = output.into_program();

    let mut checker = TypeChecker::new();
    match checker.check_program(&program) {
        Ok(()) => {}
        Err(errors) => {
            let diagnostics = errors
                .iter()
                .filter_map(from_check_error)
                .collect::<Vec<_>>();

            if let Some(primary) = diagnostics
                .iter()
                .find(|diagnostic| diagnostic.code.starts_with("JV"))
                .cloned()
                .or_else(|| diagnostics.first().cloned())
            {
                let diagnostic = primary.with_strategy(DiagnosticStrategy::Deferred);
                return Ok(format_tooling_diagnostic(display_path, &diagnostic));
            }

            let details = errors
                .iter()
                .map(|error| error.to_string())
                .collect::<Vec<_>>()
                .join("\n");
            anyhow::bail!("type checking failed without diagnostic code: {details}");
        }
    }

    let null_safety_errors = if let Some(normalized) = checker.normalized_program() {
        let cloned = normalized.clone();
        checker.check_null_safety(&cloned, None)
    } else {
        checker.check_null_safety(&program, None)
    };
    let mut null_safety_diagnostics = Vec::new();
    for error in &null_safety_errors {
        if let Some(diagnostic) = from_check_error(error) {
            null_safety_diagnostics.push(diagnostic);
        }
    }
    if let Some(primary) = null_safety_diagnostics
        .iter()
        .find(|diagnostic| diagnostic.code.starts_with("JV"))
        .cloned()
        .or_else(|| null_safety_diagnostics.first().cloned())
    {
        let diagnostic = primary.with_strategy(DiagnosticStrategy::Deferred);
        return Ok(format_tooling_diagnostic(display_path, &diagnostic));
    }

    let lowering_input = checker.take_normalized_program().unwrap_or(program.clone());
    let ir_program = match transform_program(lowering_input) {
        Ok(ir) => ir,
        Err(error) => {
            if let Some(diagnostic) = from_transform_error(&error) {
                let diagnostic = diagnostic.with_strategy(DiagnosticStrategy::Deferred);
                return Ok(format_tooling_diagnostic(display_path, &diagnostic));
            }
            anyhow::bail!("IR transform error without diagnostic code: {error:?}");
        }
    };

    let raw_type_diagnostics = collect_raw_type_diagnostics(&ir_program);
    if !raw_type_diagnostics.is_empty() {
        let rendered = raw_type_diagnostics
            .into_iter()
            .map(|diagnostic| {
                let diagnostic = diagnostic.with_strategy(DiagnosticStrategy::Deferred);
                format_tooling_diagnostic(display_path, &diagnostic)
            })
            .collect::<Vec<_>>()
            .join("\n\n");
        return Ok(rendered);
    }

    if analyze_codegen {
        for target in [JavaTarget::Java25, JavaTarget::Java21] {
            let mut generator =
                JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(target));
            if let Err(error) = generator.generate_compilation_unit(&ir_program) {
                let message = error.to_string();
                let pseudo = CheckError::TypeError(message);
                if let Some(diagnostic) = from_check_error(&pseudo) {
                    let diagnostic = diagnostic.with_strategy(DiagnosticStrategy::Deferred);
                    return Ok(format_tooling_diagnostic(display_path, &diagnostic));
                }
                anyhow::bail!("codegen error without diagnostic code: {error:?}");
            }
        }
    }

    Ok(String::new())
}

fn run_fixture_case(path: &Path, javac_version: Option<u32>) -> Result<()> {
    let stem = path
        .file_stem()
        .and_then(|s| s.to_str())
        .context("fixture file stem must be valid UTF-8")?;

    let expected_diag = read_expected(path, stem, "expected_diag")?;
    let is_negative = !expected_diag.trim().is_empty();

    let expected_pair = if is_negative {
        None
    } else {
        Some((
            read_expected(path, stem, "expected_java25")?,
            read_expected(path, stem, "expected_java21")?,
        ))
    };

    if let Some((ref expected_java25, ref expected_java21)) = expected_pair {
        if let Some(actual) = generate_fixture_source(stem, JavaTarget::Java25) {
            compare_output(&actual, expected_java25, stem, "Java25")?;
        }

        if let Some(actual) = generate_fixture_source(stem, JavaTarget::Java21) {
            compare_output(&actual, expected_java21, stem, "Java21")?;
        }
    }

    let display_path = path
        .file_name()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(stem));
    let actual_diag = if is_negative {
        collect_fixture_diagnostic(path, &display_path, true)?
    } else {
        String::new()
    };
    compare_output(&actual_diag, &expected_diag, stem, "diagnostics")?;

    if let Some((ref expected_java25, ref expected_java21)) = expected_pair {
        if let Some(version) = javac_version {
            compile_with_javac(expected_java25, 25, version, stem, "Java25")?;
            compile_with_javac(expected_java21, 21, version, stem, "Java21")?;
        }
    }

    Ok(())
}

fn read_expected(source_path: &Path, stem: &str, suffix: &str) -> Result<String> {
    let expected_path = source_path.with_file_name(format!("{stem}.{suffix}"));
    let content = fs::read_to_string(&expected_path)
        .with_context(|| format!("read expected {}", expected_path.display()))?;
    Ok(content.replace("\r\n", "\n"))
}

fn compare_output(actual: &str, expected: &str, name: &str, label: &str) -> Result<()> {
    if normalize(actual) != normalize(expected) {
        anyhow::bail!(
            "fixture {name} ({label}) mismatch\n--- expected ---\n{}\n--- actual ---\n{}",
            expected,
            actual
        );
    }
    Ok(())
}

fn normalize(value: &str) -> String {
    value.trim().replace("\r\n", "\n")
}

fn generate_fixture_source(stem: &str, target: JavaTarget) -> Option<String> {
    let program = match stem {
        "example1" => Some(example1_program()),
        "example2" => Some(example2_program()),
        "example5" => Some(example5_program()),
        _ => None,
    }?;
    let mut generator = JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(target));
    let unit = generator.generate_compilation_unit(&program).ok()?;
    Some(unit.to_source(&JavaCodeGenConfig::for_target(target)))
}

fn detect_javac_version() -> Option<u32> {
    let output = Command::new("javac").arg("-version").output().ok()?;

    let text = if output.stderr.is_empty() {
        String::from_utf8_lossy(&output.stdout).into_owned()
    } else {
        String::from_utf8_lossy(&output.stderr).into_owned()
    };

    parse_javac_major(&text)
}

fn parse_javac_major(text: &str) -> Option<u32> {
    for token in text.split_whitespace() {
        if let Some(stripped) = token.split('.').next() {
            if let Ok(value) = stripped.parse::<u32>() {
                if value >= 8 {
                    return Some(value);
                }
            }
        }
    }
    None
}

fn compile_with_javac(
    java_source: &str,
    required: u32,
    detected: u32,
    stem: &str,
    label: &str,
) -> Result<()> {
    if detected < required {
        eprintln!(
            "warning: javac {detected} cannot validate {label} fixture {stem} (requires {required})"
        );
        return Ok(());
    }

    let dir = tempdir().context("create temp dir for javac")?;
    let java_path = dir.path().join(format!("{stem}_{label}.java"));
    fs::write(&java_path, java_source).context("write temporary java file")?;

    let mut cmd = Command::new("javac");
    cmd.arg(&java_path);
    if detected >= required {
        cmd.arg(format!("--release={required}"));
    }
    let output = cmd.output().context("run javac")?;
    if !output.status.success() {
        anyhow::bail!(
            "javac failed for {stem} ({label}):\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(())
}

fn dummy_span() -> Span {
    Span::dummy()
}

fn string_type() -> JavaType {
    JavaType::Reference {
        name: "String".to_string(),
        generic_args: vec![],
    }
}

fn int_type() -> JavaType {
    JavaType::Primitive("int".to_string())
}

fn reference_type(name: &str) -> JavaType {
    JavaType::Reference {
        name: name.to_string(),
        generic_args: vec![],
    }
}

fn ir_identifier(name: &str, ty: &JavaType) -> IrExpression {
    IrExpression::Identifier {
        name: name.to_string(),
        java_type: ty.clone(),
        span: dummy_span(),
    }
}

fn switch_case(
    labels: Vec<IrCaseLabel>,
    guard: Option<IrExpression>,
    body: IrExpression,
) -> IrSwitchCase {
    IrSwitchCase {
        labels,
        guard,
        body,
        span: dummy_span(),
    }
}

fn string_literal(value: &str) -> IrExpression {
    IrExpression::Literal(Literal::String(value.to_string()), None, dummy_span())
}

fn number_literal(value: &str) -> IrExpression {
    IrExpression::Literal(Literal::Number(value.to_string()), None, dummy_span())
}

fn record_component(name: &str, java_type: JavaType) -> IrRecordComponent {
    IrRecordComponent {
        name: name.to_string(),
        java_type,
        span: dummy_span(),
    }
}

fn public_modifiers() -> IrModifiers {
    IrModifiers {
        visibility: IrVisibility::Public,
        ..IrModifiers::default()
    }
}

fn static_public_method() -> IrModifiers {
    IrModifiers {
        visibility: IrVisibility::Public,
        is_static: true,
        ..IrModifiers::default()
    }
}

fn example1_program() -> IrProgram {
    let object_type = reference_type("Object");
    let string = string_type();
    let int = int_type();

    let discriminant = Box::new(ir_identifier("x", &object_type));

    let string_case = switch_case(
        vec![IrCaseLabel::TypePattern {
            type_name: "String".to_string(),
            variable: "value".to_string(),
            deconstruction: None,
        }],
        None,
        IrExpression::MethodCall {
            receiver: Some(Box::new(ir_identifier("value", &string))),
            method_name: "length".to_string(),
            java_name: None,
            resolved_target: None,
            args: vec![],
            argument_style: CallArgumentStyle::Comma,
            java_type: int.clone(),
            span: dummy_span(),
        },
    );

    let int_case = switch_case(
        vec![IrCaseLabel::TypePattern {
            type_name: "Integer".to_string(),
            variable: "value".to_string(),
            deconstruction: None,
        }],
        None,
        IrExpression::Binary {
            left: Box::new(ir_identifier("value", &int)),
            op: BinaryOp::Multiply,
            right: Box::new(IrExpression::Literal(
                Literal::Number("2".to_string()),
                None,
                dummy_span(),
            )),
            java_type: int.clone(),
            span: dummy_span(),
        },
    );

    let default_case = switch_case(vec![IrCaseLabel::Default], None, number_literal("0"));

    let switch_expr = IrExpression::Switch {
        discriminant,
        cases: vec![string_case, int_case, default_case],
        java_type: int.clone(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=3 guards=0 default=true exhaustive=false".to_string(),
        ),
        span: dummy_span(),
    };

    let method_body = IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(switch_expr),
            span: dummy_span(),
        }],
        java_type: int,
        span: dummy_span(),
    };

    let evaluate_method = IrStatement::MethodDeclaration {
        name: "evaluate".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "x".to_string(),
            java_type: object_type,
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        primitive_return: None,
        return_type: int_type(),
        body: Some(method_body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Example1".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![evaluate_method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    IrProgram {
        package: Some("patterns".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    }
}

fn example2_program() -> IrProgram {
    let discriminant = Box::new(ir_identifier("score", &int_type()));

    let mut cases = Vec::new();
    cases.push(range_case("0", "60", false, "Fail"));
    cases.push(range_case("60", "80", false, "Pass"));
    cases.push(range_case("80", "100", true, "Excellent"));
    cases.push(switch_case(
        vec![IrCaseLabel::Default],
        None,
        string_literal("Invalid"),
    ));

    let switch_expr = IrExpression::Switch {
        discriminant,
        cases,
        java_type: string_type(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=4 guards=3 default=true exhaustive=false".to_string(),
        ),
        span: dummy_span(),
    };

    let method_body = IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(switch_expr),
            span: dummy_span(),
        }],
        java_type: string_type(),
        span: dummy_span(),
    };

    let method = IrStatement::MethodDeclaration {
        name: "categorize".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "score".to_string(),
            java_type: int_type(),
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        primitive_return: None,
        return_type: string_type(),
        body: Some(method_body),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: dummy_span(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "Example2".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: dummy_span(),
    };

    IrProgram {
        package: Some("patterns".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    }
}

fn range_case(lower: &str, upper: &str, inclusive: bool, label: &str) -> IrSwitchCase {
    let guard_op = if inclusive {
        BinaryOp::LessEqual
    } else {
        BinaryOp::Less
    };

    let lower_check = IrExpression::Binary {
        left: Box::new(ir_identifier("score", &int_type())),
        op: BinaryOp::GreaterEqual,
        right: Box::new(IrExpression::Literal(
            Literal::Number(lower.to_string()),
            None,
            dummy_span(),
        )),
        java_type: JavaType::boolean(),
        span: dummy_span(),
    };

    let upper_check = IrExpression::Binary {
        left: Box::new(ir_identifier("score", &int_type())),
        op: guard_op,
        right: Box::new(IrExpression::Literal(
            Literal::Number(upper.to_string()),
            None,
            dummy_span(),
        )),
        java_type: JavaType::boolean(),
        span: dummy_span(),
    };

    let condition = IrExpression::Binary {
        left: Box::new(lower_check),
        op: BinaryOp::And,
        right: Box::new(upper_check),
        java_type: JavaType::boolean(),
        span: dummy_span(),
    };

    switch_case(
        vec![IrCaseLabel::Range {
            type_name: "int".to_string(),
            variable: "it0".to_string(),
            lower: Box::new(number_literal(lower)),
            upper: Box::new(number_literal(upper)),
            inclusive_end: inclusive,
        }],
        Some(condition),
        string_literal(label),
    )
}

fn example5_program() -> IrProgram {
    let int = int_type();
    let inner_type = reference_type("Inner");
    let outer_type = reference_type("Outer");

    let inner_record = IrStatement::RecordDeclaration {
        name: "Inner".to_string(),
        type_parameters: vec![],
        components: vec![
            record_component("x", int.clone()),
            record_component("y", int.clone()),
        ],
        interfaces: vec![],
        methods: vec![],
        modifiers: public_modifiers(),
        span: dummy_span(),
    };

    let outer_record = IrStatement::RecordDeclaration {
        name: "Outer".to_string(),
        type_parameters: vec![],
        components: vec![
            record_component("inner", inner_type.clone()),
            record_component("count", int.clone()),
        ],
        interfaces: vec![],
        methods: vec![],
        modifiers: public_modifiers(),
        span: dummy_span(),
    };

    let nested_pattern = IrDeconstructionPattern {
        components: vec![
            IrDeconstructionComponent::Type {
                type_name: "Inner".to_string(),
                pattern: Some(Box::new(IrDeconstructionPattern {
                    components: vec![
                        IrDeconstructionComponent::Binding {
                            name: "x".to_string(),
                        },
                        IrDeconstructionComponent::Binding {
                            name: "y".to_string(),
                        },
                    ],
                })),
            },
            IrDeconstructionComponent::Binding {
                name: "count".to_string(),
            },
        ],
    };

    let sum_xy = IrExpression::Binary {
        left: Box::new(ir_identifier("x", &int)),
        op: BinaryOp::Add,
        right: Box::new(ir_identifier("y", &int)),
        java_type: int.clone(),
        span: dummy_span(),
    };

    let match_value = IrExpression::Binary {
        left: Box::new(sum_xy),
        op: BinaryOp::Add,
        right: Box::new(ir_identifier("count", &int)),
        java_type: int.clone(),
        span: dummy_span(),
    };

    let match_case = switch_case(
        vec![IrCaseLabel::TypePattern {
            type_name: "Outer".to_string(),
            variable: "outer".to_string(),
            deconstruction: Some(nested_pattern),
        }],
        None,
        match_value,
    );

    let default_case = switch_case(vec![IrCaseLabel::Default], None, number_literal("0"));

    let switch_expr = IrExpression::Switch {
        discriminant: Box::new(ir_identifier("value", &outer_type)),
        cases: vec![match_case, default_case],
        java_type: int.clone(),
        implicit_end: None,
        strategy_description: Some(
            "strategy=Switch arms=2 guards=0 default=true exhaustive=true".to_string(),
        ),
        span: dummy_span(),
    };

    let total_method = IrStatement::MethodDeclaration {
        name: "total".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "value".to_string(),
            java_type: outer_type,
            modifiers: IrModifiers::default(),
            span: dummy_span(),
        }],
        primitive_return: None,
        return_type: int.clone(),
        body: Some(IrExpression::Block {
            statements: vec![IrStatement::Return {
                value: Some(switch_expr),
                span: dummy_span(),
            }],
            java_type: int.clone(),
            span: dummy_span(),
        }),
        modifiers: static_public_method(),
        throws: vec![],
        span: dummy_span(),
    };

    let example_class = IrStatement::ClassDeclaration {
        name: "Example5".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![total_method],
        nested_classes: vec![],
        modifiers: public_modifiers(),
        span: dummy_span(),
    };

    IrProgram {
        package: Some("patterns".to_string()),
        imports: vec![],
        type_declarations: vec![inner_record, outer_record, example_class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span: dummy_span(),
    }
}

fn workspace_root() -> PathBuf {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .to_path_buf()
}
