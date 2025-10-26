use std::path::Path;

use crate::frontend::RowanPipeline;
use crate::lowering::LoweringDiagnosticSeverity;
use crate::verification::{self, HarnessReport, StatementKindKey};
use jv_ast::expression::{Argument, ParameterProperty, StringPart};
use jv_ast::Expression;
use jv_lexer::{StringInterpolationSegment, TokenMetadata, TokenType};

#[test]
fn specification_fixtures_produce_expected_ast() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let spec_dir = manifest_dir.join("../../../tests/parser_rowan_specs");

    let report =
        verification::run_spec_directory(&spec_dir).expect("verification harness to execute");

    let workspace_root = manifest_dir
        .ancestors()
        .nth(3)
        .expect("workspace root to exist");
    let report_path = HarnessReport::default_report_path(workspace_root);
    report
        .write_report(&report_path)
        .expect("verification report to be written");

    if !report.is_success() {
        let details = report
            .fixtures
            .iter()
            .filter(|fixture| !fixture.violations.is_empty())
            .map(|fixture| {
                let violations = fixture
                    .violations
                    .iter()
                    .map(|violation| format!("{}: {}", violation.rule, violation.message))
                    .collect::<Vec<_>>()
                    .join("; ");
                format!("{} -> {}", fixture.spec, violations)
            })
            .collect::<Vec<_>>()
            .join("\n");

        panic!("verification harness reported violations:\n{}", details);
    }
}

#[test]
fn harness_fixture_parses_successfully() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let source_path =
        manifest_dir.join("../../../tests/parser_rowan_specs/fixtures/package_and_bindings.jv");
    let source =
        std::fs::read_to_string(&source_path).expect("package_and_bindings fixture to be readable");
    let pipeline = RowanPipeline::default();
    let debug = pipeline
        .execute_with_debug(&source)
        .expect("Rowan pipeline to execute on harness fixture");

    let tokens = debug.artifacts().tokens();
    assert!(
        matches!(
            tokens.first().map(|token| &token.token_type),
            Some(TokenType::Package)
        ),
        "expected first token to be package keyword"
    );

    assert!(
        debug.parser_diagnostics().is_empty(),
        "expected no parser diagnostics, got {:?}",
        debug.parser_diagnostics()
    );
    assert!(
        debug
            .lowering_diagnostics()
            .iter()
            .all(|diag| diag.severity != LoweringDiagnosticSeverity::Error),
        "expected no lowering errors, got {:?}",
        debug.lowering_diagnostics()
    );
    let warning_count = debug
        .lowering_diagnostics()
        .iter()
        .filter(|diag| diag.severity == LoweringDiagnosticSeverity::Warning)
        .count();
    assert_eq!(
        warning_count,
        0,
        "expected no lowering warnings, got {:?}",
        debug.lowering_diagnostics()
    );

    assert_eq!(
        debug.statements().len(),
        4,
        "expected four statements (package, import, val, var)"
    );
}

#[test]
fn function_block_statements_are_lowered() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let source_path = manifest_dir.join("../../../jv/tests/fixtures/02-variables.jv");
    let source =
        std::fs::read_to_string(&source_path).expect("variables fixture should be readable");
    let pipeline = RowanPipeline::default();
    let debug = pipeline
        .execute_with_debug(&source)
        .expect("Rowan pipeline to execute on variables fixture");
    assert!(
        debug
            .lowering_diagnostics()
            .iter()
            .all(|diag| diag.severity != LoweringDiagnosticSeverity::Error),
        "expected no lowering errors, got {:?}",
        debug.lowering_diagnostics()
    );

    let function_body = debug
        .statements()
        .iter()
        .find_map(|statement| match statement {
            jv_ast::Statement::FunctionDeclaration { name, body, .. } if name == "main" => {
                Some(body.as_ref())
            }
            _ => None,
        })
        .expect("main function to be lowered");

    let block_statements = match function_body {
        jv_ast::Expression::Block { statements, .. } => statements,
        other => panic!("expected block body, found {:?}", other),
    };

    let kinds: Vec<StatementKindKey> = block_statements
        .iter()
        .map(StatementKindKey::from_statement)
        .collect();

    assert_eq!(
        kinds,
        vec![
            StatementKindKey::Comment,
            StatementKindKey::ValDeclaration,
            StatementKindKey::VarDeclaration,
            StatementKindKey::Assignment,
            StatementKindKey::Expression
        ],
        "block statements lowered with expected kinds"
    );
}

#[test]
fn comment_fixture_emits_comment_statements() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let source_path =
        manifest_dir.join("../../../tests/parser_rowan_specs/fixtures/comment_visibility.jv");
    let source = std::fs::read_to_string(&source_path)
        .expect("comment_visibility fixture should be readable");
    let pipeline = RowanPipeline::default();
    let debug = pipeline
        .execute_with_debug(&source)
        .expect("Rowan pipeline to execute on comment fixture");

    assert!(
        debug
            .lowering_diagnostics()
            .iter()
            .all(|diag| diag.severity != LoweringDiagnosticSeverity::Error),
        "expected no lowering errors, got {:?}",
        debug.lowering_diagnostics()
    );

    let kinds: Vec<StatementKindKey> = debug
        .statements()
        .iter()
        .map(StatementKindKey::from_statement)
        .collect();

    assert_eq!(
        kinds,
        vec![
            StatementKindKey::Package,
            StatementKindKey::Comment,
            StatementKindKey::Comment,
            StatementKindKey::Comment,
            StatementKindKey::ValDeclaration
        ],
        "expected package + three comments + val statement order, got {:?}",
        kinds
    );
}

#[test]
fn function_parameters_preserve_modifiers() {
    let source = r#"
        package demo

        fun modifiers(val name: String, var age: Int, ref mut handle: Handle) = name
    "#;

    let pipeline = RowanPipeline::default();
    let debug = pipeline
        .execute_with_debug(source)
        .expect("Rowan pipeline to execute on inline modifiers source");

    assert!(
        debug.parser_diagnostics().is_empty(),
        "expected no parser diagnostics, got {:?}",
        debug.parser_diagnostics()
    );
    assert!(
        debug
            .lowering_diagnostics()
            .iter()
            .all(|diag| diag.severity != LoweringDiagnosticSeverity::Error),
        "expected no lowering errors, got {:?}",
        debug.lowering_diagnostics()
    );

    let function = debug
        .statements()
        .iter()
        .find_map(|statement| match statement {
            jv_ast::Statement::FunctionDeclaration {
                name, parameters, ..
            } if name == "modifiers" => Some(parameters),
            _ => None,
        })
        .expect("function declaration to be lowered");

    assert_eq!(function.len(), 3, "expected three parameters");

    let first = &function[0];
    assert_eq!(first.name, "name");
    assert_eq!(first.modifiers.property, ParameterProperty::Val);
    assert!(!first.modifiers.is_mut);
    assert!(!first.modifiers.is_ref);

    let second = &function[1];
    assert_eq!(second.name, "age");
    assert_eq!(second.modifiers.property, ParameterProperty::Var);
    assert!(!second.modifiers.is_mut);
    assert!(!second.modifiers.is_ref);

    let third = &function[2];
    assert_eq!(third.name, "handle");
    assert_eq!(third.modifiers.property, ParameterProperty::None);
    assert!(third.modifiers.is_ref);
    assert!(third.modifiers.is_mut);
}

#[test]
fn rowan_pipeline_preserves_interpolation_after_preprocess() {
    let source = r#"
        package checkpoints

        fun announce(stage: Int, name: String) {
            println("Checkpoint ${stage}: Welcome, ${name}!")
        }
    "#;

    let pipeline = RowanPipeline::default();
    let debug = pipeline
        .execute_with_debug(source)
        .expect("Rowan pipeline to execute on interpolation sample");

    let tokens = debug.artifacts().tokens();
    let interpolation_segments = tokens.iter().find_map(|token| {
        token.metadata.iter().find_map(|metadata| match metadata {
            TokenMetadata::StringInterpolation { segments } => Some(segments),
            _ => None,
        })
    });

    let interpolation_segments =
        interpolation_segments.expect("preprocess pipeline should not drop interpolation metadata");
    let expression_count = interpolation_segments
        .iter()
        .filter(|segment| matches!(segment, StringInterpolationSegment::Expression(_)))
        .count();
    assert!(
        expression_count >= 2,
        "expected at least two expression segments, got {:?}",
        interpolation_segments
    );

    fn extract_interpolation<'a>(expr: &'a Expression) -> Option<&'a Expression> {
        match expr {
            Expression::StringInterpolation { .. } => Some(expr),
            Expression::Call { args, .. } => args.iter().find_map(|arg| match arg {
                Argument::Positional(inner) => extract_interpolation(inner),
                Argument::Named { value, .. } => extract_interpolation(value),
            }),
            Expression::Block { statements, .. } => {
                statements.iter().find_map(|statement| match statement {
                    jv_ast::Statement::Expression { expr, .. } => extract_interpolation(expr),
                    jv_ast::Statement::Return {
                        value: Some(expr), ..
                    } => extract_interpolation(expr),
                    jv_ast::Statement::ValDeclaration { initializer, .. } => {
                        extract_interpolation(initializer)
                    }
                    jv_ast::Statement::VarDeclaration {
                        initializer: Some(expr),
                        ..
                    } => extract_interpolation(expr),
                    _ => None,
                })
            }
            _ => None,
        }
    }

    let interpolation_expr = debug
        .statements()
        .iter()
        .find_map(|statement| match statement {
            jv_ast::Statement::FunctionDeclaration { name, body, .. } if name == "announce" => {
                extract_interpolation(body)
            }
            _ => None,
        })
        .expect("function body should contain interpolation expression");

    match interpolation_expr {
        Expression::StringInterpolation { parts, .. } => {
            assert!(
                parts
                    .iter()
                    .filter(|part| matches!(part, StringPart::Expression(_)))
                    .count()
                    >= 2,
                "expected at least two expression parts inside interpolation, got {:?}",
                parts
            );
            assert!(
                parts.iter().any(|part| matches!(
                    part,
                    StringPart::Expression(Expression::Identifier(name, _)) if name == "stage"
                )),
                "stage identifier should be preserved in interpolation parts: {:?}",
                parts
            );
            assert!(
                parts.iter().any(|part| matches!(
                    part,
                    StringPart::Expression(Expression::Identifier(name, _)) if name == "name"
                )),
                "name identifier should be preserved in interpolation parts: {:?}",
                parts
            );
        }
        other => panic!("expected interpolation expression, found {:?}", other),
    }
}
