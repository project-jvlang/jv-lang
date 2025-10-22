use std::path::Path;

use crate::lowering::{lower_program, LoweringDiagnosticSeverity};
use crate::parser::parse as parse_tokens;
use crate::verification::{self, HarnessReport, StatementKindKey};
use crate::{JvLanguage, ParseBuilder};
use jv_lexer::{Lexer, TokenType};
use rowan::SyntaxNode;

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
    let tokens = Lexer::new(source)
        .tokenize()
        .expect("lexing harness fixture");

    assert!(
        matches!(
            tokens.first().map(|token| &token.token_type),
            Some(TokenType::Package)
        ),
        "expected first token to be package keyword"
    );

    let parse_output = parse_tokens(&tokens);
    assert!(
        parse_output.diagnostics.is_empty(),
        "expected no parser diagnostics, got {:?}",
        parse_output.diagnostics
    );

    let green = ParseBuilder::build_from_events(&parse_output.events, &tokens);
    let syntax: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let lowering = lower_program(&syntax, &tokens);
    assert!(
        lowering
            .diagnostics
            .iter()
            .all(|diag| diag.severity != LoweringDiagnosticSeverity::Error),
        "expected no lowering errors, got {:?}",
        lowering.diagnostics
    );
    let warning_count = lowering
        .diagnostics
        .iter()
        .filter(|diag| diag.severity == LoweringDiagnosticSeverity::Warning)
        .count();
    assert_eq!(
        warning_count, 0,
        "expected no lowering warnings, got {:?}",
        lowering.diagnostics
    );

    assert_eq!(
        lowering.statements.len(),
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
    let tokens = Lexer::new(source)
        .tokenize()
        .expect("lexing variables fixture");

    let parse_output = parse_tokens(&tokens);
    let green = ParseBuilder::build_from_events(&parse_output.events, &tokens);
    let syntax: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let lowering = lower_program(&syntax, &tokens);
    assert!(
        lowering
            .diagnostics
            .iter()
            .all(|diag| diag.severity != LoweringDiagnosticSeverity::Error),
        "expected no lowering errors, got {:?}",
        lowering.diagnostics
    );

    let function_body = lowering
        .statements
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
    let tokens = Lexer::new(source)
        .tokenize()
        .expect("lexing comment_visibility fixture");

    let parse_output = parse_tokens(&tokens);
    let green = ParseBuilder::build_from_events(&parse_output.events, &tokens);
    let syntax: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let lowering = lower_program(&syntax, &tokens);

    assert!(
        lowering
            .diagnostics
            .iter()
            .all(|diag| diag.severity != LoweringDiagnosticSeverity::Error),
        "expected no lowering errors, got {:?}",
        lowering.diagnostics
    );

    let kinds: Vec<StatementKindKey> = lowering
        .statements
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
