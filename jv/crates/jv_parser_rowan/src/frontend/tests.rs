use super::{diagnostics, RowanPipeline};
use crate::lowering::{LoweringDiagnostic, LoweringDiagnosticSeverity};
use crate::parser::{DiagnosticSeverity as ParserDiagnosticSeverity, ParserDiagnostic, TokenSpan};
use jv_ast::Span;
use jv_lexer::{Lexer, Token};
use jv_parser_frontend::{ParseError, ParserPipeline};
use jv_parser_preprocess::PreprocessDiagnostic;
use jv_parser_semantics::SemanticsDiagnostic;

fn sample_tokens(source: &str) -> Vec<Token> {
    Lexer::new(source.to_string())
        .tokenize()
        .expect("lexing should succeed")
}

#[test]
fn pipeline_produces_frontend_output_for_basic_program() {
    let source = r#"
        package demo
        val answer = 42
    "#;

    let pipeline = RowanPipeline::new();
    let output = pipeline
        .execute(source)
        .expect("pipeline should succeed")
        .into_frontend_output();

    assert_eq!(output.program().package(), Some("demo"));
    assert!(
        !output.program().statements().is_empty(),
        "expected at least one statement in the lowered program"
    );
    assert!(
        output.diagnostics().final_diagnostics().is_empty(),
        "expected no diagnostics for a clean program"
    );
}

#[test]
fn pipeline_reports_parser_error_with_stage_context() {
    let source = r#"
        fun demo() {
            while (true) {
                return
            }
        }
    "#;
    let pipeline = RowanPipeline::new();
    let error = pipeline
        .execute(source)
        .expect_err("pipeline should emit a syntax error");

    match error {
        ParseError::Syntax { message, span } => {
            assert!(
                message.contains(diagnostics::ROWAN_PARSER_STAGE),
                "expected parser stage in message, got {}",
                message
            );
            assert!(
                span.end_column >= span.start_column,
                "expected span to be well-formed, got {:?}",
                span
            );
        }
        other => panic!("unexpected error variant: {:?}", other),
    }
}

#[test]
fn diagnostics_adapter_merges_parser_lowering_and_semantics() {
    let tokens = sample_tokens("val demo = 1");

    let parser_diagnostics = vec![ParserDiagnostic {
        message: "ROWAN_PARSER_DIAG: sample parser diagnostic".to_string(),
        severity: ParserDiagnosticSeverity::Error,
        span: TokenSpan::new(0, 1),
    }];

    let lowering_diagnostics = vec![LoweringDiagnostic {
        message: "ROWAN_LOWERING_DIAG: sample lowering warning".to_string(),
        severity: LoweringDiagnosticSeverity::Warning,
        span: Some(Span::new(1, 1, 1, 5)),
        node_kind: crate::syntax::SyntaxKind::StatementList,
        identifier: None,
        annotations: vec![],
    }];

    let preprocess_diagnostics = vec![PreprocessDiagnostic::new(
        "stage0-sample",
        "STAGE0_SAMPLE: preprocess note",
        Some(Span::new(1, 1, 1, 3)),
    )];

    let semantics_diagnostics = vec![SemanticsDiagnostic::new(
        "stage2-sample",
        "STAGE2_SAMPLE: semantics note",
        Some(Span::new(1, 1, 1, 4)),
    )];

    let frontend_diagnostics = diagnostics::compose_frontend_diagnostics(
        &tokens,
        &parser_diagnostics,
        &lowering_diagnostics,
        preprocess_diagnostics,
        None,
        semantics_diagnostics,
        None,
    );

    let final_diagnostics = frontend_diagnostics.final_diagnostics();
    assert_eq!(
        final_diagnostics.len(),
        4,
        "expected preprocess, parser, lowering, and semantics diagnostics"
    );

    let parser_entry = final_diagnostics
        .iter()
        .find(|diagnostic| diagnostic.source().name() == diagnostics::ROWAN_PARSER_STAGE)
        .expect("parser diagnostic should be present");
    assert_eq!(
        parser_entry.severity(),
        jv_parser_frontend::DiagnosticSeverity::Error
    );

    let lowering_entry = final_diagnostics
        .iter()
        .find(|diagnostic| diagnostic.source().name() == diagnostics::ROWAN_LOWERING_STAGE)
        .expect("lowering diagnostic should be present");
    assert_eq!(
        lowering_entry.severity(),
        jv_parser_frontend::DiagnosticSeverity::Warning
    );
}
