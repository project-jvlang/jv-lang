use crate::lowering::{LoweringDiagnostic, LoweringDiagnosticSeverity};
use crate::parser::{DiagnosticSeverity as ParserDiagnosticSeverity, ParserDiagnostic, TokenSpan};
use crate::support::spans::{merge_spans, span_from_token};
use jv_ast::Span;
use jv_lexer::Token;
use jv_parser_frontend::{
    DiagnosticContext, DiagnosticFormatter, DiagnosticSeverity, FrontendDiagnostics,
    ParserDiagnosticView,
};
use jv_parser_preprocess::PreprocessDiagnostic;
use jv_parser_semantics::SemanticsDiagnostic;

pub(crate) const ROWAN_PARSER_STAGE: &str = "rowan-parser";
pub(crate) const ROWAN_LOWERING_STAGE: &str = "rowan-lowering";

pub(crate) fn compose_frontend_diagnostics(
    tokens: &[Token],
    parser_diagnostics: &[ParserDiagnostic],
    lowering_diagnostics: &[LoweringDiagnostic],
    preprocess_diagnostics: Vec<PreprocessDiagnostic>,
    preprocess_halted_stage: Option<&'static str>,
    semantics_diagnostics: Vec<SemanticsDiagnostic>,
    semantics_halted_stage: Option<&'static str>,
) -> FrontendDiagnostics {
    let mut parser_views = Vec::new();

    parser_views.extend(parser_diagnostics.iter().map(|diagnostic| {
        let span = token_span_to_span(diagnostic.span, tokens);
        ParserDiagnosticView::new(
            ROWAN_PARSER_STAGE,
            diagnostic.message.clone(),
            span,
            map_parser_severity(diagnostic.severity),
        )
    }));

    parser_views.extend(lowering_diagnostics.iter().map(|diagnostic| {
        ParserDiagnosticView::new(
            ROWAN_LOWERING_STAGE,
            diagnostic.message.clone(),
            diagnostic.span.clone(),
            map_lowering_severity(diagnostic.severity),
        )
    }));

    let formatter = DiagnosticFormatter::default();
    let context = DiagnosticContext::new(
        &parser_views,
        &preprocess_diagnostics,
        preprocess_halted_stage,
        &semantics_diagnostics,
        semantics_halted_stage,
    );
    let final_diagnostics = formatter.format(context);

    FrontendDiagnostics::new(
        final_diagnostics,
        preprocess_diagnostics,
        preprocess_halted_stage,
        semantics_diagnostics,
        semantics_halted_stage,
    )
}

pub(crate) fn token_span_to_span(span: TokenSpan, tokens: &[Token]) -> Option<Span> {
    if tokens.is_empty() {
        return None;
    }

    let max_index = tokens.len().saturating_sub(1);
    let start_index = span.start.min(max_index);
    let raw_end = if span.end == 0 {
        0
    } else {
        span.end.saturating_sub(1)
    };
    let end_index = raw_end.min(max_index).max(start_index);

    let start_token = tokens.get(start_index)?;
    let end_token = tokens.get(end_index)?;
    let start_span = span_from_token(start_token);
    let end_span = span_from_token(end_token);
    Some(merge_spans(&start_span, &end_span))
}

fn map_parser_severity(severity: ParserDiagnosticSeverity) -> DiagnosticSeverity {
    match severity {
        ParserDiagnosticSeverity::Error => DiagnosticSeverity::Error,
        ParserDiagnosticSeverity::Warning => DiagnosticSeverity::Warning,
    }
}

fn map_lowering_severity(severity: LoweringDiagnosticSeverity) -> DiagnosticSeverity {
    match severity {
        LoweringDiagnosticSeverity::Error => DiagnosticSeverity::Error,
        LoweringDiagnosticSeverity::Warning => DiagnosticSeverity::Warning,
    }
}
