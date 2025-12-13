use crate::lower::{LoweringDiagnostic, LoweringDiagnosticSeverity};
use crate::parser::OwnedToken;
use crate::parser::{ParserDiagnostic, TokenSpan};
use crate::support::token_compat::LineIndex;
use jv_ast::Span;
use jv_parser_frontend::{
    DiagnosticContext, DiagnosticFormatter, DiagnosticSeverity, FrontendDiagnostics,
    ParserDiagnosticView,
};
use jv_parser_preprocess::PreprocessDiagnostic;
use jv_parser_semantics::SemanticsDiagnostic;

pub(crate) const SALSA_PARSER_STAGE: &str = "salsa-parser";
pub(crate) const SALSA_LOWERING_STAGE: &str = "salsa-lowering";

/// フロントエンド診断を整形する。
#[allow(clippy::too_many_arguments)]
pub(crate) fn compose_frontend_diagnostics(
    tokens: &[OwnedToken],
    parser_diagnostics: &[ParserDiagnostic],
    lowering_diagnostics: &[LoweringDiagnostic],
    preprocess_diagnostics: Vec<PreprocessDiagnostic>,
    preprocess_halted_stage: Option<&'static str>,
    semantics_diagnostics: Vec<SemanticsDiagnostic>,
    semantics_halted_stage: Option<&'static str>,
    line_index: &LineIndex,
) -> FrontendDiagnostics {
    let mut parser_views = Vec::new();

    parser_views.extend(parser_diagnostics.iter().map(|diagnostic| {
        let span = token_span_to_span(diagnostic.span, tokens, line_index);
        ParserDiagnosticView::new(
            SALSA_PARSER_STAGE,
            diagnostic.message.clone(),
            span,
            map_parser_severity(diagnostic.severity),
        )
    }));

    parser_views.extend(lowering_diagnostics.iter().map(|diagnostic| {
        ParserDiagnosticView::new(
            SALSA_LOWERING_STAGE,
            diagnostic.message.clone(),
            diagnostic.span.clone(),
            map_lowering_severity(diagnostic.severity),
        )
    }));

    let formatter = DiagnosticFormatter;
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

/// トークンスパンを AST スパンへ変換する。
pub(crate) fn token_span_to_span(
    span: TokenSpan,
    tokens: &[OwnedToken],
    line_index: &LineIndex,
) -> Option<Span> {
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

    let start_span = line_index.span_to_ast_span(start_token.span);
    let end_span = line_index.span_to_ast_span(end_token.span);

    Some(merge_spans(&start_span, &end_span))
}

fn merge_spans(first: &Span, last: &Span) -> Span {
    Span {
        start_line: first.start_line,
        start_column: first.start_column,
        end_line: last.end_line,
        end_column: last.end_column,
    }
}

fn map_parser_severity(severity: crate::parser::DiagnosticSeverity) -> DiagnosticSeverity {
    match severity {
        crate::parser::DiagnosticSeverity::Error => DiagnosticSeverity::Error,
        crate::parser::DiagnosticSeverity::Warning => DiagnosticSeverity::Warning,
    }
}

fn map_lowering_severity(severity: LoweringDiagnosticSeverity) -> DiagnosticSeverity {
    match severity {
        LoweringDiagnosticSeverity::Error => DiagnosticSeverity::Error,
        LoweringDiagnosticSeverity::Warning => DiagnosticSeverity::Warning,
    }
}
