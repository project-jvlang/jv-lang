use chumsky::error::{Simple, SimpleReason};
use jv_ast::Span;
use jv_lexer::Token;
use jv_parser_preprocess::PreprocessDiagnostic;
use jv_parser_semantics::SemanticsDiagnostic;
use jv_parser_syntax::support::{merge_spans, span_from_token};

fn simple_error_span(error: &Simple<Token>, tokens: &[Token]) -> Span {
    if tokens.is_empty() {
        return Span::dummy();
    }

    let range = error.span();
    let tokens_len = tokens.len();

    let start_index = range.start.min(tokens_len - 1);
    let raw_end = if range.end == range.start {
        range.start
    } else {
        range.end.saturating_sub(1)
    };
    let end_index = raw_end.min(tokens_len - 1).max(start_index);

    let start_token = tokens
        .get(start_index)
        .unwrap_or_else(|| tokens.last().unwrap());
    let end_token = tokens
        .get(end_index)
        .unwrap_or_else(|| tokens.last().unwrap());

    let start_span = span_from_token(start_token);
    let end_span = span_from_token(end_token);

    merge_spans(&start_span, &end_span)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSource {
    Preprocess(&'static str),
    Parser,
    Semantics(&'static str),
}

impl DiagnosticSource {
    pub fn name(self) -> &'static str {
        match self {
            DiagnosticSource::Preprocess(stage) | DiagnosticSource::Semantics(stage) => stage,
            DiagnosticSource::Parser => "stage1-parser",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    source: DiagnosticSource,
    message: String,
    code: Option<String>,
    span: Option<Span>,
    severity: DiagnosticSeverity,
}

impl Diagnostic {
    fn new(
        source: DiagnosticSource,
        message: String,
        span: Option<Span>,
        severity: DiagnosticSeverity,
        code: Option<String>,
    ) -> Self {
        Self {
            source,
            message,
            code,
            span,
            severity,
        }
    }

    pub fn source(&self) -> DiagnosticSource {
        self.source
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn code(&self) -> Option<&str> {
        self.code.as_deref()
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub fn severity(&self) -> DiagnosticSeverity {
        self.severity
    }
}

pub struct DiagnosticContext<'a> {
    pub tokens: &'a [Token],
    pub parser_errors: &'a [Simple<Token>],
    pub preprocess_diagnostics: &'a [PreprocessDiagnostic],
    pub preprocess_halted_stage: Option<&'static str>,
    pub semantics_diagnostics: &'a [SemanticsDiagnostic],
    pub semantics_halted_stage: Option<&'static str>,
}

impl<'a> DiagnosticContext<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        tokens: &'a [Token],
        parser_errors: &'a [Simple<Token>],
        preprocess_diagnostics: &'a [PreprocessDiagnostic],
        preprocess_halted_stage: Option<&'static str>,
        semantics_diagnostics: &'a [SemanticsDiagnostic],
        semantics_halted_stage: Option<&'static str>,
    ) -> Self {
        Self {
            tokens,
            parser_errors,
            preprocess_diagnostics,
            preprocess_halted_stage,
            semantics_diagnostics,
            semantics_halted_stage,
        }
    }
}

#[derive(Default)]
pub struct DiagnosticFormatter;

impl DiagnosticFormatter {
    pub fn format(&self, context: DiagnosticContext<'_>) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        diagnostics.extend(context.preprocess_diagnostics.iter().map(|diagnostic| {
            let span = diagnostic.span().cloned();
            let severity = stage_severity(
                diagnostic.stage(),
                diagnostic.span(),
                context.preprocess_halted_stage,
            );
            Diagnostic::new(
                DiagnosticSource::Preprocess(diagnostic.stage()),
                diagnostic.message().to_string(),
                span,
                severity,
                extract_code(diagnostic.message()),
            )
        }));

        diagnostics.extend(context.parser_errors.iter().map(|error| {
            let span = simple_error_span(error, context.tokens);
            let span = if is_dummy_span(&span) {
                None
            } else {
                Some(span)
            };
            let message = format_simple_error(error);
            let severity = if span.is_some() {
                DiagnosticSeverity::Error
            } else {
                DiagnosticSeverity::Warning
            };
            Diagnostic::new(
                DiagnosticSource::Parser,
                message.clone(),
                span,
                severity,
                extract_code(&message),
            )
        }));

        diagnostics.extend(context.semantics_diagnostics.iter().map(|diagnostic| {
            let span = diagnostic.span().cloned();
            let severity = stage_severity(
                diagnostic.stage(),
                diagnostic.span(),
                context.semantics_halted_stage,
            );
            Diagnostic::new(
                DiagnosticSource::Semantics(diagnostic.stage()),
                diagnostic.message().to_string(),
                span,
                severity,
                extract_code(diagnostic.message()),
            )
        }));

        diagnostics
    }
}

fn stage_severity(
    stage_name: &'static str,
    span: Option<&Span>,
    halted_stage: Option<&'static str>,
) -> DiagnosticSeverity {
    if span.is_none() {
        DiagnosticSeverity::Warning
    } else if Some(stage_name) == halted_stage {
        DiagnosticSeverity::Error
    } else {
        DiagnosticSeverity::Warning
    }
}

fn extract_code(message: &str) -> Option<String> {
    let first_line = message.lines().next()?.trim();
    let (candidate, _) = first_line.split_once(':')?;
    let candidate = candidate.trim();

    if candidate.is_empty() {
        return None;
    }

    if candidate
        .chars()
        .all(|ch| ch.is_ascii_uppercase() || ch.is_ascii_digit() || ch == '_')
    {
        Some(candidate.to_string())
    } else {
        None
    }
}

fn format_simple_error(error: &Simple<Token>) -> String {
    match error.reason() {
        SimpleReason::Custom(message) => message.clone(),
        _ => format!("{:?}", error),
    }
}

fn is_dummy_span(span: &Span) -> bool {
    span.start_line == 0 && span.start_column == 0 && span.end_line == 0 && span.end_column == 0
}
