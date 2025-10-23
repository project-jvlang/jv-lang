use jv_ast::Span;
use jv_parser_preprocess::PreprocessDiagnostic;
use jv_parser_semantics::SemanticsDiagnostic;

/// 診断の深刻度。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
}

/// 診断の発行元を表す。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSource {
    Preprocess(&'static str),
    Parser(&'static str),
    Semantics(&'static str),
}

impl DiagnosticSource {
    /// ステージ名を返す。
    pub fn name(self) -> &'static str {
        match self {
            DiagnosticSource::Preprocess(stage)
            | DiagnosticSource::Parser(stage)
            | DiagnosticSource::Semantics(stage) => stage,
        }
    }
}

/// 整形済み診断情報。
#[derive(Debug, Clone)]
pub struct Diagnostic {
    source: DiagnosticSource,
    message: String,
    code: Option<String>,
    span: Option<Span>,
    severity: DiagnosticSeverity,
}

impl Diagnostic {
    pub fn new(
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

/// パーサ／ローワリング診断を `DiagnosticFormatter` に渡すための簡易ビュー。
#[derive(Debug, Clone)]
pub struct ParserDiagnosticView {
    stage: &'static str,
    message: String,
    span: Option<Span>,
    severity: DiagnosticSeverity,
    code: Option<String>,
}

impl ParserDiagnosticView {
    /// メッセージの先頭からコードを自動抽出して生成する。
    pub fn new(
        stage: &'static str,
        message: impl Into<String>,
        span: Option<Span>,
        severity: DiagnosticSeverity,
    ) -> Self {
        let message = message.into();
        let code = extract_code(&message);
        Self {
            stage,
            message,
            span,
            severity,
            code,
        }
    }

    /// 明示的にコードを指定して生成する。
    pub fn with_code(
        stage: &'static str,
        message: impl Into<String>,
        span: Option<Span>,
        severity: DiagnosticSeverity,
        code: Option<String>,
    ) -> Self {
        Self {
            stage,
            message: message.into(),
            span,
            severity,
            code,
        }
    }

    pub fn stage(&self) -> &'static str {
        self.stage
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub fn severity(&self) -> DiagnosticSeverity {
        self.severity
    }

    pub fn code(&self) -> Option<&str> {
        self.code.as_deref()
    }
}

/// 診断整形に必要な文脈情報。
pub struct DiagnosticContext<'a> {
    pub parser_diagnostics: &'a [ParserDiagnosticView],
    pub preprocess_diagnostics: &'a [PreprocessDiagnostic],
    pub preprocess_halted_stage: Option<&'static str>,
    pub semantics_diagnostics: &'a [SemanticsDiagnostic],
    pub semantics_halted_stage: Option<&'static str>,
}

impl<'a> DiagnosticContext<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        parser_diagnostics: &'a [ParserDiagnosticView],
        preprocess_diagnostics: &'a [PreprocessDiagnostic],
        preprocess_halted_stage: Option<&'static str>,
        semantics_diagnostics: &'a [SemanticsDiagnostic],
        semantics_halted_stage: Option<&'static str>,
    ) -> Self {
        Self {
            parser_diagnostics,
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

        diagnostics.extend(context.parser_diagnostics.iter().map(|diagnostic| {
            Diagnostic::new(
                DiagnosticSource::Parser(diagnostic.stage()),
                diagnostic.message().to_string(),
                diagnostic.span().cloned(),
                diagnostic.severity(),
                diagnostic.code().map(str::to_string),
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
