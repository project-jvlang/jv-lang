// jv_lsp - Language Server Protocol implementation
use jv_checker::diagnostics::{
    collect_raw_type_diagnostics, from_check_error, from_parse_error, from_transform_error,
    DiagnosticStrategy, EnhancedDiagnostic,
};
use jv_checker::{CheckError, TypeChecker};
use jv_inference::{service::TypeFactsSnapshot, ParallelInferenceConfig};
use jv_ir::transform_program;
use jv_parser::Parser as JvParser;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;

const COMPLETION_TEMPLATES: &[&str] = &[
    "name = value",
    "var name = value",
    "data Point(x y)",
    "data Record(name: Type)",
    "fun name(params) { }",
];

#[derive(Error, Debug)]
pub enum LspError {
    #[error("Protocol error: {0}")]
    ProtocolError(String),
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    pub uri: String,
    pub range: Range,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub range: Range,
    pub severity: Option<DiagnosticSeverity>,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub code: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub suggestions: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub strategy: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DiagnosticSeverity {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
}

pub struct JvLanguageServer {
    documents: HashMap<String, String>,
    type_facts: HashMap<String, TypeFactsSnapshot>,
    parallel_config: ParallelInferenceConfig,
}

impl JvLanguageServer {
    pub fn new() -> Self {
        Self::with_parallel_config(ParallelInferenceConfig::default())
    }

    pub fn with_parallel_config(config: ParallelInferenceConfig) -> Self {
        Self {
            documents: HashMap::new(),
            type_facts: HashMap::new(),
            parallel_config: config,
        }
    }

    pub fn set_parallel_config(&mut self, config: ParallelInferenceConfig) {
        self.parallel_config = config;
    }

    pub fn open_document(&mut self, uri: String, content: String) {
        self.documents.insert(uri.clone(), content);
        self.type_facts.remove(&uri);
    }

    pub fn get_diagnostics(&mut self, uri: &str) -> Vec<Diagnostic> {
        let Some(content) = self.documents.get(uri) else {
            return Vec::new();
        };

        let program = match JvParser::parse(content) {
            Ok(program) => program,
            Err(error) => {
                self.type_facts.remove(uri);
                return match from_parse_error(&error) {
                    Some(diagnostic) => vec![tooling_diagnostic_to_lsp(
                        uri,
                        diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                    )],
                    None => vec![fallback_diagnostic(uri, "Parser error")],
                };
            }
        };

        let mut diagnostics = Vec::new();
        let mut type_facts_snapshot: Option<TypeFactsSnapshot> = None;

        let mut checker = TypeChecker::with_parallel_config(self.parallel_config);
        match checker.check_program(&program) {
            Ok(_) => {
                let null_safety_warnings = if let Some(normalized) = checker.normalized_program() {
                    let cloned = normalized.clone();
                    checker.check_null_safety(&cloned, None)
                } else {
                    checker.check_null_safety(&program, None)
                };
                if let Some(snapshot) = checker.take_inference_snapshot() {
                    type_facts_snapshot = Some(snapshot.type_facts().clone());
                } else {
                    self.type_facts.remove(uri);
                }
                diagnostics.extend(
                    null_safety_warnings
                        .into_iter()
                        .map(|warning| warning_diagnostic(uri, warning)),
                );
            }
            Err(errors) => {
                self.type_facts.remove(uri);
                return errors
                    .into_iter()
                    .map(|error| type_error_to_diagnostic(uri, error))
                    .collect();
            }
        }

        let lowering_input = checker.take_normalized_program().unwrap_or(program);
        let ir_program = match transform_program(lowering_input) {
            Ok(ir) => Some(ir),
            Err(error) => {
                diagnostics.push(match from_transform_error(&error) {
                    Some(diagnostic) => tooling_diagnostic_to_lsp(
                        uri,
                        diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                    ),
                    None => fallback_diagnostic(uri, "IR transformation error"),
                });
                None
            }
        };

        if let Some(ir_program) = ir_program.as_ref() {
            let raw_type_diagnostics = collect_raw_type_diagnostics(ir_program);
            diagnostics.extend(raw_type_diagnostics.into_iter().map(|diagnostic| {
                tooling_diagnostic_to_lsp(
                    uri,
                    diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                )
            }));
        }

        if let Some(snapshot) = type_facts_snapshot {
            self.type_facts.insert(uri.to_string(), snapshot);
        }

        diagnostics
    }

    pub fn get_completions(&self, _uri: &str, _position: Position) -> Vec<String> {
        COMPLETION_TEMPLATES
            .iter()
            .map(|template| (*template).to_string())
            .collect()
    }

    pub fn type_facts(&self, uri: &str) -> Option<&TypeFactsSnapshot> {
        self.type_facts.get(uri)
    }
}

impl Default for JvLanguageServer {
    fn default() -> Self {
        Self::new()
    }
}

fn tooling_diagnostic_to_lsp(uri: &str, diagnostic: EnhancedDiagnostic) -> Diagnostic {
    let range = diagnostic
        .span
        .as_ref()
        .map(span_to_range)
        .unwrap_or_else(default_range);

    Diagnostic {
        range,
        severity: Some(map_severity(diagnostic.severity)),
        message: format!(
            "{code}: {title}\nSource: {uri}\n{detail}",
            code = diagnostic.code,
            title = diagnostic.title,
            uri = uri,
            detail = diagnostic.message,
        ),
        code: Some(diagnostic.code.to_string()),
        source: Some("jv-lsp".to_string()),
        help: Some(diagnostic.help.to_string()),
        suggestions: diagnostic.suggestions.clone(),
        strategy: Some(format!("{:?}", diagnostic.strategy)),
    }
}

fn fallback_diagnostic(uri: &str, label: &str) -> Diagnostic {
    let message = format!("{}: unable to analyse {}", label, uri);
    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::Warning),
        message,
        code: None,
        source: Some("jv-lsp".to_string()),
        help: None,
        suggestions: Vec::new(),
        strategy: Some("Immediate".to_string()),
    }
}

fn warning_diagnostic(uri: &str, warning: CheckError) -> Diagnostic {
    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::Warning),
        message: format!("Warning ({uri}): {warning}"),
        code: None,
        source: Some("jv-lsp".to_string()),
        help: None,
        suggestions: Vec::new(),
        strategy: Some("Deferred".to_string()),
    }
}

fn type_error_to_diagnostic(uri: &str, error: CheckError) -> Diagnostic {
    if let Some(diagnostic) = from_check_error(&error) {
        tooling_diagnostic_to_lsp(
            uri,
            diagnostic.with_strategy(DiagnosticStrategy::Interactive),
        )
    } else {
        Diagnostic {
            range: default_range(),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Type error: {error}"),
            code: None,
            source: Some("jv-lsp".to_string()),
            help: None,
            suggestions: Vec::new(),
            strategy: Some("Immediate".to_string()),
        }
    }
}

fn map_severity(severity: jv_checker::diagnostics::DiagnosticSeverity) -> DiagnosticSeverity {
    match severity {
        jv_checker::diagnostics::DiagnosticSeverity::Error => DiagnosticSeverity::Error,
        jv_checker::diagnostics::DiagnosticSeverity::Warning => DiagnosticSeverity::Warning,
        jv_checker::diagnostics::DiagnosticSeverity::Information => DiagnosticSeverity::Information,
        jv_checker::diagnostics::DiagnosticSeverity::Hint => DiagnosticSeverity::Hint,
    }
}

fn default_range() -> Range {
    Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: 0,
            character: 1,
        },
    }
}

fn span_to_range(span: &jv_ast::Span) -> Range {
    Range {
        start: Position {
            line: span.start_line.saturating_sub(1) as u32,
            character: span.start_column.saturating_sub(1) as u32,
        },
        end: Position {
            line: span.end_line.saturating_sub(1) as u32,
            character: span.end_column.saturating_sub(1) as u32,
        },
    }
}

#[cfg(test)]
mod tests;
