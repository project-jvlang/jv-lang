// jv_parser_frontend - Frontend API and diagnostics for jv language
// Extracted from jv_parser for memory-efficient compilation

mod formatter;
mod pipeline;
mod views;

pub use formatter::{
    Diagnostic, DiagnosticContext, DiagnosticFormatter, DiagnosticSeverity, DiagnosticSource,
    ParserDiagnosticView,
};
pub use pipeline::{ParseError, Parser2Pipeline, ParserPipeline, PipelineArtifacts};
pub use views::{FrontendDiagnostics, FrontendOutput, ProgramView};
