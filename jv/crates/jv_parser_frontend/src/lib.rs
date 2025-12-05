// jv_parser_frontend - Frontend API and diagnostics for jv language
// Extracted from jv_parser for memory-efficient compilation

mod formatter;
mod legacy_diagnostics;
mod pipeline;
mod tokens;
mod views;

pub use formatter::{
    Diagnostic, DiagnosticContext, DiagnosticFormatter, DiagnosticSeverity, DiagnosticSource,
    ParserDiagnosticView,
};
pub use pipeline::{ParseError, Parser2Pipeline, ParserPipeline, PipelineArtifacts};
pub use tokens::{
    CommentCarryOverMetadata, ExplicitSeparatorLocation, FieldNameLabelCandidate,
    FieldNameLabelErrorKind, FieldNameLabelIssue, FieldNameLabelKind, FieldNameLabelToken,
    InvalidImplicitParamReason, JsonCommentTrivia, JsonCommentTriviaKind, JsonConfidence,
    LabeledSpan, LayoutCommaMetadata, LayoutSequenceKind, LegacyLoopKeyword, LexError,
    NumberGroupingKind, NumberLiteralMetadata, SourceCommentKind, SourceCommentTrivia,
    StringDelimiterKind, StringInterpolationSegment, StringLiteralMetadata, Token,
    TokenDiagnostic, TokenMetadata, TokenTrivia, TokenType, UnderscoreInfoMetadata,
};
pub use views::{FrontendDiagnostics, FrontendOutput, ProgramView};
