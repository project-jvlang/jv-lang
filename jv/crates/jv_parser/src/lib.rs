#![warn(missing_docs)]

//! High-level parser facade bridging the Salsa frontend pipeline with external consumers.

pub use jv_parser_frontend as frontend;
pub use jv_parser_frontend::{
    Diagnostic, DiagnosticContext, DiagnosticFormatter, DiagnosticSeverity, DiagnosticSource,
    FrontendDiagnostics, FrontendOutput, ParseError, ParserDiagnosticView, ParserPipeline,
    PipelineArtifacts, ProgramView,
};
pub use jv_parser_preprocess as preprocess;
pub use jv_parser_semantics as semantics;

pub use jv_parser_salsa::pipeline::SalsaPipeline;

/// 高水準パーサ API。
pub struct Parser;

impl Parser {
    /// ソースコードを解析し、`FrontendOutput` を生成する。
    pub fn parse(input: &str) -> Result<FrontendOutput, ParseError> {
        // Parsing should stay lightweight; JDK symbol indexing is handled by downstream
        // tooling (CLI/LSP) when needed.
        let pipeline = SalsaPipeline::new_without_jdk();
        pipeline.parse(input)
    }

    /// 任意のパイプライン実装で解析を実行する。
    pub fn parse_with<P: ParserPipeline>(
        pipeline: &P,
        input: &str,
    ) -> Result<FrontendOutput, ParseError> {
        pipeline.parse(input)
    }
}
