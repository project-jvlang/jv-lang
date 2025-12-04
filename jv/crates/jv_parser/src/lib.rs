#![warn(missing_docs)]

//! High-level parser facade bridging the new parser2 pipeline with external consumers.

pub use jv_parser_frontend as frontend;
pub use jv_parser_frontend::{
    Diagnostic, DiagnosticContext, DiagnosticFormatter, DiagnosticSeverity, DiagnosticSource,
    FrontendDiagnostics, FrontendOutput, ParseError, ParserDiagnosticView, ParserPipeline,
    PipelineArtifacts, ProgramView,
};
pub use jv_parser_preprocess as preprocess;
pub use jv_parser_semantics as semantics;

#[cfg(feature = "parser2")]
pub use jv_parser_frontend::Parser2Pipeline;

/// 高水準パーサ API。
pub struct Parser;

impl Parser {
    /// ソースコードを解析し、`FrontendOutput` を生成する。
    #[cfg(feature = "parser2")]
    pub fn parse(input: &str) -> Result<FrontendOutput, ParseError> {
        let pipeline = Parser2Pipeline::default();
        pipeline.parse(input)
    }

    /// 任意のパイプライン実装で解析を実行する。
    #[cfg(feature = "parser2")]
    pub fn parse_with<P: ParserPipeline>(
        pipeline: &P,
        input: &str,
    ) -> Result<FrontendOutput, ParseError> {
        pipeline.parse(input)
    }
}
