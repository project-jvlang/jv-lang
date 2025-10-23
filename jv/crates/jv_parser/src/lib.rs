#![warn(missing_docs)]

//! High-level parser facade bridging the Rowan frontend pipeline with external consumers.

#[cfg(not(feature = "rowan-parser"))]
compile_error!("The `rowan-parser` feature must be enabled to use the jv parser frontend.");

pub use jv_parser_frontend as frontend;
pub use jv_parser_frontend::{
    Diagnostic, DiagnosticContext, DiagnosticFormatter, DiagnosticSeverity, DiagnosticSource,
    FrontendDiagnostics, FrontendOutput, ParseError, ParserDiagnosticView, ParserPipeline,
    PipelineArtifacts, ProgramView,
};
pub use jv_parser_preprocess as preprocess;
pub use jv_parser_semantics as semantics;

#[cfg(feature = "rowan-parser")]
pub use jv_parser_rowan::frontend::RowanPipeline;

/// 高水準パーサ API。
pub struct Parser;

impl Parser {
    /// ソースコードを解析し、`FrontendOutput` を生成する。
    #[cfg(feature = "rowan-parser")]
    pub fn parse(input: &str) -> Result<FrontendOutput, ParseError> {
        let pipeline = RowanPipeline::default();
        pipeline.parse(input)
    }

    /// 任意のパイプライン実装で解析を実行する。
    #[cfg(feature = "rowan-parser")]
    pub fn parse_with<P: ParserPipeline>(
        pipeline: &P,
        input: &str,
    ) -> Result<FrontendOutput, ParseError> {
        pipeline.parse(input)
    }
}
