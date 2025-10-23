use crate::views::{FrontendDiagnostics, FrontendOutput};
use jv_ast::{Program, Span};
use jv_lexer::{LexError, Token};
use thiserror::Error;

/// フロントエンドパイプラインの共通出力。
#[derive(Debug)]
pub struct PipelineArtifacts {
    pub program: Program,
    pub tokens: Vec<Token>,
    pub diagnostics: FrontendDiagnostics,
}

impl PipelineArtifacts {
    pub fn new(program: Program, tokens: Vec<Token>, diagnostics: FrontendDiagnostics) -> Self {
        Self {
            program,
            tokens,
            diagnostics,
        }
    }

    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn diagnostics(&self) -> &FrontendDiagnostics {
        &self.diagnostics
    }

    pub fn into_frontend_output(self) -> FrontendOutput {
        FrontendOutput::new(self.program, self.tokens, self.diagnostics)
    }

    pub fn into_parts(self) -> (Program, Vec<Token>, FrontendDiagnostics) {
        (self.program, self.tokens, self.diagnostics)
    }
}

/// フロントエンドパイプラインの実行インターフェース。
pub trait ParserPipeline {
    fn execute(&self, source: &str) -> Result<PipelineArtifacts, ParseError>;

    fn parse(&self, source: &str) -> Result<FrontendOutput, ParseError> {
        self.execute(source)
            .map(PipelineArtifacts::into_frontend_output)
    }
}

/// フロントエンド解析中に発生するエラー。
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Lexical error: {0}")]
    LexError(#[from] LexError),
    #[error("Parse error at {span:?}: {message}")]
    Syntax { message: String, span: Span },
    #[error("Unexpected end of input at {span:?}")]
    UnexpectedEof { span: Span },
}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::Syntax { span, .. } | ParseError::UnexpectedEof { span } => span.clone(),
            ParseError::LexError(_) => Span::dummy(),
        }
    }
}
