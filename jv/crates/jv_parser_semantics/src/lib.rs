// jv_parser_semantics - Semantic analysis module for jv language
// Extracted from jv_parser for memory-efficient compilation

mod annotators;
mod metadata;

pub use annotators::{GenericDirectivePass, PrimitiveReturnPass};
pub use metadata::{collect_raw_directives_from_token, primitive_reference_from_segments};

use jv_ast::{Program, Span};
use jv_lexer::Token;

#[derive(Debug, Clone)]
pub struct SemanticsDiagnostic {
    stage: &'static str,
    message: String,
    span: Option<Span>,
}

impl SemanticsDiagnostic {
    pub fn new(stage: &'static str, message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            stage,
            message: message.into(),
            span,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticsStatus {
    Continue,
    Halt,
}

pub trait SemanticsPass {
    fn name(&self) -> &'static str;
    fn run(&self, context: &mut SemanticsContext<'_>) -> SemanticsStatus;
}

pub struct SemanticsContext<'a> {
    tokens: &'a [Token],
    program: &'a mut Program,
    staged_diagnostics: &'a mut Vec<SemanticsDiagnostic>,
}

impl<'a> SemanticsContext<'a> {
    pub fn new(
        tokens: &'a [Token],
        program: &'a mut Program,
        staged_diagnostics: &'a mut Vec<SemanticsDiagnostic>,
    ) -> Self {
        Self {
            tokens,
            program,
            staged_diagnostics,
        }
    }

    pub fn tokens(&self) -> &'a [Token] {
        self.tokens
    }

    pub fn program(&self) -> &Program {
        self.program
    }

    pub fn program_mut(&mut self) -> &mut Program {
        self.program
    }

    pub fn push_diagnostic(&mut self, diagnostic: SemanticsDiagnostic) {
        self.staged_diagnostics.push(diagnostic);
    }
}

pub struct SemanticsPipeline {
    passes: Vec<Box<dyn SemanticsPass>>,
}

impl SemanticsPipeline {
    pub fn builder() -> SemanticsPipelineBuilder {
        SemanticsPipelineBuilder::new()
    }

    pub fn run(&self, tokens: &[Token], mut program: Program) -> SemanticsResult {
        let mut diagnostics = Vec::new();
        let mut halted_stage = None;

        for pass in &self.passes {
            let status = {
                let mut context = SemanticsContext::new(tokens, &mut program, &mut diagnostics);
                pass.run(&mut context)
            };

            if matches!(status, SemanticsStatus::Halt) {
                halted_stage = Some(pass.name());
                break;
            }
        }

        SemanticsResult {
            program,
            staged_diagnostics: diagnostics,
            halted_stage,
        }
    }
}

impl Default for SemanticsPipeline {
    fn default() -> Self {
        SemanticsPipelineBuilder::new()
            .with_pass(PrimitiveReturnPass::default())
            .with_pass(GenericDirectivePass::default())
            .build()
    }
}

pub struct SemanticsPipelineBuilder {
    passes: Vec<Box<dyn SemanticsPass>>,
}

impl SemanticsPipelineBuilder {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn with_pass(mut self, pass: impl SemanticsPass + 'static) -> Self {
        self.passes.push(Box::new(pass));
        self
    }

    pub fn build(self) -> SemanticsPipeline {
        SemanticsPipeline {
            passes: self.passes,
        }
    }
}

pub struct SemanticsResult {
    pub program: Program,
    pub staged_diagnostics: Vec<SemanticsDiagnostic>,
    pub halted_stage: Option<&'static str>,
}

pub fn run(tokens: &[Token], program: Program) -> SemanticsResult {
    SemanticsPipeline::default().run(tokens, program)
}
