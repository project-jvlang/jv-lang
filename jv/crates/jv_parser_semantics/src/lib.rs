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

/// ラベルが紐づく構造体の種別。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelTargetKind {
    Loop,
    When,
    Block,
    Lambda,
}

/// ラベルのバインディング情報。
#[derive(Debug, Clone, PartialEq)]
pub struct LabelBinding {
    pub name: String,
    pub target: LabelTargetKind,
    pub span: Span,
}

/// ラベル追加時の結果。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelInsertionOutcome {
    Inserted,
    DuplicateInScope,
}

pub struct SemanticsContext<'a> {
    tokens: &'a [Token],
    program: &'a mut Program,
    staged_diagnostics: &'a mut Vec<SemanticsDiagnostic>,
    label_stack: Vec<LabelBinding>,
    label_scope_markers: Vec<usize>,
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
            label_stack: Vec::new(),
            label_scope_markers: vec![0],
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

    /// 現在のスコープにラベルを追加し、重複があるかを返す。
    pub fn push_label(
        &mut self,
        name: impl Into<String>,
        target: LabelTargetKind,
        span: Span,
    ) -> LabelInsertionOutcome {
        let name = name.into();
        let scope_start = *self.label_scope_markers.last().unwrap_or(&0);
        let is_duplicate = self.label_stack[scope_start..]
            .iter()
            .any(|binding| binding.name == name);

        self.label_stack.push(LabelBinding { name, target, span });

        if is_duplicate {
            LabelInsertionOutcome::DuplicateInScope
        } else {
            LabelInsertionOutcome::Inserted
        }
    }

    /// 最も内側のラベルをポップする。
    pub fn pop_label(&mut self) -> Option<LabelBinding> {
        self.label_stack.pop()
    }

    /// 新しいラベルスコープを開始する。
    pub fn enter_label_scope(&mut self) {
        self.label_scope_markers.push(self.label_stack.len());
    }

    /// 現在のスコープを終了し、削除されたラベルを返す。
    pub fn exit_label_scope(&mut self) -> Vec<LabelBinding> {
        if self.label_scope_markers.len() <= 1 {
            return Vec::new();
        }

        let scope_start = self
            .label_scope_markers
            .pop()
            .expect("label scope marker should exist");

        let mut removed = Vec::new();
        while self.label_stack.len() > scope_start {
            if let Some(binding) = self.label_stack.pop() {
                removed.push(binding);
            }
        }
        removed
    }

    /// 指定された名前のラベルを、内側から探索して返す。
    pub fn find_label(&self, name: &str) -> Option<&LabelBinding> {
        self.label_stack
            .iter()
            .rev()
            .find(|binding| binding.name == name)
    }

    /// 現在のラベルスタックを参照用に返す。
    pub fn label_stack(&self) -> &[LabelBinding] {
        &self.label_stack
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

#[cfg(test)]
mod tests {
    use super::*;

    struct ContextFixture {
        tokens: Vec<Token>,
        program: Program,
        diagnostics: Vec<SemanticsDiagnostic>,
    }

    impl ContextFixture {
        fn new() -> Self {
            Self {
                tokens: Vec::new(),
                program: Program {
                    package: None,
                    imports: Vec::new(),
                    statements: Vec::new(),
                    span: Span::default(),
                },
                diagnostics: Vec::new(),
            }
        }
    }

    fn span() -> Span {
        Span::new(1, 1, 1, 5)
    }

    #[test]
    fn label_scopes_push_and_exit_correctly() {
        let mut fixture = ContextFixture::new();
        let mut context = SemanticsContext::new(
            &fixture.tokens,
            &mut fixture.program,
            &mut fixture.diagnostics,
        );
        context.enter_label_scope();
        context.push_label("outer", LabelTargetKind::Loop, span());
        context.enter_label_scope();
        context.push_label("inner", LabelTargetKind::Block, span());

        assert!(context.find_label("inner").is_some());
        let removed_inner = context.exit_label_scope();
        assert_eq!(removed_inner.len(), 1);
        assert!(context.find_label("inner").is_none());
        assert!(context.find_label("outer").is_some());

        let removed_outer = context.exit_label_scope();
        assert_eq!(removed_outer.len(), 1);
        assert!(context.find_label("outer").is_none());
    }

    #[test]
    fn label_shadowing_is_detected_only_in_same_scope() {
        let mut fixture = ContextFixture::new();
        let mut context = SemanticsContext::new(
            &fixture.tokens,
            &mut fixture.program,
            &mut fixture.diagnostics,
        );
        context.enter_label_scope();
        let first = context.push_label("label", LabelTargetKind::Loop, span());
        assert_eq!(first, LabelInsertionOutcome::Inserted);

        let duplicate_same_scope = context.push_label("label", LabelTargetKind::Loop, span());
        assert_eq!(
            duplicate_same_scope,
            LabelInsertionOutcome::DuplicateInScope
        );

        context.enter_label_scope();
        let shadowed = context.push_label("label", LabelTargetKind::Block, span());
        assert_eq!(shadowed, LabelInsertionOutcome::Inserted);
        assert!(matches!(
            context.find_label("label"),
            Some(binding) if binding.target == LabelTargetKind::Block
        ));
    }
}
