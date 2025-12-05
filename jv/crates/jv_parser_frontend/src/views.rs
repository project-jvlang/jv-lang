use jv_ast::{Program, Span, Statement};

use crate::tokens::Token;

use crate::legacy_diagnostics::{PreprocessDiagnostic, SemanticsDiagnostic};

use crate::formatter::Diagnostic;

/// `Program` の読み取り専用ビュー。
///
/// 直接`Program`構造体に依存せず、必要なプロパティのみを公開する。
#[derive(Debug, Clone, Copy)]
pub struct ProgramView<'a> {
    program: &'a Program,
}

impl<'a> ProgramView<'a> {
    pub(crate) fn new(program: &'a Program) -> Self {
        Self { program }
    }

    /// `package`宣言を取得する。
    pub fn package(&self) -> Option<&'a str> {
        self.program.package.as_deref()
    }

    /// `import`文の一覧を返す。
    pub fn imports(&self) -> &'a [Statement] {
        &self.program.imports
    }

    /// トップレベルステートメントの一覧を返す。
    pub fn statements(&self) -> &'a [Statement] {
        &self.program.statements
    }

    /// プログラム全体のスパンを取得する。
    pub fn span(&self) -> &'a Span {
        &self.program.span
    }
}

/// フロントエンド診断の集約結果。
#[derive(Debug, Clone)]
pub struct FrontendDiagnostics {
    final_diagnostics: Vec<Diagnostic>,
    preprocess_diagnostics: Vec<PreprocessDiagnostic>,
    preprocess_halted_stage: Option<&'static str>,
    semantics_diagnostics: Vec<SemanticsDiagnostic>,
    semantics_halted_stage: Option<&'static str>,
}

impl FrontendDiagnostics {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        final_diagnostics: Vec<Diagnostic>,
        preprocess_diagnostics: Vec<PreprocessDiagnostic>,
        preprocess_halted_stage: Option<&'static str>,
        semantics_diagnostics: Vec<SemanticsDiagnostic>,
        semantics_halted_stage: Option<&'static str>,
    ) -> Self {
        Self {
            final_diagnostics,
            preprocess_diagnostics,
            preprocess_halted_stage,
            semantics_diagnostics,
            semantics_halted_stage,
        }
    }

    pub fn final_diagnostics(&self) -> &[Diagnostic] {
        &self.final_diagnostics
    }

    pub fn preprocess_diagnostics(&self) -> &[PreprocessDiagnostic] {
        &self.preprocess_diagnostics
    }

    pub fn preprocess_halted_stage(&self) -> Option<&'static str> {
        self.preprocess_halted_stage
    }

    pub fn semantics_diagnostics(&self) -> &[SemanticsDiagnostic] {
        &self.semantics_diagnostics
    }

    pub fn semantics_halted_stage(&self) -> Option<&'static str> {
        self.semantics_halted_stage
    }
}

/// パーサ全体の出力を保持する構造体。
#[derive(Debug, Clone)]
pub struct FrontendOutput {
    program: Program,
    tokens: Vec<Token>,
    diagnostics: FrontendDiagnostics,
}

impl FrontendOutput {
    pub fn new(program: Program, tokens: Vec<Token>, diagnostics: FrontendDiagnostics) -> Self {
        Self {
            program,
            tokens,
            diagnostics,
        }
    }

    pub fn program(&self) -> ProgramView<'_> {
        ProgramView::new(&self.program)
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn diagnostics(&self) -> &FrontendDiagnostics {
        &self.diagnostics
    }

    pub fn into_parts(self) -> (Program, Vec<Token>, FrontendDiagnostics) {
        (self.program, self.tokens, self.diagnostics)
    }

    pub fn into_program(self) -> Program {
        let (program, _, _) = self.into_parts();
        program
    }

    pub fn into_tokens(self) -> Vec<Token> {
        let (_, tokens, _) = self.into_parts();
        tokens
    }

    pub fn into_diagnostics(self) -> FrontendDiagnostics {
        let (_, _, diagnostics) = self.into_parts();
        diagnostics
    }
}
