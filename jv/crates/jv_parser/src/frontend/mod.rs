mod views;

pub use views::ProgramView;

use crate::diagnostics::Diagnostic;
use crate::preprocess::PreprocessDiagnostic;
use crate::semantics::SemanticsDiagnostic;
use jv_ast::Program;
use jv_lexer::Token;

/// パーサフロントエンドのパイプライン結果を保持する構造体。
///
/// 読み取り専用ビューと最終診断を提供し、内部実装へ依存せずに
/// コンパイラ下流のクレートが参照できるようにする。
#[derive(Debug, Clone)]
pub struct FrontendOutput {
    program: Program,
    tokens: Vec<Token>,
    diagnostics: FrontendDiagnostics,
}

impl FrontendOutput {
    /// 新しいフロントエンド出力を生成する。
    pub fn new(program: Program, tokens: Vec<Token>, diagnostics: FrontendDiagnostics) -> Self {
        Self {
            program,
            tokens,
            diagnostics,
        }
    }

    /// AST全体への読み取り専用ビューを取得する。
    pub fn program_view(&self) -> ProgramView<'_> {
        ProgramView::new(&self.program)
    }

    /// ASTを直接参照する。可変アクセスは提供しない。
    pub fn program(&self) -> &Program {
        &self.program
    }

    /// ASTを所有権ごと取り出す。既存の処理系との互換性確保用。
    pub fn into_program(self) -> Program {
        self.program
    }

    /// プリプロセス後のトークン列を返す。
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// トークン列を所有権ごと取り出す。
    pub fn into_tokens(self) -> Vec<Token> {
        self.tokens
    }

    /// パイプライン全体で生成された診断情報を返す。
    pub fn diagnostics(&self) -> &FrontendDiagnostics {
        &self.diagnostics
    }

    /// 診断情報を所有権ごと取り出す。
    pub fn into_diagnostics(self) -> FrontendDiagnostics {
        self.diagnostics
    }

    /// Stage3整形済みの最終診断一覧を直接取得する。
    pub fn final_diagnostics(&self) -> &[Diagnostic] {
        self.diagnostics.finalized()
    }
}

/// フロントエンドパイプラインで得られた診断情報セット。
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

    /// Stage3整形済みの最終診断一覧。
    pub fn finalized(&self) -> &[Diagnostic] {
        &self.final_diagnostics
    }

    /// Stage0で蓄積した未整形診断一覧。
    pub fn preprocess(&self) -> &[PreprocessDiagnostic] {
        &self.preprocess_diagnostics
    }

    /// Stage0で処理が停止した場合のステージ識別子。
    pub fn preprocess_halted_stage(&self) -> Option<&'static str> {
        self.preprocess_halted_stage
    }

    /// Stage2で蓄積した未整形診断一覧。
    pub fn semantics(&self) -> &[SemanticsDiagnostic] {
        &self.semantics_diagnostics
    }

    /// Stage2で処理が停止した場合のステージ識別子。
    pub fn semantics_halted_stage(&self) -> Option<&'static str> {
        self.semantics_halted_stage
    }

    /// 最終診断を所有権ごと取り出す。
    pub fn into_final(self) -> Vec<Diagnostic> {
        self.final_diagnostics
    }

    /// Stage0診断を所有権ごと取り出す。
    pub fn into_preprocess(self) -> Vec<PreprocessDiagnostic> {
        self.preprocess_diagnostics
    }

    /// Stage2診断を所有権ごと取り出す。
    pub fn into_semantics(self) -> Vec<SemanticsDiagnostic> {
        self.semantics_diagnostics
    }
}
