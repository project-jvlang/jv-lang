use jv_ast::Span;
use jv_lexer::Token;

/// Preprocess ステージが発行する診断情報（未整形）。
#[derive(Debug, Clone, PartialEq)]
pub struct PreprocessDiagnostic {
    stage: &'static str,
    message: String,
    span: Option<Span>,
}

impl PreprocessDiagnostic {
    /// 診断インスタンスを生成するヘルパ。
    pub fn new(stage: &'static str, message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            stage,
            message: message.into(),
            span,
        }
    }

    /// 診断を発行したステージ名を返す。
    pub fn stage(&self) -> &'static str {
        self.stage
    }

    /// メッセージ本文を返す。
    pub fn message(&self) -> &str {
        &self.message
    }

    /// 診断に紐づくスパンを返す。
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
}

/// ステージ実行後にパイプラインへ返す状態。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StageStatus {
    /// 後続ステージの実行を継続する。
    Continue,
    /// 後続ステージの実行を停止する。
    Halt,
}

/// Stage 0 前処理ステージ共通の実行コンテキスト。
pub struct StageContext<'a> {
    tokens: &'a mut Vec<Token>,
    diagnostics: &'a mut Vec<PreprocessDiagnostic>,
}

impl<'a> StageContext<'a> {
    pub fn new(tokens: &'a mut Vec<Token>, diagnostics: &'a mut Vec<PreprocessDiagnostic>) -> Self {
        Self {
            tokens,
            diagnostics,
        }
    }

    /// トークン列への参照を取得する。
    pub fn tokens(&self) -> &Vec<Token> {
        self.tokens
    }

    /// トークン列への可変参照を取得する。
    pub fn tokens_mut(&mut self) -> &mut Vec<Token> {
        self.tokens
    }

    /// 診断リストへの参照を取得する。
    pub fn diagnostics(&self) -> &Vec<PreprocessDiagnostic> {
        self.diagnostics
    }

    /// 診断リストへの可変参照を取得する。
    pub fn diagnostics_mut(&mut self) -> &mut Vec<PreprocessDiagnostic> {
        self.diagnostics
    }

    /// 診断を追加するショートカット。
    pub fn push_diagnostic(&mut self, diagnostic: PreprocessDiagnostic) {
        self.diagnostics.push(diagnostic);
    }
}

/// 前処理ステージの共通インターフェース。
pub trait PreprocessStage {
    /// ステージ識別名（ログ・診断用途）。
    fn name(&self) -> &'static str;

    /// ステージ本体の実行処理。
    fn run(&self, context: &mut StageContext<'_>) -> StageStatus;
}
