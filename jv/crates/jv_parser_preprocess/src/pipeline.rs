use super::stage::{PreprocessDiagnostic, PreprocessStage, StageContext, StageStatus};
use jv_lexer::Token;

/// Stage 0 の前処理パイプライン。
pub struct ProcessingPipeline {
    stages: Vec<Box<dyn PreprocessStage>>,
}

impl ProcessingPipeline {
    /// パイプラインビルダーを生成する。
    pub fn builder() -> PipelineBuilder {
        PipelineBuilder::new()
    }

    /// パイプラインを実行し、結果を返す。
    pub fn run(&self, tokens: Vec<Token>) -> PreprocessResult {
        let mut tokens = tokens;
        let mut diagnostics = Vec::new();
        let mut halted_stage = None;

        for stage in &self.stages {
            let status = {
                let mut context = StageContext::new(&mut tokens, &mut diagnostics);
                stage.run(&mut context)
            };

            if matches!(status, StageStatus::Halt) {
                halted_stage = Some(stage.name());
                break;
            }
        }

        PreprocessResult {
            tokens,
            diagnostics,
            halted_stage,
        }
    }
}

/// パイプライン生成用のビルダー。
#[derive(Default)]
pub struct PipelineBuilder {
    stages: Vec<Box<dyn PreprocessStage>>,
}

impl PipelineBuilder {
    pub fn new() -> Self {
        Self { stages: Vec::new() }
    }

    /// ステージを追加する（所有権を移動）。
    pub fn with_stage(mut self, stage: impl PreprocessStage + 'static) -> Self {
        self.stages.push(Box::new(stage));
        self
    }

    /// ステージを後段に追加する。
    #[allow(dead_code)]
    pub fn push_stage(&mut self, stage: impl PreprocessStage + 'static) -> &mut Self {
        self.stages.push(Box::new(stage));
        self
    }

    /// パイプラインを構築する。
    pub fn build(self) -> ProcessingPipeline {
        ProcessingPipeline {
            stages: self.stages,
        }
    }
}

/// 前処理パイプラインの実行結果。
pub struct PreprocessResult {
    tokens: Vec<Token>,
    diagnostics: Vec<PreprocessDiagnostic>,
    halted_stage: Option<&'static str>,
}

#[allow(dead_code)]
impl PreprocessResult {
    /// 前処理後のトークン列を借用で取得する。
    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    /// 診断一覧を取得する。
    pub fn diagnostics(&self) -> &Vec<PreprocessDiagnostic> {
        &self.diagnostics
    }

    /// パイプラインが途中停止した場合は停止ステージ名を返す。
    pub fn halted_stage(&self) -> Option<&'static str> {
        self.halted_stage
    }

    /// パイプラインが正常完走したかを返す。
    pub fn is_success(&self) -> bool {
        self.halted_stage.is_none()
    }

    /// トークン列を所有権ごと取り出し、残りも返す。
    pub fn into_parts(self) -> (Vec<Token>, Vec<PreprocessDiagnostic>, Option<&'static str>) {
        (self.tokens, self.diagnostics, self.halted_stage)
    }

    pub fn map_tokens<F>(self, f: F) -> Self
    where
        F: FnOnce(Vec<Token>) -> Vec<Token>,
    {
        let (tokens, diagnostics, halted_stage) = self.into_parts();
        Self {
            tokens: f(tokens),
            diagnostics,
            halted_stage,
        }
    }
}
