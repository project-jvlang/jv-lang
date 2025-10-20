mod legacy;
mod pipeline;
mod stage;

pub use legacy::LegacyPreprocessStage;
pub use pipeline::{PreprocessResult, ProcessingPipeline};
pub use stage::{PreprocessDiagnostic, PreprocessStage, StageContext, StageStatus};

use jv_lexer::Token;

/// 既存の Stage 0 パイプラインを実行してトークンを整形する。
pub fn preprocess_tokens(tokens: Vec<Token>) -> Vec<Token> {
    let result = run(tokens);
    let _ = result.tokens();
    let _ = result.diagnostics();
    let _ = result.halted_stage();
    let _ = result.is_success();
    let (tokens, _, _) = result.into_parts();
    tokens
}

/// デフォルト構成の Stage 0 パイプラインを実行する。
pub fn run(tokens: Vec<Token>) -> PreprocessResult {
    default_pipeline().run(tokens)
}

fn default_pipeline() -> ProcessingPipeline {
    ProcessingPipeline::builder()
        .with_stage(LegacyPreprocessStage::default())
        .build()
}

#[allow(dead_code)]
fn _preprocess_api_sanity_check(
    stage: &dyn PreprocessStage,
    context: &mut StageContext<'_>,
) -> StageStatus {
    let status = stage.run(context);
    if matches!(status, StageStatus::Halt) {
        let diagnostic = PreprocessDiagnostic::new(stage.name(), String::new(), None);
        context.push_diagnostic(diagnostic.clone());

        // accessor usage to keep API validated during compilation
        let _ = diagnostic.stage();
        let _ = diagnostic.message();
        let _ = diagnostic.span();
    }

    {
        let _ = context.tokens();
    }
    {
        let _ = context.diagnostics();
    }
    let _ = context.tokens_mut();
    let _ = context.diagnostics_mut();
    let _ = StageStatus::Halt;

    status
}
