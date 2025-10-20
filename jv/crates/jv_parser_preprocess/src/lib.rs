// jv_parser_preprocess - Preprocessing pipeline for jv language
// Extracted from jv_parser for memory-efficient compilation

mod call;
mod comments;
mod json;
mod layout;
#[cfg(test)]
mod legacy;
mod pipeline;
mod shared;
mod stage;

pub use pipeline::{PreprocessResult, ProcessingPipeline};
pub use stage::{PreprocessDiagnostic, PreprocessStage, StageContext, StageStatus};

use std::cell::RefCell;
use std::rc::Rc;

use jv_lexer::Token;

/// 既存の Stage 0 パイプラインを実行してトークンを整形する。
#[allow(dead_code)]
pub fn preprocess_tokens(tokens: Vec<Token>) -> Vec<Token> {
    let result = run(tokens);
    let (tokens, _, _) = result.into_parts();
    tokens
}

/// デフォルト構成の Stage 0 パイプラインを実行する。
pub fn run(tokens: Vec<Token>) -> PreprocessResult {
    default_pipeline().run(tokens)
}

fn default_pipeline() -> ProcessingPipeline {
    let shared_state = Rc::new(RefCell::new(shared::StageSharedState::default()));
    ProcessingPipeline::builder()
        .with_stage(json::JsonStage::default())
        .with_stage(call::CallStage::new(Rc::clone(&shared_state)))
        .with_stage(comments::CommentsStage::new(Rc::clone(&shared_state)))
        .with_stage(layout::LayoutStage::new(shared_state))
        .build()
}
