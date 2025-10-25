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
mod when_tracker;

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

#[cfg(test)]
mod tests {
    use super::*;
    use jv_lexer::{Lexer, StringInterpolationSegment, TokenMetadata};

    fn lex(source: &str) -> Vec<Token> {
        Lexer::new(source.to_string())
            .tokenize()
            .expect("lex interpolation source")
    }

    fn interpolation_metadata<'a>(tokens: &'a [Token]) -> Option<&'a Vec<StringInterpolationSegment>> {
        tokens.iter().find_map(|token| {
            token
                .metadata
                .iter()
                .find_map(|metadata| match metadata {
                    TokenMetadata::StringInterpolation { segments } => Some(segments),
                    _ => None,
                })
        })
    }

    #[test]
    fn interpolation_metadata_survives_default_pipeline() {
        let source = r#"println("Checkpoint ${stage}: Welcome, ${name}!")"#;
        let tokens = lex(source);
        assert!(
            interpolation_metadata(&tokens).is_some(),
            "lexer should tag interpolation tokens with metadata"
        );

        let (tokens, diagnostics, halted_stage) = run(tokens).into_parts();
        assert!(
            diagnostics.is_empty(),
            "preprocess diagnostics should be empty: {:?}",
            diagnostics
        );
        assert!(
            halted_stage.is_none(),
            "preprocess pipeline should not halt, got {:?}",
            halted_stage
        );

        let segments = interpolation_metadata(&tokens)
            .expect("string interpolation metadata should survive pipeline");
        let expression_count = segments
            .iter()
            .filter(|segment| matches!(segment, StringInterpolationSegment::Expression(_)))
            .count();
        assert_eq!(
            expression_count, 2,
            "expected two interpolation expressions, got {:?}",
            segments
        );

        // Spot-check that the segments retain both identifiers and literal text portions.
        assert!(
            segments.iter().any(|segment| {
                matches!(
                    segment,
                    StringInterpolationSegment::Expression(expr) if expr == "stage"
                )
            }),
            "stage expression should be preserved in interpolation segments: {:?}",
            segments
        );
        assert!(
            segments.iter().any(|segment| {
                matches!(
                    segment,
                    StringInterpolationSegment::Expression(expr) if expr == "name"
                )
            }),
            "name expression should be preserved in interpolation segments: {:?}",
            segments
        );
        assert!(
            segments.iter().any(|segment| matches!(segment, StringInterpolationSegment::Literal(literal) if literal.contains("Checkpoint"))),
            "literal segments should be preserved alongside expressions: {:?}",
            segments
        );
    }
}
