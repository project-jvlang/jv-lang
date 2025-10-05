use crate::{
    pipeline::{
        context::LexerContext,
        pipeline::ClassifierStage,
        types::{ClassifiedToken, NormalizedToken, RawTokenKind},
    },
    LexError, TokenDiagnostic, TokenMetadata, TokenType,
};

mod comment;
mod json_detection;
mod keyword;
mod layout_analysis;
mod number_literal;
mod operator;
mod string_interpolation;

use comment::CommentModule;
use json_detection::JsonDetectionModule;
use keyword::KeywordModule;
use layout_analysis::LayoutAnalysisModule;
use number_literal::NumberLiteralModule;
use operator::OperatorModule;
use string_interpolation::StringInterpolationModule;

/// 分類処理中に蓄積する中間状態。
pub struct ClassificationState<'source> {
    token_type: Option<TokenType>,
    metadata: Vec<TokenMetadata>,
    diagnostics: Vec<TokenDiagnostic>,
    token: &'source NormalizedToken<'source>,
}

impl<'source> ClassificationState<'source> {
    fn new(token: &'source NormalizedToken<'source>) -> Self {
        Self {
            token_type: None,
            metadata: token.metadata.provisional_metadata.clone(),
            diagnostics: token.metadata.provisional_diagnostics.clone(),
            token,
        }
    }

    pub fn token(&self) -> &NormalizedToken<'source> {
        self.token
    }

    pub fn token_type(&self) -> Option<&TokenType> {
        self.token_type.as_ref()
    }

    pub fn set_token_type(&mut self, token_type: TokenType) {
        if self.token_type.is_none() {
            self.token_type = Some(token_type);
        }
    }

    pub fn overwrite_token_type(&mut self, token_type: TokenType) {
        self.token_type = Some(token_type);
    }

    pub fn metadata(&mut self) -> &mut Vec<TokenMetadata> {
        &mut self.metadata
    }

    pub fn diagnostics(&mut self) -> &mut Vec<TokenDiagnostic> {
        &mut self.diagnostics
    }

    fn into_parts(self) -> (Option<TokenType>, Vec<TokenMetadata>, Vec<TokenDiagnostic>) {
        (self.token_type, self.metadata, self.diagnostics)
    }
}

/// Classifier モジュールが実装すべき共通インターフェース。
trait ClassificationModule: Send {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError>;
}

pub struct Classifier {
    modules: Vec<Box<dyn ClassificationModule>>,
}

impl Classifier {
    pub fn new() -> Self {
        Self {
            modules: vec![
                Box::new(KeywordModule::new()),
                Box::new(StringInterpolationModule::new()),
                Box::new(JsonDetectionModule::new()),
                Box::new(LayoutAnalysisModule::new()),
                Box::new(CommentModule::new()),
                Box::new(NumberLiteralModule::new()),
                Box::new(OperatorModule::new()),
            ],
        }
    }
}

impl Default for Classifier {
    fn default() -> Self {
        Self::new()
    }
}

impl ClassifierStage for Classifier {
    fn classify<'source>(
        &mut self,
        token: NormalizedToken<'source>,
        ctx: &mut LexerContext<'source>,
    ) -> Result<ClassifiedToken<'source>, LexError> {
        let mut state = ClassificationState::new(&token);

        for module in &mut self.modules {
            module.apply(&token, ctx, &mut state)?;
        }

        let (token_type, mut metadata, diagnostics) = state.into_parts();
        let resolved_type = match token_type {
            Some(ty) => ty,
            None => match token.raw.kind {
                RawTokenKind::Identifier => TokenType::Identifier(token.normalized_text.clone()),
                RawTokenKind::NumberCandidate => TokenType::Number(token.normalized_text.clone()),
                RawTokenKind::Symbol => TokenType::Invalid(token.normalized_text.clone()),
                RawTokenKind::Whitespace => TokenType::Whitespace(token.normalized_text.clone()),
                RawTokenKind::CommentCandidate => {
                    TokenType::LineComment(token.normalized_text.clone())
                }
                RawTokenKind::Eof => TokenType::Eof,
            },
        };

        if matches!(resolved_type, TokenType::Eof) {
            metadata.clear();
        }

        Ok(ClassifiedToken::new(
            token,
            resolved_type,
            diagnostics,
            metadata,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipeline::types::{PreMetadata, RawToken, Span};

    fn make_raw_token(kind: RawTokenKind, text: &str) -> RawToken<'static> {
        let leaked: &'static str = Box::leak(text.to_string().into_boxed_str());
        RawToken {
            kind,
            text: leaked,
            span: Span::empty(Default::default()),
            trivia: None,
        }
    }

    fn build_context(source: &str) -> LexerContext<'_> {
        LexerContext::new(source)
    }

    #[test]
    fn classifier_identifies_keywords() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::Identifier, "val");
        let token = NormalizedToken::new(raw, "val".to_string(), PreMetadata::default());
        let mut ctx = build_context("val");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(classified.token_type, TokenType::Val));
    }

    #[test]
    fn classifier_handles_numbers() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::NumberCandidate, "1_000");
        let token = NormalizedToken::new(raw, "1000".to_string(), PreMetadata::default());
        let mut ctx = build_context("1_000");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(classified.token_type, TokenType::Number(ref value) if value == "1000"));
    }

    #[test]
    fn classifier_defaults_to_identifier() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::Identifier, "custom");
        let token = NormalizedToken::new(raw.clone(), "custom".to_string(), PreMetadata::default());
        let mut ctx = build_context("custom");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(
            matches!(classified.token_type, TokenType::Identifier(ref value) if value == "custom")
        );
    }

    #[test]
    fn classifier_resolves_operators() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::Symbol, "==");
        let token = NormalizedToken::new(raw, "==".to_string(), PreMetadata::default());
        let mut ctx = build_context("==");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(classified.token_type, TokenType::Equal));
    }
}
