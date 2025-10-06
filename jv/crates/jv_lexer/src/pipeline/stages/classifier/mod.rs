use crate::{
    pipeline::{
        context::LexerContext,
        pipeline::ClassifierStage,
        types::{ClassifiedToken, EmissionPlan, NormalizedToken, RawTokenKind},
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
    emission_plan: EmissionPlan,
}

impl<'source> ClassificationState<'source> {
    fn new(token: &'source NormalizedToken<'source>) -> Self {
        Self {
            token_type: None,
            metadata: token.metadata.provisional_metadata.clone(),
            diagnostics: token.metadata.provisional_diagnostics.clone(),
            token,
            emission_plan: EmissionPlan::Direct,
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

    pub fn metadata(&self) -> &[TokenMetadata] {
        &self.metadata
    }

    pub fn metadata_mut(&mut self) -> &mut Vec<TokenMetadata> {
        &mut self.metadata
    }

    pub fn metadata_contains<F>(&self, predicate: F) -> bool
    where
        F: Fn(&TokenMetadata) -> bool,
    {
        self.metadata.iter().any(predicate)
    }

    pub fn diagnostics(&mut self) -> &mut Vec<TokenDiagnostic> {
        &mut self.diagnostics
    }

    pub fn set_emission_plan(&mut self, emission_plan: EmissionPlan) {
        self.emission_plan = emission_plan;
    }

    fn into_parts(
        self,
    ) -> (
        Option<TokenType>,
        Vec<TokenMetadata>,
        Vec<TokenDiagnostic>,
        EmissionPlan,
    ) {
        (
            self.token_type,
            self.metadata,
            self.diagnostics,
            self.emission_plan,
        )
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

        let (token_type, mut metadata, diagnostics, emission_plan) = state.into_parts();
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

        let emission_plan = match resolved_type {
            TokenType::Eof => {
                metadata.clear();
                EmissionPlan::Direct
            }
            _ => emission_plan,
        };

        Ok(ClassifiedToken::with_plan(
            token,
            resolved_type,
            diagnostics,
            metadata,
            emission_plan,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipeline::types::{EmissionPlan, PreMetadata, RawToken, Span};
    use crate::{
        JsonConfidence, LayoutCommaMetadata, LayoutSequenceKind, StringDelimiterKind,
        StringInterpolationSegment, StringLiteralMetadata, TokenMetadata,
    };

    fn make_raw_token(kind: RawTokenKind, text: &str) -> RawToken<'static> {
        let leaked: &'static str = Box::leak(text.to_string().into_boxed_str());
        RawToken {
            kind,
            text: leaked,
            span: Span::empty(Default::default()),
            trivia: None,
            carry_over: None,
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

    #[test]
    fn classifier_preserves_pre_metadata_entries() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::Identifier, "jsonCandidate");
        let mut pre = PreMetadata::default();
        pre.provisional_metadata
            .push(TokenMetadata::PotentialJsonStart {
                confidence: JsonConfidence::High,
            });
        let token = NormalizedToken::new(raw, "jsonCandidate".to_string(), pre);
        let mut ctx = build_context("jsonCandidate");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(classified.metadata.iter().any(|meta| matches!(
            meta,
            TokenMetadata::PotentialJsonStart {
                confidence: JsonConfidence::High
            }
        )));
    }

    #[test]
    fn classifier_marks_string_literals_from_metadata() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::Symbol, "\"hello\"");
        let mut pre = PreMetadata::default();
        pre.provisional_metadata
            .push(TokenMetadata::StringLiteral(StringLiteralMetadata {
                delimiter: StringDelimiterKind::DoubleQuote,
                allows_interpolation: true,
                normalize_indentation: false,
            }));
        let token = NormalizedToken::new(raw, "hello".to_string(), pre);
        let mut ctx = build_context("\"hello\"");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(classified.token_type, TokenType::String(ref value) if value == "hello"));
        assert!(matches!(classified.emission_plan, EmissionPlan::Direct));
    }

    #[test]
    fn classifier_marks_string_interpolation_from_metadata() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::Symbol, "\"Hello ${name}!\"");
        let mut pre = PreMetadata::default();
        pre.provisional_metadata
            .push(TokenMetadata::StringLiteral(StringLiteralMetadata {
                delimiter: StringDelimiterKind::DoubleQuote,
                allows_interpolation: true,
                normalize_indentation: false,
            }));
        pre.provisional_metadata
            .push(TokenMetadata::StringInterpolation {
                segments: vec![
                    StringInterpolationSegment::Literal("Hello ".to_string()),
                    StringInterpolationSegment::Expression("name".to_string()),
                    StringInterpolationSegment::Literal("!".to_string()),
                ],
            });
        let token = NormalizedToken::new(raw, "Hello ${name}!".to_string(), pre);
        let mut ctx = build_context("\"Hello ${name}!\"");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(
            classified.token_type,
            TokenType::StringInterpolation(ref value) if value == "Hello ${name}!"
        ));
        assert!(classified
            .metadata
            .iter()
            .any(|meta| matches!(meta, TokenMetadata::StringInterpolation { .. })));
        match &classified.emission_plan {
            EmissionPlan::StringInterpolation { segments } => {
                assert_eq!(segments.len(), 3);
                assert!(
                    matches!(segments[0], StringInterpolationSegment::Literal(ref s) if s == "Hello ")
                );
                assert!(
                    matches!(segments[1], StringInterpolationSegment::Expression(ref s) if s == "name")
                );
                assert!(
                    matches!(segments[2], StringInterpolationSegment::Literal(ref s) if s == "!")
                );
            }
            other => panic!("unexpected emission plan: {:?}", other),
        }
    }

    #[test]
    fn classifier_emits_layout_comma_when_metadata_present() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(
            RawTokenKind::Whitespace,
            "
",
        );
        let mut pre = PreMetadata::default();
        pre.provisional_metadata
            .push(TokenMetadata::LayoutComma(LayoutCommaMetadata {
                sequence: LayoutSequenceKind::Array,
                explicit_separator: None,
            }));
        let token = NormalizedToken::new(
            raw,
            "
"
            .to_string(),
            pre,
        );
        let mut ctx = build_context(
            "
",
        );

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(classified.token_type, TokenType::LayoutComma));
        assert!(classified
            .metadata
            .iter()
            .any(|meta| matches!(meta, TokenMetadata::LayoutComma(_))));
    }

    #[test]
    fn classifier_classifies_line_comment_candidate() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::CommentCandidate, "// trailing comment");
        let token =
            NormalizedToken::new(raw, " trailing comment".to_string(), PreMetadata::default());
        let mut ctx = build_context("// trailing comment");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(
            classified.token_type,
            TokenType::LineComment(ref value) if value == " trailing comment"
        ));
    }

    #[test]
    fn classifier_classifies_block_comment_candidate() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::CommentCandidate, "/* block */");
        let token = NormalizedToken::new(raw, " block ".to_string(), PreMetadata::default());
        let mut ctx = build_context("/* block */");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(
            classified.token_type,
            TokenType::BlockComment(ref value) if value == " block "
        ));
    }

    #[test]
    fn classifier_classifies_javadoc_comment_candidate() {
        let mut classifier = Classifier::new();
        let raw = make_raw_token(RawTokenKind::CommentCandidate, "/** docs */");
        let token = NormalizedToken::new(raw, " docs ".to_string(), PreMetadata::default());
        let mut ctx = build_context("/** docs */");

        let classified = classifier.classify(token, &mut ctx).unwrap();
        assert!(matches!(
            classified.token_type,
            TokenType::JavaDocComment(ref value) if value == " docs "
        ));
    }
}
