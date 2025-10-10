use crate::{
    pipeline::{
        context::LexerContext,
        pipeline::EmitterStage,
        types::{ClassifiedToken, EmissionPlan},
    },
    CommentCarryOverMetadata, LexError, Token, TokenDiagnostic, TokenMetadata, TokenTrivia,
    TokenType,
};

use crate::{Lexer, StringInterpolationSegment};

#[derive(Default)]
pub struct Emitter;

impl Emitter {
    pub fn new() -> Self {
        Self
    }

    fn build_token(
        token_type: TokenType,
        lexeme: String,
        trivia: TokenTrivia,
        line: usize,
        column: usize,
        diagnostic: Option<TokenDiagnostic>,
        metadata: Vec<TokenMetadata>,
    ) -> Token {
        Token {
            token_type,
            lexeme,
            line,
            column,
            leading_trivia: trivia,
            diagnostic,
            metadata,
        }
    }

    fn merge_comment_carry(trivia: &mut TokenTrivia, mut carry: CommentCarryOverMetadata) {
        if carry.passthrough.is_empty()
            && carry.jv_only.is_empty()
            && carry.json.is_empty()
            && carry.doc_comment.is_none()
        {
            return;
        }
        trivia.comments = true;
        trivia.passthrough_comments.append(&mut carry.passthrough);
        trivia.jv_comments.append(&mut carry.jv_only);
        trivia.json_comments.append(&mut carry.json);
        if let Some(doc) = carry.doc_comment {
            trivia.doc_comment = Some(doc);
        }
    }

    fn extract_comment_carry(
        metadata: &mut Vec<TokenMetadata>,
    ) -> Option<CommentCarryOverMetadata> {
        let mut merged: Option<CommentCarryOverMetadata> = None;
        metadata.retain(|meta| {
            if let TokenMetadata::CommentCarryOver(info) = meta {
                let entry = merged.get_or_insert_with(CommentCarryOverMetadata::default);
                entry.passthrough.extend(info.passthrough.clone());
                entry.jv_only.extend(info.jv_only.clone());
                entry.json.extend(info.json.clone());
                if info.doc_comment.is_some() {
                    entry.doc_comment = info.doc_comment.clone();
                }
                false
            } else {
                true
            }
        });
        merged
    }

    fn emit_string_interpolation(
        &mut self,
        segments: Vec<StringInterpolationSegment>,
        metadata: Vec<TokenMetadata>,
        diagnostics: Vec<TokenDiagnostic>,
        mut leading_trivia: TokenTrivia,
        line: usize,
        column: usize,
    ) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        let mut literal_buffer: Option<String> = None;
        let mut first_metadata = metadata;
        let mut first_diagnostic = diagnostics.first().cloned();
        let mut first_token_emitted = false;

        for segment in segments {
            match segment {
                StringInterpolationSegment::Literal(literal) => {
                    literal_buffer
                        .get_or_insert_with(String::new)
                        .push_str(&literal);
                }
                StringInterpolationSegment::Expression(expression) => {
                    let literal = literal_buffer.take().unwrap_or_default();
                    let token_type = if !first_token_emitted {
                        first_token_emitted = true;
                        TokenType::StringStart
                    } else {
                        TokenType::StringMid
                    };

                    let metadata_vec = if matches!(token_type, TokenType::StringStart) {
                        std::mem::take(&mut first_metadata)
                    } else {
                        Vec::new()
                    };

                    let diagnostic = if matches!(token_type, TokenType::StringStart) {
                        first_diagnostic.take()
                    } else {
                        None
                    };

                    let trivia = if matches!(token_type, TokenType::StringStart) {
                        std::mem::take(&mut leading_trivia)
                    } else {
                        TokenTrivia::default()
                    };

                    tokens.push(Self::build_token(
                        token_type,
                        literal,
                        trivia,
                        line,
                        column,
                        diagnostic,
                        metadata_vec,
                    ));

                    if !expression.is_empty() {
                        let mut nested_lexer = Lexer::new(expression);
                        let nested_tokens = nested_lexer.tokenize()?;
                        tokens.extend(
                            nested_tokens
                                .into_iter()
                                .filter(|token| {
                                    !matches!(
                                        token.token_type,
                                        TokenType::Eof | TokenType::LayoutComma
                                    )
                                }),
                        );
                    }
                }
            }
        }

        let trailing_literal = literal_buffer.unwrap_or_default();

        if !first_token_emitted {
            let diagnostic = first_diagnostic.take();
            let metadata_vec = std::mem::take(&mut first_metadata);
            tokens.push(Self::build_token(
                TokenType::String(trailing_literal.clone()),
                trailing_literal,
                leading_trivia,
                line,
                column,
                diagnostic,
                metadata_vec,
            ));
            return Ok(tokens);
        }

        tokens.push(Self::build_token(
            TokenType::StringEnd,
            trailing_literal,
            TokenTrivia::default(),
            line,
            column,
            None,
            Vec::new(),
        ));

        Ok(tokens)
    }
}

impl EmitterStage for Emitter {
    fn emit<'source>(
        &mut self,
        token: ClassifiedToken<'source>,
        _ctx: &mut LexerContext<'source>,
    ) -> Result<Vec<Token>, LexError> {
        let ClassifiedToken {
            normalized,
            token_type,
            diagnostics,
            mut metadata,
            emission_plan,
        } = token;

        let mut trivia = normalized.raw.trivia.clone().unwrap_or_default();
        if let Some(carry) = Self::extract_comment_carry(&mut metadata) {
            if !trivia.comments {
                Self::merge_comment_carry(&mut trivia, carry);
            }
        }

        let span_start = normalized.raw.span.start;

        match emission_plan {
            EmissionPlan::Direct => Ok(vec![Self::build_token(
                token_type,
                normalized.normalized_text,
                trivia,
                span_start.line,
                span_start.column,
                diagnostics.into_iter().next(),
                metadata,
            )]),
            EmissionPlan::StringInterpolation { segments } => self.emit_string_interpolation(
                segments,
                metadata,
                diagnostics,
                trivia,
                span_start.line,
                span_start.column,
            ),
        }
    }
}
