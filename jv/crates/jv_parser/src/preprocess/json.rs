use jv_lexer::{JsonConfidence, Token, TokenMetadata, TokenType};

use super::stage::{PreprocessStage, StageContext, StageStatus};

pub struct JsonStage;

impl Default for JsonStage {
    fn default() -> Self {
        Self
    }
}

impl JsonStage {
    fn detect_contexts(&self, tokens: &[Token]) -> Vec<Option<JsonConfidence>> {
        tokens
            .iter()
            .enumerate()
            .map(|(index, token)| match token.token_type {
                TokenType::LeftBrace => Self::detect_object(tokens, index),
                TokenType::LeftBracket => Self::detect_array(tokens, index),
                _ => None,
            })
            .collect()
    }

    fn detect_object(tokens: &[Token], index: usize) -> Option<JsonConfidence> {
        if matches!(
            Self::confidence_from_metadata(&tokens[index]),
            Some(JsonConfidence::High)
        ) {
            return Some(JsonConfidence::High);
        }

        let mut cursor = index + 1;

        while let Some(next_index) = Self::skip_comments(tokens, cursor) {
            match &tokens[next_index].token_type {
                TokenType::RightBrace => return Some(JsonConfidence::High),
                TokenType::String(_) | TokenType::Identifier(_) => {
                    if Self::has_following_colon(tokens, next_index + 1) {
                        return Some(JsonConfidence::High);
                    }
                    if matches!(tokens[next_index].token_type, TokenType::Identifier(_)) {
                        return None;
                    }
                }
                TokenType::Val
                | TokenType::Var
                | TokenType::Fun
                | TokenType::Class
                | TokenType::Data
                | TokenType::When
                | TokenType::If
                | TokenType::For
                | TokenType::While
                | TokenType::Do
                | TokenType::Return
                | TokenType::Break
                | TokenType::Continue => return None,
                TokenType::Comma => {
                    cursor = next_index + 1;
                    continue;
                }
                _ => return None,
            }

            break;
        }

        None
    }

    fn detect_array(tokens: &[Token], index: usize) -> Option<JsonConfidence> {
        if matches!(
            Self::confidence_from_metadata(&tokens[index]),
            Some(JsonConfidence::High)
        ) {
            return Some(JsonConfidence::High);
        }

        let mut cursor = index + 1;

        while let Some(next_index) = Self::skip_comments(tokens, cursor) {
            match &tokens[next_index].token_type {
                TokenType::RightBracket => return Some(JsonConfidence::High),
                TokenType::LeftBrace
                | TokenType::LeftBracket
                | TokenType::String(_)
                | TokenType::Number(_)
                | TokenType::Boolean(_)
                | TokenType::Null => {
                    cursor = next_index + 1;
                    continue;
                }
                _ => return None,
            }
        }

        None
    }

    fn skip_comments(tokens: &[Token], mut index: usize) -> Option<usize> {
        while index < tokens.len() {
            match tokens[index].token_type {
                TokenType::LineComment(_)
                | TokenType::BlockComment(_)
                | TokenType::JavaDocComment(_) => {
                    index += 1;
                }
                _ => return Some(index),
            }
        }
        None
    }

    fn has_following_colon(tokens: &[Token], start: usize) -> bool {
        if let Some(index) = Self::skip_comments(tokens, start) {
            matches!(tokens[index].token_type, TokenType::Colon)
        } else {
            false
        }
    }

    fn confidence_from_metadata(token: &Token) -> Option<JsonConfidence> {
        token.metadata.iter().find_map(|metadata| match metadata {
            TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
            _ => None,
        })
    }

    fn update_metadata(token: &mut Token, confidence: Option<JsonConfidence>) {
        token
            .metadata
            .retain(|metadata| !matches!(metadata, TokenMetadata::PotentialJsonStart { .. }));

        if let Some(confidence) = confidence {
            token
                .metadata
                .push(TokenMetadata::PotentialJsonStart { confidence });
        }
    }
}

impl PreprocessStage for JsonStage {
    fn name(&self) -> &'static str {
        "stage0-json"
    }

    fn run(&self, context: &mut StageContext<'_>) -> StageStatus {
        let tokens = context.tokens_mut();
        let contexts = self.detect_contexts(tokens);

        for (token, confidence) in tokens.iter_mut().zip(contexts.into_iter()) {
            Self::update_metadata(token, confidence);
        }

        StageStatus::Continue
    }
}

#[cfg(test)]
mod tests {
    use crate::preprocess;
    use jv_lexer::{JsonConfidence, TokenMetadata};

    fn run_stage(source: &str) -> Vec<jv_lexer::Token> {
        let mut lexer = jv_lexer::Lexer::new(source.to_string());
        let tokens = lexer.tokenize().expect("lexing should succeed");
        let (tokens, _, _) = preprocess::run(tokens).into_parts();
        tokens
    }

    fn json_confidence(token: &jv_lexer::Token) -> Option<jv_lexer::JsonConfidence> {
        token.metadata.iter().find_map(|metadata| match metadata {
            TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
            _ => None,
        })
    }

    #[test]
    fn json_object_metadata_preserved() {
        let tokens = run_stage("{\"key\": 1}");
        let first = tokens
            .first()
            .expect("token stream should contain left brace");
        assert!(matches!(
            json_confidence(first),
            Some(jv_lexer::JsonConfidence::High)
        ));
    }

    #[test]
    fn block_expression_metadata_removed() {
        let tokens = run_stage("{ val x = 1 }");
        let first = tokens
            .first()
            .expect("token stream should contain left brace");
        assert!(json_confidence(first).is_none());
    }

    #[test]
    fn json_array_of_strings_detected() {
        let tokens = run_stage("[\"a\", \"b\"]");
        let first = tokens
            .first()
            .expect("token stream should contain left bracket");
        assert!(matches!(
            json_confidence(first),
            Some(jv_lexer::JsonConfidence::High)
        ));
    }

    #[test]
    fn jv_array_of_numbers_not_flagged_as_json() {
        let tokens = run_stage("[1, 2, 3]");
        let first = tokens
            .first()
            .expect("token stream should contain left bracket");
        assert!(json_confidence(first).is_none());
    }
}
