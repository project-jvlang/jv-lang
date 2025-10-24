use super::stage::{PreprocessStage, StageContext, StageStatus};
use jv_lexer::{
    ExplicitSeparatorLocation, JsonConfidence, LayoutCommaMetadata, LayoutSequenceKind, Token,
    TokenMetadata, TokenTrivia, TokenType,
};

/// 既存の単一ステージ実装をパイプライン互換でラップする。
pub struct LegacyPreprocessStage;

impl LegacyPreprocessStage {
    pub fn new() -> Self {
        Self
    }
}

impl Default for LegacyPreprocessStage {
    fn default() -> Self {
        Self::new()
    }
}

impl PreprocessStage for LegacyPreprocessStage {
    fn name(&self) -> &'static str {
        "legacy-preprocess"
    }

    fn run(&self, context: &mut StageContext<'_>) -> StageStatus {
        let tokens = std::mem::take(context.tokens_mut());
        let processed = run_legacy_preprocess(tokens);
        *context.tokens_mut() = processed;
        StageStatus::Continue
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SequenceContextKind {
    Array,
    Call,
}

struct SequenceContext {
    kind: SequenceContextKind,
    prev_was_separator: bool,
    pending_layout: bool,
    last_explicit_separator: Option<ExplicitSeparatorLocation>,
}

impl SequenceContext {
    fn new(kind: SequenceContextKind) -> Self {
        Self {
            kind,
            prev_was_separator: true,
            pending_layout: false,
            last_explicit_separator: None,
        }
    }
}

fn allows_call_suffix(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Identifier(_)
            | TokenType::Number(_)
            | TokenType::Character(_)
            | TokenType::String(_)
            | TokenType::StringInterpolation(_)
            | TokenType::Boolean(_)
            | TokenType::Null
            | TokenType::RightParen
            | TokenType::RightBracket
            | TokenType::RightBrace
            | TokenType::StringEnd
            | TokenType::Greater
    )
}

fn requires_right_operand(token_type: &TokenType) -> bool {
    matches!(
        token_type,
        TokenType::Plus
            | TokenType::Minus
            | TokenType::Multiply
            | TokenType::Divide
            | TokenType::Modulo
            | TokenType::Equal
            | TokenType::NotEqual
            | TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::And
            | TokenType::Or
            | TokenType::RangeExclusive
            | TokenType::RangeInclusive
            | TokenType::Elvis
    )
}

fn is_sequence_layout_candidate(
    prev_token: Option<&TokenType>,
    token_type: &TokenType,
    next_token: Option<&Token>,
) -> bool {
    if let Some(prev) = prev_token {
        if requires_right_operand(prev) {
            return false;
        }
    }

    match token_type {
        TokenType::Plus | TokenType::Minus => {
            if let Some(next) = next_token {
                matches!(next.token_type, TokenType::Number(_))
                    && next.leading_trivia.spaces == 0
                    && next.leading_trivia.newlines == 0
                    && !next.leading_trivia.comments
            } else {
                false
            }
        }
        _ => !matches!(
            token_type,
            TokenType::Comma
                | TokenType::LayoutComma
                | TokenType::RightBracket
                | TokenType::RightParen
                | TokenType::Assign
                | TokenType::Colon
                | TokenType::Dot
                | TokenType::Arrow
                | TokenType::FatArrow
        ),
    }
}

pub(super) fn run_legacy_preprocess(tokens: Vec<Token>) -> Vec<Token> {
    let json_contexts = detect_json_contexts(&tokens);
    let mut result = Vec::with_capacity(tokens.len());
    let mut stack: Vec<SequenceContext> = Vec::new();
    let mut call_eligible = false;
    let mut suppress_definition_call = false;
    let mut in_interpolation_expr = false;
    let mut expect_interpolation_expr = false;
    let mut prev_token_type: Option<TokenType> = None;

    let mut iter = tokens.into_iter().enumerate().peekable();

    while let Some((index, mut token)) = iter.next() {
        let next_token = iter.peek().map(|(_, token)| token);
        let json_confidence = json_contexts.get(index).copied().flatten();
        update_token_json_metadata(&mut token, json_confidence);

        if matches!(token.token_type, TokenType::JavaDocComment(_)) {
            if let Some(ctx) = stack.last_mut() {
                ctx.pending_layout = true;
            }
            continue;
        }

        if matches!(
            token.token_type,
            TokenType::LineComment(_) | TokenType::BlockComment(_)
        ) {
            if let Some(ctx) = stack.last_mut() {
                ctx.pending_layout = true;
            }
            if stack.is_empty() {
                result.push(token);
            }
            continue;
        }

        let token_type_ref = &token.token_type;
        let current_token_type = token.token_type.clone();
        if expect_interpolation_expr {
            in_interpolation_expr = true;
            expect_interpolation_expr = false;
        }
        if matches!(
            token.token_type,
            TokenType::StringStart | TokenType::StringMid | TokenType::StringEnd
        ) {
            in_interpolation_expr = false;
        }
        if matches!(
            token_type_ref,
            TokenType::Fun | TokenType::Class | TokenType::Data
        ) {
            suppress_definition_call = true;
        }

        let mut is_call_left_paren =
            matches!(token_type_ref, TokenType::LeftParen) && call_eligible;
        if is_call_left_paren && suppress_definition_call {
            is_call_left_paren = false;
            suppress_definition_call = false;
        }
        if matches!(token_type_ref, TokenType::LeftParen) {
            suppress_definition_call = false;
        }
        let mut next_call_state = allows_call_suffix(token_type_ref);

        if !in_interpolation_expr {
            if let Some(ctx) = stack.last_mut() {
                let eligible = match ctx.kind {
                    SequenceContextKind::Array => {
                        !matches!(token.token_type, TokenType::Comma | TokenType::RightBracket)
                    }
                    SequenceContextKind::Call => {
                        !matches!(token.token_type, TokenType::Comma | TokenType::RightParen)
                    }
                } && is_sequence_layout_candidate(
                    prev_token_type.as_ref(),
                    token_type_ref,
                    next_token,
                );
                if eligible {
                    let layout_needed = !ctx.prev_was_separator
                        && (ctx.pending_layout || has_layout_trivia(&token.leading_trivia));
                    if layout_needed {
                        match ctx.kind {
                            SequenceContextKind::Array => {
                                let metadata = LayoutCommaMetadata {
                                    sequence: LayoutSequenceKind::Array,
                                    explicit_separator: ctx.last_explicit_separator.take(),
                                };
                                let mut synthetic = make_layout_comma_token(&token);
                                synthetic
                                    .metadata
                                    .push(TokenMetadata::LayoutComma(metadata));
                                result.push(synthetic);
                            }
                            SequenceContextKind::Call => {
                                ctx.last_explicit_separator = None;
                            }
                        }

                        ctx.prev_was_separator = true;
                    }
                    ctx.pending_layout = false;
                } else {
                    ctx.pending_layout = false;
                }
            }
        }

        match token.token_type {
            TokenType::LeftBracket => {
                if let Some(ctx) = stack.last_mut() {
                    ctx.prev_was_separator = false;
                    ctx.last_explicit_separator = None;
                }
                stack.push(SequenceContext::new(SequenceContextKind::Array));
                next_call_state = false;
                result.push(token);
            }
            TokenType::RightBracket => {
                if matches!(
                    stack.last().map(|ctx| ctx.kind),
                    Some(SequenceContextKind::Array)
                ) {
                    stack.pop();
                }
                if let Some(ctx) = stack.last_mut() {
                    ctx.prev_was_separator = false;
                    ctx.last_explicit_separator = None;
                }
                result.push(token);
            }
            TokenType::StringStart | TokenType::StringMid => {
                if let Some(ctx) = stack.last_mut() {
                    ctx.prev_was_separator = false;
                    ctx.last_explicit_separator = None;
                }
                expect_interpolation_expr = true;
                result.push(token);
            }
            TokenType::StringEnd => {
                if let Some(ctx) = stack.last_mut() {
                    ctx.prev_was_separator = false;
                    ctx.last_explicit_separator = None;
                }
                in_interpolation_expr = false;
                expect_interpolation_expr = false;
                result.push(token);
            }
            TokenType::LeftParen => {
                if let Some(ctx) = stack.last_mut() {
                    ctx.prev_was_separator = false;
                    ctx.last_explicit_separator = None;
                }

                if is_call_left_paren {
                    stack.push(SequenceContext::new(SequenceContextKind::Call));
                }

                next_call_state = false;
                result.push(token);
            }
            TokenType::RightParen => {
                if matches!(
                    stack.last().map(|ctx| ctx.kind),
                    Some(SequenceContextKind::Call)
                ) {
                    stack.pop();
                }
                if let Some(ctx) = stack.last_mut() {
                    ctx.prev_was_separator = false;
                    ctx.last_explicit_separator = None;
                }
                result.push(token);
            }
            TokenType::Comma => {
                if let Some(ctx) = stack.last_mut() {
                    ctx.prev_was_separator = true;
                    ctx.pending_layout = false;
                    ctx.last_explicit_separator = Some(ExplicitSeparatorLocation {
                        line: token.line,
                        column: token.column,
                    });
                }
                next_call_state = false;
                result.push(token);
            }
            _ => {
                if let Some(ctx) = stack.last_mut() {
                    ctx.prev_was_separator = false;
                    ctx.last_explicit_separator = None;
                }
                result.push(token);
            }
        }

        call_eligible = next_call_state;
        prev_token_type = Some(current_token_type);
    }

    result
}

fn detect_json_contexts(tokens: &[Token]) -> Vec<Option<JsonConfidence>> {
    tokens
        .iter()
        .enumerate()
        .map(|(index, token)| match token.token_type {
            TokenType::LeftBrace => detect_json_object(tokens, index),
            TokenType::LeftBracket => detect_json_array(tokens, index),
            _ => None,
        })
        .collect()
}

fn detect_json_object(tokens: &[Token], index: usize) -> Option<JsonConfidence> {
    if matches!(
        json_confidence_from_metadata(&tokens[index]),
        Some(JsonConfidence::High)
    ) {
        return Some(JsonConfidence::High);
    }

    let mut cursor = index + 1;

    while let Some(next_index) = skip_comment_tokens(tokens, cursor) {
        match &tokens[next_index].token_type {
            TokenType::RightBrace => return Some(JsonConfidence::High),
            TokenType::String(_) | TokenType::Identifier(_) => {
                if has_following_colon(tokens, next_index + 1) {
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

fn skip_comment_tokens(tokens: &[Token], mut index: usize) -> Option<usize> {
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

fn detect_json_array(tokens: &[Token], index: usize) -> Option<JsonConfidence> {
    if matches!(
        json_confidence_from_metadata(&tokens[index]),
        Some(JsonConfidence::High)
    ) {
        return Some(JsonConfidence::High);
    }

    let mut cursor = index + 1;

    while let Some(next_index) = skip_comment_tokens(tokens, cursor) {
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

fn has_following_colon(tokens: &[Token], start: usize) -> bool {
    if let Some(index) = skip_comment_tokens(tokens, start) {
        matches!(tokens[index].token_type, TokenType::Colon)
    } else {
        false
    }
}

fn json_confidence_from_metadata(token: &Token) -> Option<JsonConfidence> {
    token.metadata.iter().find_map(|metadata| match metadata {
        TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
        _ => None,
    })
}

fn update_token_json_metadata(token: &mut Token, confidence: Option<JsonConfidence>) {
    token
        .metadata
        .retain(|metadata| !matches!(metadata, TokenMetadata::PotentialJsonStart { .. }));

    if let Some(confidence) = confidence {
        token
            .metadata
            .push(TokenMetadata::PotentialJsonStart { confidence });
    }
}

fn has_layout_trivia(trivia: &TokenTrivia) -> bool {
    trivia.spaces > 0 || trivia.newlines > 0 || trivia.comments
}

fn make_layout_comma_token(reference: &Token) -> Token {
    Token {
        token_type: TokenType::LayoutComma,
        lexeme: ",".to_string(),
        line: reference.line,
        column: reference.column,
        leading_trivia: TokenTrivia::default(),
        diagnostic: None,
        metadata: Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::run_legacy_preprocess;
    use crate::run;

    fn tokenize(source: &str) -> Vec<jv_lexer::Token> {
        let mut lexer = jv_lexer::Lexer::new(source.to_string());
        lexer.tokenize().expect("lexing should succeed")
    }

    fn json_confidence(token: &jv_lexer::Token) -> Option<jv_lexer::JsonConfidence> {
        token.metadata.iter().find_map(|metadata| match metadata {
            jv_lexer::TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
            _ => None,
        })
    }

    #[test]
    fn json_object_metadata_preserved() {
        let source = "{\"key\": 1}";
        let legacy = run_legacy_preprocess(tokenize(source));
        let (modern, _, _) = run(tokenize(source)).into_parts();
        assert_eq!(modern, legacy);
        let first = modern
            .first()
            .expect("token stream should contain left brace");
        assert!(matches!(
            json_confidence(first),
            Some(jv_lexer::JsonConfidence::High)
        ));
    }

    #[test]
    fn block_expression_metadata_removed() {
        let source = "{ val x = 1 }";
        let legacy = run_legacy_preprocess(tokenize(source));
        let (modern, _, _) = run(tokenize(source)).into_parts();
        assert_eq!(modern, legacy);
        let first = modern
            .first()
            .expect("token stream should contain left brace");
        assert!(json_confidence(first).is_none());
    }

    #[test]
    fn json_array_of_strings_detected() {
        let source = "[\"a\", \"b\"]";
        let legacy = run_legacy_preprocess(tokenize(source));
        let (modern, _, _) = run(tokenize(source)).into_parts();
        assert_eq!(modern, legacy);
        let first = modern
            .first()
            .expect("token stream should contain left bracket");
        assert!(matches!(
            json_confidence(first),
            Some(jv_lexer::JsonConfidence::High)
        ));
    }

    #[test]
    fn jv_array_of_numbers_not_flagged_as_json() {
        let source = "[1, 2, 3]";
        let legacy = run_legacy_preprocess(tokenize(source));
        let (modern, _, _) = run(tokenize(source)).into_parts();
        assert_eq!(modern, legacy);
        let first = modern
            .first()
            .expect("token stream should contain left bracket");
        assert!(json_confidence(first).is_none());
    }
}
