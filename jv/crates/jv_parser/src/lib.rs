// jv_parser - Syntax parsing with chumsky for jv language
// P0-002: parser basics - Implements jv language syntax parsing using combinator approach
//
// This parser follows Test-Driven Development principles and uses the chumsky parsing library
// to transform jv source code into an Abstract Syntax Tree (AST). The parser handles:
// - Variable declarations (val, var)
// - Function declarations
// - Data class declarations
// - Binary expressions with operator precedence
// - Function calls and member access
// - When expressions (pattern matching)
// - String interpolation
//
// Implementation uses recursive descent parsing with chumsky combinators for clean,
// composable parser construction.

//! 延期構文の最新状況は `docs/deferred-syntax.md` を参照してください。
//! The up-to-date deferred syntax inventory lives in `docs/deferred-syntax.md`.

use chumsky::error::{Simple, SimpleReason};
use chumsky::prelude::*;
use chumsky::Parser as ChumskyParser;
use jv_ast::*;
use jv_lexer::{
    ExplicitSeparatorLocation, JsonConfidence, LayoutCommaMetadata, LayoutSequenceKind, LexError,
    Token, TokenMetadata, TokenTrivia, TokenType,
};
use thiserror::Error;

mod syntax;

use crate::syntax::support::{merge_spans, span_from_token};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Lexical error: {0}")]
    LexError(#[from] LexError),
    #[error("Parse error at {span:?}: {message}")]
    Syntax { message: String, span: Span },
    #[error("Unexpected end of input at {span:?}")]
    UnexpectedEof { span: Span },
}

pub struct Parser;

impl Parser {
    /// Parse jv source code into an Abstract Syntax Tree
    ///
    /// This is the main entry point for the parser. It performs lexical analysis
    /// first, then applies the grammar rules to construct a Program AST node.
    ///
    /// # Arguments
    /// * `input` - The jv source code as a string
    ///
    /// # Returns
    /// * `Ok(Program)` - Successfully parsed AST
    /// * `Err(ParseError)` - Lexical or syntax error
    ///
    /// # Example
    /// ```rust,ignore
    /// let result = Parser::parse("val x = 42");
    /// ```
    pub fn parse(input: &str) -> Result<Program, ParseError> {
        let mut lexer = jv_lexer::Lexer::new(input.to_string());
        let tokens = preprocess_tokens(lexer.tokenize()?);

        let parser = Self::program_parser();

        match parser.parse(tokens.clone()) {
            Ok(program) => Ok(program),
            Err(errors) => {
                let format_simple = |error: &Simple<Token>| match error.reason() {
                    SimpleReason::Custom(message) => message.clone(),
                    _ => format!("{:?}", error),
                };

                let mut errors_iter = errors.into_iter();

                if let Some(error) = errors_iter.next() {
                    let error_span = simple_error_span(&error, &tokens);
                    let mut message = format_simple(&error);
                    for extra in errors_iter {
                        message.push_str("; ");
                        message.push_str(&format_simple(&extra));
                    }

                    let is_custom = matches!(error.reason(), SimpleReason::Custom(_));
                    let is_eof = match error.found() {
                        None => !is_custom,
                        Some(token) => matches!(&token.token_type, TokenType::Eof),
                    };

                    if is_eof {
                        Err(ParseError::UnexpectedEof { span: error_span })
                    } else {
                        Err(ParseError::Syntax {
                            message,
                            span: error_span,
                        })
                    }
                } else {
                    let span = tokens
                        .last()
                        .map(|token| span_from_token(token))
                        .unwrap_or_else(Span::dummy);
                    Err(ParseError::UnexpectedEof { span })
                }
            }
        }
    }

    /// A program consists of zero or more statements followed by EOF.
    fn program_parser() -> impl ChumskyParser<Token, Program, Error = Simple<Token>> + Clone {
        syntax::statement_parser()
            .repeated()
            .then_ignore(filter(|token: &Token| {
                matches!(token.token_type, TokenType::Eof)
            }))
            .map(|statements| {
                let span = if statements.is_empty() {
                    Span {
                        start_line: 1,
                        start_column: 1,
                        end_line: 1,
                        end_column: 1,
                    }
                } else {
                    let first_span = syntax::statement_span(&statements[0]);
                    let last_span = syntax::statement_span(statements.last().unwrap());
                    syntax::merge_spans(&first_span, &last_span)
                };

                Program {
                    statements,
                    package: None,
                    imports: Vec::new(),
                    span,
                }
            })
    }
}

impl ParseError {
    pub fn span(&self) -> Option<&Span> {
        match self {
            ParseError::LexError(_) => None,
            ParseError::Syntax { span, .. } => Some(span),
            ParseError::UnexpectedEof { span } => Some(span),
        }
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

fn is_sequence_layout_candidate(token_type: &TokenType) -> bool {
    !matches!(
        token_type,
        TokenType::Comma
            | TokenType::RightBracket
            | TokenType::RightParen
            | TokenType::Assign
            | TokenType::Colon
            | TokenType::Dot
            | TokenType::Arrow
            | TokenType::FatArrow
    )
}

fn preprocess_tokens(tokens: Vec<Token>) -> Vec<Token> {
    let json_contexts = detect_json_contexts(&tokens);
    let mut result = Vec::with_capacity(tokens.len());
    let mut stack: Vec<SequenceContext> = Vec::new();
    let mut call_eligible = false;
    let mut suppress_definition_call = false;
    let mut in_interpolation_expr = false;
    let mut expect_interpolation_expr = false;

    for (index, mut token) in tokens.into_iter().enumerate() {
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
                } && is_sequence_layout_candidate(token_type_ref);
                if eligible {
                    let layout_needed = !ctx.prev_was_separator
                        && (ctx.pending_layout || has_layout_trivia(&token.leading_trivia));
                    if layout_needed {
                        let mut synthetic = make_layout_comma_token(&token);
                        let sequence = match ctx.kind {
                            SequenceContextKind::Array => LayoutSequenceKind::Array,
                            SequenceContextKind::Call => LayoutSequenceKind::Call,
                        };
                        let metadata = LayoutCommaMetadata {
                            sequence,
                            explicit_separator: ctx.last_explicit_separator.take(),
                        };
                        synthetic
                            .metadata
                            .push(TokenMetadata::LayoutComma(metadata));
                        result.push(synthetic);
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
                // Identifier without colon is likely a block; stop scanning.
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

fn detect_json_array(tokens: &[Token], index: usize) -> Option<JsonConfidence> {
    if matches!(
        json_confidence_from_metadata(&tokens[index]),
        Some(JsonConfidence::High)
    ) {
        return Some(JsonConfidence::High);
    }

    if let Some(next_index) = skip_comment_tokens(tokens, index + 1) {
        match &tokens[next_index].token_type {
            TokenType::RightBracket => return Some(JsonConfidence::High),
            TokenType::LeftBrace | TokenType::LeftBracket => return Some(JsonConfidence::High),
            TokenType::String(_) => return Some(JsonConfidence::High),
            TokenType::Boolean(_) | TokenType::Null => return Some(JsonConfidence::High),
            TokenType::Identifier(_) => return None,
            TokenType::Val
            | TokenType::Var
            | TokenType::Fun
            | TokenType::Class
            | TokenType::Data => return None,
            _ => {}
        }
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

#[cfg(test)]
mod preprocess_tests {
    use super::{preprocess_tokens, JsonConfidence, TokenMetadata};

    fn tokenize(source: &str) -> Vec<jv_lexer::Token> {
        let mut lexer = jv_lexer::Lexer::new(source.to_string());
        lexer.tokenize().expect("lexing should succeed")
    }

    fn json_confidence(token: &jv_lexer::Token) -> Option<JsonConfidence> {
        token.metadata.iter().find_map(|metadata| match metadata {
            TokenMetadata::PotentialJsonStart { confidence } => Some(*confidence),
            _ => None,
        })
    }

    #[test]
    fn json_object_metadata_preserved() {
        let tokens = preprocess_tokens(tokenize("{\"key\": 1}"));
        let first = tokens
            .first()
            .expect("token stream should contain left brace");
        assert!(matches!(json_confidence(first), Some(JsonConfidence::High)));
    }

    #[test]
    fn block_expression_metadata_removed() {
        let tokens = preprocess_tokens(tokenize("{ val x = 1 }"));
        let first = tokens
            .first()
            .expect("token stream should contain left brace");
        assert!(json_confidence(first).is_none());
    }

    #[test]
    fn json_array_of_strings_detected() {
        let tokens = preprocess_tokens(tokenize("[\"a\", \"b\"]"));
        let first = tokens
            .first()
            .expect("token stream should contain left bracket");
        assert!(matches!(json_confidence(first), Some(JsonConfidence::High)));
    }

    #[test]
    fn jv_array_of_numbers_not_flagged_as_json() {
        let tokens = preprocess_tokens(tokenize("[1, 2, 3]"));
        let first = tokens
            .first()
            .expect("token stream should contain left bracket");
        assert!(json_confidence(first).is_none());
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

fn simple_error_span(error: &Simple<Token>, tokens: &[Token]) -> Span {
    if tokens.is_empty() {
        return Span::dummy();
    }

    let range = error.span();
    let tokens_len = tokens.len();

    let start_index = range.start.min(tokens_len - 1);
    let raw_end = if range.end == range.start {
        range.start
    } else {
        range.end.saturating_sub(1)
    };
    let end_index = raw_end.min(tokens_len - 1).max(start_index);

    let start_token = tokens
        .get(start_index)
        .unwrap_or_else(|| tokens.last().unwrap());
    let end_token = tokens
        .get(end_index)
        .unwrap_or_else(|| tokens.last().unwrap());

    let start_span = span_from_token(start_token);
    let end_span = span_from_token(end_token);

    merge_spans(&start_span, &end_span)
}

#[cfg(test)]
mod tests;
