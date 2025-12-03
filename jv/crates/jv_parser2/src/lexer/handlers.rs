use crate::{token::TokenKind, Token};

use super::{is_ascii_ident_continue, keywords, Lexer};

pub(crate) fn handle_unknown(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();
    lexer.advance();
    lexer.make_token(TokenKind::Invalid, start)
}

pub(crate) fn handle_whitespace(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();
    lexer.consume_while(|b| matches!(b, b' ' | b'\t' | b'\r'));
    lexer.make_token(TokenKind::Whitespace, start)
}

pub(crate) fn handle_newline(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();
    lexer.advance();
    lexer.make_token(TokenKind::Newline, start)
}

pub(crate) fn handle_identifier(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();
    lexer.advance();
    lexer.consume_while(is_ascii_ident_continue);

    let len = lexer.current_offset().saturating_sub(start);
    let text = &lexer.slice_from(start)[..len];

    let kind = classify_identifier(text);
    lexer.make_token(kind, start)
}

fn classify_identifier(text: &[u8]) -> TokenKind {
    if let Some(kind) = keywords::keyword_from_bytes(text) {
        return kind;
    }
    if text == b"_" {
        return TokenKind::Underscore;
    }
    if text.len() > 1 && text[0] == b'_' && text[1..].iter().all(u8::is_ascii_digit) {
        return TokenKind::ImplicitParam;
    }
    TokenKind::Identifier
}
