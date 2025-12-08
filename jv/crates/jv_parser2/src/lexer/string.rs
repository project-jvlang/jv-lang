use crate::{Token, token::TokenKind};

use super::{Lexer, Mode, StringContext};

/// レキサー状態に応じて文字列セグメントを返す。
pub(crate) fn lex_string_segment(lexer: &mut Lexer<'_>, ctx: &mut StringContext) -> (Token, Mode) {
    let start = ctx.segment_start;
    // 先頭呼び出しでは開きクオートを読む
    if ctx.first_segment {
        lexer.advance(); // consume opening quote
    }

    while let Some(byte) = lexer.peek() {
        match byte {
            b'"' => {
                lexer.advance();
                let kind = if ctx.has_interpolation {
                    TokenKind::StringEnd
                } else {
                    TokenKind::String
                };
                return (lexer.make_token(kind, start), Mode::Normal);
            }
            b'\\' => {
                lexer.advance();
                if !validate_escape(lexer) {
                    return (lexer.make_token(TokenKind::Invalid, start), Mode::Normal);
                }
            }
            b'$' if matches!(lexer.peek2(), Some((b'$', b'{'))) => {
                // interpolation start
                ctx.has_interpolation = true;
                lexer.advance_by(2); // consume ${
                let kind = if ctx.first_segment {
                    TokenKind::StringStart
                } else {
                    TokenKind::StringMid
                };
                ctx.first_segment = false;
                let token = lexer.make_token(kind, start);
                return (token, Mode::InInterpolation(ctx.clone(), 1));
            }
            b'\n' => {
                // 未終端
                let token = lexer.make_token(TokenKind::Invalid, start);
                lexer.advance(); // consume newline
                return (token, Mode::Normal);
            }
            _ => {
                lexer.advance();
            }
        }
    }

    // EOFに到達
    (lexer.make_token(TokenKind::Invalid, start), Mode::Normal)
}

fn validate_escape(lexer: &mut Lexer<'_>) -> bool {
    match lexer.peek() {
        Some(b'n' | b't' | b'\\' | b'"') => {
            lexer.advance();
            true
        }
        Some(b'u') => {
            lexer.advance();
            let mut digits = 0;
            while digits < 4 {
                match lexer.peek() {
                    Some(b) if is_hex(b) => {
                        lexer.advance();
                        digits += 1;
                    }
                    _ => break,
                }
            }
            digits == 4
        }
        Some(_) | None => false,
    }
}

fn is_hex(b: u8) -> bool {
    matches!(b, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
}

pub(crate) fn lex_char_literal(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();
    lexer.advance(); // opening '

    let mut closed = false;
    let mut saw_payload = false;
    if let Some(byte) = lexer.peek() {
        match byte {
            b'\\' => {
                lexer.advance();
                saw_payload = validate_char_escape(lexer);
            }
            b'\'' => {}
            _ => {
                lexer.advance();
                saw_payload = true;
            }
        }
        if matches!(lexer.peek(), Some(b'\'')) {
            lexer.advance();
            closed = true;
        }
    }

    let mut token = lexer.make_token(TokenKind::Character, start);
    if !closed || !saw_payload {
        token.kind = TokenKind::Invalid;
    }
    token
}

fn validate_char_escape(lexer: &mut Lexer<'_>) -> bool {
    match lexer.peek() {
        Some(b'n' | b't' | b'\\' | b'"' | b'\'') => {
            lexer.advance();
            true
        }
        Some(b'u') => {
            lexer.advance();
            let mut digits = 0;
            while digits < 4 {
                match lexer.peek() {
                    Some(b) if is_hex(b) => {
                        lexer.advance();
                        digits += 1;
                    }
                    _ => break,
                }
            }
            digits == 4
        }
        Some(_) | None => false,
    }
}

/// Lex a raw/multiline string literal delimited by triple quotes (`"""`).
/// The content is preserved as-is without escape processing.
pub(crate) fn lex_raw_string(lexer: &mut Lexer<'_>) -> crate::Token {
    let start = lexer.current_offset();

    // Consume opening """
    lexer.advance_by(3);

    // Skip optional leading newline after opening """
    if matches!(lexer.peek(), Some(b'\n')) {
        lexer.advance();
    } else if matches!(lexer.peek(), Some(b'\r')) {
        lexer.advance();
        if matches!(lexer.peek(), Some(b'\n')) {
            lexer.advance();
        }
    }

    // Consume until closing """
    loop {
        match lexer.peek() {
            None => {
                // EOF without closing - return invalid token
                return lexer.make_token(TokenKind::Invalid, start);
            }
            Some(b'"') => {
                // Check for closing """
                let remaining = lexer.source().remaining_bytes();
                if remaining.len() >= 3
                    && remaining[0] == b'"'
                    && remaining[1] == b'"'
                    && remaining[2] == b'"'
                {
                    // Found closing """
                    lexer.advance_by(3);
                    return lexer.make_token(TokenKind::RawString, start);
                }
                lexer.advance();
            }
            Some(_) => {
                lexer.advance();
            }
        }
    }
}
