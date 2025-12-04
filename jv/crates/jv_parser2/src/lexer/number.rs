use crate::{Token, token::TokenKind};

use super::Lexer;

pub(crate) fn lex_number(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();
    let first = lexer.advance().unwrap_or_default();

    if first == b'0' {
        if let Some((b'x' | b'X', _)) = lexer.peek2() {
            lexer.advance(); // consume 'x'
            let digits = consume_digits(lexer, is_hex_digit);
            return if digits > 0 {
                lexer.make_token(TokenKind::Number, start)
            } else {
                lexer.make_token(TokenKind::Invalid, start)
            };
        }
        if let Some((b'b' | b'B', _)) = lexer.peek2() {
            lexer.advance();
            let digits = consume_digits(lexer, is_bin_digit);
            return if digits > 0 {
                lexer.make_token(TokenKind::Number, start)
            } else {
                lexer.make_token(TokenKind::Invalid, start)
            };
        }
        if let Some((b'o' | b'O', _)) = lexer.peek2() {
            lexer.advance();
            let digits = consume_digits(lexer, is_oct_digit);
            return if digits > 0 {
                lexer.make_token(TokenKind::Number, start)
            } else {
                lexer.make_token(TokenKind::Invalid, start)
            };
        }
    }

    let mut int_digits = consume_digits(lexer, |b| b.is_ascii_digit());

    // 小数部
    if matches!(lexer.peek(), Some(b'.')) && !matches!(lexer.peek2(), Some((b'.', b'.'))) {
        lexer.advance(); // '.'
        let frac = consume_digits(lexer, |b| b.is_ascii_digit());
        if frac == 0 {
            return lexer.make_token(TokenKind::Invalid, start);
        }
        int_digits += frac;
    }

    // 指数部
    if matches!(lexer.peek(), Some(b'e' | b'E')) {
        lexer.advance();
        if matches!(lexer.peek(), Some(b'+' | b'-')) {
            lexer.advance();
        }
        let exp = consume_digits(lexer, |b| b.is_ascii_digit());
        if exp == 0 {
            return lexer.make_token(TokenKind::Invalid, start);
        }
    }

    // 単位サフィックス @ms, @km など
    if matches!(lexer.peek(), Some(b'@')) {
        lexer.advance();
        let unit = consume_digits(lexer, |b| b.is_ascii_alphabetic());
        if unit == 0 {
            return lexer.make_token(TokenKind::Invalid, start);
        }
    }

    if int_digits == 0 {
        lexer.make_token(TokenKind::Invalid, start)
    } else {
        lexer.make_token(TokenKind::Number, start)
    }
}

fn consume_digits<F>(lexer: &mut Lexer<'_>, mut predicate: F) -> usize
where
    F: FnMut(u8) -> bool,
{
    let mut count = 0;
    while let Some(b) = lexer.peek() {
        if predicate(b) {
            lexer.advance();
            count += 1;
        } else if b == b'_' {
            lexer.advance();
        } else {
            break;
        }
    }
    count
}

#[inline]
const fn is_hex_digit(b: u8) -> bool {
    matches!(b, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
}

#[inline]
const fn is_bin_digit(b: u8) -> bool {
    matches!(b, b'0' | b'1')
}

#[inline]
const fn is_oct_digit(b: u8) -> bool {
    matches!(b, b'0'..=b'7')
}
