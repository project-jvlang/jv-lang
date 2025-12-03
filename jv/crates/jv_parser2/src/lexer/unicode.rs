use crate::{token::TokenKind, Token};

use super::Lexer;

pub(crate) fn lex_unicode_identifier(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();
    let Some((ch, len)) = decode_char(lexer.slice_from(start)) else {
        lexer.advance();
        return lexer.make_token(TokenKind::Invalid, start);
    };

    if !unicode_ident::is_xid_start(ch) && ch != '_' {
        lexer.advance_by(len);
        return lexer.make_token(TokenKind::Invalid, start);
    }

    lexer.advance_by(len);
    while let Some((next, next_len)) = decode_char(lexer.slice_from(lexer.current_offset())) {
        if unicode_ident::is_xid_continue(next) || next == '_' {
            lexer.advance_by(next_len);
        } else {
            break;
        }
    }

    lexer.make_token(TokenKind::Identifier, start)
}

fn decode_char(bytes: &[u8]) -> Option<(char, usize)> {
    let len = bytes.len().min(4);
    for i in 1..=len {
        if let Ok(s) = std::str::from_utf8(&bytes[..i]) {
            if let Some(ch) = s.chars().next() {
                return Some((ch, ch.len_utf8()));
            }
        }
    }
    None
}
