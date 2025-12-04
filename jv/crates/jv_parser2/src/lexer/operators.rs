use crate::{Token, token::TokenKind};

use super::{Lexer, can_start_regex, lex_regex};

pub(crate) fn lex_operator(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();
    let byte = lexer.advance().unwrap_or_default();

    let kind = match byte {
        b'-' => {
            if matches!(lexer.peek(), Some(b'>')) {
                lexer.advance();
                TokenKind::Arrow
            } else {
                TokenKind::Minus
            }
        }
        b'=' => {
            if matches!(lexer.peek(), Some(b'>')) {
                lexer.advance();
                TokenKind::FatArrow
            } else if matches!(lexer.peek(), Some(b'=')) {
                lexer.advance();
                TokenKind::Equal
            } else {
                TokenKind::Assign
            }
        }
        b'+' => TokenKind::Plus,
        b'*' => TokenKind::Multiply,
        b'%' => TokenKind::Modulo,
        b'!' => {
            if matches!(lexer.peek(), Some(b'=')) {
                lexer.advance();
                TokenKind::NotEqual
            } else {
                TokenKind::Not
            }
        }
        b'<' => {
            if matches!(lexer.peek(), Some(b'=')) {
                lexer.advance();
                TokenKind::LessEqual
            } else {
                TokenKind::Less
            }
        }
        b'>' => {
            if matches!(lexer.peek(), Some(b'=')) {
                lexer.advance();
                TokenKind::GreaterEqual
            } else {
                TokenKind::Greater
            }
        }
        b'&' => {
            if matches!(lexer.peek(), Some(b'&')) {
                lexer.advance();
                TokenKind::And
            } else {
                TokenKind::Invalid
            }
        }
        b'|' => {
            if matches!(lexer.peek(), Some(b'{')) {
                lexer.advance();
                TokenKind::PipeLeft
            } else if matches!(lexer.peek(), Some(b'|')) {
                lexer.advance();
                TokenKind::Or
            } else if matches!(lexer.peek(), Some(b'}')) {
                lexer.advance();
                TokenKind::PipeRight
            } else {
                TokenKind::Invalid
            }
        }
        b'?' => {
            if matches!(lexer.peek(), Some(b'.')) {
                lexer.advance();
                TokenKind::NullSafe
            } else if matches!(lexer.peek(), Some(b':')) {
                lexer.advance();
                TokenKind::Elvis
            } else {
                TokenKind::Question
            }
        }
        b'.' => {
            if matches!(lexer.peek2(), Some((b'.', b'='))) {
                lexer.advance_by(2); // consume ".="
                TokenKind::RangeInclusive
            } else if matches!(lexer.peek(), Some(b'.')) {
                lexer.advance();
                TokenKind::RangeExclusive
            } else {
                TokenKind::Dot
            }
        }
        b',' => TokenKind::Comma,
        b';' => TokenKind::Semicolon,
        b':' => {
            if matches!(lexer.peek(), Some(b':')) {
                lexer.advance();
                TokenKind::DoubleColon
            } else {
                TokenKind::Colon
            }
        }
        b'(' => TokenKind::LeftParen,
        b')' => TokenKind::RightParen,
        b'{' => TokenKind::LeftBrace,
        b'}' => {
            if matches!(lexer.peek(), Some(b'|')) {
                lexer.advance();
                TokenKind::PipeRight
            } else {
                TokenKind::RightBrace
            }
        }
        b'[' => TokenKind::LeftBracket,
        b']' => TokenKind::RightBracket,
        b'@' => TokenKind::At,
        b'/' => match lexer.peek() {
            Some(b'/') => lex_line_comment(lexer),
            Some(b'*') => lex_block_comment(lexer),
            _ => TokenKind::Divide,
        },
        _ => TokenKind::Invalid,
    };

    lexer.make_token(kind, start)
}

/// `/` を文脈依存で処理する。正規表現リテラルまたは除算演算子を返す。
///
/// この関数はバイトハンドラとして使用され、`/` の文脈判定を行う。
pub(crate) fn lex_slash(lexer: &mut Lexer<'_>) -> Token {
    // コメントかどうかを先読み
    if let Some((b'/', next)) = lexer.peek2() {
        if next == b'/' || next == b'*' {
            // コメント: 通常の演算子処理へ
            return lex_operator(lexer);
        }
    }

    // 正規表現リテラルを開始できるか判定
    if can_start_regex(lexer.last_token_kind) {
        return lex_regex(lexer);
    }

    // 除算演算子
    let start = lexer.current_offset();
    lexer.advance();
    lexer.make_token(TokenKind::Divide, start)
}

fn lex_line_comment(lexer: &mut Lexer<'_>) -> TokenKind {
    lexer.advance(); // second slash
    while let Some(byte) = lexer.peek() {
        if byte == b'\n' {
            break;
        }
        lexer.advance();
    }
    TokenKind::LineComment
}

fn lex_block_comment(lexer: &mut Lexer<'_>) -> TokenKind {
    lexer.advance(); // '*'
    let mut saw_star = false;
    while let Some(byte) = lexer.peek() {
        lexer.advance();
        if saw_star && byte == b'/' {
            return TokenKind::BlockComment;
        }
        saw_star = byte == b'*';
    }
    // 未終端
    TokenKind::Invalid
}
