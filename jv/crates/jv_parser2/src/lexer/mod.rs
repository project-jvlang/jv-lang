//! レキサー本体とバイトハンドラディスパッチ。

use crate::{source::Source, span::Span, token::TokenKind, Token};

mod handlers;
mod keywords;
mod number;
mod operators;
mod string;
mod unicode;

type ByteHandler = fn(&mut Lexer<'_>) -> Token;

/// ASCII 1バイトに対するハンドラテーブル。
static BYTE_HANDLERS: [ByteHandler; 128] = build_byte_handlers();

const fn build_byte_handlers() -> [ByteHandler; 128] {
    let mut table: [ByteHandler; 128] = [handlers::handle_unknown; 128];

    // 数値
    let mut i = b'0';
    while i <= b'9' {
        table[i as usize] = number::lex_number;
        i += 1;
    }

    // 識別子開始（ASCII）
    let mut upper = b'A';
    while upper <= b'Z' {
        table[upper as usize] = handlers::handle_identifier;
        upper += 1;
    }
    let mut lower = b'a';
    while lower <= b'z' {
        table[lower as usize] = handlers::handle_identifier;
        lower += 1;
    }
    table[b'_' as usize] = handlers::handle_identifier;

    // 空白
    table[b' ' as usize] = handlers::handle_whitespace;
    table[b'\t' as usize] = handlers::handle_whitespace;
    table[b'\r' as usize] = handlers::handle_whitespace;
    table[b'\n' as usize] = handlers::handle_newline;

    // 文字列/文字リテラル
    table[b'\"' as usize] = handlers::handle_unknown;
    table[b'\'' as usize] = string::lex_char_literal;

    // 演算子/記号
    table[b'+' as usize] = operators::lex_operator;
    table[b'-' as usize] = operators::lex_operator;
    table[b'*' as usize] = operators::lex_operator;
    table[b'/' as usize] = operators::lex_operator;
    table[b'%' as usize] = operators::lex_operator;
    table[b'=' as usize] = operators::lex_operator;
    table[b'!' as usize] = operators::lex_operator;
    table[b'<' as usize] = operators::lex_operator;
    table[b'>' as usize] = operators::lex_operator;
    table[b'&' as usize] = operators::lex_operator;
    table[b'|' as usize] = operators::lex_operator;
    table[b'?' as usize] = operators::lex_operator;
    table[b'.' as usize] = operators::lex_operator;
    table[b',' as usize] = operators::lex_operator;
    table[b';' as usize] = operators::lex_operator;
    table[b':' as usize] = operators::lex_operator;
    table[b'(' as usize] = operators::lex_operator;
    table[b')' as usize] = operators::lex_operator;
    table[b'{' as usize] = operators::lex_operator;
    table[b'}' as usize] = operators::lex_operator;
    table[b'[' as usize] = operators::lex_operator;
    table[b']' as usize] = operators::lex_operator;
    table[b'@' as usize] = operators::lex_operator;

    table
}

#[derive(Debug)]
pub struct Lexer<'src> {
    source: Source<'src>,
    current: Token,
    mode: Mode,
}

#[derive(Debug)]
enum Mode {
    Normal,
    InString(StringContext),
    InInterpolation(StringContext, usize), // ctx, brace depth
}

#[derive(Debug, Clone)]
struct StringContext {
    segment_start: usize,
    first_segment: bool,
    has_interpolation: bool,
}

impl<'src> Lexer<'src> {
    pub fn new(source: Source<'src>) -> Self {
        let mut lexer = Self {
            source,
            current: Token::new(TokenKind::Invalid, Span::new(0, 0)),
            mode: Mode::Normal,
        };
        lexer.current = lexer.read_next_token();
        lexer
    }

    /// 現在のトークンを返し、次のトークンへ進める。
    pub fn next_token(&mut self) -> Token {
        let token = self.current;
        self.current = self.read_next_token();
        token
    }

    /// 次のトークンを先読みする。
    pub fn peek_token(&self) -> &Token {
        &self.current
    }

    fn read_next_token(&mut self) -> Token {
        let mode = std::mem::replace(&mut self.mode, Mode::Normal);
        match mode {
            Mode::InString(mut ctx) => {
                let (token, next_mode) = string::lex_string_segment(self, &mut ctx);
                self.mode = next_mode;
                token
            }
            Mode::InInterpolation(mut ctx, mut depth) => {
                let (token, new_mode) = self.read_normal_token_with_mode();
                if !matches!(new_mode, Mode::Normal) {
                    // 字句解析が文字列など別モードへ遷移した場合は優先する
                    self.mode = new_mode;
                    return token;
                }
                match token.kind {
                    TokenKind::LeftBrace => {
                        depth += 1;
                        self.mode = Mode::InInterpolation(ctx, depth);
                    }
                    TokenKind::RightBrace => {
                        if depth > 0 {
                            depth -= 1;
                        }
                        if depth == 0 {
                            ctx.segment_start = self.source.offset();
                            self.mode = Mode::InString(ctx);
                        } else {
                            self.mode = Mode::InInterpolation(ctx, depth);
                        }
                    }
                    _ => self.mode = Mode::InInterpolation(ctx, depth),
                }
                token
            }
            Mode::Normal => self.read_normal_token(),
        }
    }

    fn read_normal_token_with_mode(&mut self) -> (Token, Mode) {
        let token = self.read_normal_token();
        let next_mode = std::mem::replace(&mut self.mode, Mode::Normal);
        (token, next_mode)
    }

    fn read_normal_token(&mut self) -> Token {
        let start = self.source.offset();
        match self.source.peek() {
            None => Token::new(TokenKind::Eof, self.source.span_from(start, start)),
            Some(byte) if byte < 128 => {
                if byte == b'"' {
                    let mut ctx = StringContext {
                        segment_start: start,
                        first_segment: true,
                        has_interpolation: false,
                    };
                    let (token, next_mode) = string::lex_string_segment(self, &mut ctx);
                    self.mode = next_mode;
                    return token;
                }
                let handler = BYTE_HANDLERS[byte as usize];
                handler(self)
            }
            Some(_) => unicode::lex_unicode_identifier(self),
        }
    }

    #[inline]
    pub(crate) fn current_offset(&self) -> usize {
        self.source.offset()
    }

    #[inline]
    pub(crate) fn make_token(&self, kind: TokenKind, start: usize) -> Token {
        Token::new(kind, self.source.span_from(start, self.source.offset()))
    }

    #[inline]
    pub(crate) fn peek(&self) -> Option<u8> {
        self.source.peek()
    }

    #[inline]
    pub(crate) fn peek2(&self) -> Option<(u8, u8)> {
        self.source.peek2()
    }

    #[inline]
    pub(crate) fn advance(&mut self) -> Option<u8> {
        self.source.advance()
    }

    #[inline]
    pub(crate) fn advance_by(&mut self, count: usize) {
        self.source.advance_by(count)
    }

    #[inline]
    pub(crate) fn slice_from(&self, start: usize) -> &'src [u8] {
        self.source.slice_from(start)
    }

    #[inline]
    pub(crate) fn consume_while<F>(&mut self, mut predicate: F)
    where
        F: FnMut(u8) -> bool,
    {
        while let Some(byte) = self.peek() {
            if predicate(byte) {
                self.advance();
            } else {
                break;
            }
        }
    }
}

#[inline]
pub(crate) const fn is_ascii_ident_continue(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}
