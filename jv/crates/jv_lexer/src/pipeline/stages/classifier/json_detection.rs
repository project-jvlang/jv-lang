use crate::{
    pipeline::{context::LexerContext, types::NormalizedToken},
    JsonConfidence, LexError, TokenMetadata,
};

use super::{ClassificationModule, ClassificationState};

pub struct JsonDetectionModule;

impl JsonDetectionModule {
    pub fn new() -> Self {
        Self
    }
}

impl ClassificationModule for JsonDetectionModule {
    fn apply<'source>(
        &mut self,
        token: &NormalizedToken<'source>,
        ctx: &LexerContext<'source>,
        state: &mut ClassificationState<'source>,
    ) -> Result<(), LexError> {
        let offset = token.raw.span.byte_range.end;
        let source = ctx.source;
        match token.raw.text {
            "{" => {
                if let Some(confidence) = detect_object_confidence(source, offset) {
                    state
                        .metadata()
                        .push(TokenMetadata::PotentialJsonStart { confidence });
                }
            }
            "[" => {
                if let Some(confidence) = detect_array_confidence(source, offset) {
                    state
                        .metadata()
                        .push(TokenMetadata::PotentialJsonStart { confidence });
                }
            }
            _ => {}
        }
        Ok(())
    }
}

fn detect_object_confidence(source: &str, offset: usize) -> Option<JsonConfidence> {
    let mut lookahead = Lookahead::new(source, offset);
    lookahead.skip_trivia();

    match lookahead.peek() {
        Some('}') | Some('"') => return Some(JsonConfidence::High),
        _ => {}
    }

    let mut depth = 0usize;
    while let Some(ch) = lookahead.bump() {
        match ch {
            '"' => lookahead.skip_string(),
            '{' => depth += 1,
            '}' => {
                if depth == 0 {
                    break;
                }
                depth = depth.saturating_sub(1);
            }
            ':' if depth == 0 => return Some(JsonConfidence::High),
            _ => {}
        }
    }

    None
}

fn detect_array_confidence(source: &str, offset: usize) -> Option<JsonConfidence> {
    let mut lookahead = Lookahead::new(source, offset);
    lookahead.skip_trivia();

    match lookahead.peek() {
        Some(']') | Some('{') | Some('[') | Some('"') => return Some(JsonConfidence::High),
        Some('t') | Some('T') | Some('f') | Some('F') | Some('n') | Some('N') => {
            return Some(JsonConfidence::Medium)
        }
        Some('-') => return Some(JsonConfidence::Medium),
        Some(ch) if ch.is_ascii_digit() => return Some(JsonConfidence::Medium),
        _ => {}
    }

    None
}

struct Lookahead<'a> {
    source: &'a str,
    index: usize,
}

impl<'a> Lookahead<'a> {
    fn new(source: &'a str, offset: usize) -> Self {
        Self {
            source,
            index: offset,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source[self.index..].chars().next()
    }

    fn starts_with(&self, pattern: &str) -> bool {
        self.source[self.index..].starts_with(pattern)
    }

    fn bump(&mut self) -> Option<char> {
        let mut iter = self.source[self.index..].char_indices();
        let (offset, ch) = iter.next()?;
        debug_assert_eq!(offset, 0);
        self.index += ch.len_utf8();
        Some(ch)
    }

    fn skip_trivia(&mut self) {
        loop {
            self.skip_whitespace();
            if self.skip_comment() {
                continue;
            }
            break;
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), Some(ch) if ch.is_whitespace()) {
            self.bump();
        }
    }

    fn skip_comment(&mut self) -> bool {
        if self.starts_with("//") {
            while let Some(ch) = self.bump() {
                if ch == '\n' {
                    break;
                }
            }
            return true;
        }

        if self.starts_with("/*") {
            // consume the initial "/*"
            self.bump();
            self.bump();
            while let Some(ch) = self.bump() {
                if ch == '*' && self.starts_with("/") {
                    self.bump();
                    break;
                }
            }
            return true;
        }

        false
    }

    fn skip_string(&mut self) {
        while let Some(ch) = self.bump() {
            match ch {
                '\\' => {
                    self.bump();
                }
                '"' => break,
                _ => {}
            }
        }
    }
}
