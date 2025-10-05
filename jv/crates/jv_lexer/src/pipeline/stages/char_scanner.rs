use std::collections::VecDeque;

use crate::{
    pipeline::{
        context::LexerContext,
        pipeline::CharScannerStage,
        pipeline::DEFAULT_LOOKAHEAD_LIMIT,
        types::{RawToken, RawTokenKind, ScannerPosition, Span},
    },
    JsonCommentTrivia, JsonCommentTriviaKind, LexError, SourceCommentKind, SourceCommentTrivia,
    TokenTrivia,
};

#[derive(Debug)]
struct RingBuffer {
    buffer: VecDeque<u8>,
    start_offset: usize,
    capacity: usize,
}

impl RingBuffer {
    fn new(capacity: usize) -> Self {
        Self {
            buffer: VecDeque::with_capacity(capacity),
            start_offset: 0,
            capacity,
        }
    }

    fn min_checkpoint_offset(checkpoints: &[Checkpoint]) -> usize {
        checkpoints
            .iter()
            .map(|cp| cp.position.byte_offset)
            .min()
            .unwrap_or(usize::MAX)
    }

    fn push_bytes(&mut self, bytes: &[u8], checkpoints: &[Checkpoint]) -> Result<(), LexError> {
        for &byte in bytes {
            if self.buffer.len() == self.capacity {
                let min_checkpoint = Self::min_checkpoint_offset(checkpoints);
                if min_checkpoint == usize::MAX || self.start_offset < min_checkpoint {
                    self.buffer.pop_front();
                    self.start_offset += 1;
                } else {
                    return Err(LexError::LookaheadOverflow {
                        requested: self.capacity + 1,
                    });
                }
            }
            self.buffer.push_back(byte);
        }
        Ok(())
    }

    fn commit(&mut self, committed_offset: usize) {
        while self.start_offset < committed_offset {
            if self.buffer.pop_front().is_some() {
                self.start_offset += 1;
            } else {
                self.start_offset = committed_offset;
                break;
            }
        }
    }

    fn reset(&mut self) {
        self.buffer.clear();
        self.start_offset = 0;
    }
}

#[derive(Debug, Clone, Copy)]
struct Checkpoint {
    position: ScannerPosition,
}

#[derive(Debug, Default, Clone)]
struct TriviaTracker {
    trivia: TokenTrivia,
}

impl TriviaTracker {
    fn record_space(&mut self) {
        self.trivia.spaces = self.trivia.spaces.saturating_add(1);
    }

    fn record_newline(&mut self) {
        self.trivia.newlines = self.trivia.newlines.saturating_add(1);
        self.trivia.spaces = 0;
    }

    fn record_line_comment(&mut self, text: String, line: usize, column: usize) {
        self.trivia.comments = true;
        self.trivia.passthrough_comments.push(SourceCommentTrivia {
            kind: SourceCommentKind::Line,
            text,
            line,
            column,
        });
    }

    fn record_block_comment(&mut self, text: String, line: usize, column: usize) {
        self.trivia.comments = true;
        self.trivia.passthrough_comments.push(SourceCommentTrivia {
            kind: SourceCommentKind::Block,
            text,
            line,
            column,
        });
    }

    fn record_json_comment(&mut self, text: String, line: usize, column: usize, is_block: bool) {
        self.trivia.comments = true;
        self.trivia.json_comments.push(JsonCommentTrivia {
            kind: if is_block {
                JsonCommentTriviaKind::Block
            } else {
                JsonCommentTriviaKind::Line
            },
            text,
            line,
            column,
        });
    }

    #[allow(dead_code)]
    fn record_doc_comment(&mut self, text: String) {
        self.trivia.comments = true;
        self.trivia.doc_comment = Some(text);
    }

    fn take(&mut self) -> Option<TokenTrivia> {
        if self.trivia == TokenTrivia::default() {
            None
        } else {
            Some(std::mem::take(&mut self.trivia))
        }
    }

    fn reset(&mut self) {
        self.trivia = TokenTrivia::default();
    }
}

#[derive(Debug)]
pub struct CharScanner {
    cursor: usize,
    position: ScannerPosition,
    checkpoints: Vec<Checkpoint>,
    ring_buffer: RingBuffer,
    trivia_tracker: TriviaTracker,
    lookahead_limit: usize,
    last_source_ptr: Option<usize>,
}

impl Default for CharScanner {
    fn default() -> Self {
        Self::new()
    }
}

impl CharScanner {
    pub fn new() -> Self {
        Self::with_lookahead_limit(DEFAULT_LOOKAHEAD_LIMIT)
    }

    pub fn with_lookahead_limit(limit: usize) -> Self {
        let capacity = limit.max(1);
        Self {
            cursor: 0,
            position: ScannerPosition::default(),
            checkpoints: Vec::new(),
            ring_buffer: RingBuffer::new(capacity),
            trivia_tracker: TriviaTracker::default(),
            lookahead_limit: capacity,
            last_source_ptr: None,
        }
    }

    pub fn reset(&mut self) {
        self.reset_state();
        self.last_source_ptr = None;
    }

    fn reset_state(&mut self) {
        self.cursor = 0;
        self.position = ScannerPosition::default();
        self.checkpoints.clear();
        self.ring_buffer.reset();
        self.trivia_tracker.reset();
    }

    fn synchronize_context<'source>(&mut self, ctx: &mut LexerContext<'source>) {
        let source_ptr = ctx.source.as_ptr() as usize;
        if self.last_source_ptr != Some(source_ptr) {
            self.reset_state();
            self.last_source_ptr = Some(source_ptr);
        }

        if ctx.current_position != self.position {
            ctx.update_position(self.position);
        }
    }

    fn remaining<'source>(&self, source: &'source str) -> &'source str {
        if self.cursor >= source.len() {
            ""
        } else {
            &source[self.cursor..]
        }
    }

    fn peek_char_from<'source>(&self, source: &'source str) -> Option<char> {
        self.remaining(source).chars().next()
    }

    fn peek_n_from<'source>(&self, source: &'source str, count: usize) -> &'source str {
        if count == 0 {
            return "";
        }
        let mut end = self.cursor;
        let mut consumed = 0;
        for (offset, ch) in source[self.cursor..].char_indices() {
            end = self.cursor + offset + ch.len_utf8();
            consumed += 1;
            if consumed >= count {
                break;
            }
        }
        &source[self.cursor..end]
    }

    fn advance_char<'source>(&mut self, ch: char, source: &'source str) -> Result<(), LexError> {
        let len = ch.len_utf8();
        let end = self.cursor + len;
        let bytes = &source.as_bytes()[self.cursor..end];
        self.ring_buffer.push_bytes(bytes, &self.checkpoints)?;
        self.cursor = end;
        if ch == '\n' {
            self.position.advance(len, 1, 1);
        } else {
            self.position.advance(len, 0, 1);
        }
        Ok(())
    }

    fn consume_while<'source, F>(
        &mut self,
        source: &'source str,
        mut predicate: F,
    ) -> Result<(), LexError>
    where
        F: FnMut(char) -> bool,
    {
        while let Some(ch) = self.peek_char_from(source) {
            if predicate(ch) {
                self.advance_char(ch, source)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    fn take_slice<'source>(&self, source: &'source str, start: usize, end: usize) -> &'source str {
        &source[start..end]
    }

    fn read_identifier<'source>(&mut self, source: &'source str) -> Result<&'source str, LexError> {
        let start = self.cursor;
        self.consume_while(source, |ch| ch.is_alphanumeric() || ch == '_')?;
        Ok(self.take_slice(source, start, self.cursor))
    }

    fn read_number<'source>(&mut self, source: &'source str) -> Result<&'source str, LexError> {
        let start = self.cursor;
        self.consume_while(source, |ch| {
            ch.is_ascii_digit() || matches!(ch, '.' | '_' | ',')
        })?;
        Ok(self.take_slice(source, start, self.cursor))
    }

    fn read_line_comment<'source>(&mut self, source: &'source str) -> Result<String, LexError> {
        let mut text = String::new();
        while let Some(ch) = self.peek_char_from(source) {
            if ch == '\n' {
                break;
            }
            text.push(ch);
            self.advance_char(ch, source)?;
        }
        Ok(text)
    }

    fn read_block_comment<'source>(&mut self, source: &'source str) -> Result<String, LexError> {
        let mut text = String::new();
        while let Some(ch) = self.peek_char_from(source) {
            self.advance_char(ch, source)?;
            if ch == '*' {
                if let Some('/') = self.peek_char_from(source) {
                    self.advance_char('/', source)?;
                    break;
                }
            }
            text.push(ch);
        }
        Ok(text)
    }

    fn read_string_literal<'source>(
        &mut self,
        source: &'source str,
    ) -> Result<&'source str, LexError> {
        let delimiter = self.peek_char_from(source).unwrap();
        let start = self.cursor;
        self.advance_char(delimiter, source)?;
        while let Some(ch) = self.peek_char_from(source) {
            self.advance_char(ch, source)?;
            if ch == delimiter {
                break;
            }
        }
        Ok(self.take_slice(source, start, self.cursor))
    }

    fn read_symbol<'source>(&mut self, source: &'source str) -> Result<&'source str, LexError> {
        let start = self.cursor;
        let multi_char_symbols = [
            "==", "!=", "<=", ">=", "&&", "||", "..=", "..", "->", "=>", "::", "?.", "?:",
        ];

        for symbol in multi_char_symbols {
            if self.remaining(source).starts_with(symbol) {
                for ch in symbol.chars() {
                    self.advance_char(ch, source)?;
                }
                return Ok(self.take_slice(source, start, self.cursor));
            }
        }

        if let Some(ch) = self.peek_char_from(source) {
            self.advance_char(ch, source)?;
        }
        Ok(self.take_slice(source, start, self.cursor))
    }

    fn consume_trivia<'source>(
        &mut self,
        source: &'source str,
    ) -> Result<Option<TokenTrivia>, LexError> {
        let mut saw_trivia = false;
        while let Some(ch) = self.peek_char_from(source) {
            match ch {
                ' ' | '\t' => {
                    saw_trivia = true;
                    self.trivia_tracker.record_space();
                    self.advance_char(ch, source)?;
                }
                '\r' => {
                    saw_trivia = true;
                    self.advance_char(ch, source)?;
                    if let Some('\n') = self.peek_char_from(source) {
                        self.advance_char('\n', source)?;
                    }
                    self.trivia_tracker.record_newline();
                }
                '\n' => {
                    saw_trivia = true;
                    self.advance_char(ch, source)?;
                    self.trivia_tracker.record_newline();
                }
                '/' => {
                    let ahead = self.peek_n_from(source, 2);
                    if ahead.starts_with("//") {
                        saw_trivia = true;
                        let line = self.position.line;
                        let column = self.position.column;
                        self.advance_char('/', source)?;
                        self.advance_char('/', source)?;
                        let text = self.read_line_comment(source)?;
                        self.trivia_tracker.record_line_comment(text, line, column);
                    } else if ahead.starts_with("/*") {
                        saw_trivia = true;
                        let line = self.position.line;
                        let column = self.position.column;
                        self.advance_char('/', source)?;
                        self.advance_char('*', source)?;
                        let text = self.read_block_comment(source)?;
                        self.trivia_tracker.record_block_comment(text, line, column);
                    } else {
                        break;
                    }
                }
                '#' => {
                    saw_trivia = true;
                    let line = self.position.line;
                    let column = self.position.column;
                    self.advance_char('#', source)?;
                    let text = self.read_line_comment(source)?;
                    self.trivia_tracker
                        .record_json_comment(text, line, column, false);
                }
                _ => break,
            }
        }

        if saw_trivia {
            Ok(self.trivia_tracker.take())
        } else {
            Ok(None)
        }
    }

    fn current_span(&self, start_offset: usize, start_position: ScannerPosition) -> Span {
        Span::new(start_offset..self.cursor, start_position, self.position)
    }

    fn determine_kind(first_char: char, slice: &str) -> RawTokenKind {
        if slice.is_empty() {
            RawTokenKind::Eof
        } else if first_char.is_ascii_digit() {
            RawTokenKind::NumberCandidate
        } else if first_char.is_ascii_alphabetic() || first_char == '_' {
            RawTokenKind::Identifier
        } else if slice.trim().is_empty() {
            RawTokenKind::Whitespace
        } else {
            RawTokenKind::Symbol
        }
    }
}

impl CharScannerStage for CharScanner {
    fn save_position(&mut self) -> ScannerPosition {
        let position = self.position;
        self.checkpoints.push(Checkpoint { position });
        position
    }

    fn restore_position(&mut self, position: ScannerPosition) {
        self.cursor = position.byte_offset;
        self.position = position;
    }

    fn discard_checkpoint(&mut self, position: ScannerPosition) {
        if let Some(index) = self
            .checkpoints
            .iter()
            .position(|checkpoint| checkpoint.position.byte_offset == position.byte_offset)
        {
            self.checkpoints.remove(index);
        }
    }

    fn commit_position(&mut self) {
        self.ring_buffer.commit(self.position.byte_offset);
    }

    fn scan_next_token<'source>(
        &mut self,
        ctx: &mut LexerContext<'source>,
    ) -> Result<RawToken<'source>, LexError> {
        self.synchronize_context(ctx);

        let source = ctx.source;
        let trivia = self.consume_trivia(source)?;

        if self.cursor >= source.len() {
            let span = Span::empty(self.position);
            ctx.update_position(self.position);
            return Ok(RawToken {
                kind: RawTokenKind::Eof,
                text: "",
                span,
                trivia,
            });
        }

        let start_offset = self.cursor;
        let start_position = self.position;
        let current_char = self.peek_char_from(source).unwrap_or('\0');

        let slice = if current_char.is_ascii_alphabetic() || current_char == '_' {
            self.read_identifier(source)?
        } else if current_char.is_ascii_digit() {
            self.read_number(source)?
        } else if matches!(current_char, '"' | '\'' | '`') {
            self.read_string_literal(source)?
        } else {
            self.read_symbol(source)?
        };

        let kind = Self::determine_kind(current_char, slice);
        let span = self.current_span(start_offset, start_position);
        ctx.update_position(self.position);

        Ok(RawToken {
            kind,
            text: slice,
            span,
            trivia,
        })
    }

    fn peek_char<'source>(&mut self, ctx: &mut LexerContext<'source>) -> Option<char> {
        self.peek_char_from(ctx.source)
    }

    fn peek_chars<'source>(
        &mut self,
        ctx: &mut LexerContext<'source>,
        count: usize,
    ) -> &'source str {
        let limit = count.min(self.lookahead_limit);
        self.peek_n_from(ctx.source, limit)
    }

    fn peek_until<'source, F>(
        &mut self,
        ctx: &mut LexerContext<'source>,
        predicate: F,
    ) -> &'source str
    where
        F: Fn(char) -> bool,
    {
        let mut end = self.cursor;
        let mut consumed = 0;
        for (offset, ch) in ctx.source[self.cursor..].char_indices() {
            if !predicate(ch) {
                break;
            }
            let next_end = self.cursor + offset + ch.len_utf8();
            consumed += ch.len_utf8();
            if consumed > self.lookahead_limit {
                break;
            }
            end = next_end;
        }
        &ctx.source[self.cursor..end]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipeline::pipeline::DEFAULT_LOOKAHEAD_LIMIT;

    #[test]
    fn scan_identifier_captures_trivia_and_span() {
        let mut scanner = CharScanner::new();
        let source = "  // keep\nval answer";
        let mut ctx = LexerContext::new(source);

        let token = scanner.scan_next_token(&mut ctx).expect("token");
        assert_eq!(token.kind, RawTokenKind::Identifier);
        assert_eq!(token.text, "val");

        let trivia = token.trivia.expect("expected leading trivia");
        assert!(trivia.comments, "comment trivia should be recorded");
        assert!(trivia.newlines >= 1, "newline trivia should be tracked");

        let expected_offset = source.find("val").expect("lexeme location");
        assert_eq!(token.span.start.byte_offset, expected_offset);
        assert_eq!(token.span.end.byte_offset, expected_offset + "val".len());
    }

    #[test]
    fn peek_limit_and_checkpoint_restore_roundtrip() {
        let mut scanner = CharScanner::new();
        let source = "a".repeat(DEFAULT_LOOKAHEAD_LIMIT + 64);
        let mut ctx = LexerContext::new(&source);

        let preview = scanner.peek_chars(&mut ctx, DEFAULT_LOOKAHEAD_LIMIT * 2);
        assert_eq!(preview.len(), DEFAULT_LOOKAHEAD_LIMIT);

        let checkpoint = scanner.save_position();
        let first = scanner.scan_next_token(&mut ctx).expect("first token");

        scanner.restore_position(checkpoint);
        ctx.update_position(checkpoint);
        let second = scanner.scan_next_token(&mut ctx).expect("second token");

        assert_eq!(first.kind, second.kind);
        assert_eq!(first.text, second.text);
        assert_eq!(first.span.byte_range, second.span.byte_range);

        scanner.discard_checkpoint(checkpoint);
    }
}
