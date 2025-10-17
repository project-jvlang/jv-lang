use std::collections::VecDeque;

use crate::{
    pipeline::{
        context::LexerContext,
        pipeline::CharScannerStage,
        pipeline::DEFAULT_LOOKAHEAD_LIMIT,
        types::{RawToken, RawTokenKind, ScannerPosition, Span},
    },
    CommentCarryOverMetadata, JsonCommentTrivia, JsonCommentTriviaKind, LexError,
    SourceCommentKind, SourceCommentTrivia, TokenTrivia, TokenType,
};
use unicode_ident::{is_xid_continue, is_xid_start};

const MULTI_CHAR_SYMBOLS: &[&str] = &[
    "..=", "=>", "->", "?.", "?:", "::", "==", "!=", "<=", ">=", "&&", "||", "..",
];

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
    pending_comment_trivia: Option<TokenTrivia>,
    pending_comment_carry: Option<CommentCarryOverMetadata>,
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
            pending_comment_trivia: None,
            pending_comment_carry: None,
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
        self.pending_comment_trivia = None;
        self.pending_comment_carry = None;
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

    fn is_identifier_start(ch: char) -> bool {
        ch == '_' || is_xid_start(ch)
    }

    fn is_identifier_continue(ch: char) -> bool {
        ch == '_' || ch.is_ascii_digit() || is_xid_continue(ch)
    }

    fn peek_char_offset<'source>(&self, source: &'source str, offset: usize) -> Option<char> {
        self.remaining(source).chars().nth(offset)
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

    fn consume_radix_digits<'source>(
        &mut self,
        source: &'source str,
        radix: u32,
    ) -> Result<bool, LexError> {
        let mut consumed_digit = false;
        let mut last_was_underscore = false;

        while let Some(ch) = self.peek_char_from(source) {
            if ch == '_' {
                if let Some(next) = self.peek_char_offset(source, 1) {
                    if next.to_digit(radix).is_some() {
                        self.advance_char(ch, source)?;
                        last_was_underscore = true;
                        continue;
                    }
                }
                return Err(LexError::UnexpectedChar(
                    '_',
                    self.position.line,
                    self.position.column,
                ));
            }

            if ch.to_digit(radix).is_some() {
                self.advance_char(ch, source)?;
                consumed_digit = true;
                last_was_underscore = false;
            } else {
                break;
            }
        }

        if last_was_underscore {
            return Err(LexError::UnexpectedChar(
                '_',
                self.position.line,
                self.position.column,
            ));
        }

        Ok(consumed_digit)
    }

    fn take_slice<'source>(&self, source: &'source str, start: usize, end: usize) -> &'source str {
        &source[start..end]
    }

    fn read_identifier<'source>(&mut self, source: &'source str) -> Result<&'source str, LexError> {
        let start = self.cursor;

        if let Some(ch) = self.peek_char_from(source) {
            self.advance_char(ch, source)?;
        }

        while let Some(ch) = self.peek_char_from(source) {
            if Self::is_identifier_continue(ch) {
                self.advance_char(ch, source)?;
            } else {
                break;
            }
        }

        Ok(self.take_slice(source, start, self.cursor))
    }

    fn read_number<'source>(&mut self, source: &'source str) -> Result<&'source str, LexError> {
        let start = self.cursor;
        let remaining = self.remaining(source);

        if remaining.starts_with("0x") || remaining.starts_with("0X") {
            self.advance_char('0', source)?;
            self.advance_char(remaining.chars().nth(1).unwrap(), source)?;
            if !self.consume_radix_digits(source, 16)? {
                return Err(LexError::UnexpectedChar(
                    'x',
                    self.position.line,
                    self.position.column,
                ));
            }
            self.consume_numeric_suffix(source)?;
            return Ok(self.take_slice(source, start, self.cursor));
        }

        if remaining.starts_with("0b") || remaining.starts_with("0B") {
            self.advance_char('0', source)?;
            self.advance_char(remaining.chars().nth(1).unwrap(), source)?;
            if !self.consume_radix_digits(source, 2)? {
                return Err(LexError::UnexpectedChar(
                    'b',
                    self.position.line,
                    self.position.column,
                ));
            }
            self.consume_numeric_suffix(source)?;
            return Ok(self.take_slice(source, start, self.cursor));
        }

        if remaining.starts_with("0o") || remaining.starts_with("0O") {
            self.advance_char('0', source)?;
            self.advance_char(remaining.chars().nth(1).unwrap(), source)?;
            if !self.consume_radix_digits(source, 8)? {
                return Err(LexError::UnexpectedChar(
                    'o',
                    self.position.line,
                    self.position.column,
                ));
            }
            self.consume_numeric_suffix(source)?;
            return Ok(self.take_slice(source, start, self.cursor));
        }

        let mut saw_digit = false;
        let mut saw_dot = false;
        let mut saw_exponent = false;
        let mut exponent_digits = 0;
        let mut trailing_separator: Option<char> = None;

        while let Some(ch) = self.peek_char_from(source) {
            match ch {
                '0'..='9' => {
                    self.advance_char(ch, source)?;
                    saw_digit = true;
                    trailing_separator = None;
                    if saw_exponent {
                        exponent_digits += 1;
                    }
                }
                ',' => {
                    if saw_exponent {
                        break;
                    }
                    let mut digits_after = 0;
                    let remaining_bytes = self.remaining(source);
                    if remaining_bytes.len() > 1 {
                        for c in remaining_bytes[1..].chars() {
                            if c.is_ascii_digit() {
                                digits_after += 1;
                            } else {
                                break;
                            }
                        }
                    }

                    if digits_after >= 3 {
                        self.advance_char(ch, source)?;
                        trailing_separator = Some(',');
                    } else {
                        break;
                    }
                }
                '_' => {
                    if let Some(next) = self.peek_char_offset(source, 1) {
                        if next.is_ascii_digit() {
                            self.advance_char(ch, source)?;
                            trailing_separator = Some('_');
                            continue;
                        }
                    }
                    return Err(LexError::UnexpectedChar(
                        '_',
                        self.position.line,
                        self.position.column,
                    ));
                }
                '.' => {
                    if saw_dot || saw_exponent {
                        return Err(LexError::UnexpectedChar(
                            '.',
                            self.position.line,
                            self.position.column,
                        ));
                    }
                    let after = self.peek_char_offset(source, 1);
                    if matches!(after, Some('.') | Some('=')) {
                        break;
                    }
                    if after.map(|c| c.is_ascii_digit()).unwrap_or(false) {
                        self.advance_char(ch, source)?;
                        saw_dot = true;
                        trailing_separator = None;
                    } else {
                        break;
                    }
                }
                'e' | 'E' => {
                    if saw_exponent {
                        break;
                    }
                    let mut lookahead = 1;
                    if let Some(sign) = self.peek_char_offset(source, 1) {
                        if matches!(sign, '+' | '-') {
                            lookahead += 1;
                        }
                    }
                    if self
                        .peek_char_offset(source, lookahead)
                        .map(|c| c.is_ascii_digit())
                        .unwrap_or(false)
                    {
                        self.advance_char(ch, source)?;
                        if lookahead > 1 {
                            let sign = self.peek_char_from(source).unwrap();
                            self.advance_char(sign, source)?;
                        }
                        saw_exponent = true;
                        exponent_digits = 0;
                        trailing_separator = None;
                    } else {
                        break;
                    }
                }
                'f' | 'F' | 'd' | 'D' | 'l' | 'L' => break,
                _ => break,
            }
        }

        if let Some(separator) = trailing_separator {
            return Err(LexError::UnexpectedChar(
                separator,
                self.position.line,
                self.position.column,
            ));
        }

        if !saw_digit {
            return Err(LexError::UnexpectedChar(
                self.peek_char_from(source).unwrap_or('\0'),
                self.position.line,
                self.position.column,
            ));
        }

        if saw_exponent && exponent_digits == 0 {
            return Err(LexError::UnexpectedChar(
                'e',
                self.position.line,
                self.position.column,
            ));
        }

        self.consume_numeric_suffix(source)?;
        Ok(self.take_slice(source, start, self.cursor))
    }

    fn consume_numeric_suffix(&mut self, source: &str) -> Result<(), LexError> {
        if let Some(suffix) = self.peek_char_from(source) {
            if matches!(suffix, 'f' | 'F' | 'd' | 'D' | 'l' | 'L') {
                let next_is_ident = self
                    .peek_char_offset(source, 1)
                    .map(Self::is_identifier_continue)
                    .unwrap_or(false);
                if !next_is_ident {
                    self.advance_char(suffix, source)?;
                }
            }
        }
        Ok(())
    }

    fn read_line_comment<'source>(
        &mut self,
        source: &'source str,
    ) -> Result<&'source str, LexError> {
        let start = self.cursor;
        self.advance_char('/', source)?;
        self.advance_char('/', source)?;

        while let Some(ch) = self.peek_char_from(source) {
            if ch == '\n' {
                break;
            }
            self.advance_char(ch, source)?;
        }

        Ok(self.take_slice(source, start, self.cursor))
    }

    fn read_block_comment<'source>(
        &mut self,
        source: &'source str,
    ) -> Result<&'source str, LexError> {
        let start = self.cursor;
        let start_line = self.position.line;
        let start_column = self.position.column;

        self.advance_char('/', source)?;
        self.advance_char('*', source)?;

        loop {
            let Some(ch) = self.peek_char_from(source) else {
                return Err(LexError::UnterminatedString(start_line, start_column));
            };

            self.advance_char(ch, source)?;
            if ch == '*' && self.peek_char_from(source) == Some('/') {
                self.advance_char('/', source)?;
                break;
            }
        }

        Ok(self.take_slice(source, start, self.cursor))
    }

    fn read_jv_only_block_comment<'source>(
        &mut self,
        source: &'source str,
    ) -> Result<&'source str, LexError> {
        let start = self.cursor;
        let start_line = self.position.line;
        let start_column = self.position.column;

        self.advance_char('/', source)?;
        self.advance_char('/', source)?;
        if self.peek_char_from(source) == Some('/') {
            self.advance_char('/', source)?;
        }

        if self.peek_char_from(source) != Some('*') {
            return Ok(self.take_slice(source, start, self.cursor));
        }
        self.advance_char('*', source)?;

        loop {
            let Some(ch) = self.peek_char_from(source) else {
                return Err(LexError::UnterminatedString(start_line, start_column));
            };

            self.advance_char(ch, source)?;
            if ch == '*'
                && self.peek_char_from(source) == Some('/')
                && self.peek_char_offset(source, 1) == Some('/')
            {
                self.advance_char('/', source)?;
                self.advance_char('/', source)?;
                break;
            }
        }

        Ok(self.take_slice(source, start, self.cursor))
    }

    fn read_hash_comment<'source>(
        &mut self,
        source: &'source str,
    ) -> Result<&'source str, LexError> {
        let start = self.cursor;
        self.advance_char('#', source)?;
        while let Some(ch) = self.peek_char_from(source) {
            if ch == '\n' {
                break;
            }
            self.advance_char(ch, source)?;
        }
        Ok(self.take_slice(source, start, self.cursor))
    }

    fn is_jv_block_comment(rest: &str) -> bool {
        rest.contains("*//")
    }

    fn build_line_comment_carry(
        &self,
        text: &str,
        line: usize,
        column: usize,
        is_jv_only: bool,
    ) -> CommentCarryOverMetadata {
        let mut carry = CommentCarryOverMetadata::default();
        if is_jv_only {
            carry.jv_only.push(SourceCommentTrivia {
                kind: SourceCommentKind::Line,
                text: text.to_string(),
                line,
                column,
            });
        } else {
            let sanitized = Self::sanitize_line_comment(text);
            if !sanitized.is_empty() {
                carry.json.push(JsonCommentTrivia {
                    kind: JsonCommentTriviaKind::Line,
                    text: sanitized,
                    line,
                    column,
                });
            }
            carry.passthrough.push(SourceCommentTrivia {
                kind: SourceCommentKind::Line,
                text: text.to_string(),
                line,
                column,
            });
        }
        carry
    }

    fn build_hash_comment_carry(
        &self,
        text: &str,
        line: usize,
        column: usize,
    ) -> CommentCarryOverMetadata {
        let mut carry = CommentCarryOverMetadata::default();
        let sanitized = text.trim_start_matches('#').trim().to_string();
        if !sanitized.is_empty() {
            carry.json.push(JsonCommentTrivia {
                kind: JsonCommentTriviaKind::Line,
                text: sanitized,
                line,
                column,
            });
        }
        carry
    }

    fn build_block_comment_carry(
        &self,
        text: &str,
        line: usize,
        column: usize,
        is_javadoc: bool,
    ) -> CommentCarryOverMetadata {
        let mut carry = CommentCarryOverMetadata::default();
        if is_javadoc {
            let doc = text.trim_start_matches('/').to_string();
            carry.doc_comment = Some(doc);
        } else {
            let sanitized = Self::sanitize_block_comment(text);
            if !sanitized.is_empty() {
                carry.json.push(JsonCommentTrivia {
                    kind: JsonCommentTriviaKind::Block,
                    text: sanitized,
                    line,
                    column,
                });
            }
            carry.passthrough.push(SourceCommentTrivia {
                kind: SourceCommentKind::Block,
                text: text.to_string(),
                line,
                column,
            });
        }
        carry
    }

    fn build_jv_block_comment_carry(
        &self,
        text: &str,
        line: usize,
        column: usize,
    ) -> CommentCarryOverMetadata {
        let mut carry = CommentCarryOverMetadata::default();
        carry.jv_only.push(SourceCommentTrivia {
            kind: SourceCommentKind::Block,
            text: text.to_string(),
            line,
            column,
        });
        carry
    }

    fn sanitize_line_comment(text: &str) -> String {
        let trimmed = text.trim_start();
        if let Some(rest) = trimmed.strip_prefix("//") {
            rest.trim().to_string()
        } else if let Some(rest) = trimmed.strip_prefix('#') {
            rest.trim().to_string()
        } else {
            trimmed.trim().to_string()
        }
    }

    fn sanitize_block_comment(text: &str) -> String {
        text.trim()
            .trim_start_matches("/*")
            .trim_end_matches("*/")
            .trim()
            .to_string()
    }

    fn read_string_literal<'source>(
        &mut self,
        source: &'source str,
    ) -> Result<&'source str, LexError> {
        let start = self.cursor;
        let start_line = self.position.line;
        let start_column = self.position.column;
        let remaining = self.remaining(source);

        if remaining.starts_with('\'') {
            return Err(LexError::UnexpectedChar('\'', start_line, start_column));
        }

        let (opening, closing, is_raw) = if remaining.starts_with("```") {
            ("```", "```", true)
        } else if remaining.starts_with("\"\"\"") {
            ("\"\"\"", "\"\"\"", false)
        } else if remaining.starts_with("\"") {
            ("\"", "\"", false)
        } else if remaining.starts_with("'") {
            ("'", "'", false)
        } else {
            ("\"", "\"", false)
        };

        for ch in opening.chars() {
            self.advance_char(ch, source)?;
        }

        loop {
            if self.remaining(source).is_empty() {
                return Err(LexError::UnterminatedString(start_line, start_column));
            }

            if self.remaining(source).starts_with(closing) {
                for ch in closing.chars() {
                    self.advance_char(ch, source)?;
                }
                break;
            }

            let ch = self.peek_char_from(source).unwrap();
            self.advance_char(ch, source)?;

            if !is_raw && ch == '\\' {
                let Some(escaped) = self.peek_char_from(source) else {
                    return Err(LexError::UnterminatedString(start_line, start_column));
                };
                self.advance_char(escaped, source)?;
            }
        }

        Ok(self.take_slice(source, start, self.cursor))
    }

    fn regex_cannot_follow(token: &TokenType) -> bool {
        matches!(
            token,
            TokenType::Identifier(_)
                | TokenType::Number(_)
                | TokenType::String(_)
                | TokenType::StringInterpolation(_)
                | TokenType::StringEnd
                | TokenType::Boolean(_)
                | TokenType::True
                | TokenType::False
                | TokenType::Null
                | TokenType::RightParen
                | TokenType::RightBracket
                | TokenType::RightBrace
                | TokenType::RegexLiteral(_)
        )
    }

    fn can_start_regex(ctx: &LexerContext<'_>) -> bool {
        match ctx.last_token_type() {
            None => true,
            Some(token) => !Self::regex_cannot_follow(token),
        }
    }

    fn read_regex_literal<'source>(
        &mut self,
        source: &'source str,
    ) -> Result<&'source str, LexError> {
        let start = self.cursor;
        let start_line = self.position.line;
        let start_column = self.position.column;

        self.advance_char('/', source)?;
        let mut escaped = false;

        while let Some(ch) = self.peek_char_from(source) {
            if ch == '\n' || ch == '\r' {
                return Err(LexError::UnterminatedRegex {
                    line: start_line,
                    column: start_column,
                });
            }

            if ch == '\t' {
                return Err(LexError::InvalidRegexCharacter {
                    character: '\t',
                    line: start_line,
                    column: start_column,
                });
            }

            self.advance_char(ch, source)?;

            if escaped {
                escaped = false;
                continue;
            }

            if ch == '\\' {
                escaped = true;
                continue;
            }

            if ch == '/' {
                return Ok(self.take_slice(source, start, self.cursor));
            }
        }

        Err(LexError::UnterminatedRegex {
            line: start_line,
            column: start_column,
        })
    }

    fn push_comment_trivia(&mut self, trivia: TokenTrivia) {
        if let Some(existing) = &mut self.pending_comment_trivia {
            Self::merge_trivia(existing, trivia);
        } else {
            self.pending_comment_trivia = Some(trivia);
        }
    }

    fn push_comment_carry(&mut self, carry: CommentCarryOverMetadata) {
        if let Some(existing) = &mut self.pending_comment_carry {
            Self::merge_carry(existing, carry);
        } else {
            self.pending_comment_carry = Some(carry);
        }
    }

    fn merge_carry(target: &mut CommentCarryOverMetadata, mut incoming: CommentCarryOverMetadata) {
        target.passthrough.append(&mut incoming.passthrough);
        target.jv_only.append(&mut incoming.jv_only);
        target.json.append(&mut incoming.json);
        if incoming.doc_comment.is_some() {
            target.doc_comment = incoming.doc_comment;
        }
    }

    fn merge_trivia(target: &mut TokenTrivia, mut incoming: TokenTrivia) {
        target.spaces = target.spaces.saturating_add(incoming.spaces);
        target.newlines = target.newlines.saturating_add(incoming.newlines);
        target.comments |= incoming.comments;
        target.json_comments.append(&mut incoming.json_comments);
        if incoming.doc_comment.is_some() {
            target.doc_comment = incoming.doc_comment;
        }
        target
            .passthrough_comments
            .append(&mut incoming.passthrough_comments);
        target.jv_comments.append(&mut incoming.jv_comments);
    }

    fn build_line_comment_trivia(
        &self,
        text: &str,
        line: usize,
        column: usize,
        is_jv_only: bool,
    ) -> TokenTrivia {
        let mut trivia = TokenTrivia::default();
        trivia.comments = true;
        if is_jv_only {
            trivia.jv_comments.push(SourceCommentTrivia {
                kind: SourceCommentKind::Line,
                text: text.to_string(),
                line,
                column,
            });
        } else {
            let sanitized = Self::sanitize_line_comment(text);
            if !sanitized.is_empty() {
                trivia.json_comments.push(JsonCommentTrivia {
                    kind: JsonCommentTriviaKind::Line,
                    text: sanitized,
                    line,
                    column,
                });
            }
            trivia.passthrough_comments.push(SourceCommentTrivia {
                kind: SourceCommentKind::Line,
                text: text.to_string(),
                line,
                column,
            });
        }
        trivia
    }

    fn build_hash_comment_trivia(&self, text: &str, line: usize, column: usize) -> TokenTrivia {
        let mut trivia = TokenTrivia::default();
        trivia.comments = true;
        let sanitized = text.trim_start_matches('#').trim().to_string();
        if !sanitized.is_empty() {
            trivia.json_comments.push(JsonCommentTrivia {
                kind: JsonCommentTriviaKind::Line,
                text: sanitized,
                line,
                column,
            });
        }
        trivia
    }

    fn build_block_comment_trivia(
        &self,
        text: &str,
        line: usize,
        column: usize,
        is_javadoc: bool,
    ) -> TokenTrivia {
        let mut trivia = TokenTrivia::default();
        trivia.comments = true;
        if is_javadoc {
            let doc = text.trim_start_matches('/').to_string();
            trivia.doc_comment = Some(doc);
        } else {
            let sanitized = Self::sanitize_block_comment(text);
            if !sanitized.is_empty() {
                trivia.json_comments.push(JsonCommentTrivia {
                    kind: JsonCommentTriviaKind::Block,
                    text: sanitized,
                    line,
                    column,
                });
            }
            trivia.passthrough_comments.push(SourceCommentTrivia {
                kind: SourceCommentKind::Block,
                text: text.to_string(),
                line,
                column,
            });
        }
        trivia
    }

    fn build_jv_block_comment_trivia(&self, text: &str, line: usize, column: usize) -> TokenTrivia {
        let mut trivia = TokenTrivia::default();
        trivia.comments = true;
        trivia.jv_comments.push(SourceCommentTrivia {
            kind: SourceCommentKind::Block,
            text: text.to_string(),
            line,
            column,
        });
        trivia
    }

    fn read_symbol<'source>(&mut self, source: &'source str) -> Result<&'source str, LexError> {
        let start = self.cursor;

        for symbol in MULTI_CHAR_SYMBOLS {
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
        } else if Self::is_identifier_start(first_char) {
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
            let mut final_trivia = trivia;
            if let Some(mut comment_trivia) = self.pending_comment_trivia.take() {
                if let Some(existing) = final_trivia.take() {
                    Self::merge_trivia(&mut comment_trivia, existing);
                }
                final_trivia = Some(comment_trivia);
            }
            let carry_over = self.pending_comment_carry.take();
            return Ok(RawToken {
                kind: RawTokenKind::Eof,
                text: "",
                span,
                trivia: final_trivia,
                carry_over,
            });
        }

        let start_offset = self.cursor;
        let start_position = self.position;
        let remaining = self.remaining(source);

        if remaining.starts_with("//") {
            let rest = &remaining[2..];
            let line = start_position.line;
            let column = start_position.column;
            let is_jv_block = Self::is_jv_block_comment(rest);
            let (slice, trivia_piece, carry_piece) = if is_jv_block {
                let slice = self.read_jv_only_block_comment(source)?;
                let trivia_piece = self.build_jv_block_comment_trivia(slice, line, column);
                let carry_piece = self.build_jv_block_comment_carry(slice, line, column);
                (slice, trivia_piece, carry_piece)
            } else {
                let slice = self.read_line_comment(source)?;
                let is_jv_only = slice.starts_with("///") || slice.starts_with("//*");
                let trivia_piece = self.build_line_comment_trivia(slice, line, column, is_jv_only);
                let carry_piece = self.build_line_comment_carry(slice, line, column, is_jv_only);
                (slice, trivia_piece, carry_piece)
            };
            if !is_jv_block {
                self.push_comment_trivia(trivia_piece);
                self.push_comment_carry(carry_piece.clone());
            }
            let span = self.current_span(start_offset, start_position);
            ctx.update_position(self.position);
            return Ok(RawToken {
                kind: RawTokenKind::CommentCandidate,
                text: slice,
                span,
                trivia,
                carry_over: (!is_jv_block).then_some(carry_piece),
            });
        }

        if remaining.starts_with("/*") {
            let line = start_position.line;
            let column = start_position.column;
            let slice = self.read_block_comment(source)?;
            let is_javadoc = slice.starts_with("/**");
            let trivia_piece = self.build_block_comment_trivia(slice, line, column, is_javadoc);
            let carry_piece = self.build_block_comment_carry(slice, line, column, is_javadoc);
            self.push_comment_trivia(trivia_piece);
            self.push_comment_carry(carry_piece.clone());
            let span = self.current_span(start_offset, start_position);
            ctx.update_position(self.position);
            return Ok(RawToken {
                kind: RawTokenKind::CommentCandidate,
                text: slice,
                span,
                trivia,
                carry_over: Some(carry_piece),
            });
        }

        if remaining.starts_with('#') {
            let slice = self.read_hash_comment(source)?;
            let trivia_piece =
                self.build_hash_comment_trivia(slice, start_position.line, start_position.column);
            let carry_piece =
                self.build_hash_comment_carry(slice, start_position.line, start_position.column);
            self.push_comment_trivia(trivia_piece);
            self.push_comment_carry(carry_piece.clone());
            let span = self.current_span(start_offset, start_position);
            ctx.update_position(self.position);
            return Ok(RawToken {
                kind: RawTokenKind::CommentCandidate,
                text: slice,
                span,
                trivia,
                carry_over: Some(carry_piece),
            });
        }

        let current_char = self.peek_char_from(source).unwrap_or('\0');

        let mut kind_override = None;
        let slice = if Self::is_identifier_start(current_char) {
            self.read_identifier(source)?
        } else if current_char.is_ascii_digit() {
            self.read_number(source)?
        } else if current_char == '/' && Self::can_start_regex(ctx) {
            kind_override = Some(RawTokenKind::RegexCandidate);
            self.read_regex_literal(source)?
        } else if matches!(current_char, '"' | '\'' | '`') {
            self.read_string_literal(source)?
        } else {
            self.read_symbol(source)?
        };

        let kind = kind_override.unwrap_or_else(|| Self::determine_kind(current_char, slice));
        let span = self.current_span(start_offset, start_position);
        ctx.update_position(self.position);

        let mut final_trivia = trivia;
        if let Some(mut comment_trivia) = self.pending_comment_trivia.take() {
            if let Some(existing) = final_trivia.take() {
                Self::merge_trivia(&mut comment_trivia, existing);
            }
            final_trivia = Some(comment_trivia);
        }

        let carry_over = self.pending_comment_carry.take();

        Ok(RawToken {
            kind,
            text: slice,
            span,
            trivia: final_trivia,
            carry_over,
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

        let comment = scanner.scan_next_token(&mut ctx).expect("comment token");
        assert_eq!(comment.kind, RawTokenKind::CommentCandidate);
        assert!(comment.text.starts_with("//"));

        let token = scanner.scan_next_token(&mut ctx).expect("identifier");
        assert_eq!(token.kind, RawTokenKind::Identifier);
        assert_eq!(token.text, "val");

        let trivia = token.trivia.expect("expected leading trivia");
        assert!(
            trivia.comments,
            "leading trivia should now include comments"
        );
        assert!(trivia.newlines >= 1, "newline trivia should be tracked");
        assert_eq!(trivia.passthrough_comments.len(), 1);
        assert_eq!(trivia.passthrough_comments[0].text, "// keep");

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

    #[test]
    fn scan_unicode_identifier_and_keyword_split() {
        let mut scanner = CharScanner::new();
        let source = "ναῦς val";
        let mut ctx = LexerContext::new(source);

        let greek = scanner.scan_next_token(&mut ctx).expect("greek token");
        assert_eq!(greek.kind, RawTokenKind::Identifier);
        assert_eq!(greek.text, "ναῦς");

        let latin = scanner.scan_next_token(&mut ctx).expect("latin token");
        assert_eq!(latin.text, "val");
    }

    #[test]
    fn scan_radix_and_grouped_numbers() {
        let mut scanner = CharScanner::new();
        let source = "0x1F 1_234,567 1.2e+3";
        let mut ctx = LexerContext::new(source);

        let hex = scanner.scan_next_token(&mut ctx).expect("hex token");
        assert_eq!(hex.text, "0x1F");

        let decimal = scanner.scan_next_token(&mut ctx).expect("decimal token");
        assert_eq!(decimal.text, "1_234,567");

        let exponent = scanner.scan_next_token(&mut ctx).expect("exponent token");
        assert_eq!(exponent.text, "1.2e+3");
        assert_eq!(exponent.kind, RawTokenKind::NumberCandidate);
    }

    #[test]
    fn scan_comment_candidates() {
        let mut scanner = CharScanner::new();
        let source = "# json\n///*doc*//\nval";
        let mut ctx = LexerContext::new(source);

        let hash_comment = scanner.scan_next_token(&mut ctx).expect("hash comment");
        assert_eq!(hash_comment.kind, RawTokenKind::CommentCandidate);
        assert_eq!(hash_comment.text, "# json");

        let jv_comment = scanner.scan_next_token(&mut ctx).expect("jv block comment");
        assert_eq!(jv_comment.kind, RawTokenKind::CommentCandidate);
        assert!(jv_comment.text.contains("doc"));

        let identifier = scanner.scan_next_token(&mut ctx).expect("identifier");
        assert_eq!(identifier.kind, RawTokenKind::Identifier);
        assert_eq!(identifier.text, "val");
        assert!(identifier.trivia.is_some());
    }

    #[test]
    fn comment_carry_over_attaches_to_next_token() {
        let mut scanner = CharScanner::new();
        let source = "// note\nval";
        let mut ctx = LexerContext::new(source);

        let comment = scanner.scan_next_token(&mut ctx).expect("comment token");
        assert_eq!(comment.kind, RawTokenKind::CommentCandidate);
        assert!(comment.carry_over.is_some());

        let identifier = scanner.scan_next_token(&mut ctx).expect("identifier token");
        assert_eq!(identifier.kind, RawTokenKind::Identifier);
        let trivia = identifier.trivia.expect("identifier trivia");
        assert!(trivia.comments);
        assert!(trivia.newlines >= 1);
        let carry = identifier.carry_over.expect("carry metadata");
        assert_eq!(carry.passthrough.len(), 1);
        assert_eq!(carry.passthrough[0].text, "// note");
        assert_eq!(carry.json.len(), 1);
        assert_eq!(carry.json[0].text, "note");
    }

    #[test]
    fn string_literal_handles_triple_and_backtick() {
        let mut scanner = CharScanner::new();
        let source = "\"\"\"hello\"\"\" ```raw```";
        let mut ctx = LexerContext::new(source);

        let triple = scanner.scan_next_token(&mut ctx).expect("triple");
        assert_eq!(triple.text, "\"\"\"hello\"\"\"");
        assert_eq!(triple.kind, RawTokenKind::Symbol);

        let raw = scanner.scan_next_token(&mut ctx).expect("raw");
        assert_eq!(raw.text, "```raw```");
        assert_eq!(raw.kind, RawTokenKind::Symbol);
    }
}
