use crate::{LayoutMode, LayoutSequenceKind, LexError, TokenType};

use super::types::ScannerPosition;

/// パイプライン全体で共有されるコンテキスト情報。

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RegexCommandScanState {
    pattern_start: usize,
    pattern_end: usize,
}

impl RegexCommandScanState {
    pub fn new(pattern_start: usize, pattern_end: usize) -> Self {
        Self {
            pattern_start,
            pattern_end,
        }
    }

    pub fn pattern_start(&self) -> usize {
        self.pattern_start
    }

    pub fn pattern_end(&self) -> usize {
        self.pattern_end
    }
}

#[derive(Debug)]
pub struct LexerContext<'source> {
    pub source: &'source str,
    pub current_position: ScannerPosition,
    pub lookahead_window: Option<&'source str>,
    pub emitted_tokens: usize,
    pub errors: Vec<LexError>,
    pub last_token_type: Option<TokenType>,
    regex_command_state: Option<RegexCommandScanState>,
    layout_stack: Vec<LayoutSequenceKind>,
    layout_mode: LayoutMode,
    trivia_newlines: u16,
}

impl<'source> LexerContext<'source> {
    pub fn new(source: &'source str) -> Self {
        Self::with_layout_mode(source, LayoutMode::Enabled)
    }

    pub fn with_layout_mode(source: &'source str, layout_mode: LayoutMode) -> Self {
        Self {
            source,
            current_position: ScannerPosition::default(),
            lookahead_window: None,
            emitted_tokens: 0,
            errors: Vec::new(),
            last_token_type: None,
            regex_command_state: None,
            layout_stack: Vec::new(),
            layout_mode,
            trivia_newlines: 0,
        }
    }

    pub fn update_position(&mut self, position: ScannerPosition) {
        self.current_position = position;
    }

    pub fn set_lookahead_window(&mut self, window: &'source str) {
        self.lookahead_window = Some(window);
    }

    pub fn clear_lookahead_window(&mut self) {
        self.lookahead_window = None;
    }

    pub fn increment_emitted(&mut self) {
        self.emitted_tokens = self.emitted_tokens.saturating_add(1);
    }

    pub fn record_error(&mut self, error: LexError) {
        self.errors.push(error);
    }

    pub fn last_token_type(&self) -> Option<&TokenType> {
        self.last_token_type.as_ref()
    }

    pub fn set_last_token_type(&mut self, token_type: Option<TokenType>) {
        self.last_token_type = token_type;
    }

    pub fn clear_last_token_type(&mut self) {
        self.last_token_type = None;
    }

    pub fn push_layout_sequence(&mut self, kind: LayoutSequenceKind) {
        if self.is_layout_disabled() {
            return;
        }
        self.layout_stack.push(kind);
    }

    pub fn regex_command_state(&self) -> Option<RegexCommandScanState> {
        self.regex_command_state
    }

    pub fn set_regex_command_state(&mut self, state: Option<RegexCommandScanState>) {
        self.regex_command_state = state;
    }

    pub fn clear_regex_command_state(&mut self) {
        self.regex_command_state = None;
    }

    pub fn pop_layout_sequence(&mut self, kind: LayoutSequenceKind) {
        if self.is_layout_disabled() {
            return;
        }
        match self.layout_stack.last().copied() {
            Some(last) if last == kind => {
                self.layout_stack.pop();
            }
            Some(_) => {
                if let Some(pos) = self.layout_stack.iter().rposition(|&entry| entry == kind) {
                    self.layout_stack.truncate(pos);
                }
            }
            None => {}
        }
    }

    pub fn current_layout_sequence(&self) -> Option<LayoutSequenceKind> {
        if self.is_layout_disabled() {
            return None;
        }
        self.layout_stack.last().copied()
    }

    pub fn layout_mode(&self) -> LayoutMode {
        self.layout_mode
    }

    pub fn is_layout_enabled(&self) -> bool {
        matches!(self.layout_mode, LayoutMode::Enabled)
    }

    pub fn is_layout_disabled(&self) -> bool {
        !self.is_layout_enabled()
    }

    pub fn set_trivia_newlines(&mut self, count: u16) {
        self.trivia_newlines = count;
    }

    pub fn trivia_newlines(&self) -> u16 {
        self.trivia_newlines
    }
}
