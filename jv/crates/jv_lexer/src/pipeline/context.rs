use crate::{LexError, TokenType};

use super::types::ScannerPosition;

/// パイプライン全体で共有されるコンテキスト情報。
#[derive(Debug)]
pub struct LexerContext<'source> {
    pub source: &'source str,
    pub current_position: ScannerPosition,
    pub lookahead_window: Option<&'source str>,
    pub emitted_tokens: usize,
    pub errors: Vec<LexError>,
    pub last_token_type: Option<TokenType>,
}

impl<'source> LexerContext<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            current_position: ScannerPosition::default(),
            lookahead_window: None,
            emitted_tokens: 0,
            errors: Vec::new(),
            last_token_type: None,
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
}
