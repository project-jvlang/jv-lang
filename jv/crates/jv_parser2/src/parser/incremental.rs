//! インクリメンタルパースのスタブ。

use super::{ParseResult, Parser};

/// 差分情報（暫定）。
#[derive(Debug, Clone)]
pub struct TextChange {
    pub start: usize,
    pub old_end: usize,
    pub new_text: String,
    pub start_line: u32,
    pub end_line: u32,
}

/// 変更に基づき最小限の再パースを行う（現状はフルパースに委譲）。
pub(crate) fn parse_incremental<'src, 'alloc>(
    parser: &mut Parser<'src, 'alloc>,
    _change: &TextChange,
) -> ParseResult {
    parser.parse()
}
