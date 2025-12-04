//! 式のパース（スタブ実装）。

use super::Parser;
use jv_ast::expression::Expression;

/// Prattパーサー本体（未実装）。
pub(crate) fn parse_expression<'src, 'alloc>(
    _parser: &mut Parser<'src, 'alloc>,
) -> Option<Expression> {
    None
}
