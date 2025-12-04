//! パターンマッチング構文のパース（スタブ実装）。

use super::Parser;
use jv_ast::binding_pattern::BindingPatternKind;

pub(crate) fn parse_pattern<'src, 'alloc>(
    _parser: &mut Parser<'src, 'alloc>,
) -> Option<BindingPatternKind> {
    None
}
