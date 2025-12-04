//! 型アノテーションのパース（スタブ実装）。

use super::Parser;
use jv_ast::types::TypeAnnotation;

pub(crate) fn parse_type<'src, 'alloc>(
    _parser: &mut Parser<'src, 'alloc>,
) -> Option<TypeAnnotation> {
    None
}
