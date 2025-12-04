//! 文のパース処理（暫定スタブ）。

use super::Parser;
use crate::token::TokenKind;
use jv_ast::{Span as AstSpan, statement::Program};

/// プログラム全体をパースする。
///
/// 現時点ではAST構築は行わず、トークンを消費するのみ。
pub(crate) fn parse_program<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) -> Option<Program> {
    while parser.current().kind != TokenKind::Eof {
        parser.advance();
    }

    Some(Program {
        package: None,
        imports: Vec::new(),
        statements: Vec::new(),
        span: AstSpan::dummy(),
    })
}
