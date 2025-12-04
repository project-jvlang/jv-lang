//! エラー回復の簡易実装。

use super::Parser;
use crate::token::TokenKind;

/// 回復メトリクス（簡易カウンタ）。
#[derive(Debug, Default)]
pub struct RecoveryMetrics {
    pub recovered: usize,
}

/// 同期ポイントまで進める簡易回復。
pub(crate) fn recover_to_sync_point<'src, 'alloc>(parser: &mut Parser<'src, 'alloc>) {
    while !matches!(
        parser.current().kind,
        TokenKind::Semicolon | TokenKind::RightBrace | TokenKind::Eof
    ) {
        parser.advance();
    }
    parser.mark_recovered();
}
