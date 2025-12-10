use crate::lower::{LoweringContext, LoweringDiagnostic};
use crate::parser::OwnedToken;

/// spawn/async の本体が存在するかを検証する。
pub fn ensure_concurrency_body(
    ctx: &mut LoweringContext<'_>,
    keyword: &OwnedToken,
    has_body: bool,
) {
    if !has_body {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-003: spawn/async ブロック本体が必要です",
            Some(ctx.span_for_token(keyword)),
        ));
    }
}
