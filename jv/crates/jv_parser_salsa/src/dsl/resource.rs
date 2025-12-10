use crate::lower::{LoweringContext, LoweringDiagnostic};
use crate::parser::OwnedToken;

pub fn ensure_use_resource(
    ctx: &mut LoweringContext<'_>,
    keyword: &OwnedToken,
    has_resource: bool,
) {
    if !has_resource {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-004: use ブロックにはリソース式が必要です",
            Some(ctx.span_for_token(keyword)),
        ));
    }
}

pub fn ensure_defer_body(ctx: &mut LoweringContext<'_>, keyword: &OwnedToken, has_body: bool) {
    if !has_body {
        ctx.push_diagnostic(LoweringDiagnostic::error(
            "JV-DSL-005: defer ブロック本体が必要です",
            Some(ctx.span_for_token(keyword)),
        ));
    }
}
