use super::context::LexerContext;
use super::types::{ClassifiedToken, NormalizedToken, RawToken};
use crate::Token;

#[cfg(feature = "trace-stages")]
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(feature = "trace-stages")]
use tracing::{debug, info};

#[cfg(feature = "trace-stages")]
static TRACE_ENABLED: AtomicBool = AtomicBool::new(false);

#[cfg(feature = "trace-stages")]
#[inline]
pub fn enable() {
    TRACE_ENABLED.store(true, Ordering::Relaxed);
}

#[cfg(not(feature = "trace-stages"))]
#[inline]
pub fn enable() {}

#[cfg(feature = "trace-stages")]
#[inline]
pub fn disable() {
    TRACE_ENABLED.store(false, Ordering::Relaxed);
}

#[cfg(not(feature = "trace-stages"))]
#[inline]
pub fn disable() {}

#[cfg(feature = "trace-stages")]
#[inline]
fn is_enabled() -> bool {
    TRACE_ENABLED.load(Ordering::Relaxed)
}

#[cfg(not(feature = "trace-stages"))]
#[allow(dead_code)]
#[inline]
fn is_enabled() -> bool {
    false
}

#[cfg(feature = "trace-stages")]
fn preview_snippet(text: &str) -> String {
    const LIMIT: usize = 32;
    if text.chars().count() <= LIMIT {
        text.to_string()
    } else {
        let snippet: String = text.chars().take(LIMIT).collect();
        format!("{}...", snippet)
    }
}

#[cfg(not(feature = "trace-stages"))]
#[allow(dead_code)]
#[inline]
fn preview_snippet(text: &str) -> String {
    let _ = text;
    String::new()
}

#[inline]
pub fn stage_event(stage: &'static str, event: &'static str, ctx: &LexerContext<'_>) {
    #[cfg(feature = "trace-stages")]
    if is_enabled() {
        info!(
            target = "jv_lexer::pipeline",
            stage,
            event,
            emitted = ctx.emitted_tokens,
            byte_offset = ctx.current_position.byte_offset,
            line = ctx.current_position.line,
            column = ctx.current_position.column,
            "pipeline stage event"
        );
    }
    let _ = (stage, event, ctx);
}

#[inline]
pub fn raw_token(token: &RawToken<'_>) {
    #[cfg(feature = "trace-stages")]
    if is_enabled() {
        debug!(
            target = "jv_lexer::pipeline",
            stage = "char_scanner",
            kind = ?token.kind,
            start = token.span.byte_range.start,
            end = token.span.byte_range.end,
            preview = preview_snippet(token.text),
            "raw token"
        );
    }
    let _ = token;
}

#[inline]
pub fn normalized_token(token: &NormalizedToken<'_>) {
    #[cfg(feature = "trace-stages")]
    if is_enabled() {
        debug!(
            target = "jv_lexer::pipeline",
            stage = "normalizer",
            kind = ?token.raw.kind,
            len = token.normalized_text.len(),
            preview = preview_snippet(&token.normalized_text),
            diagnostics = token.metadata.provisional_diagnostics.len(),
            "normalized token"
        );
    }
    let _ = token;
}

#[inline]
pub fn classified_token(token: &ClassifiedToken<'_>) {
    #[cfg(feature = "trace-stages")]
    if is_enabled() {
        debug!(
            target = "jv_lexer::pipeline",
            stage = "classifier",
            token_type = ?token.token_type,
            diagnostics = token.diagnostics.len(),
            metadata = token.metadata.len(),
            "classified token"
        );
    }
    let _ = token;
}

#[inline]
pub fn emitted_tokens(tokens: &[Token]) {
    #[cfg(feature = "trace-stages")]
    if is_enabled() {
        for token in tokens {
            debug!(
                target = "jv_lexer::pipeline",
                stage = "emitter",
                token_type = ?token.token_type,
                lexeme = preview_snippet(&token.lexeme),
                line = token.line,
                column = token.column,
                "emitted token"
            );
        }
    }
    let _ = tokens;
}
