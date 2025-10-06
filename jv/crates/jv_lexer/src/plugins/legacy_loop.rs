use crate::{
    pipeline::{context::LexerContext, pipeline::TokenPlugin, types::ClassifiedToken},
    LegacyLoopKeyword, LexError, TokenDiagnostic, TokenType,
};

#[derive(Default)]
pub struct LegacyLoopPlugin;

impl TokenPlugin for LegacyLoopPlugin {
    fn name(&self) -> &'static str {
        "legacy-loop"
    }

    fn apply<'source>(
        &self,
        token: &mut ClassifiedToken<'source>,
        _ctx: &mut LexerContext<'source>,
    ) -> Result<(), LexError> {
        let diagnostic = match token.token_type {
            TokenType::While => Some(LegacyLoopKeyword::While),
            TokenType::Do => Some(LegacyLoopKeyword::Do),
            _ => None,
        };

        if let Some(keyword) = diagnostic {
            let diag = TokenDiagnostic::LegacyLoop { keyword };
            if !token.diagnostics.iter().any(|existing| existing == &diag) {
                token.diagnostics.push(diag);
            }
        }

        Ok(())
    }
}
