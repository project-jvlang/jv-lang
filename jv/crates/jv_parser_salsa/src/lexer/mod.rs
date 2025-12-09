//! ゼロコピー Lexer のエントリポイント。
//!
//! 実装は既存の `jv_lexer` パイプラインを活用しつつ、ソースへの参照を保持した
//! トークン列を生成する薄いラッパーで構成する。

pub mod compat;
pub mod span;
pub mod token;
pub mod trivia;

pub use compat::{kind_from_token_type, token_type_from_kind};
pub use span::Span;
pub use token::{Token, TokenDiagnostic, TokenKind, TokenMetadata, TokenTrivia};

pub type LexError = jv_lexer::LexError;

/// ゼロコピーでトークンを反復する Lexer。
pub struct Lexer<'src> {
    source: &'src str,
    tokens: Vec<Token<'src>>,
    index: usize,
}

impl<'src> Lexer<'src> {
    /// ソースコードから Lexer を初期化し、トークン列を生成する。
    pub fn new(source: &'src str) -> Result<Self, LexError> {
        let mut base = jv_lexer::Lexer::new(source);
        let raw_tokens = base.tokenize()?;

        let tokens = build_zero_copy_tokens(&source, &raw_tokens);

        Ok(Self {
            source,
            tokens,
            index: 0,
        })
    }

    /// 次のトークンを返す。末尾に到達した場合は `None`。
    pub fn next_token(&mut self) -> Option<Token<'src>> {
        let token = self.tokens.get(self.index).cloned();
        if token.is_some() {
            self.index += 1;
        }
        token
    }

    /// 生成済みトークンを所有値として回収する。
    pub fn collect_owned_tokens(&self) -> Vec<crate::parser::OwnedToken> {
        self.tokens
            .iter()
            .map(|tok| crate::parser::OwnedToken {
                kind: tok.kind,
                span: tok.span,
                lexeme: tok.lexeme.to_string(),
                leading_trivia: tok.leading_trivia.clone(),
                metadata: tok.metadata.clone(),
                diagnostic: tok.diagnostic.clone(),
            })
            .collect()
    }

    /// ソース全体への参照を返す（デバッグや比較用）。
    pub fn source(&self) -> &'src str {
        self.source
    }
}

fn build_zero_copy_tokens<'src>(
    source: &'src str,
    raw_tokens: &[jv_lexer::Token],
) -> Vec<Token<'src>> {
    let line_starts = line_start_offsets(source);
    let mut tokens = Vec::with_capacity(raw_tokens.len());

    for raw in raw_tokens {
        let kind = compat::kind_from_token_type(&raw.token_type);

        let line_idx = raw
            .line
            .saturating_sub(1)
            .min(line_starts.len().saturating_sub(1));
        let line_start = line_starts[line_idx];
        let line_end = line_starts
            .get(line_idx + 1)
            .copied()
            .unwrap_or_else(|| source.len());
        let line_slice = &source[line_start..line_end];

        let column_offset = raw.column.saturating_sub(1);
        let byte_in_line = line_slice
            .char_indices()
            .nth(column_offset)
            .map(|(i, _)| i)
            .unwrap_or_else(|| line_slice.len());

        let start = line_start.saturating_add(byte_in_line);
        let end = start.saturating_add(raw.lexeme.len());

        let span = Span {
            start: start as u32,
            end: end.min(source.len()) as u32,
        };

        let lexeme = source
            .get(span.start as usize..span.end as usize)
            .unwrap_or("");
        tokens.push(Token {
            kind,
            span,
            lexeme,
            leading_trivia: raw.leading_trivia.clone(),
            metadata: raw.metadata.clone(),
            diagnostic: raw.diagnostic.clone(),
        });
    }

    tokens
}

fn line_start_offsets(source: &str) -> Vec<usize> {
    let mut offsets = Vec::with_capacity(128);
    offsets.push(0);
    for (idx, ch) in source.char_indices() {
        if ch == '\n' {
            offsets.push(idx + 1);
        }
    }
    offsets
}

#[cfg(test)]
mod tests;
