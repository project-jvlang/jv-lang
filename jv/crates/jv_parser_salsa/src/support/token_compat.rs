use crate::lexer::{Span as LexerSpan, token_type_from_kind};
use crate::parser::OwnedToken;
use crate::support::trivia::Trivia;
use jv_ast::Span;
use jv_lexer::{SourceCommentKind, SourceCommentTrivia, Token as LegacyToken, TokenTrivia};

/// 行頭オフセットを保持し、バイトオフセットから行・桁を算出する。
#[derive(Debug, Clone)]
pub struct LineIndex {
    line_starts: Vec<usize>,
}

impl LineIndex {
    /// ソースコードから行開始オフセットを構築する。
    pub fn new(source: &str) -> Self {
        let mut line_starts = Vec::with_capacity(128);
        line_starts.push(0);
        for (idx, ch) in source.char_indices() {
            if ch == '\n' {
                line_starts.push(idx + 1);
            }
        }
        Self { line_starts }
    }

    /// バイトオフセットを 1 始まりの (行, 桁) へ変換する。
    pub fn offset_to_line_col(&self, offset: usize) -> (usize, usize) {
        let idx = self.line_starts.partition_point(|&start| start <= offset);
        let line_idx = idx.saturating_sub(1);
        let line_start = *self.line_starts.get(line_idx).unwrap_or(&0);
        let column = offset.saturating_sub(line_start);
        (line_idx + 1, column + 1)
    }

    /// lexer スパンを AST スパンへ変換する。
    pub fn span_to_ast_span(&self, span: LexerSpan) -> Span {
        let (start_line, start_column) = self.offset_to_line_col(span.start as usize);
        let (end_line, end_column) = self.offset_to_line_col(span.end as usize);
        Span {
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }
}

/// OwnedToken をレガシーな `jv_lexer::Token` へ変換する。
pub fn to_legacy_token(token: &OwnedToken, source: &str) -> LegacyToken {
    let index = LineIndex::new(source);
    to_legacy_token_with_index(token, &index)
}

/// `LineIndex` を再利用する場合の変換ヘルパ。
pub fn to_legacy_token_with_index(token: &OwnedToken, index: &LineIndex) -> LegacyToken {
    let (line, column) = index.offset_to_line_col(token.span.start as usize);
    LegacyToken {
        token_type: token_type_from_kind(token.kind),
        lexeme: token.lexeme_string(),
        line,
        column,
        leading_trivia: token.leading_trivia.clone(),
        diagnostic: token.diagnostic.clone(),
        metadata: token.metadata.clone(),
    }
}

/// レガシートークンの行・桁情報からゼロコピー用スパンを再構成する。
pub fn span_from_legacy_token(token: &LegacyToken, index: &LineIndex, source: &str) -> LexerSpan {
    let line_idx = token
        .line
        .saturating_sub(1)
        .min(index.line_starts.len().saturating_sub(1));
    let line_start = index.line_starts[line_idx];
    let line_end = index
        .line_starts
        .get(line_idx + 1)
        .copied()
        .unwrap_or_else(|| source.len());
    let line_slice = &source[line_start..line_end];

    let column_offset = token.column.saturating_sub(1);
    let byte_in_line = line_slice
        .char_indices()
        .nth(column_offset)
        .map(|(i, _)| i)
        .unwrap_or_else(|| line_slice.len());

    let start = line_start.saturating_add(byte_in_line);
    let end = start.saturating_add(token.lexeme.len()).min(source.len());

    LexerSpan {
        start: start as u32,
        end: end as u32,
    }
}

/// トリビアを `TokenTrivia` へマージする。
fn merge_trivia(target: &mut TokenTrivia, trivia: &[Trivia], line: usize, column: usize) {
    for item in trivia {
        match item {
            Trivia::Whitespace(text) => {
                let add = text.len() as u16;
                target.spaces = target.spaces.saturating_add(add);
            }
            Trivia::Newline => {
                target.merge_line_breaks(1);
            }
            Trivia::LineComment(text) => {
                target.comments = true;
                target.passthrough_comments.push(SourceCommentTrivia {
                    kind: SourceCommentKind::Line,
                    text: text.clone(),
                    line,
                    column,
                });
            }
            Trivia::BlockComment(text) => {
                target.comments = true;
                target.passthrough_comments.push(SourceCommentTrivia {
                    kind: SourceCommentKind::Block,
                    text: text.clone(),
                    line,
                    column,
                });
            }
        }
    }
}

/// トリビアを既存トークンへ付与する。
pub fn attach_trivia(token: &mut LegacyToken, trivia: &[Trivia]) {
    let (line, column) = {
        let start = token
            .leading_trivia
            .passthrough_comments
            .first()
            .map(|c| (c.line, c.column))
            .unwrap_or((token.line, token.column));
        start
    };
    merge_trivia(&mut token.leading_trivia, trivia, line, column);
}
