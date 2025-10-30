use jv_ast::Span;
use jv_lexer::{RawStringFlavor, Token, TokenMetadata};

/// ハイライト対象となるトークン種別。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HighlightKind {
    /// シングルクォートで囲まれた生文字列。
    RawSingle,
    /// 三重シングルクォートで囲まれた生文字列。
    RawTriple,
}

/// ハイライトの範囲と分類を表す構造体。
#[derive(Debug, Clone, PartialEq)]
pub struct HighlightToken {
    pub span: Span,
    pub kind: HighlightKind,
}

/// トークン列からハイライト対象を抽出する。
pub fn collect_highlights(tokens: &[Token]) -> Vec<HighlightToken> {
    let mut highlights = Vec::new();
    for token in tokens {
        let Some((kind, span)) = highlight_from_token(token) else {
            continue;
        };
        highlights.push(HighlightToken { span, kind });
    }
    highlights
}

fn highlight_from_token(token: &Token) -> Option<(HighlightKind, Span)> {
    let mut kind = None;
    for meta in &token.metadata {
        if let TokenMetadata::StringLiteral(info) = meta {
            if !info.is_raw {
                continue;
            }
            kind = Some(match info.raw_flavor {
                Some(RawStringFlavor::MultiLine) => HighlightKind::RawTriple,
                _ => HighlightKind::RawSingle,
            });
            break;
        }
    }

    let Some(kind) = kind else {
        return None;
    };

    let (end_line, end_column) = advance_position(token.line, token.column, &token.lexeme);
    let span = Span::new(token.line, token.column, end_line, end_column);
    Some((kind, span))
}

fn advance_position(mut line: usize, mut column: usize, lexeme: &str) -> (usize, usize) {
    for ch in lexeme.chars() {
        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }
    (line, column)
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_lexer::{Lexer, TokenType};

    #[test]
    fn collects_highlight_for_raw_single_literal() {
        let mut lexer = Lexer::new("'path'".to_string());
        let tokens = lexer.tokenize().expect("lex raw literal");
        assert_eq!(tokens[0].token_type, TokenType::String("path".to_string()));

        let highlights = collect_highlights(&tokens);
        assert_eq!(highlights.len(), 1);
        assert_eq!(highlights[0].kind, HighlightKind::RawSingle);
        let span = &highlights[0].span;
        assert_eq!(span.start_line, 1);
        assert_eq!(span.end_line, 1);
        assert!(
            span.end_column > span.start_column,
            "raw single highlight should advance column: {span:?}"
        );
    }

    #[test]
    fn collects_highlight_for_raw_triple_literal() {
        let mut lexer = Lexer::new("'''foo\nbar'''".to_string());
        let tokens = lexer.tokenize().expect("lex raw triple literal");
        assert_eq!(
            tokens[0].token_type,
            TokenType::String("foo\nbar".to_string())
        );

        let highlights = collect_highlights(&tokens);
        assert_eq!(highlights.len(), 1);
        assert_eq!(highlights[0].kind, HighlightKind::RawTriple);
        let span = &highlights[0].span;
        assert_eq!(span.start_line, 1);
        assert!(
            span.end_line >= span.start_line,
            "multi-line highlight should extend beyond or equal to start line: {span:?}"
        );
        assert!(
            span.end_column > 0,
            "end column should be positive for multi-line literal: {span:?}"
        );
    }
}
