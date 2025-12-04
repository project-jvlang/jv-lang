//! 正規表現リテラルの字句解析。
//!
//! jvでは `/pattern/flags` 構文で正規表現リテラルを記述できる。
//! `/` は除算演算子と衝突するため、前のトークンに基づいて文脈依存で判定する。

use crate::{Token, token::TokenKind};

use super::Lexer;

/// 直前のトークンがこれらの場合、`/` は正規表現リテラルを開始できない（除算として扱う）。
///
/// 例:
/// - `x / y` → 除算
/// - `(a) / b` → 除算
/// - `if x / y` → 正規表現（ifは値を生成しない）
pub fn can_start_regex(last_token: TokenKind) -> bool {
    !regex_cannot_follow(last_token)
}

/// これらのトークンの直後では `/` は除算として扱う。
fn regex_cannot_follow(token: TokenKind) -> bool {
    matches!(
        token,
        TokenKind::Identifier
            | TokenKind::Underscore
            | TokenKind::ImplicitParam
            | TokenKind::Number
            | TokenKind::Character
            | TokenKind::String
            | TokenKind::StringInterpolation
            | TokenKind::StringEnd
            | TokenKind::Boolean
            | TokenKind::TrueKw
            | TokenKind::FalseKw
            | TokenKind::NullKw
            | TokenKind::RightParen
            | TokenKind::RightBracket
            | TokenKind::RightBrace
            | TokenKind::Regex
    )
}

/// 正規表現リテラルを読み取る。
///
/// 呼び出し時点でカーソルは `/` の位置にある。
/// `/pattern/flags` を消費して `TokenKind::Regex` を返す。
pub(crate) fn lex_regex(lexer: &mut Lexer<'_>) -> Token {
    let start = lexer.current_offset();

    // 開始の `/` を消費
    lexer.advance();

    let mut escaped = false;
    let mut in_char_class = false;

    loop {
        match lexer.peek() {
            None => {
                // 未終端の正規表現
                return lexer.make_token(TokenKind::Invalid, start);
            }
            Some(b'\n') | Some(b'\r') => {
                // 改行は正規表現内で許可されない
                return lexer.make_token(TokenKind::Invalid, start);
            }
            Some(b'\t') => {
                // タブは正規表現内で許可されない
                return lexer.make_token(TokenKind::Invalid, start);
            }
            Some(byte) => {
                lexer.advance();

                if escaped {
                    escaped = false;
                    continue;
                }

                match byte {
                    b'\\' => {
                        escaped = true;
                    }
                    b'[' => {
                        in_char_class = true;
                    }
                    b']' if in_char_class => {
                        in_char_class = false;
                    }
                    b'/' if !in_char_class => {
                        // パターン終了、フラグを消費
                        consume_regex_flags(lexer);
                        return lexer.make_token(TokenKind::Regex, start);
                    }
                    _ => {}
                }
            }
        }
    }
}

/// 正規表現フラグ（i, m, s, g など）を消費する。
fn consume_regex_flags(lexer: &mut Lexer<'_>) {
    while let Some(byte) = lexer.peek() {
        if byte.is_ascii_alphabetic() {
            lexer.advance();
        } else {
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::Source;

    fn lex_all(input: &str) -> Vec<Token> {
        let source = Source::new(input.as_bytes()).unwrap();
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    #[test]
    fn can_start_regex_rules() {
        // can_start_regex のテスト
        assert!(can_start_regex(TokenKind::Assign));
        assert!(can_start_regex(TokenKind::LeftParen));
        assert!(can_start_regex(TokenKind::Comma));
        assert!(can_start_regex(TokenKind::If));
        assert!(can_start_regex(TokenKind::Eof)); // ファイル先頭

        assert!(!can_start_regex(TokenKind::Identifier));
        assert!(!can_start_regex(TokenKind::Number));
        assert!(!can_start_regex(TokenKind::RightParen));
        assert!(!can_start_regex(TokenKind::RightBracket));
    }

    #[test]
    fn regex_at_start() {
        // ファイル先頭では正規表現として認識される
        let tokens = lex_all("/abc/");
        assert_eq!(tokens[0].kind, TokenKind::Regex);
    }

    #[test]
    fn regex_after_assign() {
        // = の後は正規表現として認識される
        let tokens = lex_all("x = /abc/");
        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].kind, TokenKind::Whitespace);
        assert_eq!(tokens[2].kind, TokenKind::Assign);
        assert_eq!(tokens[3].kind, TokenKind::Whitespace);
        assert_eq!(tokens[4].kind, TokenKind::Regex);
    }

    #[test]
    fn divide_after_identifier() {
        // 識別子の後は除算として認識される
        let tokens = lex_all("x / y");
        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::Divide);
        assert_eq!(tokens[4].kind, TokenKind::Identifier);
    }

    #[test]
    fn regex_with_flags() {
        let tokens = lex_all("/abc/gi");
        assert_eq!(tokens[0].kind, TokenKind::Regex);
    }

    #[test]
    fn regex_with_escape() {
        let tokens = lex_all(r#"/\d+\/\s*/"#);
        assert_eq!(tokens[0].kind, TokenKind::Regex);
    }

    #[test]
    fn regex_with_char_class() {
        // 文字クラス内の / はパターン終了ではない
        let tokens = lex_all("/[/]+/");
        assert_eq!(tokens[0].kind, TokenKind::Regex);
    }

    #[test]
    fn unterminated_regex() {
        let tokens = lex_all("/abc");
        assert_eq!(tokens[0].kind, TokenKind::Invalid);
    }

    #[test]
    fn regex_with_newline_is_invalid() {
        let tokens = lex_all("/abc\ndef/");
        assert_eq!(tokens[0].kind, TokenKind::Invalid);
    }
}
