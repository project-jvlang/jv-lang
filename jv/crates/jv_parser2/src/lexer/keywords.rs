use crate::token::TokenKind;

/// 先頭文字分岐によるキーワード判定。
pub(crate) fn keyword_from_bytes(text: &[u8]) -> Option<TokenKind> {
    if text.is_empty() {
        return None;
    }
    match text[0] {
        b'a' => match text {
            b"assert" => Some(TokenKind::Assert),
            _ => None,
        },
        b'b' => match text {
            b"break" => Some(TokenKind::Break),
            _ => None,
        },
        b'c' => match text {
            b"class" => Some(TokenKind::Class),
            b"continue" => Some(TokenKind::Continue),
            _ => None,
        },
        b'd' => match text {
            b"data" => Some(TokenKind::Data),
            b"debug" => Some(TokenKind::Debug),
            b"do" => Some(TokenKind::Do),
            _ => None,
        },
        b'e' => match text {
            b"else" => Some(TokenKind::Else),
            b"error" => Some(TokenKind::Error),
            _ => None,
        },
        b'f' => match text {
            b"false" => Some(TokenKind::FalseKw),
            b"for" => Some(TokenKind::For),
            b"fun" => Some(TokenKind::Fun),
            _ => None,
        },
        b'i' => match text {
            b"if" => Some(TokenKind::If),
            b"import" => Some(TokenKind::Import),
            b"in" => Some(TokenKind::In),
            b"info" => Some(TokenKind::Info),
            _ => None,
        },
        b'l' => match text {
            b"log" => Some(TokenKind::Log),
            _ => None,
        },
        b'n' => match text {
            b"null" => Some(TokenKind::NullKw),
            _ => None,
        },
        b'p' => match text {
            b"package" => Some(TokenKind::Package),
            _ => None,
        },
        b'r' => match text {
            b"return" => Some(TokenKind::Return),
            _ => None,
        },
        b't' => match text {
            b"test" => Some(TokenKind::Test),
            b"throw" => Some(TokenKind::Throw),
            b"trace" => Some(TokenKind::Trace),
            b"true" => Some(TokenKind::TrueKw),
            _ => None,
        },
        b'v' => match text {
            b"val" => Some(TokenKind::Val),
            b"var" => Some(TokenKind::Var),
            _ => None,
        },
        b'w' => match text {
            b"warn" => Some(TokenKind::Warn),
            b"when" => Some(TokenKind::When),
            b"while" => Some(TokenKind::While),
            b"where" => Some(TokenKind::Where),
            _ => None,
        },
        _ => None,
    }
}
