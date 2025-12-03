use jv_parser2::{Lexer, Source, TokenKind};

fn lex_kinds(input: &str) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(Source::from_str(input));
    let mut kinds = Vec::new();
    loop {
        let token = lexer.next_token();
        let kind = token.kind;
        kinds.push(kind);
        if kind == TokenKind::Eof {
            break;
        }
    }
    kinds
}

#[test]
fn lexes_basic_whitespace_and_identifiers() {
    let kinds = lex_kinds("a b\n_c");
    assert_eq!(
        kinds,
        vec![
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Newline,
            TokenKind::Identifier,
            TokenKind::Eof
        ]
    );
}

#[test]
fn recognizes_keywords_and_unicode_identifiers() {
    let kinds = lex_kinds("val 変数 where");
    assert_eq!(kinds[0], TokenKind::Val);
    assert_eq!(kinds[2], TokenKind::Identifier);
    assert_eq!(kinds[3], TokenKind::Whitespace);
    assert_eq!(kinds[4], TokenKind::Where);
}

#[test]
fn parses_number_variants() {
    let kinds = lex_kinds("0xFF 0b10 0o77 1.5 1.0e-10 42@ms");
    let numbers = kinds
        .into_iter()
        .filter(|k| *k != TokenKind::Whitespace && *k != TokenKind::Eof)
        .collect::<Vec<_>>();
    assert_eq!(
        numbers,
        vec![
            TokenKind::Number,
            TokenKind::Number,
            TokenKind::Number,
            TokenKind::Number,
            TokenKind::Number,
            TokenKind::Number
        ]
    );
}

#[test]
fn parses_strings_with_interpolation_and_plain() {
    let plain = lex_kinds("\"hello\"");
    assert_eq!(plain[0], TokenKind::String);

    let interpolated = lex_kinds("\"hello ${name}\"");
    assert_eq!(interpolated[0], TokenKind::StringStart);
}

#[test]
fn recognizes_operators_and_comments() {
    let kinds = lex_kinds("?. ?: .. ..= |{ }| && || -> => /* */");
    assert!(kinds.contains(&TokenKind::Arrow));
    assert!(kinds.contains(&TokenKind::FatArrow));
    assert!(kinds.contains(&TokenKind::RangeInclusive));
    assert!(kinds.contains(&TokenKind::RangeExclusive));
    assert!(kinds.contains(&TokenKind::PipeLeft));
    assert!(kinds.contains(&TokenKind::PipeRight));
}

#[test]
fn rejects_single_amp_and_pipe_and_unterminated_block_comment() {
    let kinds = lex_kinds("& | /* no end");
    assert!(kinds.iter().any(|k| *k == TokenKind::Invalid));
}

#[test]
fn validates_number_prefixes_and_units() {
    let kinds = lex_kinds("0x 0b 0o 1. 123e 42@ foo");
    let invalids = kinds
        .into_iter()
        .filter(|k| matches!(k, TokenKind::Invalid))
        .count();
    assert!(invalids >= 5);
}

#[test]
fn string_interpolation_segments_and_escapes() {
    let kinds = lex_kinds("\"a${b}c\"");
    assert_eq!(kinds[0], TokenKind::StringStart);
    assert!(kinds.iter().any(|k| *k == TokenKind::StringEnd));

    let invalid_escape = lex_kinds("\"\\q\"");
    assert!(invalid_escape.contains(&TokenKind::Invalid));

    // nested interpolation inside interpolation should not stall
    let nested = lex_kinds("\"${\"${x}\"}\"");
    assert!(nested.iter().any(|k| *k == TokenKind::StringEnd));

    let short_unicode = lex_kinds("\"\\u123\"");
    assert!(short_unicode.contains(&TokenKind::Invalid));
}

#[test]
fn char_literals_validate_escapes() {
    let valid = lex_kinds("'a' '\\n'");
    assert!(valid.contains(&TokenKind::Character));

    let invalid = lex_kinds("'\\q' '\\'");
    assert!(invalid.iter().any(|k| *k == TokenKind::Invalid));
}
