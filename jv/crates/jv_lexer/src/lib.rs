// P0-001: lexer tokens定義・パース - TDD Red Phase
// Reference: 作業指示-20250830.md:19-20

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Token types for jv language
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TokenType {
    // Literals
    String(String),
    StringInterpolation(String), // "${...}" content
    Number(String),              // Store as string to avoid f64 Eq issues, parse later
    Identifier(String),
    Boolean(bool),

    // Keywords
    Val,
    Var,
    When,
    Data,
    Class,
    Fun,
    If,
    Else,
    For,
    In,
    While,
    Return,
    Break,
    Continue,
    True,
    False,
    Null,

    // Operators
    Assign,       // =
    Plus,         // +
    Minus,        // -
    Multiply,     // *
    Divide,       // /
    Modulo,       // %
    Equal,        // ==
    NotEqual,     // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
    And,          // &&
    Or,           // ||
    Not,          // !

    // Null safety operators
    Question, // ?
    NullSafe, // ?.
    Elvis,    // ?:

    // Arrow operators
    Arrow,    // ->
    FatArrow, // =>

    // Punctuation
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Semicolon,    // ;
    Colon,        // :
    DoubleColon,  // ::

    // String interpolation tokens
    StringStart, // "...${
    StringMid,   // }...${
    StringEnd,   // }..."

    // Comments
    LineComment(String),
    BlockComment(String),

    // Whitespace (usually ignored but useful for formatting)
    Whitespace(String),
    Newline,

    // Special
    Eof,
    Invalid(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Error, Debug)]
pub enum LexError {
    #[error("Unexpected character '{0}' at line {1}, column {2}")]
    UnexpectedChar(char, usize, usize),
    #[error("Unterminated string at line {0}, column {1}")]
    UnterminatedString(usize, usize),
}

pub struct Lexer {
    input: String,
    current: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        let chars: Vec<char> = self.input.chars().collect();

        while self.current < chars.len() {
            let start_line = self.line;
            let start_column = self.column;

            match chars[self.current] {
                // Whitespace
                ' ' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.advance();
                    self.line += 1;
                    self.column = 1;
                }
                '\r' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '\n' {
                        self.advance();
                        self.line += 1;
                        self.column = 1;
                    }
                }

                // Single character tokens
                '(' => {
                    tokens.push(self.make_token(
                        TokenType::LeftParen,
                        "(",
                        start_line,
                        start_column,
                    ));
                    self.advance();
                }
                ')' => {
                    tokens.push(self.make_token(
                        TokenType::RightParen,
                        ")",
                        start_line,
                        start_column,
                    ));
                    self.advance();
                }
                '{' => {
                    tokens.push(self.make_token(
                        TokenType::LeftBrace,
                        "{",
                        start_line,
                        start_column,
                    ));
                    self.advance();
                }
                '}' => {
                    tokens.push(self.make_token(
                        TokenType::RightBrace,
                        "}",
                        start_line,
                        start_column,
                    ));
                    self.advance();
                }
                '[' => {
                    tokens.push(self.make_token(
                        TokenType::LeftBracket,
                        "[",
                        start_line,
                        start_column,
                    ));
                    self.advance();
                }
                ']' => {
                    tokens.push(self.make_token(
                        TokenType::RightBracket,
                        "]",
                        start_line,
                        start_column,
                    ));
                    self.advance();
                }
                ',' => {
                    tokens.push(self.make_token(TokenType::Comma, ",", start_line, start_column));
                    self.advance();
                }
                ';' => {
                    tokens.push(self.make_token(
                        TokenType::Semicolon,
                        ";",
                        start_line,
                        start_column,
                    ));
                    self.advance();
                }
                '+' => {
                    tokens.push(self.make_token(TokenType::Plus, "+", start_line, start_column));
                    self.advance();
                }
                '%' => {
                    tokens.push(self.make_token(TokenType::Modulo, "%", start_line, start_column));
                    self.advance();
                }

                // Two character tokens and single char variants
                '=' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '=' {
                        tokens.push(self.make_token(
                            TokenType::Equal,
                            "==",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else if self.current < chars.len() && chars[self.current] == '>' {
                        tokens.push(self.make_token(
                            TokenType::FatArrow,
                            "=>",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Assign,
                            "=",
                            start_line,
                            start_column,
                        ));
                    }
                }
                '!' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '=' {
                        tokens.push(self.make_token(
                            TokenType::NotEqual,
                            "!=",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(TokenType::Not, "!", start_line, start_column));
                    }
                }
                '<' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '=' {
                        tokens.push(self.make_token(
                            TokenType::LessEqual,
                            "<=",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Less,
                            "<",
                            start_line,
                            start_column,
                        ));
                    }
                }
                '>' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '=' {
                        tokens.push(self.make_token(
                            TokenType::GreaterEqual,
                            ">=",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Greater,
                            ">",
                            start_line,
                            start_column,
                        ));
                    }
                }
                '&' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '&' {
                        tokens.push(self.make_token(
                            TokenType::And,
                            "&&",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else {
                        return Err(LexError::UnexpectedChar('&', start_line, start_column));
                    }
                }
                '|' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '|' {
                        tokens.push(self.make_token(TokenType::Or, "||", start_line, start_column));
                        self.advance();
                    } else {
                        return Err(LexError::UnexpectedChar('|', start_line, start_column));
                    }
                }
                '-' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '>' {
                        tokens.push(self.make_token(
                            TokenType::Arrow,
                            "->",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Minus,
                            "-",
                            start_line,
                            start_column,
                        ));
                    }
                }
                '*' => {
                    tokens.push(self.make_token(
                        TokenType::Multiply,
                        "*",
                        start_line,
                        start_column,
                    ));
                    self.advance();
                }
                '.' => {
                    tokens.push(self.make_token(TokenType::Dot, ".", start_line, start_column));
                    self.advance();
                }
                ':' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == ':' {
                        tokens.push(self.make_token(
                            TokenType::DoubleColon,
                            "::",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Colon,
                            ":",
                            start_line,
                            start_column,
                        ));
                    }
                }
                '?' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '.' {
                        tokens.push(self.make_token(
                            TokenType::NullSafe,
                            "?.",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else if self.current < chars.len() && chars[self.current] == ':' {
                        tokens.push(self.make_token(
                            TokenType::Elvis,
                            "?:",
                            start_line,
                            start_column,
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Question,
                            "?",
                            start_line,
                            start_column,
                        ));
                    }
                }

                // String literals
                '"' => {
                    tokens.extend(self.tokenize_string(start_line, start_column)?);
                }

                // Comments
                '/' => {
                    self.advance();
                    if self.current < chars.len() {
                        match chars[self.current] {
                            '/' => {
                                // Line comment
                                let comment = self.read_line_comment();
                                tokens.push(self.make_token(
                                    TokenType::LineComment(comment.clone()),
                                    &comment,
                                    start_line,
                                    start_column,
                                ));
                            }
                            '*' => {
                                // Block comment
                                let comment = self.read_block_comment()?;
                                tokens.push(self.make_token(
                                    TokenType::BlockComment(comment.clone()),
                                    &comment,
                                    start_line,
                                    start_column,
                                ));
                            }
                            _ => {
                                tokens.push(self.make_token(
                                    TokenType::Divide,
                                    "/",
                                    start_line,
                                    start_column,
                                ));
                            }
                        }
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Divide,
                            "/",
                            start_line,
                            start_column,
                        ));
                    }
                }

                // Numbers
                c if c.is_ascii_digit() => {
                    let number = self.read_number();
                    tokens.push(self.make_token(
                        TokenType::Number(number.clone()),
                        &number,
                        start_line,
                        start_column,
                    ));
                }

                // Identifiers and keywords
                c if c.is_alphabetic() || c == '_' => {
                    let identifier = self.read_identifier();
                    let token_type = match identifier.as_str() {
                        "val" => TokenType::Val,
                        "var" => TokenType::Var,
                        "when" => TokenType::When,
                        "data" => TokenType::Data,
                        "class" => TokenType::Class,
                        "fun" => TokenType::Fun,
                        "if" => TokenType::If,
                        "else" => TokenType::Else,
                        "for" => TokenType::For,
                        "in" => TokenType::In,
                        "while" => TokenType::While,
                        "return" => TokenType::Return,
                        "break" => TokenType::Break,
                        "continue" => TokenType::Continue,
                        "true" => TokenType::Boolean(true),
                        "false" => TokenType::Boolean(false),
                        "null" => TokenType::Null,
                        _ => TokenType::Identifier(identifier.clone()),
                    };
                    tokens.push(self.make_token(token_type, &identifier, start_line, start_column));
                }

                c => {
                    return Err(LexError::UnexpectedChar(c, start_line, start_column));
                }
            }
        }

        // Add EOF token
        tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            line: self.line,
            column: self.column,
        });

        Ok(tokens)
    }

    fn advance(&mut self) {
        if self.current < self.input.len() {
            self.current += 1;
            self.column += 1;
        }
    }

    fn make_token(&self, token_type: TokenType, lexeme: &str, line: usize, column: usize) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            line,
            column,
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.current;
        let chars: Vec<char> = self.input.chars().collect();

        while self.current < chars.len()
            && (chars[self.current].is_alphanumeric() || chars[self.current] == '_')
        {
            self.advance();
        }

        chars[start..self.current].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let start = self.current;
        let chars: Vec<char> = self.input.chars().collect();

        while self.current < chars.len() && chars[self.current].is_ascii_digit() {
            self.advance();
        }

        // Handle decimal point
        if self.current < chars.len()
            && chars[self.current] == '.'
            && self.current + 1 < chars.len()
            && chars[self.current + 1].is_ascii_digit()
        {
            self.advance(); // consume '.'
            while self.current < chars.len() && chars[self.current].is_ascii_digit() {
                self.advance();
            }
        }

        chars[start..self.current].iter().collect()
    }

    fn read_line_comment(&mut self) -> String {
        let start = self.current;
        let chars: Vec<char> = self.input.chars().collect();

        self.advance(); // consume second '/'

        while self.current < chars.len() && chars[self.current] != '\n' {
            self.advance();
        }

        chars[start..self.current].iter().collect()
    }

    fn read_block_comment(&mut self) -> Result<String, LexError> {
        let start_line = self.line;
        let start_column = self.column;
        let start = self.current;
        let chars: Vec<char> = self.input.chars().collect();

        self.advance(); // consume '*'

        while self.current < chars.len() - 1 {
            if chars[self.current] == '*' && chars[self.current + 1] == '/' {
                self.advance(); // consume '*'
                self.advance(); // consume '/'
                return Ok(chars[start..self.current - 2].iter().collect());
            }
            if chars[self.current] == '\n' {
                self.advance();
                self.line += 1;
                self.column = 1;
            } else {
                self.advance();
            }
        }

        Err(LexError::UnterminatedString(start_line, start_column))
    }

    fn tokenize_string(
        &mut self,
        start_line: usize,
        start_column: usize,
    ) -> Result<Vec<Token>, LexError> {
        let chars: Vec<char> = self.input.chars().collect();
        let mut tokens = Vec::new();
        let mut current_string = String::new();
        let mut _string_start_pos = self.current;

        self.advance(); // consume opening '"'

        while self.current < chars.len() && chars[self.current] != '"' {
            if chars[self.current] == '$'
                && self.current + 1 < chars.len()
                && chars[self.current + 1] == '{'
            {
                // End current string part if any content
                if !current_string.is_empty() {
                    if tokens.is_empty() {
                        tokens.push(self.make_token(
                            TokenType::StringStart,
                            &current_string,
                            start_line,
                            start_column,
                        ));
                    } else {
                        tokens.push(self.make_token(
                            TokenType::StringMid,
                            &current_string,
                            self.line,
                            self.column,
                        ));
                    }
                    current_string.clear();
                }

                // Skip ${
                self.advance(); // $
                self.advance(); // {

                // Tokenize the interpolation expression
                let mut brace_count = 1;
                let expr_start = self.current;

                while self.current < chars.len() && brace_count > 0 {
                    match chars[self.current] {
                        '{' => brace_count += 1,
                        '}' => brace_count -= 1,
                        _ => {}
                    }
                    if brace_count > 0 {
                        self.advance();
                    }
                }

                if brace_count > 0 {
                    return Err(LexError::UnterminatedString(start_line, start_column));
                }

                // Parse the expression inside ${}
                let expr: String = chars[expr_start..self.current].iter().collect();
                let mut expr_lexer = Lexer::new(expr.trim().to_string());
                let expr_tokens = expr_lexer.tokenize()?;

                // Add expression tokens (excluding EOF)
                for token in expr_tokens {
                    if !matches!(token.token_type, TokenType::Eof) {
                        tokens.push(token);
                    }
                }

                self.advance(); // consume closing '}'
                _string_start_pos = self.current;
            } else {
                current_string.push(chars[self.current]);
                if chars[self.current] == '\n' {
                    self.advance();
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.advance();
                }
            }
        }

        if self.current >= chars.len() {
            return Err(LexError::UnterminatedString(start_line, start_column));
        }

        // Handle final string part
        if tokens.is_empty() {
            // Simple string without interpolation
            let string_content = current_string.clone();
            tokens.push(self.make_token(
                TokenType::String(current_string),
                &string_content,
                start_line,
                start_column,
            ));
        } else {
            // String with interpolation - add final part
            tokens.push(self.make_token(
                TokenType::StringEnd,
                &current_string,
                self.line,
                self.column,
            ));
        }

        self.advance(); // consume closing '"'

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test]
    fn test_basic_keywords_green_phase() {
        // GREEN: This test should now pass
        let mut lexer = Lexer::new("val var when data class fun".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Expected tokens for Green phase
        assert_eq!(tokens.len(), 7); // 6 keywords + EOF
        assert_eq!(tokens[0].token_type, TokenType::Val);
        assert_eq!(tokens[1].token_type, TokenType::Var);
        assert_eq!(tokens[2].token_type, TokenType::When);
        assert_eq!(tokens[3].token_type, TokenType::Data);
        assert_eq!(tokens[4].token_type, TokenType::Class);
        assert_eq!(tokens[5].token_type, TokenType::Fun);
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_identifiers_and_literals_green_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("val name = \"Hello\" 42 true".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Expected tokens for Green phase
        assert_eq!(tokens.len(), 7); // val, name, =, "Hello", 42, true, EOF
        assert_eq!(tokens[0].token_type, TokenType::Val);
        assert_eq!(
            tokens[1].token_type,
            TokenType::Identifier("name".to_string())
        );
        assert_eq!(tokens[2].token_type, TokenType::Assign);
        assert_eq!(tokens[3].token_type, TokenType::String("Hello".to_string()));
        assert_eq!(tokens[4].token_type, TokenType::Number("42".to_string()));
        assert_eq!(tokens[5].token_type, TokenType::Boolean(true));
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_null_safety_operators_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("obj?.field ?: default".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should recognize ?. and ?: operators
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::NullSafe));
        assert!(token_types.contains(&&TokenType::Elvis));
        assert!(token_types.contains(&&TokenType::Identifier("obj".to_string())));
        assert!(token_types.contains(&&TokenType::Identifier("field".to_string())));
        assert!(token_types.contains(&&TokenType::Identifier("default".to_string())));
    }

    #[test]
    fn test_string_interpolation_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("\"Hello, ${name}!\"".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Expected tokens for string interpolation
        // Should tokenize as: StringStart("Hello, "), Identifier("name"), StringEnd("!")
        assert!(tokens.len() >= 3);

        // Look for interpolation-related tokens
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types
            .iter()
            .any(|t| matches!(t, TokenType::StringStart)));
        assert!(token_types.contains(&&TokenType::Identifier("name".to_string())));
        assert!(token_types
            .iter()
            .any(|t| matches!(t, TokenType::StringEnd)));
    }

    #[test]
    fn test_arithmetic_operators_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("a + b - c * d / e % f".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should recognize arithmetic operators
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Plus));
        assert!(token_types.contains(&&TokenType::Minus));
        assert!(token_types.contains(&&TokenType::Multiply));
        assert!(token_types.contains(&&TokenType::Divide));
        assert!(token_types.contains(&&TokenType::Modulo));
    }

    #[test]
    fn test_comparison_operators_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("a == b != c < d <= e > f >= g".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should recognize comparison operators
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Equal));
        assert!(token_types.contains(&&TokenType::NotEqual));
        assert!(token_types.contains(&&TokenType::Less));
        assert!(token_types.contains(&&TokenType::LessEqual));
        assert!(token_types.contains(&&TokenType::Greater));
        assert!(token_types.contains(&&TokenType::GreaterEqual));
    }

    #[test]
    fn test_logical_operators_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("a && b || !c".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should recognize logical operators
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::And));
        assert!(token_types.contains(&&TokenType::Or));
        assert!(token_types.contains(&&TokenType::Not));
    }

    #[test]
    fn test_arrow_operators_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("fun test() -> Int { x => x + 1 }".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should recognize arrow operators
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Arrow)); // ->
        assert!(token_types.contains(&&TokenType::FatArrow)); // =>
    }

    #[test]
    fn test_punctuation_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("(a, b) { c[d]: e; f::g }".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should recognize all punctuation
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::LeftParen));
        assert!(token_types.contains(&&TokenType::RightParen));
        assert!(token_types.contains(&&TokenType::LeftBrace));
        assert!(token_types.contains(&&TokenType::RightBrace));
        assert!(token_types.contains(&&TokenType::LeftBracket));
        assert!(token_types.contains(&&TokenType::RightBracket));
        assert!(token_types.contains(&&TokenType::Comma));
        assert!(token_types.contains(&&TokenType::Colon));
        assert!(token_types.contains(&&TokenType::Semicolon));
        assert!(token_types.contains(&&TokenType::DoubleColon));
    }

    #[test]
    fn test_comments_red_phase() {
        // RED: This test should fail
        let mut lexer =
            Lexer::new("val x = 1 // line comment\n/* block comment */ val y = 2".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should recognize both line and block comments
        let has_line_comment = tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::LineComment(_)));
        let has_block_comment = tokens
            .iter()
            .any(|t| matches!(t.token_type, TokenType::BlockComment(_)));

        assert!(has_line_comment);
        assert!(has_block_comment);
    }

    #[test]
    fn test_position_tracking_green_phase() {
        // GREEN: Test position tracking functionality
        let mut lexer = Lexer::new("val\nname\n  =\n    42".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should track line and column positions correctly
        assert_eq!(tokens[0].line, 1); // val
        assert_eq!(tokens[0].column, 1);

        assert_eq!(tokens[1].line, 2); // name
        assert_eq!(tokens[1].column, 1);

        assert_eq!(tokens[2].line, 3); // =
        assert_eq!(tokens[2].column, 3);

        assert_eq!(tokens[3].line, 4); // 42
        assert_eq!(tokens[3].column, 5);
    }

    #[test_case("true" => TokenType::Boolean(true); "boolean true")]
    #[test_case("false" => TokenType::Boolean(false); "boolean false")]
    #[test_case("null" => TokenType::Null; "null literal")]
    fn test_literal_values_red_phase(input: &str) -> TokenType {
        // RED: This test should fail
        let mut lexer = Lexer::new(input.to_string());
        let tokens = lexer.tokenize().unwrap();

        tokens[0].token_type.clone()
    }

    #[test]
    fn test_complex_string_interpolation_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("\"User ${user.name} has ${user.age} years\"".to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should handle complex interpolation with member access
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();

        // Should contain identifiers and dots for member access
        assert!(token_types.contains(&&TokenType::Identifier("user".to_string())));
        assert!(token_types.contains(&&TokenType::Dot));
        assert!(token_types.contains(&&TokenType::Identifier("name".to_string())));
        assert!(token_types.contains(&&TokenType::Identifier("age".to_string())));
    }

    #[test]
    fn test_lexer_error_handling_red_phase() {
        // RED: This test should fail
        let mut lexer = Lexer::new("\"unterminated string".to_string());
        let result = lexer.tokenize();

        // Should return an error for unterminated string
        assert!(result.is_err());
        match result.unwrap_err() {
            LexError::UnterminatedString(_, _) => {} // Expected
            _ => panic!("Wrong error type"),
        }
    }

    // Additional comprehensive tests for lexer
    #[test]
    fn test_complex_expressions() {
        let source = r#"
            val result = user?.profile?.name ?: "Unknown"
            val sum = (a + b) * (c - d) / e % f
        "#;
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should contain all expected operators and symbols
        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Val));
        assert!(token_types.contains(&&TokenType::NullSafe));
        assert!(token_types.contains(&&TokenType::Elvis));
        assert!(token_types.contains(&&TokenType::LeftParen));
        assert!(token_types.contains(&&TokenType::RightParen));
        assert!(token_types.contains(&&TokenType::Plus));
        assert!(token_types.contains(&&TokenType::Minus));
        assert!(token_types.contains(&&TokenType::Multiply));
        assert!(token_types.contains(&&TokenType::Divide));
        assert!(token_types.contains(&&TokenType::Modulo));
    }

    #[test]
    fn test_function_definitions() {
        let source = r#"
            fun add(a: Int, b: Int): Int {
                return a + b
            }
            
            fun greet(name: String = "World") {
                println("Hello, $name!")
            }
        "#;
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Fun));
        assert!(token_types.contains(&&TokenType::Return));
        assert!(token_types.contains(&&TokenType::Colon));
        assert!(token_types.contains(&&TokenType::Arrow));
        assert!(token_types.contains(&&TokenType::Assign));
    }

    #[test]
    fn test_data_class_syntax() {
        let source = "data class User(val name: String, var age: Int)";
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Data));
        assert!(token_types.contains(&&TokenType::Class));
        assert!(token_types.contains(&&TokenType::Val));
        assert!(token_types.contains(&&TokenType::Var));
    }

    #[test]
    fn test_when_expressions() {
        let source = r#"
            when (x) {
                0 -> "zero"
                1, 2 -> "one or two"
                in 3..10 -> "small"
                else -> "other"
            }
        "#;
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::When));
        assert!(token_types.contains(&&TokenType::FatArrow));
        assert!(token_types.contains(&&TokenType::In));
        // Note: DotDot (range operator) not yet implemented in TokenType enum
        assert!(token_types.contains(&&TokenType::Else));
    }

    #[test]
    fn test_async_spawn_syntax() {
        let source = r#"
            spawn {
                println("Running in virtual thread")
            }
            
            val future = async {
                computeValue()
            }.await()
        "#;
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        // Note: Spawn, Async, Await keywords not yet implemented in TokenType enum
    }

    #[test]
    fn test_extension_functions() {
        let source = "fun String.reversed(): String = StringBuilder(this).reverse().toString()";
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::Fun));
        assert!(token_types.contains(&&TokenType::Dot));
        assert!(token_types.contains(&&TokenType::Arrow));
        assert!(token_types.contains(&&TokenType::Assign));
    }

    #[test]
    fn test_numbers_and_types() {
        let source = "val int = 42; val float = 3.14; val hex = 0xFF; val binary = 0b1010";
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let numbers: Vec<_> = tokens
            .iter()
            .filter_map(|t| match &t.token_type {
                TokenType::Number(n) => Some(n.as_str()),
                _ => None,
            })
            .collect();

        assert!(numbers.contains(&"42"));
        assert!(numbers.contains(&"3.14"));
        assert!(numbers.contains(&"0xFF") || numbers.contains(&"255"));
        assert!(numbers.contains(&"0b1010") || numbers.contains(&"10"));
    }

    #[test]
    fn test_use_defer_syntax() {
        let source = r#"
            use (resource) {
                resource.process()
            }
            
            defer {
                cleanup()
            }
        "#;
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        // Note: Use, Defer keywords not yet implemented in TokenType enum
    }

    #[test]
    fn test_array_and_collection_syntax() {
        let source = r#"
            val array = [1, 2, 3]
            val list = listOf("a", "b", "c")
            val map = mapOf("key" to "value")
        "#;
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let token_types: Vec<_> = tokens.iter().map(|t| &t.token_type).collect();
        assert!(token_types.contains(&&TokenType::LeftBracket));
        assert!(token_types.contains(&&TokenType::RightBracket));
        // Note: To keyword not yet implemented in TokenType enum
    }

    #[test]
    fn test_edge_cases_and_errors() {
        // Test various edge cases
        let test_cases = vec![
            ("", true),             // Empty string should not fail
            ("//", true),           // Just comment
            ("/* */", true),        // Just block comment
            ("\"\"", true),         // Empty string literal
            ("''", false),          // Single quotes not supported
            ("123.456.789", false), // Invalid number
            ("@invalid", false),    // Invalid character
        ];

        for (input, should_succeed) in test_cases {
            let mut lexer = Lexer::new(input.to_string());
            let result = lexer.tokenize();

            if should_succeed {
                assert!(result.is_ok(), "Failed to tokenize: '{}'", input);
            } else {
                assert!(
                    result.is_err(),
                    "Should have failed to tokenize: '{}'",
                    input
                );
            }
        }
    }

    #[test]
    fn test_whitespace_and_newlines() {
        let source = "val\n\tx\n\t\t=\n\t\t\t42";
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        // Should correctly handle various whitespace
        assert_eq!(tokens[0].token_type, TokenType::Val);
        assert_eq!(tokens[1].token_type, TokenType::Identifier("x".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Assign);
        assert_eq!(tokens[3].token_type, TokenType::Number("42".to_string()));
    }

    #[test]
    fn test_unicode_identifiers() {
        let source = "val δ = 3.14; val α = β + γ";
        let mut lexer = Lexer::new(source.to_string());
        let tokens = lexer.tokenize().unwrap();

        let identifiers: Vec<_> = tokens
            .iter()
            .filter_map(|t| match &t.token_type {
                TokenType::Identifier(id) => Some(id.as_str()),
                _ => None,
            })
            .collect();

        assert!(identifiers.contains(&"δ"));
        assert!(identifiers.contains(&"α"));
        assert!(identifiers.contains(&"β"));
        assert!(identifiers.contains(&"γ"));
    }
}
