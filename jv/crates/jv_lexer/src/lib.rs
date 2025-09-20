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
mod tests;
