// P0-001: lexer tokens定義・パース - TDD Red Phase
// Reference: 作業指示-20250830.md:19-20

pub mod pipeline;

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
    Where,
    If,
    Else,
    For,
    In,
    While,
    Do,
    Return,
    Break,
    Continue,
    True,
    False,
    Null,

    // Operators
    Assign,         // =
    Plus,           // +
    Minus,          // -
    Multiply,       // *
    Divide,         // /
    Modulo,         // %
    Equal,          // ==
    NotEqual,       // !=
    Less,           // <
    LessEqual,      // <=
    Greater,        // >
    GreaterEqual,   // >=
    And,            // &&
    Or,             // ||
    Not,            // !
    RangeExclusive, // ..
    RangeInclusive, // ..=

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
    LayoutComma,  // synthetic comma for layout-delimited sequences
    Dot,          // .
    Semicolon,    // ;
    Colon,        // :
    DoubleColon,  // ::
    At,           // @

    // String interpolation tokens
    StringStart, // "...${
    StringMid,   // }...${
    StringEnd,   // }..."

    // Comments
    LineComment(String),
    BlockComment(String),
    JavaDocComment(String),

    // Whitespace (usually ignored but useful for formatting)
    Whitespace(String),
    Newline,

    // Special
    Eof,
    Invalid(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JsonCommentTriviaKind {
    Line,
    Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct JsonCommentTrivia {
    pub kind: JsonCommentTriviaKind,
    pub text: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SourceCommentKind {
    Line,
    Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SourceCommentTrivia {
    pub kind: SourceCommentKind,
    pub text: String,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct TokenTrivia {
    pub spaces: u16,
    pub newlines: u16,
    pub comments: bool,
    #[serde(default)]
    pub json_comments: Vec<JsonCommentTrivia>,
    #[serde(default)]
    pub doc_comment: Option<String>,
    #[serde(default)]
    pub passthrough_comments: Vec<SourceCommentTrivia>,
    #[serde(default)]
    pub jv_comments: Vec<SourceCommentTrivia>,
}

impl TokenTrivia {
    pub fn merge_line_breaks(&mut self, count: u16) {
        self.newlines = self.newlines.saturating_add(count);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum JsonConfidence {
    None,
    Low,
    Medium,
    High,
}

impl Default for JsonConfidence {
    fn default() -> Self {
        JsonConfidence::None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StringDelimiterKind {
    DoubleQuote,
    TripleQuote,
    BacktickBlock,
}

impl StringDelimiterKind {
    fn opening_sequence(self) -> &'static str {
        match self {
            StringDelimiterKind::DoubleQuote => "\"",
            StringDelimiterKind::TripleQuote => "\"\"\"",
            StringDelimiterKind::BacktickBlock => "```",
        }
    }

    fn closing_sequence(self) -> &'static str {
        self.opening_sequence()
    }

    fn allows_interpolation(self) -> bool {
        true
    }

    fn normalize_indentation(self) -> bool {
        matches!(self, StringDelimiterKind::TripleQuote)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StringLiteralMetadata {
    pub delimiter: StringDelimiterKind,
    pub allows_interpolation: bool,
    pub normalize_indentation: bool,
}

impl StringLiteralMetadata {
    fn from_kind(kind: StringDelimiterKind) -> Self {
        Self {
            delimiter: kind,
            allows_interpolation: kind.allows_interpolation(),
            normalize_indentation: kind.normalize_indentation(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum NumberGroupingKind {
    None,
    Comma,
    Underscore,
    Mixed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NumberLiteralMetadata {
    pub grouping: NumberGroupingKind,
    pub original_lexeme: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TokenMetadata {
    PotentialJsonStart { confidence: JsonConfidence },
    StringLiteral(StringLiteralMetadata),
    NumberLiteral(NumberLiteralMetadata),
    LayoutComma(LayoutCommaMetadata),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LayoutSequenceKind {
    Array,
    Call,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ExplicitSeparatorLocation {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LayoutCommaMetadata {
    pub sequence: LayoutSequenceKind,
    #[serde(default)]
    pub explicit_separator: Option<ExplicitSeparatorLocation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LegacyLoopKeyword {
    While,
    Do,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TokenDiagnostic {
    LegacyLoop { keyword: LegacyLoopKeyword },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
    pub leading_trivia: TokenTrivia,
    pub diagnostic: Option<TokenDiagnostic>,
    #[serde(default)]
    pub metadata: Vec<TokenMetadata>,
}

#[derive(Debug, Default, Clone)]
struct TriviaTracker {
    trivia: TokenTrivia,
}

impl TriviaTracker {
    fn record_space(&mut self) {
        self.trivia.spaces = self.trivia.spaces.saturating_add(1);
    }

    fn record_newline(&mut self) {
        self.trivia.newlines = self.trivia.newlines.saturating_add(1);
        self.trivia.spaces = 0;
    }

    fn take(&mut self) -> TokenTrivia {
        std::mem::take(&mut self.trivia)
    }

    fn record_comment(
        &mut self,
        comment: Option<JsonCommentTrivia>,
        passthrough: Option<SourceCommentTrivia>,
        jv_only: Option<SourceCommentTrivia>,
    ) {
        self.trivia.comments = true;
        self.trivia.doc_comment = None;
        if let Some(comment) = comment {
            self.trivia.json_comments.push(comment);
        }
        if let Some(comment) = passthrough {
            self.trivia.passthrough_comments.push(comment);
        }
        if let Some(comment) = jv_only {
            self.trivia.jv_comments.push(comment);
        }
    }

    fn record_doc_comment(&mut self, comment: String) {
        self.trivia.comments = true;
        self.trivia.doc_comment = Some(comment);
    }
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
        let mut trivia = TriviaTracker::default();

        while self.current < chars.len() {
            let start_line = self.line;
            let start_column = self.column;

            match chars[self.current] {
                // Whitespace
                ' ' | '\t' => {
                    trivia.record_space();
                    self.advance();
                }
                '\n' => {
                    trivia.record_newline();
                    self.advance();
                    self.line += 1;
                    self.column = 1;
                }
                '\r' => {
                    trivia.record_newline();
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
                        trivia.take(),
                    ));
                    self.advance();
                }
                ')' => {
                    tokens.push(self.make_token(
                        TokenType::RightParen,
                        ")",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
                    self.advance();
                }
                '{' => {
                    let metadata = self.json_start_metadata_for_brace(&chars);
                    tokens.push(self.make_token_with_metadata(
                        TokenType::LeftBrace,
                        "{",
                        start_line,
                        start_column,
                        trivia.take(),
                        metadata,
                    ));
                    self.advance();
                }
                '}' => {
                    tokens.push(self.make_token(
                        TokenType::RightBrace,
                        "}",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
                    self.advance();
                }
                '[' => {
                    let metadata = self.json_start_metadata_for_bracket(&chars);
                    tokens.push(self.make_token_with_metadata(
                        TokenType::LeftBracket,
                        "[",
                        start_line,
                        start_column,
                        trivia.take(),
                        metadata,
                    ));
                    self.advance();
                }
                ']' => {
                    tokens.push(self.make_token(
                        TokenType::RightBracket,
                        "]",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
                    self.advance();
                }
                ',' => {
                    tokens.push(self.make_token(
                        TokenType::Comma,
                        ",",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
                    self.advance();
                }
                ';' => {
                    tokens.push(self.make_token(
                        TokenType::Semicolon,
                        ";",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
                    self.advance();
                }
                '+' => {
                    tokens.push(self.make_token(
                        TokenType::Plus,
                        "+",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
                    self.advance();
                }
                '%' => {
                    tokens.push(self.make_token(
                        TokenType::Modulo,
                        "%",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
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
                            trivia.take(),
                        ));
                        self.advance();
                    } else if self.current < chars.len() && chars[self.current] == '>' {
                        tokens.push(self.make_token(
                            TokenType::FatArrow,
                            "=>",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Assign,
                            "=",
                            start_line,
                            start_column,
                            trivia.take(),
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
                            trivia.take(),
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Not,
                            "!",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
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
                            trivia.take(),
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Less,
                            "<",
                            start_line,
                            start_column,
                            trivia.take(),
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
                            trivia.take(),
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Greater,
                            ">",
                            start_line,
                            start_column,
                            trivia.take(),
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
                            trivia.take(),
                        ));
                        self.advance();
                    } else {
                        return Err(LexError::UnexpectedChar('&', start_line, start_column));
                    }
                }
                '|' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '|' {
                        tokens.push(self.make_token(
                            TokenType::Or,
                            "||",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
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
                            trivia.take(),
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Minus,
                            "-",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
                    }
                }
                '*' => {
                    tokens.push(self.make_token(
                        TokenType::Multiply,
                        "*",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
                    self.advance();
                }
                '@' => {
                    tokens.push(self.make_token(
                        TokenType::At,
                        "@",
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
                    self.advance();
                }
                '.' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == '.' {
                        self.advance();
                        if self.current < chars.len() && chars[self.current] == '=' {
                            self.advance();
                            tokens.push(self.make_token(
                                TokenType::RangeInclusive,
                                "..=",
                                start_line,
                                start_column,
                                trivia.take(),
                            ));
                        } else {
                            tokens.push(self.make_token(
                                TokenType::RangeExclusive,
                                "..",
                                start_line,
                                start_column,
                                trivia.take(),
                            ));
                        }
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Dot,
                            ".",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
                    }
                }
                ':' => {
                    self.advance();
                    if self.current < chars.len() && chars[self.current] == ':' {
                        tokens.push(self.make_token(
                            TokenType::DoubleColon,
                            "::",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Colon,
                            ":",
                            start_line,
                            start_column,
                            trivia.take(),
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
                            trivia.take(),
                        ));
                        self.advance();
                    } else if self.current < chars.len() && chars[self.current] == ':' {
                        tokens.push(self.make_token(
                            TokenType::Elvis,
                            "?:",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
                        self.advance();
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Question,
                            "?",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
                    }
                }

                // String literals
                '"' => {
                    let leading_trivia = trivia.take();
                    let delimiter = if self.starts_with_sequence(&chars, "\"\"\"") {
                        StringDelimiterKind::TripleQuote
                    } else {
                        StringDelimiterKind::DoubleQuote
                    };
                    tokens.extend(self.tokenize_string(
                        start_line,
                        start_column,
                        leading_trivia,
                        delimiter,
                    )?);
                }
                '`' => {
                    if !self.starts_with_sequence(&chars, "```") {
                        return Err(LexError::UnexpectedChar('`', start_line, start_column));
                    }
                    let leading_trivia = trivia.take();
                    tokens.extend(self.tokenize_string(
                        start_line,
                        start_column,
                        leading_trivia,
                        StringDelimiterKind::BacktickBlock,
                    )?);
                }

                // Comments
                '/' => {
                    self.advance();
                    if self.current < chars.len() {
                        match chars[self.current] {
                            '/' => {
                                let mut has_jv_block_terminator = false;
                                if self.current + 1 < chars.len() && chars[self.current + 1] == '*'
                                {
                                    let mut idx = self.current + 1;
                                    while idx + 2 < chars.len() {
                                        if chars[idx] == '*'
                                            && chars[idx + 1] == '/'
                                            && chars[idx + 2] == '/'
                                        {
                                            has_jv_block_terminator = true;
                                            break;
                                        }
                                        idx += 1;
                                    }
                                }

                                if has_jv_block_terminator {
                                    let comment_body = self.read_jv_only_block_comment()?;
                                    let comment_text = format!("/*{}*//", comment_body);
                                    tokens.push(self.make_token(
                                        TokenType::LineComment(comment_text.clone()),
                                        &comment_text,
                                        start_line,
                                        start_column,
                                        trivia.take(),
                                    ));
                                } else {
                                    // Line comment
                                    let is_jv_only = matches!(
                                        chars.get(self.current + 1),
                                        Some('/') | Some('*')
                                    );
                                    let comment = self.read_line_comment();
                                    let sanitized = Self::sanitize_comment_text(
                                        JsonCommentTriviaKind::Line,
                                        &comment,
                                    );
                                    let comment_info = JsonCommentTrivia {
                                        kind: JsonCommentTriviaKind::Line,
                                        text: sanitized,
                                        line: start_line,
                                        column: start_column,
                                    };
                                    let full_text = format!("/{}", comment);
                                    let comment_trivia = SourceCommentTrivia {
                                        kind: SourceCommentKind::Line,
                                        text: full_text,
                                        line: start_line,
                                        column: start_column,
                                    };
                                    let (passthrough, jv_only_comment) = if is_jv_only {
                                        (None, Some(comment_trivia.clone()))
                                    } else {
                                        (Some(comment_trivia.clone()), None)
                                    };
                                    tokens.push(self.make_token(
                                        TokenType::LineComment(comment.clone()),
                                        &comment,
                                        start_line,
                                        start_column,
                                        trivia.take(),
                                    ));
                                    trivia.record_comment(
                                        Some(comment_info),
                                        passthrough,
                                        jv_only_comment,
                                    );
                                }
                            }
                            '*' => {
                                let is_javadoc = self.current + 1 < chars.len()
                                    && chars[self.current + 1] == '*';
                                let comment = self.read_block_comment()?;

                                if is_javadoc {
                                    tokens.push(self.make_token(
                                        TokenType::JavaDocComment(comment.clone()),
                                        &comment,
                                        start_line,
                                        start_column,
                                        trivia.take(),
                                    ));
                                    trivia.record_doc_comment(comment);
                                } else {
                                    let sanitized = Self::sanitize_comment_text(
                                        JsonCommentTriviaKind::Block,
                                        &comment,
                                    );
                                    let comment_info = JsonCommentTrivia {
                                        kind: JsonCommentTriviaKind::Block,
                                        text: sanitized,
                                        line: start_line,
                                        column: start_column,
                                    };
                                    let full_text = format!("/*{}*/", comment);
                                    let comment_trivia = SourceCommentTrivia {
                                        kind: SourceCommentKind::Block,
                                        text: full_text,
                                        line: start_line,
                                        column: start_column,
                                    };
                                    tokens.push(self.make_token(
                                        TokenType::BlockComment(comment.clone()),
                                        &comment,
                                        start_line,
                                        start_column,
                                        trivia.take(),
                                    ));
                                    trivia.record_comment(
                                        Some(comment_info),
                                        Some(comment_trivia),
                                        None,
                                    );
                                }
                            }
                            _ => {
                                tokens.push(self.make_token(
                                    TokenType::Divide,
                                    "/",
                                    start_line,
                                    start_column,
                                    trivia.take(),
                                ));
                            }
                        }
                    } else {
                        tokens.push(self.make_token(
                            TokenType::Divide,
                            "/",
                            start_line,
                            start_column,
                            trivia.take(),
                        ));
                    }
                }

                // Numbers
                c if c.is_ascii_digit() => {
                    let leading_trivia = trivia.take();
                    let (number, metadata) = self.read_number()?;
                    let mut metadata_vec = Vec::new();
                    if let Some(meta) = metadata {
                        metadata_vec.push(TokenMetadata::NumberLiteral(meta));
                    }
                    tokens.push(self.make_token_with_metadata(
                        TokenType::Number(number.clone()),
                        &number,
                        start_line,
                        start_column,
                        leading_trivia,
                        metadata_vec,
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
                        "where" => TokenType::Where,
                        "do" => TokenType::Do,
                        "while" => TokenType::While,
                        "return" => TokenType::Return,
                        "break" => TokenType::Break,
                        "continue" => TokenType::Continue,
                        "true" => TokenType::Boolean(true),
                        "false" => TokenType::Boolean(false),
                        "null" => TokenType::Null,
                        _ => TokenType::Identifier(identifier.clone()),
                    };
                    tokens.push(self.make_token(
                        token_type,
                        &identifier,
                        start_line,
                        start_column,
                        trivia.take(),
                    ));
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
            leading_trivia: trivia.take(),
            diagnostic: None,
            metadata: Vec::new(),
        });

        Ok(tokens)
    }

    fn advance(&mut self) {
        if self.current < self.input.len() {
            self.current += 1;
            self.column += 1;
        }
    }

    fn make_token(
        &self,
        token_type: TokenType,
        lexeme: &str,
        line: usize,
        column: usize,
        leading_trivia: TokenTrivia,
    ) -> Token {
        self.make_token_with_metadata(token_type, lexeme, line, column, leading_trivia, Vec::new())
    }

    fn make_token_with_metadata(
        &self,
        token_type: TokenType,
        lexeme: &str,
        line: usize,
        column: usize,
        leading_trivia: TokenTrivia,
        metadata: Vec<TokenMetadata>,
    ) -> Token {
        let diagnostic = Self::diagnostic_for(&token_type);
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            line,
            column,
            leading_trivia,
            diagnostic,
            metadata,
        }
    }

    fn diagnostic_for(token_type: &TokenType) -> Option<TokenDiagnostic> {
        match token_type {
            TokenType::While => Some(TokenDiagnostic::LegacyLoop {
                keyword: LegacyLoopKeyword::While,
            }),
            TokenType::Do => Some(TokenDiagnostic::LegacyLoop {
                keyword: LegacyLoopKeyword::Do,
            }),
            _ => None,
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

    fn read_number(&mut self) -> Result<(String, Option<NumberLiteralMetadata>), LexError> {
        let start = self.current;
        let chars: Vec<char> = self.input.chars().collect();
        let len = chars.len();

        if self.current < len && chars[self.current] == '0' && self.current + 1 < len {
            match chars[self.current + 1] {
                'x' | 'X' => {
                    self.advance();
                    self.advance();
                    let digits_start = self.current;
                    while self.current < len && chars[self.current].is_ascii_hexdigit() {
                        self.advance();
                    }
                    if self.current == digits_start {
                        let offending = chars.get(self.current).copied().unwrap_or('x');
                        return Err(LexError::UnexpectedChar(offending, self.line, self.column));
                    }
                    let value = chars[start..self.current].iter().collect();
                    return Ok((value, None));
                }
                'b' | 'B' => {
                    self.advance();
                    self.advance();
                    let digits_start = self.current;
                    while self.current < len && matches!(chars[self.current], '0' | '1') {
                        self.advance();
                    }
                    if self.current == digits_start {
                        let offending = chars.get(self.current).copied().unwrap_or('b');
                        return Err(LexError::UnexpectedChar(offending, self.line, self.column));
                    }
                    let value = chars[start..self.current].iter().collect();
                    return Ok((value, None));
                }
                _ => {}
            }
        }

        let mut normalized = String::new();
        let mut seen_dot = false;
        let mut saw_comma = false;
        let mut saw_underscore = false;

        while self.current < len {
            match chars[self.current] {
                '0'..='9' => {
                    normalized.push(chars[self.current]);
                    self.advance();
                }
                ',' => {
                    let mut lookahead = self.current + 1;
                    let mut digits_after = 0;
                    while lookahead < len && chars[lookahead].is_ascii_digit() {
                        digits_after += 1;
                        lookahead += 1;
                    }
                    if digits_after >= 3 {
                        saw_comma = true;
                        self.advance();
                    } else {
                        break;
                    }
                }
                '_' => {
                    if self.current + 1 >= len || !chars[self.current + 1].is_ascii_digit() {
                        return Err(LexError::UnexpectedChar('_', self.line, self.column));
                    }
                    saw_underscore = true;
                    self.advance();
                }
                '.' => {
                    if seen_dot {
                        return Err(LexError::UnexpectedChar('.', self.line, self.column));
                    }
                    if self.current + 1 >= len || !chars[self.current + 1].is_ascii_digit() {
                        break;
                    }
                    seen_dot = true;
                    normalized.push('.');
                    self.advance();
                }
                _ => break,
            }
        }

        if normalized.is_empty() {
            let offending = chars.get(self.current).copied().unwrap_or('\0');
            return Err(LexError::UnexpectedChar(offending, self.line, self.column));
        }

        let raw: String = chars[start..self.current].iter().collect();
        let metadata = match (saw_comma, saw_underscore) {
            (false, false) => None,
            (true, false) => Some(NumberLiteralMetadata {
                grouping: NumberGroupingKind::Comma,
                original_lexeme: raw.clone(),
            }),
            (false, true) => Some(NumberLiteralMetadata {
                grouping: NumberGroupingKind::Underscore,
                original_lexeme: raw.clone(),
            }),
            (true, true) => Some(NumberLiteralMetadata {
                grouping: NumberGroupingKind::Mixed,
                original_lexeme: raw.clone(),
            }),
        };

        Ok((normalized, metadata))
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

    fn read_jv_only_block_comment(&mut self) -> Result<String, LexError> {
        let start_line = self.line;
        let start_column = self.column;
        let chars: Vec<char> = self.input.chars().collect();

        self.advance(); // consume second '/'
        self.advance(); // consume '*'
        let start = self.current;

        while self.current + 2 < chars.len() {
            if chars[self.current] == '*'
                && chars[self.current + 1] == '/'
                && chars[self.current + 2] == '/'
            {
                let comment: String = chars[start..self.current].iter().collect();
                self.advance(); // consume '*'
                self.advance(); // consume '/'
                self.advance(); // consume '/'
                return Ok(comment);
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
        initial_trivia: TokenTrivia,
        delimiter_kind: StringDelimiterKind,
    ) -> Result<Vec<Token>, LexError> {
        let chars: Vec<char> = self.input.chars().collect();
        let metadata = StringLiteralMetadata::from_kind(delimiter_kind);
        let allows_interpolation = metadata.allows_interpolation;
        let closing_chars: Vec<char> = metadata.delimiter.closing_sequence().chars().collect();
        let opening_len = metadata.delimiter.opening_sequence().chars().count();

        for _ in 0..opening_len {
            self.advance();
        }

        let mut tokens = Vec::new();
        let mut current_string = String::new();
        let mut leading_trivia = Some(initial_trivia);
        let interpolation_prefix: [char; 2] = ['$', '{'];

        while self.current < chars.len() {
            if self.starts_with_chars(&chars, &closing_chars) {
                break;
            }

            if allows_interpolation && self.starts_with_chars(&chars, &interpolation_prefix) {
                if tokens.is_empty() {
                    tokens.push(self.make_token(
                        TokenType::StringStart,
                        &current_string,
                        start_line,
                        start_column,
                        leading_trivia.take().unwrap_or_default(),
                    ));
                } else {
                    tokens.push(self.make_token(
                        TokenType::StringMid,
                        &current_string,
                        self.line,
                        self.column,
                        TokenTrivia::default(),
                    ));
                }
                current_string.clear();

                self.advance(); // $
                self.advance(); // {

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

                let expr: String = chars[expr_start..self.current].iter().collect();
                let mut expr_lexer = Lexer::new(expr.trim().to_string());
                let expr_tokens = expr_lexer.tokenize()?;

                for token in expr_tokens
                    .into_iter()
                    .filter(|t| !matches!(t.token_type, TokenType::Eof))
                {
                    tokens.push(token);
                }

                self.advance(); // consume closing '}'
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

        if self.current >= chars.len() || !self.starts_with_chars(&chars, &closing_chars) {
            return Err(LexError::UnterminatedString(start_line, start_column));
        }

        if tokens.is_empty() {
            let string_content = current_string.clone();
            tokens.push(self.make_token(
                TokenType::String(current_string),
                &string_content,
                start_line,
                start_column,
                leading_trivia.take().unwrap_or_default(),
            ));
        } else {
            tokens.push(self.make_token(
                TokenType::StringEnd,
                &current_string,
                self.line,
                self.column,
                TokenTrivia::default(),
            ));
        }

        for _ in 0..closing_chars.len() {
            self.advance();
        }

        if let Some(first_token) = tokens.first_mut() {
            first_token
                .metadata
                .push(TokenMetadata::StringLiteral(metadata));
        }

        Ok(tokens)
    }

    fn starts_with_chars(&self, chars: &[char], sequence: &[char]) -> bool {
        if self.current + sequence.len() > chars.len() {
            return false;
        }

        for (idx, expected) in sequence.iter().enumerate() {
            if chars[self.current + idx] != *expected {
                return false;
            }
        }

        true
    }

    fn starts_with_sequence(&self, chars: &[char], sequence: &str) -> bool {
        let seq_chars: Vec<char> = sequence.chars().collect();
        self.starts_with_chars(chars, &seq_chars)
    }

    fn json_start_metadata_for_brace(&self, chars: &[char]) -> Vec<TokenMetadata> {
        self.detect_json_confidence_for_brace(chars)
            .map(|confidence| vec![TokenMetadata::PotentialJsonStart { confidence }])
            .unwrap_or_default()
    }

    fn json_start_metadata_for_bracket(&self, chars: &[char]) -> Vec<TokenMetadata> {
        self.detect_json_confidence_for_bracket(chars)
            .map(|confidence| vec![TokenMetadata::PotentialJsonStart { confidence }])
            .unwrap_or_default()
    }

    fn detect_json_confidence_for_brace(&self, chars: &[char]) -> Option<JsonConfidence> {
        let len = chars.len();
        if self.current >= len {
            return None;
        }

        let mut idx = self.current + 1;
        idx = self.skip_ws_and_comments(chars, idx);
        if idx >= len {
            return None;
        }

        match chars[idx] {
            '}' => return Some(JsonConfidence::High),
            '"' => return Some(JsonConfidence::High),
            _ => {}
        }

        if self.find_colon_before_block_close(chars, idx) {
            Some(JsonConfidence::High)
        } else {
            None
        }
    }

    fn detect_json_confidence_for_bracket(&self, chars: &[char]) -> Option<JsonConfidence> {
        let len = chars.len();
        if self.current >= len {
            return None;
        }

        let mut idx = self.current + 1;
        idx = self.skip_ws_and_comments(chars, idx);
        if idx >= len {
            return None;
        }

        let confidence = match chars[idx] {
            ']' => JsonConfidence::High,
            '{' | '[' | '"' => JsonConfidence::High,
            't' | 'T' | 'f' | 'F' | 'n' | 'N' => JsonConfidence::Medium,
            '-' | '0'..='9' => JsonConfidence::Medium,
            _ => JsonConfidence::None,
        };

        match confidence {
            JsonConfidence::None => None,
            value => Some(value),
        }
    }

    fn skip_ws_and_comments(&self, chars: &[char], mut idx: usize) -> usize {
        let len = chars.len();

        loop {
            while idx < len && chars[idx].is_whitespace() {
                idx += 1;
            }

            if idx + 1 < len && chars[idx] == '/' {
                match chars[idx + 1] {
                    '/' => {
                        idx += 2;
                        while idx < len && chars[idx] != '\n' {
                            idx += 1;
                        }
                        if idx < len && chars[idx] == '\n' {
                            idx += 1;
                        }
                        continue;
                    }
                    '*' => {
                        idx += 2;
                        while idx + 1 < len && !(chars[idx] == '*' && chars[idx + 1] == '/') {
                            idx += 1;
                        }
                        if idx + 1 < len {
                            idx += 2;
                        } else {
                            idx = len;
                        }
                        continue;
                    }
                    _ => {}
                }
            }

            break;
        }

        idx
    }

    fn find_colon_before_block_close(&self, chars: &[char], mut idx: usize) -> bool {
        let len = chars.len();
        let mut depth = 0usize;

        while idx < len {
            idx = self.skip_ws_and_comments(chars, idx);
            if idx >= len {
                break;
            }

            match chars[idx] {
                '"' => {
                    idx = self.skip_string_literal(chars, idx + 1);
                }
                '{' => {
                    depth += 1;
                    idx += 1;
                }
                '}' => {
                    if depth == 0 {
                        break;
                    }
                    depth = depth.saturating_sub(1);
                    idx += 1;
                }
                ':' if depth == 0 => {
                    return true;
                }
                _ => {
                    idx += 1;
                }
            }
        }

        false
    }

    fn skip_string_literal(&self, chars: &[char], mut idx: usize) -> usize {
        let len = chars.len();

        while idx < len {
            match chars[idx] {
                '\\' => {
                    idx = (idx + 2).min(len);
                }
                '"' => {
                    idx += 1;
                    break;
                }
                _ => idx += 1,
            }
        }

        idx
    }

    fn sanitize_comment_text(kind: JsonCommentTriviaKind, text: &str) -> String {
        match kind {
            JsonCommentTriviaKind::Line => {
                let trimmed = text.trim_start();
                let without_slashes = trimmed.trim_start_matches('/');
                without_slashes.trim_start().to_string()
            }
            JsonCommentTriviaKind::Block => text.trim().to_string(),
        }
    }
}

#[cfg(test)]
mod tests;
