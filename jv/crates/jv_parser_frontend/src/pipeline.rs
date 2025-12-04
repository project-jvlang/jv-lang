use crate::{
    formatter::{DiagnosticContext, DiagnosticFormatter, DiagnosticSeverity, ParserDiagnosticView},
    views::{FrontendDiagnostics, FrontendOutput},
};
use jv_ast::{Program, Span};
use jv_lexer::{FieldNameLabelToken, LexError, Token, TokenMetadata, TokenTrivia, TokenType};
use jv_parser2::{
    Arena, Lexer as Parser2Lexer, Parser as Parser2, Source as Parser2Source, TokenKind,
    span::{compute_line_starts, offset_to_location},
};
use thiserror::Error;

/// フロントエンドパイプラインの共通出力。
#[derive(Debug)]
pub struct PipelineArtifacts {
    pub program: Program,
    pub tokens: Vec<Token>,
    pub diagnostics: FrontendDiagnostics,
}

impl PipelineArtifacts {
    pub fn new(program: Program, tokens: Vec<Token>, diagnostics: FrontendDiagnostics) -> Self {
        Self {
            program,
            tokens,
            diagnostics,
        }
    }

    pub fn program(&self) -> &Program {
        &self.program
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn diagnostics(&self) -> &FrontendDiagnostics {
        &self.diagnostics
    }

    pub fn into_frontend_output(self) -> FrontendOutput {
        FrontendOutput::new(self.program, self.tokens, self.diagnostics)
    }

    pub fn into_parts(self) -> (Program, Vec<Token>, FrontendDiagnostics) {
        (self.program, self.tokens, self.diagnostics)
    }
}

/// フロントエンドパイプラインの実行インターフェース。
pub trait ParserPipeline {
    fn execute(&self, source: &str) -> Result<PipelineArtifacts, ParseError>;

    fn parse(&self, source: &str) -> Result<FrontendOutput, ParseError> {
        self.execute(source)
            .map(PipelineArtifacts::into_frontend_output)
    }
}

/// フロントエンド解析中に発生するエラー。
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Lexical error: {0}")]
    LexError(#[from] LexError),
    #[error("Parse error at {span:?}: {message}")]
    Syntax { message: String, span: Span },
    #[error("Unexpected end of input at {span:?}")]
    UnexpectedEof { span: Span },
}

impl ParseError {
    pub fn span(&self) -> Span {
        match self {
            ParseError::Syntax { span, .. } | ParseError::UnexpectedEof { span } => span.clone(),
            ParseError::LexError(_) => Span::dummy(),
        }
    }
}

/// 新パーサー実装に基づくパイプライン。
#[derive(Default, Debug, Clone, Copy)]
pub struct Parser2Pipeline;

impl Parser2Pipeline {
    fn lex_tokens(source: Parser2Source, line_starts: &[u32]) -> Vec<Token> {
        let mut lexer = Parser2Lexer::new(source.clone());
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            let lexeme = source.slice_span(token.span).unwrap_or("");
            let token_type = map_token_type(token.kind, lexeme);
            let start = offset_to_location(line_starts, token.span.start);
            let metadata = build_metadata(
                token.kind,
                lexeme,
                start.line as usize,
                start.column as usize,
            );

            tokens.push(Token {
                token_type,
                lexeme: lexeme.to_string(),
                line: start.line as usize,
                column: start.column as usize,
                leading_trivia: TokenTrivia::default(),
                diagnostic: None,
                metadata,
            });

            if token.kind == TokenKind::Eof {
                break;
            }
        }

        tokens
    }

    fn parse_program(
        source: Parser2Source,
        line_starts: &[u32],
    ) -> Result<(Program, Vec<ParserDiagnosticView>), ParseError> {
        let (program, diagnostics) = {
            let arena = Arena::new();
            let mut parser = Parser2::new(Parser2Lexer::new(source), &arena);
            let result = parser.parse();

            let diagnostics = result
                .diagnostics
                .iter()
                .map(|diag| {
                    ParserDiagnosticView::with_code(
                        "parser2",
                        diag.message.clone(),
                        Some(to_ast_span(diag.span, line_starts)),
                        map_severity(diag.kind),
                        None,
                    )
                })
                .collect::<Vec<_>>();

            let program = result
                .ast
                .as_ref()
                .map(|root| root.to_owned())
                .ok_or_else(|| {
                    let span = diagnostics
                        .first()
                        .and_then(|diag| diag.span().cloned())
                        .unwrap_or_else(Span::dummy);
                    let message = diagnostics
                        .first()
                        .map(|diag| diag.message().to_string())
                        .unwrap_or_else(|| "Parse failed".to_string());
                    ParseError::Syntax { message, span }
                })?;

            (program, diagnostics)
        };

        Ok((program, diagnostics))
    }
}

impl ParserPipeline for Parser2Pipeline {
    fn execute(&self, source: &str) -> Result<PipelineArtifacts, ParseError> {
        let line_starts = compute_line_starts(source);
        let parser_source = Parser2Source::from_str(source);
        let tokens = Self::lex_tokens(parser_source.clone(), &line_starts);
        let (program, parser_diagnostics) = Self::parse_program(parser_source, &line_starts)?;

        let formatter = DiagnosticFormatter::default();
        let formatted = formatter.format(DiagnosticContext::new(
            &parser_diagnostics,
            &[],
            None,
            &[],
            None,
        ));
        let diagnostics = FrontendDiagnostics::new(formatted, Vec::new(), None, Vec::new(), None);

        Ok(PipelineArtifacts::new(program, tokens, diagnostics))
    }
}

fn to_ast_span(span: jv_parser2::span::Span, line_starts: &[u32]) -> Span {
    let start = offset_to_location(line_starts, span.start);
    let end = offset_to_location(line_starts, span.end);
    Span::new(
        start.line as usize,
        start.column as usize,
        end.line as usize,
        end.column as usize,
    )
}

fn map_severity(kind: jv_parser2::diagnostics::DiagnosticKind) -> DiagnosticSeverity {
    match kind {
        jv_parser2::diagnostics::DiagnosticKind::Error => DiagnosticSeverity::Error,
        jv_parser2::diagnostics::DiagnosticKind::Warning => DiagnosticSeverity::Warning,
        jv_parser2::diagnostics::DiagnosticKind::Info => DiagnosticSeverity::Information,
    }
}

fn map_token_type(kind: TokenKind, lexeme: &str) -> TokenType {
    match kind {
        TokenKind::Identifier => TokenType::Identifier(lexeme.to_string()),
        TokenKind::Underscore => TokenType::Underscore,
        TokenKind::ImplicitParam => {
            let index = lexeme
                .chars()
                .filter(|ch| ch.is_ascii_digit())
                .collect::<String>()
                .parse()
                .unwrap_or(0);
            TokenType::ImplicitParam(index)
        }
        TokenKind::Number => TokenType::Number(lexeme.to_string()),
        TokenKind::String => TokenType::String(lexeme.to_string()),
        TokenKind::StringInterpolation => TokenType::StringInterpolation(lexeme.to_string()),
        TokenKind::Character => {
            let ch = lexeme.trim_matches('\'').chars().next().unwrap_or_default();
            TokenType::Character(ch)
        }
        TokenKind::Boolean => TokenType::Boolean(lexeme.trim().eq_ignore_ascii_case("true")),
        TokenKind::TrueKw => TokenType::True,
        TokenKind::FalseKw => TokenType::False,
        TokenKind::Regex => TokenType::RegexLiteral(lexeme.to_string()),
        TokenKind::Assign => TokenType::Assign,
        TokenKind::Plus => TokenType::Plus,
        TokenKind::Minus => TokenType::Minus,
        TokenKind::Multiply => TokenType::Multiply,
        TokenKind::Divide => TokenType::Divide,
        TokenKind::Modulo => TokenType::Modulo,
        TokenKind::Equal => TokenType::Equal,
        TokenKind::NotEqual => TokenType::NotEqual,
        TokenKind::Less => TokenType::Less,
        TokenKind::LessEqual => TokenType::LessEqual,
        TokenKind::Greater => TokenType::Greater,
        TokenKind::GreaterEqual => TokenType::GreaterEqual,
        TokenKind::And => TokenType::And,
        TokenKind::Or => TokenType::Or,
        TokenKind::Not => TokenType::Not,
        TokenKind::RangeExclusive => TokenType::RangeExclusive,
        TokenKind::RangeInclusive => TokenType::RangeInclusive,
        TokenKind::NullSafe => TokenType::NullSafe,
        TokenKind::Elvis => TokenType::Elvis,
        TokenKind::Question => TokenType::Question,
        TokenKind::Arrow => TokenType::Arrow,
        TokenKind::FatArrow => TokenType::FatArrow,
        TokenKind::PipeLeft | TokenKind::LeftBrace => TokenType::LeftBrace,
        TokenKind::PipeRight | TokenKind::RightBrace => TokenType::RightBrace,
        TokenKind::LeftParen => TokenType::LeftParen,
        TokenKind::RightParen => TokenType::RightParen,
        TokenKind::LeftBracket => TokenType::LeftBracket,
        TokenKind::RightBracket => TokenType::RightBracket,
        TokenKind::Comma => TokenType::Comma,
        TokenKind::LayoutComma => TokenType::LayoutComma,
        TokenKind::Dot => TokenType::Dot,
        TokenKind::Semicolon => TokenType::Semicolon,
        TokenKind::Colon => TokenType::Colon,
        TokenKind::DoubleColon => TokenType::DoubleColon,
        TokenKind::At => TokenType::At,
        TokenKind::Val => TokenType::Val,
        TokenKind::Var => TokenType::Var,
        TokenKind::Test => TokenType::Identifier(lexeme.to_string()),
        TokenKind::When => TokenType::When,
        TokenKind::Data => TokenType::Data,
        TokenKind::Class => TokenType::Class,
        TokenKind::Fun => TokenType::Fun,
        TokenKind::Where => TokenType::Where,
        TokenKind::If => TokenType::If,
        TokenKind::Else => TokenType::Else,
        TokenKind::For => TokenType::For,
        TokenKind::In => TokenType::In,
        TokenKind::While => TokenType::While,
        TokenKind::Do => TokenType::Do,
        TokenKind::Return => TokenType::Return,
        TokenKind::Throw => TokenType::Throw,
        TokenKind::Break => TokenType::Break,
        TokenKind::Continue => TokenType::Continue,
        TokenKind::NullKw => TokenType::Null,
        TokenKind::Package => TokenType::Package,
        TokenKind::Import => TokenType::Import,
        TokenKind::Log => TokenType::Log,
        TokenKind::Trace => TokenType::Trace,
        TokenKind::Debug => TokenType::Debug,
        TokenKind::Info => TokenType::Info,
        TokenKind::Warn => TokenType::Warn,
        TokenKind::Error => TokenType::Error,
        TokenKind::Assert => TokenType::Identifier(lexeme.to_string()),
        TokenKind::StringStart => TokenType::StringStart,
        TokenKind::StringMid => TokenType::StringMid,
        TokenKind::StringEnd => TokenType::StringEnd,
        TokenKind::LineComment => TokenType::LineComment(lexeme.to_string()),
        TokenKind::BlockComment => TokenType::BlockComment(lexeme.to_string()),
        TokenKind::JavaDocComment => TokenType::JavaDocComment(lexeme.to_string()),
        TokenKind::FieldNameLabel => TokenType::FieldNameLabel(FieldNameLabelToken {
            primary: Some(lexeme.to_string()),
            primary_span: None,
            secondary: Vec::new(),
        }),
        TokenKind::Whitespace => TokenType::Whitespace(lexeme.to_string()),
        TokenKind::Newline => TokenType::Newline,
        TokenKind::Eof => TokenType::Eof,
        TokenKind::Invalid => TokenType::Invalid(lexeme.to_string()),
        TokenKind::KeywordSentinel => TokenType::Invalid(lexeme.to_string()),
        TokenKind::__Count => TokenType::Invalid(lexeme.to_string()),
    }
}

fn build_metadata(kind: TokenKind, lexeme: &str, line: usize, column: usize) -> Vec<TokenMetadata> {
    match kind {
        TokenKind::Regex => vec![TokenMetadata::RegexLiteral {
            raw: lexeme.to_string(),
            pattern: lexeme.trim_matches('/').to_string(),
        }],
        TokenKind::LayoutComma => vec![TokenMetadata::LayoutComma(jv_lexer::LayoutCommaMetadata {
            sequence: jv_lexer::LayoutSequenceKind::Array,
            explicit_separator: Some(jv_lexer::ExplicitSeparatorLocation { line, column }),
        })],
        _ => Vec::new(),
    }
}
