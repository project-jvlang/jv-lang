use jv_ast::Literal as AstLiteral;
use jv_ir::{IrExpression, LogMessage};

/// ログメッセージを解析し、各種フォーマットに応じたプレースホルダー情報を提供する補助構造体。
pub struct LogMessageInterpolator<'a> {
    kind: InterpolationKind<'a>,
}

enum InterpolationKind<'a> {
    Literal(&'a str),
    Placeholder {
        raw_format: &'a str,
        segments: Vec<String>,
        args: Vec<&'a IrExpression>,
    },
    Expression(&'a IrExpression),
}

/// プレースホルダー形式で表現できる場合の結果。
#[derive(Clone)]
pub struct PlaceholderResult<'a> {
    pattern: String,
    args: Vec<&'a IrExpression>,
}

impl<'a> PlaceholderResult<'a> {
    pub fn pattern(&self) -> &str {
        &self.pattern
    }

    pub fn args(&self) -> &[&'a IrExpression] {
        &self.args
    }
}

/// Commons Logging 等の連結出力に利用するセグメント情報。
pub struct ConcatenationSegments<'a, 'b> {
    segments: &'b [String],
    args: &'b [&'a IrExpression],
}

impl<'a, 'b> ConcatenationSegments<'a, 'b> {
    pub fn segments(&self) -> &'b [String] {
        self.segments
    }

    pub fn args(&self) -> &'b [&'a IrExpression] {
        self.args
    }
}

impl<'a> LogMessageInterpolator<'a> {
    pub fn new(message: &'a LogMessage) -> Self {
        let expr = &message.expression;
        let kind = match expr {
            IrExpression::Literal(AstLiteral::String(text), _) => {
                InterpolationKind::Literal(text.as_str())
            }
            IrExpression::StringFormat {
                format_string,
                args,
                ..
            } => {
                if let Some(segments) = split_format_segments(format_string, args.len()) {
                    InterpolationKind::Placeholder {
                        raw_format: format_string.as_str(),
                        segments,
                        args: args.iter().collect(),
                    }
                } else {
                    InterpolationKind::Expression(expr)
                }
            }
            _ => InterpolationKind::Expression(expr),
        };
        Self { kind }
    }

    /// 引数を必要としない純粋なリテラル文字列を返す。
    pub fn literal_text(&self) -> Option<&str> {
        match &self.kind {
            InterpolationKind::Literal(text) => Some(text),
            InterpolationKind::Placeholder { segments, args, .. } if args.is_empty() => {
                segments.first().map(|s| s.as_str())
            }
            _ => None,
        }
    }

    /// 可変長引数を伴う `{}` 形式のパターンを生成する。
    pub fn braces_pattern(&self) -> Option<PlaceholderResult<'a>> {
        match &self.kind {
            InterpolationKind::Placeholder { segments, args, .. } if !args.is_empty() => {
                let pattern = build_brace_pattern(segments, args.len());
                Some(PlaceholderResult {
                    pattern,
                    args: args.clone(),
                })
            }
            _ => None,
        }
    }

    /// `%s` 形式のパターンを返す。
    pub fn percent_pattern(&self) -> Option<PlaceholderResult<'a>> {
        match &self.kind {
            InterpolationKind::Placeholder {
                raw_format, args, ..
            } if !args.is_empty() => Some(PlaceholderResult {
                pattern: raw_format.to_string(),
                args: args.clone(),
            }),
            _ => None,
        }
    }

    /// `{0}` 形式の番号付きパターンを生成する。
    pub fn numbered_pattern(&self) -> Option<PlaceholderResult<'a>> {
        match &self.kind {
            InterpolationKind::Placeholder { segments, args, .. } if !args.is_empty() => {
                let pattern = build_numbered_pattern(segments, args.len());
                Some(PlaceholderResult {
                    pattern,
                    args: args.clone(),
                })
            }
            _ => None,
        }
    }

    /// 文字列連結式を構築する際に必要なセグメント群を提供する。
    pub fn concatenation_segments(&self) -> Option<ConcatenationSegments<'a, '_>> {
        match &self.kind {
            InterpolationKind::Placeholder { segments, args, .. } => {
                Some(ConcatenationSegments { segments, args })
            }
            _ => None,
        }
    }

    /// プレースホルダーによる表現が行えない任意式を返す。
    pub fn expression(&self) -> Option<&'a IrExpression> {
        match &self.kind {
            InterpolationKind::Expression(expr) => Some(expr),
            _ => None,
        }
    }
}

fn split_format_segments(format: &str, arg_count: usize) -> Option<Vec<String>> {
    let mut segments = Vec::new();
    let mut remaining = format;
    let mut placeholders = 0usize;

    while let Some(index) = remaining.find("%s") {
        let (prefix, rest) = remaining.split_at(index);
        segments.push(unescape_percent(prefix));
        remaining = &rest[2..];
        placeholders += 1;
    }
    segments.push(unescape_percent(remaining));

    if placeholders == arg_count {
        Some(segments)
    } else {
        None
    }
}

fn unescape_percent(segment: &str) -> String {
    let mut result = String::with_capacity(segment.len());
    let mut chars = segment.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '%' {
            if matches!(chars.peek(), Some('%')) {
                chars.next();
                result.push('%');
                continue;
            }
        }
        result.push(ch);
    }
    result
}

fn build_brace_pattern(segments: &[String], arg_count: usize) -> String {
    let mut pattern = String::new();
    for index in 0..arg_count {
        pattern.push_str(&segments[index]);
        pattern.push_str("{}");
    }
    pattern.push_str(&segments[arg_count]);
    pattern
}

fn build_numbered_pattern(segments: &[String], arg_count: usize) -> String {
    let mut pattern = String::new();
    for index in 0..arg_count {
        pattern.push_str(&segments[index]);
        pattern.push('{');
        pattern.push_str(&index.to_string());
        pattern.push('}');
    }
    pattern.push_str(&segments[arg_count]);
    pattern
}
