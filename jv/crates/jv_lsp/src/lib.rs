// jv_lsp - Language Server Protocol implementation
use jv_checker::diagnostics::{
    collect_raw_type_diagnostics, from_check_error, from_parse_error, from_transform_error,
    DiagnosticStrategy, EnhancedDiagnostic,
};
use jv_checker::regex::RegexValidator;
use jv_checker::{CheckError, RegexAnalysis, TypeChecker};
use jv_inference::{service::TypeFactsSnapshot, ParallelInferenceConfig};
use jv_ir::transform_program;
use jv_parser::Parser as JvParser;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use thiserror::Error;

const COMPLETION_TEMPLATES: &[&str] = &[
    "name = value",
    "var name = value",
    "data Point(x y)",
    "data Record(name: Type)",
    "fun name(params) { }",
];

const REGEX_COMPLETION_TEMPLATES: &[&str] =
    &[r"^\d+$", r"^[a-z0-9_]+$", r"^[\w\.-]+@[\w\.-]+\.\w+$"];

const MAX_REGEX_PREVIEW_LENGTH: usize = 48;
const HOVER_TEXT_MAX_LENGTH: usize = 160;

struct SequenceOperationDoc {
    name: &'static str,
    description: &'static str,
    example: &'static str,
    java_output: &'static str,
    completion: &'static str,
    requires_lambda: bool,
}

const SEQUENCE_OPERATIONS: &[SequenceOperationDoc] = &[
    SequenceOperationDoc {
        name: "map",
        description: "遅延評価Sequenceへ変換し、各要素を変換します。",
        example: "numbers.map { value -> value * 2 }.toList()",
        java_output: ".stream().map(value -> value * 2)",
        completion: "map { value -> value * 2 }",
        requires_lambda: true,
    },
    SequenceOperationDoc {
        name: "filter",
        description: "述語に一致する要素だけを保持します。",
        example: "numbers.filter { value -> value % 2 == 0 }.toList()",
        java_output: ".stream().filter(value -> value % 2 == 0)",
        completion: "filter { value -> value % 2 == 0 }",
        requires_lambda: true,
    },
    SequenceOperationDoc {
        name: "flatMap",
        description: "各要素をイテラブルへ展開し、遅延的に平坦化します。",
        example: "groups.flatMap { entry -> entry.values }.toList()",
        java_output: ".stream().flatMap(entry -> entry.values.stream())",
        completion: "flatMap { value -> value }",
        requires_lambda: true,
    },
    SequenceOperationDoc {
        name: "take",
        description: "先頭から指定数の要素だけを取り出します。",
        example: "numbers.take(3).toList()",
        java_output: ".stream().limit(3)",
        completion: "take(3)",
        requires_lambda: false,
    },
    SequenceOperationDoc {
        name: "drop",
        description: "先頭から指定数の要素をスキップします。",
        example: "numbers.drop(2).toList()",
        java_output: ".stream().skip(2)",
        completion: "drop(2)",
        requires_lambda: false,
    },
    SequenceOperationDoc {
        name: "sorted",
        description: "自然順序でソートされた遅延シーケンスを返します。",
        example: "numbers.sorted().toList()",
        java_output: ".stream().sorted()",
        completion: "sorted()",
        requires_lambda: false,
    },
    SequenceOperationDoc {
        name: "sortedBy",
        description: "キー選択ラムダを用いてソート順を指定します。",
        example: "people.sortedBy { person -> person.age }.toList()",
        java_output: ".stream().sorted(Comparator.comparing(person -> person.age))",
        completion: "sortedBy { value -> value.key }",
        requires_lambda: true,
    },
    SequenceOperationDoc {
        name: "groupBy",
        description: "キーごとに要素を`LinkedHashMap`へ集約します。",
        example: "words.groupBy { word -> word.substring(0 1) }",
        java_output: ".stream().collect(Collectors.groupingBy(word -> word.substring(0, 1), LinkedHashMap::new, Collectors.toList()))",
        completion: "groupBy { value -> value.key() }",
        requires_lambda: true,
    },
    SequenceOperationDoc {
        name: "associate",
        description: "キーと値のペアからマップを生成します。",
        example: "entries.associate { entry -> Map.entry(entry.id entry.value) }",
        java_output: ".stream().collect(Collectors.toMap(entry -> entry.id, entry -> entry.value, (left, _right) -> left, LinkedHashMap::new))",
        completion: "associate { value -> Map.entry(value.key value.value) }",
        requires_lambda: true,
    },
    SequenceOperationDoc {
        name: "fold",
        description: "初期値とラムダで累積演算を行います。",
        example: "numbers.fold(0) { acc value -> acc + value }",
        java_output: ".stream().reduce(0, (acc, value) -> acc + value)",
        completion: "fold(initial) { acc value -> acc }",
        requires_lambda: true,
    },
    SequenceOperationDoc {
        name: "reduce",
        description: "先頭要素を初期値として累積演算を行います。",
        example: "numbers.reduce { left right -> left + right }",
        java_output: ".stream().reduce((left, right) -> left + right)",
        completion: "reduce { left right -> left }",
        requires_lambda: true,
    },
    SequenceOperationDoc {
        name: "count",
        description: "遅延チェーンを評価し要素数を返します。",
        example: "numbers.count()",
        java_output: ".stream().count()",
        completion: "count()",
        requires_lambda: false,
    },
    SequenceOperationDoc {
        name: "sum",
        description: "数値要素を合計し`long`として返します。",
        example: "numbers.map { value -> value + 1 }.sum()",
        java_output: ".stream().mapToLong(value -> value + 1).sum()",
        completion: "sum()",
        requires_lambda: false,
    },
    SequenceOperationDoc {
        name: "toList",
        description: "結果を`List`へ収集します。Java 25では`.toList()`、Java 21では`Collectors.toList()`が出力されます。",
        example: "numbers.map { value -> value * 2 }.toList()",
        java_output: ".stream().toList() // Java 21: .collect(Collectors.toList())",
        completion: "toList()",
        requires_lambda: false,
    },
    SequenceOperationDoc {
        name: "forEach",
        description: "各要素に副作用ラムダを適用します（引数は明示が必要です）。",
        example: "numbers.forEach { value -> println(value) }",
        java_output: ".stream().forEach(value -> System.out.println(value))",
        completion: "forEach { value -> /* side effects */ }",
        requires_lambda: true,
    },
];

const SEQUENCE_LAMBDA_OPERATIONS: &[&str] = &[
    "map",
    "filter",
    "flatMap",
    "sortedBy",
    "groupBy",
    "associate",
    "fold",
    "reduce",
    "forEach",
];

#[derive(Error, Debug)]
pub enum LspError {
    #[error("Protocol error: {0}")]
    ProtocolError(String),
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub line: u32,
    pub character: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    pub uri: String,
    pub range: Range,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub range: Range,
    pub severity: Option<DiagnosticSeverity>,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub code: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub suggestions: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub strategy: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DiagnosticSeverity {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
}

#[derive(Debug, Clone)]
pub struct HoverResult {
    pub contents: String,
    pub range: Range,
}

pub struct JvLanguageServer {
    documents: HashMap<String, String>,
    type_facts: HashMap<String, TypeFactsSnapshot>,
    regex_metadata: HashMap<String, Vec<RegexAnalysis>>,
    parallel_config: ParallelInferenceConfig,
}

impl JvLanguageServer {
    pub fn new() -> Self {
        Self::with_parallel_config(ParallelInferenceConfig::default())
    }

    pub fn with_parallel_config(config: ParallelInferenceConfig) -> Self {
        Self {
            documents: HashMap::new(),
            type_facts: HashMap::new(),
            regex_metadata: HashMap::new(),
            parallel_config: config,
        }
    }

    pub fn set_parallel_config(&mut self, config: ParallelInferenceConfig) {
        self.parallel_config = config;
    }

    pub fn open_document(&mut self, uri: String, content: String) {
        self.documents.insert(uri.clone(), content);
        self.type_facts.remove(&uri);
        self.regex_metadata.remove(&uri);
    }

    pub fn get_diagnostics(&mut self, uri: &str) -> Vec<Diagnostic> {
        let Some(content) = self.documents.get(uri) else {
            return Vec::new();
        };

        let program = match JvParser::parse(content) {
            Ok(program) => program,
            Err(error) => {
                self.type_facts.remove(uri);
                self.regex_metadata.remove(uri);
                return match from_parse_error(&error) {
                    Some(diagnostic) => vec![tooling_diagnostic_to_lsp(
                        uri,
                        diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                    )],
                    None => vec![fallback_diagnostic(uri, "Parser error")],
                };
            }
        };

        let mut diagnostics = Vec::new();
        let mut type_facts_snapshot: Option<TypeFactsSnapshot> = None;
        let mut regex_analyses: Vec<RegexAnalysis> = Vec::new();

        let mut checker = TypeChecker::with_parallel_config(self.parallel_config);
        let check_result = checker.check_program(&program);

        if let Some(analyses) = checker.regex_analyses() {
            regex_analyses = analyses.to_vec();
        }

        if regex_analyses.is_empty() {
            let mut validator = RegexValidator::new();
            let _ = validator.validate_program(&program);
            regex_analyses = validator.take_analyses();
        }

        match check_result {
            Ok(_) => {
                let null_safety_warnings = if let Some(normalized) = checker.normalized_program() {
                    let cloned = normalized.clone();
                    checker.check_null_safety(&cloned, None)
                } else {
                    checker.check_null_safety(&program, None)
                };
                if let Some(snapshot) = checker.take_inference_snapshot() {
                    type_facts_snapshot = Some(snapshot.type_facts().clone());
                } else {
                    self.type_facts.remove(uri);
                }
                diagnostics.extend(
                    null_safety_warnings
                        .into_iter()
                        .map(|warning| warning_diagnostic(uri, warning)),
                );
            }
            Err(errors) => {
                self.type_facts.remove(uri);
                if regex_analyses.is_empty() {
                    self.regex_metadata.remove(uri);
                } else {
                    self.regex_metadata
                        .insert(uri.to_string(), regex_analyses.clone());
                }
                return errors
                    .into_iter()
                    .map(|error| type_error_to_diagnostic(uri, error))
                    .collect();
            }
        }

        let lowering_input = checker.take_normalized_program().unwrap_or(program);
        let ir_program = match transform_program(lowering_input) {
            Ok(ir) => Some(ir),
            Err(error) => {
                diagnostics.push(match from_transform_error(&error) {
                    Some(diagnostic) => tooling_diagnostic_to_lsp(
                        uri,
                        diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                    ),
                    None => fallback_diagnostic(uri, "IR transformation error"),
                });
                None
            }
        };

        if let Some(ir_program) = ir_program.as_ref() {
            let raw_type_diagnostics = collect_raw_type_diagnostics(ir_program);
            diagnostics.extend(raw_type_diagnostics.into_iter().map(|diagnostic| {
                tooling_diagnostic_to_lsp(
                    uri,
                    diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                )
            }));
        }

        for analysis in &regex_analyses {
            for diagnostic in &analysis.diagnostics {
                diagnostics.push(tooling_diagnostic_to_lsp(
                    uri,
                    diagnostic
                        .clone()
                        .with_strategy(DiagnosticStrategy::Interactive),
                ));
            }
        }

        if let Some(snapshot) = type_facts_snapshot {
            self.type_facts.insert(uri.to_string(), snapshot);
        }

        if regex_analyses.is_empty() {
            self.regex_metadata.remove(uri);
        } else {
            self.regex_metadata
                .insert(uri.to_string(), regex_analyses.clone());
        }

        diagnostics.extend(sequence_lambda_diagnostics(content));

        diagnostics
    }

    pub fn get_completions(&self, uri: &str, position: Position) -> Vec<String> {
        let mut items: Vec<String> = COMPLETION_TEMPLATES
            .iter()
            .map(|template| (*template).to_string())
            .collect();

        if let Some(content) = self.documents.get(uri) {
            if should_offer_sequence_completions(content, &position) {
                items.extend(SEQUENCE_OPERATIONS.iter().map(sequence_completion_item));
            }
        }

        if let Some(analyses) = self.regex_metadata.get(uri) {
            items.extend(
                REGEX_COMPLETION_TEMPLATES
                    .iter()
                    .map(|template| format!("regex template: {}", template)),
            );
            for analysis in analyses {
                if analysis.pattern.trim().is_empty() && analysis.raw.trim().is_empty() {
                    continue;
                }
                let preview = sanitize_regex_preview(&analysis.raw, &analysis.pattern);
                items.push(format!("regex literal: {}", preview));
            }
        }

        let mut seen = HashSet::new();
        items.retain(|entry| seen.insert(entry.clone()));
        items
    }

    pub fn type_facts(&self, uri: &str) -> Option<&TypeFactsSnapshot> {
        self.type_facts.get(uri)
    }

    pub fn get_hover(&self, uri: &str, position: Position) -> Option<HoverResult> {
        if let Some(hover) = self.sequence_hover(uri, &position) {
            return Some(hover);
        }

        let analyses = self.regex_metadata.get(uri)?;
        let analysis = analyses
            .iter()
            .find(|analysis| position_overlaps_span(&position, &analysis.span))?;
        let range = span_to_range(&analysis.span);
        let contents = format_hover_contents(analysis);
        Some(HoverResult { contents, range })
    }

    pub fn regex_metadata(&self, uri: &str) -> Option<&[RegexAnalysis]> {
        self.regex_metadata
            .get(uri)
            .map(|entries| entries.as_slice())
    }

    fn sequence_hover(&self, uri: &str, position: &Position) -> Option<HoverResult> {
        let content = self.documents.get(uri)?;
        let (start, end, word) = word_at_position(content, position)?;
        let operation = SEQUENCE_OPERATIONS
            .iter()
            .find(|candidate| candidate.name == word)?;
        let contents = build_sequence_hover(operation);
        Some(HoverResult {
            contents,
            range: Range {
                start: Position {
                    line: position.line,
                    character: start as u32,
                },
                end: Position {
                    line: position.line,
                    character: end as u32,
                },
            },
        })
    }
}

impl Default for JvLanguageServer {
    fn default() -> Self {
        Self::new()
    }
}

fn tooling_diagnostic_to_lsp(uri: &str, diagnostic: EnhancedDiagnostic) -> Diagnostic {
    let range = diagnostic
        .span
        .as_ref()
        .map(span_to_range)
        .unwrap_or_else(default_range);

    Diagnostic {
        range,
        severity: Some(map_severity(diagnostic.severity)),
        message: format!(
            "{code}: {title}\nSource: {uri}\n{detail}",
            code = diagnostic.code,
            title = diagnostic.title,
            uri = uri,
            detail = diagnostic.message,
        ),
        code: Some(diagnostic.code.to_string()),
        source: Some("jv-lsp".to_string()),
        help: Some(diagnostic.help.to_string()),
        suggestions: diagnostic.suggestions.clone(),
        strategy: Some(format!("{:?}", diagnostic.strategy)),
    }
}

fn fallback_diagnostic(uri: &str, label: &str) -> Diagnostic {
    let message = format!("{}: unable to analyse {}", label, uri);
    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::Warning),
        message,
        code: None,
        source: Some("jv-lsp".to_string()),
        help: None,
        suggestions: Vec::new(),
        strategy: Some("Immediate".to_string()),
    }
}

fn warning_diagnostic(uri: &str, warning: CheckError) -> Diagnostic {
    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::Warning),
        message: format!("Warning ({uri}): {warning}"),
        code: None,
        source: Some("jv-lsp".to_string()),
        help: None,
        suggestions: Vec::new(),
        strategy: Some("Deferred".to_string()),
    }
}

fn type_error_to_diagnostic(uri: &str, error: CheckError) -> Diagnostic {
    if let Some(diagnostic) = from_check_error(&error) {
        tooling_diagnostic_to_lsp(
            uri,
            diagnostic.with_strategy(DiagnosticStrategy::Interactive),
        )
    } else {
        Diagnostic {
            range: default_range(),
            severity: Some(DiagnosticSeverity::Error),
            message: format!("Type error: {error}"),
            code: None,
            source: Some("jv-lsp".to_string()),
            help: None,
            suggestions: Vec::new(),
            strategy: Some("Immediate".to_string()),
        }
    }
}

fn map_severity(severity: jv_checker::diagnostics::DiagnosticSeverity) -> DiagnosticSeverity {
    match severity {
        jv_checker::diagnostics::DiagnosticSeverity::Error => DiagnosticSeverity::Error,
        jv_checker::diagnostics::DiagnosticSeverity::Warning => DiagnosticSeverity::Warning,
        jv_checker::diagnostics::DiagnosticSeverity::Information => DiagnosticSeverity::Information,
        jv_checker::diagnostics::DiagnosticSeverity::Hint => DiagnosticSeverity::Hint,
    }
}

fn default_range() -> Range {
    Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: 0,
            character: 1,
        },
    }
}

fn span_to_range(span: &jv_ast::Span) -> Range {
    Range {
        start: Position {
            line: span.start_line.saturating_sub(1) as u32,
            character: span.start_column.saturating_sub(1) as u32,
        },
        end: Position {
            line: span.end_line.saturating_sub(1) as u32,
            character: span.end_column.saturating_sub(1) as u32,
        },
    }
}

fn format_hover_contents(analysis: &RegexAnalysis) -> String {
    let pattern = sanitize_for_inline(&analysis.pattern, HOVER_TEXT_MAX_LENGTH);
    let raw = sanitize_for_inline(&analysis.raw, HOVER_TEXT_MAX_LENGTH);
    let mut lines = vec![
        format!("Regex pattern: `{pattern}`"),
        format!("Raw literal: `{raw}`"),
        format!("Validation time: {:.2} ms", analysis.validation_duration_ms),
    ];

    if analysis.diagnostics.is_empty() {
        lines.push("Validation: passed".to_string());
    } else {
        lines.push("Validation issues:".to_string());
        for diagnostic in &analysis.diagnostics {
            lines.push(format!("- {}: {}", diagnostic.code, diagnostic.title));
            if !diagnostic.message.trim().is_empty() {
                lines.push(format!("  {}", diagnostic.message.trim()));
            }
            if !diagnostic.help.trim().is_empty() {
                lines.push(format!("  Help: {}", diagnostic.help.trim()));
            }
            for suggestion in &diagnostic.suggestions {
                lines.push(format!("  Suggestion: {}", suggestion));
            }
            if let Some(hint) = &diagnostic.learning_hints {
                if !hint.trim().is_empty() {
                    lines.push(format!("  Hint: {}", hint.trim()));
                }
            }
        }
    }

    lines.join("\n")
}

fn position_overlaps_span(position: &Position, span: &jv_ast::Span) -> bool {
    let range = span_to_range(span);
    position_in_range(position, &range)
}

fn position_in_range(position: &Position, range: &Range) -> bool {
    if position.line < range.start.line || position.line > range.end.line {
        return false;
    }
    if range.start.line == range.end.line {
        return position.character >= range.start.character
            && position.character <= range.end.character;
    }
    if position.line == range.start.line {
        return position.character >= range.start.character;
    }
    if position.line == range.end.line {
        return position.character <= range.end.character;
    }
    true
}

fn sanitize_regex_preview(raw: &str, pattern: &str) -> String {
    let candidate = if raw.trim().is_empty() { pattern } else { raw };
    sanitize_for_inline(candidate, MAX_REGEX_PREVIEW_LENGTH)
}

fn sanitize_for_inline(input: &str, max: usize) -> String {
    let compact = compact_whitespace(input);
    let truncated = truncate_with_ellipsis(&compact, max);
    escape_inline_code(&truncated)
}

fn compact_whitespace(input: &str) -> String {
    let mut result = String::with_capacity(input.len());
    let mut last_space = true;
    for ch in input.chars() {
        let normalized = match ch {
            '\n' | '\r' | '\t' => ' ',
            _ => ch,
        };
        if normalized == ' ' {
            if last_space {
                continue;
            }
            last_space = true;
            result.push(' ');
        } else {
            last_space = false;
            result.push(normalized);
        }
    }
    result.trim().to_string()
}

fn truncate_with_ellipsis(input: &str, max: usize) -> String {
    if max == 0 {
        return String::new();
    }
    let mut chars = input.chars();
    let mut collected = String::new();
    for _ in 0..max {
        match chars.next() {
            Some(ch) => collected.push(ch),
            None => return collected,
        }
    }
    if chars.next().is_some() {
        collected.push_str("...");
    }
    collected
}

fn escape_inline_code(input: &str) -> String {
    input.replace('`', "\\`")
}

fn sequence_completion_item(operation: &SequenceOperationDoc) -> String {
    let mut entry = format!("{} · {}", operation.name, operation.completion);
    if operation.requires_lambda {
        entry.push_str(" // 明示引数必須");
    }
    entry.push_str(&format!(" — {}", operation.description));
    entry
}

fn should_offer_sequence_completions(content: &str, position: &Position) -> bool {
    let line_index = position.line as usize;
    let lines: Vec<&str> = content.split('\n').collect();
    if line_index >= lines.len() {
        return false;
    }

    let line = lines[line_index];
    let mut char_index = position.character as usize;
    if char_index > line.len() {
        char_index = line.len();
    }
    if char_index == 0 {
        return false;
    }

    let prefix = &line[..char_index];
    let Some(dot_index) = prefix.rfind('.') else {
        return false;
    };

    if dot_index + 1 != prefix.len() {
        return false;
    }

    let identifier = prefix[..dot_index]
        .chars()
        .rev()
        .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
        .collect::<Vec<_>>();

    !identifier.is_empty()
}

fn word_at_position(content: &str, position: &Position) -> Option<(usize, usize, String)> {
    let line_index = position.line as usize;
    let lines: Vec<&str> = content.split('\n').collect();
    if line_index >= lines.len() {
        return None;
    }

    let line = lines[line_index];
    let mut char_index = position.character as usize;
    if char_index > line.len() {
        char_index = line.len();
    }

    let bytes = line.as_bytes();
    let mut start = char_index;
    while start > 0 && is_word_byte(bytes[start - 1]) {
        start -= 1;
    }

    let mut end = char_index;
    while end < line.len() && is_word_byte(bytes[end]) {
        end += 1;
    }

    if start == end {
        return None;
    }

    Some((start, end, line[start..end].to_string()))
}

fn build_sequence_hover(operation: &SequenceOperationDoc) -> String {
    let mut sections = Vec::new();
    sections.push(operation.description.to_string());
    sections.push(format!("**使用例:**\n```jv\n{}\n```", operation.example));
    if operation.requires_lambda {
        sections.push("**注意:** ラムダ引数は明示必須です。".to_string());
    }
    sections.push(format!("**Java出力:** `{}`", operation.java_output));
    sections.join("\n\n")
}

fn sequence_lambda_diagnostics(content: &str) -> Vec<Diagnostic> {
    sequence_lambda_issue_ranges(content)
        .into_iter()
        .map(|(start, end)| Diagnostic {
            range: offset_range(content, start, end),
            severity: Some(DiagnosticSeverity::Error),
            message: "ラムダ式の引数を明示してください。jvでは暗黙の `it` パラメータはサポートしていません。\n\n例: `{ x -> x > 0 }`"
                .to_string(),
            code: Some("E1001".to_string()),
            source: Some("jv-lsp".to_string()),
            help: Some("Sequenceチェーンのラムダでは名前付きパラメータを宣言してください。".to_string()),
            suggestions: vec![
                "コードアクション: `{ it ->` を `{ value ->` に置き換えてください。".to_string()
            ],
            strategy: Some("Immediate".to_string()),
        })
        .collect()
}

fn sequence_lambda_issue_ranges(content: &str) -> Vec<(usize, usize)> {
    let mut issues = Vec::new();
    let mut search_index = 0usize;
    while let Some(relative) = content[search_index..].find('{') {
        let brace_index = search_index + relative;
        if let Some(identifier) = preceding_identifier(content, brace_index) {
            if SEQUENCE_LAMBDA_OPERATIONS.contains(&identifier.as_str()) {
                if let Some((start, end)) = detect_implicit_it(content, brace_index) {
                    issues.push((start, end));
                }
            }
        }
        search_index = brace_index + 1;
    }
    issues
}

fn preceding_identifier(content: &str, brace_index: usize) -> Option<String> {
    if brace_index == 0 {
        return None;
    }
    let bytes = content.as_bytes();
    let mut index = brace_index;
    while index > 0 && bytes[index - 1].is_ascii_whitespace() {
        index -= 1;
    }
    let end = index;
    while index > 0 && is_word_byte(bytes[index - 1]) {
        index -= 1;
    }
    if index == end {
        return None;
    }
    Some(content[index..end].to_string())
}

fn detect_implicit_it(content: &str, brace_index: usize) -> Option<(usize, usize)> {
    let bytes = content.as_bytes();
    let mut index = brace_index + 1;
    while index < bytes.len() && bytes[index].is_ascii_whitespace() {
        index += 1;
    }
    if index + 1 > bytes.len() {
        return None;
    }
    if index >= bytes.len() || !is_word_byte(bytes[index]) {
        return None;
    }
    let start = index;
    while index < bytes.len() && is_word_byte(bytes[index]) {
        index += 1;
    }
    if &content[start..index] != "it" {
        return None;
    }
    Some((start, index))
}

fn offset_range(content: &str, start: usize, end: usize) -> Range {
    Range {
        start: offset_to_position(content, start),
        end: offset_to_position(content, end),
    }
}

fn offset_to_position(content: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut character = 0u32;
    let mut index = 0usize;

    for ch in content.chars() {
        if index >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
        index += ch.len_utf8();
    }

    Position { line, character }
}

fn is_word_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

#[cfg(test)]
mod tests;
