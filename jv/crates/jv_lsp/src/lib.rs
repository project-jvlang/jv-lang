// jv_lsp - Language Server Protocol implementation
mod handlers;
mod highlight;

use handlers::imports::build_imports_response;
pub use handlers::imports::{ImportItem, ImportsParams, ImportsResponse};
pub use highlight::tokens::{HighlightKind, HighlightToken};
use jv_ast::types::TypeLevelExpr;
use jv_ast::{
    Argument, ConstParameter, Expression, GenericParameter, GenericSignature, Program,
    RegexLambdaReplacement, RegexLiteral, RegexReplacement, Span, Statement, StringPart,
    TypeAnnotation,
};
use jv_build::BuildConfig;
use jv_build::metadata::{
    BuildContext as SymbolBuildContext, SymbolIndexBuilder, SymbolIndexCache,
};
use jv_checker::diagnostics::{
    DiagnosticSeverity as ToolingSeverity, DiagnosticStrategy, EnhancedDiagnostic,
    collect_raw_type_diagnostics, from_check_error, from_frontend_diagnostics, from_parse_error,
    from_transform_error,
};
use jv_checker::imports::{
    ImportResolutionService, ResolvedImport, ResolvedImportKind, diagnostics as import_diagnostics,
};
use jv_checker::regex::RegexValidator;
use jv_checker::{CheckError, RegexAnalysis, TypeChecker};
use jv_inference::{
    ParallelInferenceConfig, TypeFacts,
    service::{TypeFactsSnapshot, TypeLevelValue},
    solver::Variance,
    types::{SymbolId, TypeId},
};
use jv_ir::types::{IrImport, IrImportDetail};
use jv_ir::{TransformContext, transform_program_with_context};
use jv_lexer::{Token, TokenMetadata, TokenType};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::sync::Arc;
use thiserror::Error;

use highlight::tokens::collect_highlights;

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
const CALL_ARGUMENT_COMMA_ERROR_MESSAGE: &str = "JV2102: 関数呼び出しでカンマ区切りはサポートされません。位置引数は空白または改行で区切ってください。\nJV2102: Function calls do not support comma separators. Separate positional arguments with whitespace or newlines.\nQuick Fix: calls.whitespace.remove-commas -> func a b c（例: plot(1, 2, 3) => plot(1 2 3))\nQuick Fix: calls.whitespace.remove-commas -> func a b c (Example: plot(1, 2, 3) => plot(1 2 3))\nDoc: docs/whitespace-arrays.md#function-calls";

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GenericSymbolKind {
    Function,
    Class,
    DataClass,
    Interface,
}

impl GenericSymbolKind {
    fn label(self) -> &'static str {
        match self {
            GenericSymbolKind::Function => "関数",
            GenericSymbolKind::Class => "クラス",
            GenericSymbolKind::DataClass => "dataクラス",
            GenericSymbolKind::Interface => "インターフェース",
        }
    }
}

#[derive(Debug, Clone)]
struct GenericSymbolInfo {
    name: String,
    path: Vec<String>,
    span: jv_ast::Span,
    signature: Option<GenericSignature>,
    fallback_type_parameters: Vec<String>,
    kind: GenericSymbolKind,
}

impl GenericSymbolInfo {
    fn contains(&self, position: &Position) -> bool {
        position_overlaps_span(position, &self.span)
    }

    fn declared_type_parameter_names(&self) -> Vec<String> {
        if let Some(signature) = &self.signature {
            signature
                .type_parameters()
                .iter()
                .map(|param| param.name.clone())
                .collect()
        } else {
            self.fallback_type_parameters.clone()
        }
    }

    fn type_parameters(&self) -> Option<Vec<GenericParameter>> {
        self.signature
            .as_ref()
            .map(|sig| sig.type_parameters().to_vec())
    }

    fn const_parameters(&self) -> Vec<ConstParameter> {
        self.signature
            .as_ref()
            .map(|sig| sig.const_parameters().to_vec())
            .unwrap_or_default()
    }

    fn symbol_candidates(&self, package: Option<&str>) -> Vec<String> {
        symbol_candidates(package, &self.path)
    }
}

#[derive(Debug, Clone, Default)]
struct GenericDocumentIndex {
    package: Option<String>,
    symbols: Vec<GenericSymbolInfo>,
}

impl GenericDocumentIndex {
    fn from_program(program: &Program) -> Self {
        let mut symbols = Vec::new();
        let mut path = Vec::new();
        for statement in &program.statements {
            collect_generic_symbols(statement, &mut path, &mut symbols);
        }
        Self {
            package: program.package.clone(),
            symbols,
        }
    }

    fn package(&self) -> Option<&str> {
        self.package.as_deref()
    }

    fn symbols(&self) -> &[GenericSymbolInfo] {
        &self.symbols
    }

    fn symbol_for<'a>(&'a self, word: &str, position: &Position) -> Option<&'a GenericSymbolInfo> {
        self.symbols
            .iter()
            .find(|info| info.name == word && info.contains(position))
    }
}

fn collect_generic_symbols(
    statement: &Statement,
    path: &mut Vec<String>,
    output: &mut Vec<GenericSymbolInfo>,
) {
    match statement {
        Statement::FunctionDeclaration {
            name,
            generic_signature,
            type_parameters,
            span,
            ..
        } => {
            let mut symbol_path = path.clone();
            symbol_path.push(name.clone());
            output.push(GenericSymbolInfo {
                name: name.clone(),
                path: symbol_path,
                span: span.clone(),
                signature: generic_signature.clone(),
                fallback_type_parameters: type_parameters.clone(),
                kind: GenericSymbolKind::Function,
            });
        }
        Statement::ClassDeclaration {
            name,
            type_parameters,
            generic_signature,
            methods,
            span,
            ..
        } => {
            path.push(name.clone());
            output.push(GenericSymbolInfo {
                name: name.clone(),
                path: path.clone(),
                span: span.clone(),
                signature: generic_signature.clone(),
                fallback_type_parameters: type_parameters.clone(),
                kind: GenericSymbolKind::Class,
            });
            for method in methods {
                collect_generic_symbols(method, path, output);
            }
            path.pop();
        }
        Statement::DataClassDeclaration {
            name,
            type_parameters,
            generic_signature,
            span,
            ..
        } => {
            let mut symbol_path = path.clone();
            symbol_path.push(name.clone());
            output.push(GenericSymbolInfo {
                name: name.clone(),
                path: symbol_path,
                span: span.clone(),
                signature: generic_signature.clone(),
                fallback_type_parameters: type_parameters.clone(),
                kind: GenericSymbolKind::DataClass,
            });
        }
        Statement::InterfaceDeclaration {
            name,
            type_parameters,
            generic_signature,
            methods,
            span,
            ..
        } => {
            path.push(name.clone());
            output.push(GenericSymbolInfo {
                name: name.clone(),
                path: path.clone(),
                span: span.clone(),
                signature: generic_signature.clone(),
                fallback_type_parameters: type_parameters.clone(),
                kind: GenericSymbolKind::Interface,
            });
            for method in methods {
                collect_generic_symbols(method, path, output);
            }
            path.pop();
        }
        Statement::ExtensionFunction(extension) => {
            path.push("<extension>".to_string());
            collect_generic_symbols(&extension.function, path, output);
            path.pop();
        }
        _ => {}
    }
}

fn symbol_candidates(package: Option<&str>, path: &[String]) -> Vec<String> {
    let mut candidates = Vec::new();
    if path.is_empty() {
        return candidates;
    }

    let name = path.last().cloned().unwrap_or_default();
    push_candidate(&mut candidates, name.clone());

    let colon_join = path.join("::");
    push_candidate(&mut candidates, colon_join.clone());

    if path.len() > 1 {
        push_candidate(&mut candidates, path.join("."));
        push_candidate(&mut candidates, path.join("$"));
    }

    if let Some(pkg) = package {
        if !pkg.is_empty() {
            if !colon_join.is_empty() {
                push_candidate(&mut candidates, format!("{pkg}::{colon_join}"));
            }
            let dot = path.join(".");
            if !dot.is_empty() {
                push_candidate(&mut candidates, format!("{pkg}::{dot}"));
            }
        }
    }

    candidates
}

fn push_candidate(candidates: &mut Vec<String>, value: String) {
    if value.is_empty() {
        return;
    }
    if !candidates.contains(&value) {
        candidates.push(value);
    }
}

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
    programs: HashMap<String, Program>,
    generics: HashMap<String, GenericDocumentIndex>,
    token_highlights: HashMap<String, Vec<HighlightToken>>,
    parallel_config: ParallelInferenceConfig,
}

#[derive(Debug, Clone)]
pub struct ImportPlanError {
    message: String,
}

impl ImportPlanError {
    pub fn new(message: String) -> Self {
        Self { message }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for ImportPlanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl std::error::Error for ImportPlanError {}

impl JvLanguageServer {
    pub fn new() -> Self {
        Self::with_parallel_config(ParallelInferenceConfig::default())
    }

    pub fn with_parallel_config(config: ParallelInferenceConfig) -> Self {
        Self {
            documents: HashMap::new(),
            type_facts: HashMap::new(),
            regex_metadata: HashMap::new(),
            programs: HashMap::new(),
            generics: HashMap::new(),
            token_highlights: HashMap::new(),
            parallel_config: config,
        }
    }

    pub fn set_parallel_config(&mut self, config: ParallelInferenceConfig) {
        self.parallel_config = config;
    }

    pub fn import_plan_from_content(
        &self,
        content: &str,
    ) -> Result<Vec<IrImport>, ImportPlanError> {
        let pipeline = RowanPipeline::default();
        let frontend_output = pipeline.parse(content).map_err(|error| {
            from_parse_error(&error)
                .map(|diagnostic| ImportPlanError::new(diagnostic.message))
                .unwrap_or_else(|| ImportPlanError::new(format!("Parser error: {error}")))
        })?;
        let frontend_diagnostics =
            from_frontend_diagnostics(frontend_output.diagnostics().final_diagnostics());
        if let Some(error_diag) = frontend_diagnostics
            .iter()
            .find(|diag| diag.severity == ToolingSeverity::Error)
        {
            return Err(ImportPlanError::new(error_diag.message.clone()));
        }
        let program = frontend_output.into_program();

        let build_config = BuildConfig::default();
        let symbol_context = SymbolBuildContext::from_config(&build_config);
        let cache = SymbolIndexCache::with_default_location();
        let builder = SymbolIndexBuilder::new(&symbol_context);
        let symbol_index = builder.build_with_cache(&cache).map_err(|error| {
            ImportPlanError::new(format!(
                "シンボルインデックスの構築に失敗しました: {error}\nSymbol index build failed: {error}"
            ))
        })?;
        let symbol_index = Arc::new(symbol_index);

        let import_service =
            ImportResolutionService::new(Arc::clone(&symbol_index), build_config.target);
        let mut resolved_imports = Vec::new();
        for import_stmt in &program.imports {
            match import_service.resolve(import_stmt) {
                Ok(resolved) => resolved_imports.push(resolved),
                Err(error) => {
                    if let Some(diagnostic) = import_diagnostics::from_error(&error) {
                        return Err(ImportPlanError::new(diagnostic.message));
                    }
                    return Err(ImportPlanError::new(format!(
                        "Import resolution error: {error}"
                    )));
                }
            }
        }

        Ok(lowered_import_plan(&resolved_imports))
    }

    pub fn imports_response(&self, content: &str) -> Result<ImportsResponse, ImportPlanError> {
        let plan = self.import_plan_from_content(content)?;
        Ok(build_imports_response(&plan))
    }

    pub fn open_document(&mut self, uri: String, content: String) {
        self.documents.insert(uri.clone(), content);
        self.type_facts.remove(&uri);
        self.regex_metadata.remove(&uri);
        self.programs.remove(&uri);
        self.generics.remove(&uri);
        self.token_highlights.remove(&uri);
    }

    pub fn close_document(&mut self, uri: &str) {
        self.documents.remove(uri);
        self.type_facts.remove(uri);
        self.regex_metadata.remove(uri);
        self.programs.remove(uri);
        self.generics.remove(uri);
        self.token_highlights.remove(uri);
    }

    pub fn token_highlights(&self, uri: &str) -> Option<&[HighlightToken]> {
        self.token_highlights
            .get(uri)
            .map(|entries| entries.as_slice())
    }

    pub fn get_diagnostics(&mut self, uri: &str) -> Vec<Diagnostic> {
        let Some(content) = self.documents.get(uri) else {
            return Vec::new();
        };

        self.token_highlights.remove(uri);

        let pipeline = RowanPipeline::default();
        let frontend_output = match pipeline.parse(content) {
            Ok(output) => output,
            Err(error) => {
                self.type_facts.remove(uri);
                self.regex_metadata.remove(uri);
                self.programs.remove(uri);
                self.generics.remove(uri);
                return match from_parse_error(&error) {
                    Some(diagnostic) => vec![tooling_diagnostic_to_lsp(
                        uri,
                        diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                    )],
                    None => vec![fallback_diagnostic(uri, "Parser error")],
                };
            }
        };

        let (program, tokens, diagnostics_view) = frontend_output.into_parts();

        let mut diagnostics = Vec::new();
        let mut regex_analyses: Vec<RegexAnalysis> = Vec::new();

        let highlights = collect_highlights(&tokens);
        if highlights.is_empty() {
            self.token_highlights.remove(uri);
        } else {
            self.token_highlights.insert(uri.to_string(), highlights);
        }

        let frontend_diagnostics = from_frontend_diagnostics(diagnostics_view.final_diagnostics());
        for diagnostic in &frontend_diagnostics {
            diagnostics.push(tooling_diagnostic_to_lsp(
                uri,
                diagnostic
                    .clone()
                    .with_strategy(DiagnosticStrategy::Interactive),
            ));
        }
        if frontend_diagnostics
            .iter()
            .any(|diag| diag.severity == ToolingSeverity::Error)
        {
            self.type_facts.remove(uri);
            self.regex_metadata.remove(uri);
            self.programs.remove(uri);
            self.generics.remove(uri);
            return diagnostics;
        }

        self.programs.insert(uri.to_string(), program.clone());
        self.generics.insert(
            uri.to_string(),
            GenericDocumentIndex::from_program(&program),
        );

        let build_config = BuildConfig::default();
        let symbol_context = SymbolBuildContext::from_config(&build_config);
        let cache = SymbolIndexCache::with_default_location();
        let builder = SymbolIndexBuilder::new(&symbol_context);
        let symbol_index = match builder.build_with_cache(&cache) {
            Ok(index) => Arc::new(index),
            Err(error) => {
                diagnostics.push(fallback_diagnostic(
                    uri,
                    &format!("Symbol index build failed: {error}"),
                ));
                self.type_facts.remove(uri);
                self.regex_metadata.remove(uri);
                return diagnostics;
            }
        };

        let import_service =
            ImportResolutionService::new(Arc::clone(&symbol_index), build_config.target);
        let mut resolved_imports = Vec::new();
        for import_stmt in &program.imports {
            match import_service.resolve(import_stmt) {
                Ok(resolved) => resolved_imports.push(resolved),
                Err(error) => {
                    self.type_facts.remove(uri);
                    self.regex_metadata.remove(uri);
                    return vec![match import_diagnostics::from_error(&error) {
                        Some(diagnostic) => tooling_diagnostic_to_lsp(
                            uri,
                            diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                        ),
                        None => fallback_diagnostic(uri, "Import resolution error"),
                    }];
                }
            }
        }

        let import_plan = lowered_import_plan(&resolved_imports);

        let mut checker = TypeChecker::with_parallel_config(self.parallel_config);
        if !resolved_imports.is_empty() {
            checker.set_imports(Arc::clone(&symbol_index), resolved_imports.clone());
        }
        let check_result = checker.check_program(&program);

        if let Some(analyses) = checker.regex_analyses() {
            regex_analyses = analyses.to_vec();
        }

        if regex_analyses.is_empty() {
            let mut validator = RegexValidator::new();
            let regex_program = checker.normalized_program().unwrap_or(&program);
            let _ = validator.validate_program(regex_program);
            regex_analyses = validator.take_analyses();
        }

        if regex_analyses.is_empty() {
            if let Some(regex_program) = Self::regex_program_from_tokens(&tokens) {
                let mut validator = RegexValidator::new();
                let _ = validator.validate_program(&regex_program);
                regex_analyses = validator.take_analyses();
            }
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
                    self.type_facts
                        .insert(uri.to_string(), snapshot.type_facts().clone());
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

                diagnostics.extend(
                    errors
                        .into_iter()
                        .map(|error| type_error_to_diagnostic(uri, error)),
                );
                diagnostics.extend(sequence_lambda_diagnostics(content));

                return diagnostics;
            }
        }

        let lowering_input = checker
            .take_normalized_program()
            .unwrap_or_else(|| program.clone());
        let mut context = TransformContext::new();
        if !import_plan.is_empty() {
            context.set_resolved_imports(import_plan.clone());
        }
        let ir_program = match transform_program_with_context(lowering_input, &mut context) {
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

        let mut emitted_raw_comment_diagnostics = false;
        if let Some(ir_program) = ir_program.as_ref() {
            let raw_type_diagnostics = collect_raw_type_diagnostics(ir_program);
            if !raw_type_diagnostics.is_empty() {
                emitted_raw_comment_diagnostics = true;
            }
            diagnostics.extend(raw_type_diagnostics.into_iter().map(|diagnostic| {
                tooling_diagnostic_to_lsp(
                    uri,
                    diagnostic.with_strategy(DiagnosticStrategy::Interactive),
                )
            }));
        }

        if !emitted_raw_comment_diagnostics {
            diagnostics.extend(fallback_raw_comment_diagnostics(&tokens));
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

        if let Some(index) = self.generics.get(uri) {
            let facts_ref = self.type_facts.get(uri);
            enrich_generic_diagnostics(&mut diagnostics, facts_ref, index);
        }

        if regex_analyses.is_empty() {
            self.regex_metadata.remove(uri);
        } else {
            self.regex_metadata
                .insert(uri.to_string(), regex_analyses.clone());
        }

        if !diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.as_deref() == Some("JV2102"))
        {
            diagnostics.extend(call_argument_diagnostics(&program, &tokens));
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

        if let Some(index) = self.generics.get(uri) {
            let facts = self.type_facts(uri);
            items.extend(build_generic_completions(index, facts));
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

        if let Some(hover) = self.generic_hover(uri, &position) {
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

    fn regex_program_from_tokens(tokens: &[Token]) -> Option<Program> {
        let mut statements = Vec::new();
        for token in tokens {
            for metadata in &token.metadata {
                if let TokenMetadata::RegexLiteral { raw, pattern } = metadata {
                    let end_column = token.column + raw.chars().count();
                    let span = Span::new(token.line, token.column, token.line, end_column);
                    let literal = RegexLiteral {
                        pattern: pattern.clone(),
                        raw: raw.clone(),
                        span: span.clone(),
                    };
                    statements.push(Statement::Expression {
                        expr: Expression::RegexLiteral(literal),
                        span,
                    });
                }
            }
        }

        if statements.is_empty() {
            return None;
        }

        Some(Program {
            package: None,
            imports: Vec::new(),
            statements,
            span: Span::dummy(),
        })
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

    fn generic_hover(&self, uri: &str, position: &Position) -> Option<HoverResult> {
        let content = self.documents.get(uri)?;
        let (start, end, word) = word_at_position(content, position)?;
        let index = self.generics.get(uri)?;
        let symbol = index.symbol_for(&word, position)?;
        let facts = self.type_facts(uri);
        let contents = build_generic_hover(symbol, facts, index.package());
        if contents.is_empty() {
            return None;
        }
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

#[derive(Default)]
struct SymbolSummary {
    symbol_key: Option<String>,
    type_params: Vec<TypeParamSummary>,
    const_params: Vec<ConstParamSummary>,
    type_level_results: Vec<(String, String)>,
}

#[derive(Default)]
struct TypeParamSummary {
    name: String,
    attributes: Vec<String>,
}

#[derive(Default)]
struct ConstParamSummary {
    name: String,
    ty: String,
    attributes: Vec<String>,
}

#[derive(Clone)]
struct ResolvedSymbolData {
    key: String,
    type_ids: Vec<TypeId>,
    variance: HashMap<TypeId, Variance>,
    bounds: HashMap<TypeId, String>,
}

fn build_generic_hover(
    info: &GenericSymbolInfo,
    facts: Option<&TypeFactsSnapshot>,
    package: Option<&str>,
) -> String {
    let summary = summarize_symbol(info, facts, package);
    if summary.type_params.is_empty()
        && summary.const_params.is_empty()
        && summary.type_level_results.is_empty()
    {
        return String::new();
    }

    let mut lines = Vec::new();
    lines.push(format!(
        "{} `{}` のジェネリクス情報",
        info.kind.label(),
        info.name
    ));
    if let Some(pkg) = package {
        if !pkg.is_empty() {
            lines.push(format!("パッケージ: {}", pkg));
        }
    }

    if !summary.type_params.is_empty() {
        lines.push("型パラメータ:".to_string());
        for param in summary.type_params {
            if param.attributes.is_empty() {
                lines.push(format!("• `{}`", param.name));
            } else {
                lines.push(format!(
                    "• `{}` ({})",
                    param.name,
                    param.attributes.join(", ")
                ));
            }
        }
    }

    if !summary.const_params.is_empty() {
        lines.push("const パラメータ:".to_string());
        for param in summary.const_params {
            if param.attributes.is_empty() {
                lines.push(format!("• `{}`: {}", param.name, param.ty));
            } else {
                lines.push(format!(
                    "• `{}`: {} ({})",
                    param.name,
                    param.ty,
                    param.attributes.join(", ")
                ));
            }
        }
    }

    if !summary.type_level_results.is_empty() {
        lines.push("型レベル計算結果:".to_string());
        for (slot, value) in summary.type_level_results {
            lines.push(format!("• `{}` = {}", slot, value));
        }
    }

    if let Some(facts) = facts {
        let telemetry = facts.telemetry();
        lines.push(format!(
            "Telemetry: kind検証={} / const評価={} / キャッシュ={} entries",
            telemetry.kind_checks_count,
            telemetry.const_evaluations,
            telemetry.type_level_cache_size
        ));
    }

    lines.join("\n")
}

fn build_generic_completions(
    index: &GenericDocumentIndex,
    facts: Option<&TypeFactsSnapshot>,
) -> Vec<String> {
    let mut entries = Vec::new();
    for info in index.symbols() {
        let summary = summarize_symbol(info, facts, index.package());
        if summary.type_params.is_empty() && summary.const_params.is_empty() {
            continue;
        }
        for param in &summary.type_params {
            let mut entry = format!("{} · 型パラメータ `{}`", info.name, param.name);
            if !param.attributes.is_empty() {
                entry.push_str(&format!(" ({})", param.attributes.join(", ")));
            }
            entries.push(entry);
        }
        for param in &summary.const_params {
            let mut entry = format!("{} · const `{}`", info.name, param.name);
            if !param.attributes.is_empty() {
                entry.push_str(&format!(" ({})", param.attributes.join(", ")));
            }
            entries.push(entry);
        }
        for (slot, value) in &summary.type_level_results {
            entries.push(format!("{} · type-level `{}` = {}", info.name, slot, value));
        }
    }
    entries
}

fn enrich_generic_diagnostics(
    diagnostics: &mut [Diagnostic],
    facts: Option<&TypeFactsSnapshot>,
    index: &GenericDocumentIndex,
) {
    let _ = (facts, index);
    for diagnostic in diagnostics {
        let Some(code) = diagnostic.code.as_deref() else {
            continue;
        };
        if !matches!(code, "JV2008" | "JV3101" | "JV3102" | "JV3202") {
            continue;
        }
        if !diagnostic.suggestions.is_empty() {
            continue;
        }

        let suggestion = match code {
            "JV2008" => {
                "型引数のkind注釈か部分適用を調整し、推論されたkindと一致させてください。ホバーで期待kindを確認できます。"
            }
            "JV3101" => {
                "型レベル式の入力とconstパラメータを見直し、評価が収束するように境界やデフォルト値を修正してください。"
            }
            "JV3102" => "型レベル式の上下限・比較対象を揃え、同じ型に統一してください。",
            "JV3202" => {
                "raw型を避けるようにconstパラメータや境界を補い、安全な型情報を提供してください。"
            }
            _ => continue,
        };

        diagnostic.suggestions.push(suggestion.to_string());
    }
}

fn summarize_symbol(
    info: &GenericSymbolInfo,
    facts: Option<&TypeFactsSnapshot>,
    package: Option<&str>,
) -> SymbolSummary {
    let mut summary = SymbolSummary::default();
    let candidates = info.symbol_candidates(package);
    let resolved = facts.and_then(|snapshot| resolve_type_metadata(snapshot, &candidates));

    if let Some(data) = resolved.clone() {
        summary.symbol_key = Some(data.key.clone());
    }

    let type_params = info.type_parameters();
    if let Some(params) = type_params.as_ref() {
        let type_ids = resolved
            .as_ref()
            .map(|data| data.type_ids.clone())
            .unwrap_or_default();
        let mut variance_map = resolved
            .as_ref()
            .map(|data| data.variance.clone())
            .unwrap_or_default();
        let bounds_map = resolved
            .as_ref()
            .map(|data| data.bounds.clone())
            .unwrap_or_default();

        if let Some(facts) = facts {
            for id in &type_ids {
                if let Some(recorded) = facts.recorded_variance(*id) {
                    variance_map.entry(*id).or_insert(recorded);
                }
            }
        }

        for (idx, param) in params.iter().enumerate() {
            let mut attributes = Vec::new();
            if let Some(kind) = &param.kind {
                attributes.push(format!("宣言kind: {:?}", kind));
            }
            if let Some(bounds) = format_annotation_list(&param.bounds) {
                attributes.push(format!("宣言境界: {}", bounds));
            }
            if let Some(type_id) = type_ids.get(idx) {
                if let Some(facts) = facts {
                    if let Some(kind) = facts.kind_for(*type_id) {
                        attributes.push(format!("推論kind: {:?}", kind));
                    }
                }
                if let Some(variance) = variance_map.get(type_id) {
                    attributes.push(format!("変位: {:?}", variance));
                }
                if let Some(bound) = bounds_map.get(type_id) {
                    attributes.push(format!("推論境界: {}", bound));
                }
            }
            summary.type_params.push(TypeParamSummary {
                name: param.name.clone(),
                attributes,
            });
        }
    } else {
        for name in info.declared_type_parameter_names() {
            summary.type_params.push(TypeParamSummary {
                name,
                attributes: Vec::new(),
            });
        }
    }

    for param in info.const_parameters() {
        let mut attributes = Vec::new();
        if let Some(default) = param.default.as_ref() {
            attributes.push(format!("デフォルト: {}", format_type_level_expr(default)));
        }
        if let (Some(facts), Some(key)) = (facts, summary.symbol_key.as_ref()) {
            let symbol = SymbolId::from(key.as_str());
            if let Some(value) = facts.const_binding(&symbol, &param.name) {
                attributes.push(format!("推論値: {}", format_type_level_value(value)));
            }
        }
        summary.const_params.push(ConstParamSummary {
            name: param.name.clone(),
            ty: format_type_annotation(&param.type_annotation),
            attributes,
        });
    }

    if let (Some(facts), Some(key)) = (facts, summary.symbol_key.as_ref()) {
        let symbol = SymbolId::from(key.as_str());
        if let Some(results) = facts.type_level_results_for(&symbol) {
            summary.type_level_results.extend(
                results
                    .iter()
                    .map(|(slot, value)| (slot.clone(), format_type_level_value(value))),
            );
        }
    }

    summary
}

fn resolve_type_metadata(
    facts: &TypeFactsSnapshot,
    candidates: &[String],
) -> Option<ResolvedSymbolData> {
    for candidate in candidates {
        if let Some(signature) = facts.function_signature(candidate) {
            let mut bounds = HashMap::new();
            for type_id in &signature.generics {
                if let Some(recorded) = facts.recorded_bounds(*type_id) {
                    bounds.insert(*type_id, format!("{:?}", recorded));
                }
            }
            return Some(ResolvedSymbolData {
                key: candidate.clone(),
                type_ids: signature.generics.clone(),
                variance: signature.variance.clone(),
                bounds,
            });
        }

        if let Some(scheme) = facts.scheme_for(candidate) {
            let mut variance = scheme
                .variance()
                .iter()
                .map(|(id, variance)| (*id, *variance))
                .collect::<HashMap<_, _>>();
            let mut bounds = HashMap::new();
            for type_id in scheme.generics() {
                if let Some(bound) = scheme.bounds_for(*type_id) {
                    bounds.insert(*type_id, format!("{:?}", bound));
                }
                if let Some(recorded) = facts.recorded_bounds(*type_id) {
                    bounds
                        .entry(*type_id)
                        .or_insert_with(|| format!("{:?}", recorded));
                }
                if let Some(recorded) = facts.recorded_variance(*type_id) {
                    variance.entry(*type_id).or_insert(recorded);
                }
            }
            return Some(ResolvedSymbolData {
                key: candidate.clone(),
                type_ids: scheme.generics().to_vec(),
                variance,
                bounds,
            });
        }
    }

    if let Some(first) = candidates.first() {
        return Some(ResolvedSymbolData {
            key: first.clone(),
            type_ids: Vec::new(),
            variance: HashMap::new(),
            bounds: HashMap::new(),
        });
    }

    None
}

fn format_annotation_list(annotations: &[TypeAnnotation]) -> Option<String> {
    if annotations.is_empty() {
        None
    } else {
        Some(
            annotations
                .iter()
                .map(format_type_annotation)
                .collect::<Vec<_>>()
                .join(" + "),
        )
    }
}

fn format_type_annotation(annotation: &TypeAnnotation) -> String {
    format!("{:?}", annotation)
}

fn format_type_level_expr(expr: &TypeLevelExpr) -> String {
    format!("{:?}", expr)
}

fn format_type_level_value(value: &TypeLevelValue) -> String {
    value.to_string()
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
    if let Some(diagnostic) = from_check_error(&warning) {
        return tooling_diagnostic_to_lsp(
            uri,
            diagnostic.with_strategy(DiagnosticStrategy::Interactive),
        );
    }

    if let CheckError::ValidationError { message, span } = warning {
        let range = span
            .as_ref()
            .map(span_to_range)
            .unwrap_or_else(default_range);
        return Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::Warning),
            message,
            code: None,
            source: Some("jv-lsp".to_string()),
            help: None,
            suggestions: Vec::new(),
            strategy: Some("Deferred".to_string()),
        };
    }

    Diagnostic {
        range: default_range(),
        severity: Some(DiagnosticSeverity::Warning),
        message: format!("Warning ({uri}): {}", warning),
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
        let mut message = format!("Type error: {error}");
        if matches_call_target_mismatch(&error) {
            message.push_str(
                "\nambiguous function signature detected; add annotations or defaults to disambiguate.",
            );
        }
        Diagnostic {
            range: default_range(),
            severity: Some(DiagnosticSeverity::Error),
            message,
            code: None,
            source: Some("jv-lsp".to_string()),
            help: None,
            suggestions: Vec::new(),
            strategy: Some("Immediate".to_string()),
        }
    }
}

fn matches_call_target_mismatch(error: &CheckError) -> bool {
    matches!(error, CheckError::TypeError(message) if message.contains("call target must be compatible with argument list"))
}

fn lowered_import_plan(resolved_imports: &[ResolvedImport]) -> Vec<IrImport> {
    let mut imports = Vec::new();
    let mut module_sources: BTreeMap<String, Span> = BTreeMap::new();
    let mut explicit_modules = HashSet::new();

    for resolved in resolved_imports {
        let detail = match &resolved.kind {
            ResolvedImportKind::Type { fqcn } => IrImportDetail::Type { fqcn: fqcn.clone() },
            ResolvedImportKind::Package { name } => IrImportDetail::Package { name: name.clone() },
            ResolvedImportKind::StaticMember { owner, member } => IrImportDetail::Static {
                owner: owner.clone(),
                member: member.clone(),
            },
            ResolvedImportKind::Module { name } => {
                explicit_modules.insert(name.clone());
                IrImportDetail::Module { name: name.clone() }
            }
        };

        if let Some(module) = &resolved.module_dependency {
            module_sources
                .entry(module.clone())
                .or_insert_with(|| resolved.source_span.clone());
        }

        imports.push(IrImport {
            original: resolved.original_path.clone(),
            alias: resolved.alias.clone(),
            detail,
            module_dependency: resolved.module_dependency.clone(),
            span: resolved.source_span.clone(),
        });
    }

    for (module, span) in module_sources {
        if explicit_modules.contains(&module) {
            continue;
        }
        imports.push(IrImport {
            original: module.clone(),
            alias: None,
            detail: IrImportDetail::Module { name: module },
            module_dependency: None,
            span,
        });
    }

    imports
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

fn fallback_raw_comment_diagnostics(tokens: &[Token]) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for token in tokens {
        match &token.token_type {
            TokenType::LineComment(text) | TokenType::BlockComment(text) => {
                if let Some((mode, owner)) = parse_raw_comment_directive(text) {
                    let span = comment_span_from_token(token, text);
                    diagnostics.push(build_raw_comment_diagnostic(mode, owner, span));
                }
            }
            _ => {}
        }
    }
    diagnostics
}

fn parse_raw_comment_directive(text: &str) -> Option<(RawDirectiveMode, String)> {
    let trimmed = text.trim();
    let content = if let Some(rest) = trimmed.strip_prefix("//") {
        rest.trim_start_matches('*').trim()
    } else if let Some(rest) = trimmed.strip_prefix("/*") {
        rest.trim_end_matches("*/").trim()
    } else {
        trimmed
    };

    let (mode, payload) = if let Some(rest) = content.strip_prefix("jv:raw-allow") {
        (RawDirectiveMode::AllowContinuation, rest.trim())
    } else if let Some(rest) = content.strip_prefix("jv:raw-default") {
        (RawDirectiveMode::DefaultPolicy, rest.trim())
    } else {
        return None;
    };

    let owner_token = payload.split_whitespace().next().unwrap_or("");
    let normalized_owner = owner_token
        .split('.')
        .map(|segment| segment.trim())
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>()
        .join(".");

    if normalized_owner.is_empty() {
        return None;
    }

    Some((mode, normalized_owner))
}

fn comment_span_from_token(token: &Token, text: &str) -> Span {
    let mut end_line = token.line;
    let mut end_column = token.column;

    for ch in text.chars() {
        if ch == '\n' {
            end_line += 1;
            end_column = 1;
        } else {
            end_column += 1;
        }
    }

    Span::new(token.line, token.column, end_line, end_column)
}

fn build_raw_comment_diagnostic(mode: RawDirectiveMode, owner: String, span: Span) -> Diagnostic {
    let (code, message, severity) = match mode {
        RawDirectiveMode::DefaultPolicy => (
            "JV3202",
            format!(
                "Raw型 `{owner}` を検出し、防御コードを挿入しました。ジェネリクス型を明示して警告を解消してください。/ Raw type `{owner}` detected; defensive guards were emitted. Provide explicit generics to address the warning."
            ),
            Some(DiagnosticSeverity::Warning),
        ),
        RawDirectiveMode::AllowContinuation => (
            "JV3203",
            format!(
                "Raw型 `{owner}` はコメントによって継続されています。影響範囲を再確認してください。/ Raw type `{owner}` is continued via comment; verify that the trade-offs are acceptable."
            ),
            Some(DiagnosticSeverity::Information),
        ),
    };

    Diagnostic {
        range: span_to_range(&span),
        severity,
        message,
        code: Some(code.to_string()),
        source: Some("jv-lsp".to_string()),
        help: None,
        suggestions: Vec::new(),
        strategy: Some("Interactive".to_string()),
    }
}

enum RawDirectiveMode {
    DefaultPolicy,
    AllowContinuation,
}

fn call_argument_diagnostics(program: &Program, tokens: &[Token]) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for statement in &program.statements {
        collect_call_diagnostics_from_statement(statement, tokens, &mut diagnostics);
    }
    diagnostics
}

fn collect_call_diagnostics_from_statement(
    statement: &Statement,
    tokens: &[Token],
    diagnostics: &mut Vec<Diagnostic>,
) {
    match statement {
        Statement::ValDeclaration { initializer, .. } => {
            collect_call_diagnostics_from_expression(initializer, tokens, diagnostics);
        }
        Statement::VarDeclaration { initializer, .. } => {
            if let Some(expr) = initializer {
                collect_call_diagnostics_from_expression(expr, tokens, diagnostics);
            }
        }
        Statement::FunctionDeclaration {
            body, parameters, ..
        } => {
            for parameter in parameters {
                if let Some(default) = &parameter.default_value {
                    collect_call_diagnostics_from_expression(default, tokens, diagnostics);
                }
            }
            collect_call_diagnostics_from_expression(body, tokens, diagnostics);
        }
        Statement::ClassDeclaration {
            properties,
            methods,
            ..
        }
        | Statement::InterfaceDeclaration {
            properties,
            methods,
            ..
        } => {
            for property in properties {
                if let Some(initializer) = &property.initializer {
                    collect_call_diagnostics_from_expression(initializer, tokens, diagnostics);
                }
                if let Some(getter) = &property.getter {
                    collect_call_diagnostics_from_expression(getter, tokens, diagnostics);
                }
                if let Some(setter) = &property.setter {
                    collect_call_diagnostics_from_expression(setter, tokens, diagnostics);
                }
            }
            for method in methods {
                collect_call_diagnostics_from_statement(method, tokens, diagnostics);
            }
        }
        Statement::DataClassDeclaration { parameters, .. } => {
            for parameter in parameters {
                if let Some(default) = &parameter.default_value {
                    collect_call_diagnostics_from_expression(default, tokens, diagnostics);
                }
            }
        }
        Statement::ExtensionFunction(extension) => {
            collect_call_diagnostics_from_statement(&extension.function, tokens, diagnostics);
        }
        Statement::Expression { expr, .. } => {
            collect_call_diagnostics_from_expression(expr, tokens, diagnostics);
        }
        Statement::Return { value, .. } => {
            if let Some(expr) = value {
                collect_call_diagnostics_from_expression(expr, tokens, diagnostics);
            }
        }
        Statement::Throw { expr, .. } => {
            collect_call_diagnostics_from_expression(expr, tokens, diagnostics);
        }
        Statement::Assignment { value, .. } => {
            collect_call_diagnostics_from_expression(value, tokens, diagnostics);
        }
        Statement::ForIn(for_in) => {
            collect_call_diagnostics_from_expression(&for_in.iterable, tokens, diagnostics);
            collect_call_diagnostics_from_expression(&for_in.body, tokens, diagnostics);
        }
        Statement::Concurrency(construct) => match construct {
            jv_ast::ConcurrencyConstruct::Spawn { body, .. }
            | jv_ast::ConcurrencyConstruct::Async { body, .. } => {
                collect_call_diagnostics_from_expression(body, tokens, diagnostics)
            }
            jv_ast::ConcurrencyConstruct::Await { expr, .. } => {
                collect_call_diagnostics_from_expression(expr, tokens, diagnostics)
            }
        },
        Statement::ResourceManagement(resource) => match resource {
            jv_ast::ResourceManagement::Use { resource, body, .. } => {
                collect_call_diagnostics_from_expression(resource, tokens, diagnostics);
                collect_call_diagnostics_from_expression(body, tokens, diagnostics);
            }
            jv_ast::ResourceManagement::Defer { body, .. } => {
                collect_call_diagnostics_from_expression(body, tokens, diagnostics);
            }
        },
        Statement::Break(..)
        | Statement::Continue(..)
        | Statement::Import { .. }
        | Statement::Package { .. }
        | Statement::Comment(_) => {}
    }
}

fn collect_call_diagnostics_from_expression(
    expression: &Expression,
    tokens: &[Token],
    diagnostics: &mut Vec<Diagnostic>,
) {
    match expression {
        Expression::Call {
            function,
            args,
            span,
            ..
        } => {
            if call_expression_contains_comma(span, tokens) {
                diagnostics.push(build_call_argument_diagnostic(span));
            }
            collect_call_diagnostics_from_expression(function, tokens, diagnostics);
            for argument in args {
                match argument {
                    Argument::Positional(expr) => {
                        collect_call_diagnostics_from_expression(expr, tokens, diagnostics)
                    }
                    Argument::Named { value, .. } => {
                        collect_call_diagnostics_from_expression(value, tokens, diagnostics)
                    }
                }
            }
        }
        Expression::Binary { left, right, .. } => {
            collect_call_diagnostics_from_expression(left, tokens, diagnostics);
            collect_call_diagnostics_from_expression(right, tokens, diagnostics);
        }
        Expression::Unary { operand, .. } | Expression::TypeCast { expr: operand, .. } => {
            collect_call_diagnostics_from_expression(operand, tokens, diagnostics);
        }
        Expression::MemberAccess { object, .. }
        | Expression::NullSafeMemberAccess { object, .. } => {
            collect_call_diagnostics_from_expression(object, tokens, diagnostics);
        }
        Expression::IndexAccess { object, index, .. }
        | Expression::NullSafeIndexAccess { object, index, .. } => {
            collect_call_diagnostics_from_expression(object, tokens, diagnostics);
            collect_call_diagnostics_from_expression(index, tokens, diagnostics);
        }
        Expression::Array { elements, .. } => {
            for element in elements {
                collect_call_diagnostics_from_expression(element, tokens, diagnostics);
            }
        }
        Expression::Lambda {
            parameters, body, ..
        } => {
            for parameter in parameters {
                if let Some(default) = &parameter.default_value {
                    collect_call_diagnostics_from_expression(default, tokens, diagnostics);
                }
            }
            collect_call_diagnostics_from_expression(body, tokens, diagnostics);
        }
        Expression::Block { statements, .. } => {
            for statement in statements {
                collect_call_diagnostics_from_statement(statement, tokens, diagnostics);
            }
        }
        Expression::When {
            expr,
            arms,
            else_arm,
            ..
        } => {
            if let Some(condition) = expr {
                collect_call_diagnostics_from_expression(condition, tokens, diagnostics);
            }
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_call_diagnostics_from_expression(guard, tokens, diagnostics);
                }
                collect_call_diagnostics_from_expression(&arm.body, tokens, diagnostics);
            }
            if let Some(else_branch) = else_arm {
                collect_call_diagnostics_from_expression(else_branch, tokens, diagnostics);
            }
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => {
            collect_call_diagnostics_from_expression(condition, tokens, diagnostics);
            collect_call_diagnostics_from_expression(then_branch, tokens, diagnostics);
            if let Some(branch) = else_branch {
                collect_call_diagnostics_from_expression(branch, tokens, diagnostics);
            }
        }
        Expression::Try {
            body,
            catch_clauses,
            finally_block,
            ..
        } => {
            collect_call_diagnostics_from_expression(body, tokens, diagnostics);
            for clause in catch_clauses {
                collect_call_diagnostics_from_expression(&clause.body, tokens, diagnostics);
            }
            if let Some(finally_block) = finally_block {
                collect_call_diagnostics_from_expression(finally_block, tokens, diagnostics);
            }
        }
        Expression::JsonLiteral(_) => {}
        Expression::StringInterpolation { parts, .. } => {
            for part in parts {
                if let StringPart::Expression(expr) = part {
                    collect_call_diagnostics_from_expression(expr, tokens, diagnostics);
                }
            }
        }
        Expression::RegexCommand(command) => {
            collect_call_diagnostics_from_expression(&command.subject, tokens, diagnostics);
            if let Some(replacement) = &command.replacement {
                collect_call_diagnostics_from_regex_replacement(replacement, tokens, diagnostics);
            }
        }
        Expression::MultilineString(_)
        | Expression::Literal(_, _)
        | Expression::RegexLiteral(_)
        | Expression::Identifier(_, _)
        | Expression::This(_)
        | Expression::Super(_) => {}
    }
}

fn collect_call_diagnostics_from_regex_replacement(
    replacement: &RegexReplacement,
    tokens: &[Token],
    diagnostics: &mut Vec<Diagnostic>,
) {
    match replacement {
        RegexReplacement::Literal(_) => {}
        RegexReplacement::Expression(expr) => {
            collect_call_diagnostics_from_expression(expr, tokens, diagnostics);
        }
        RegexReplacement::Lambda(lambda) => {
            collect_call_diagnostics_from_regex_lambda(lambda, tokens, diagnostics);
        }
    }
}

fn collect_call_diagnostics_from_regex_lambda(
    lambda: &RegexLambdaReplacement,
    tokens: &[Token],
    diagnostics: &mut Vec<Diagnostic>,
) {
    for param in &lambda.params {
        if let Some(default) = &param.default_value {
            collect_call_diagnostics_from_expression(default, tokens, diagnostics);
        }
    }
    collect_call_diagnostics_from_expression(&lambda.body, tokens, diagnostics);
}

fn build_call_argument_diagnostic(span: &Span) -> Diagnostic {
    Diagnostic {
        range: span_to_range(span),
        severity: Some(DiagnosticSeverity::Error),
        message: CALL_ARGUMENT_COMMA_ERROR_MESSAGE.to_string(),
        code: Some("JV2102".to_string()),
        source: Some("jv-lsp".to_string()),
        help: None,
        suggestions: Vec::new(),
        strategy: Some("Interactive".to_string()),
    }
}

fn call_expression_contains_comma(span: &Span, tokens: &[Token]) -> bool {
    tokens
        .iter()
        .any(|token| matches!(token.token_type, TokenType::Comma) && token_within_span(token, span))
}

fn token_within_span(token: &Token, span: &Span) -> bool {
    let start_line = span.start_line;
    let end_line = span.end_line;
    if token.line < start_line || token.line > end_line {
        return false;
    }

    if token.line == start_line && token.column < span.start_column {
        return false;
    }

    if token.line == end_line && token.column > span.end_column.saturating_add(1) {
        return false;
    }

    true
}

pub(crate) fn span_to_range(span: &jv_ast::Span) -> Range {
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
