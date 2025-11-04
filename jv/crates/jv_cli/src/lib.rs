// jv_cli - CLI functionality (library interface for testing)
use anyhow::Result;
use clap::Parser;
use jv_ir::{
    sequence_pipeline,
    types::{IrImport, IrImportDetail},
};
use jv_support::i18n::{LocaleCode, catalog};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

use jv_checker::diagnostics::{
    DiagnosticSeverity, DiagnosticStrategy, EnhancedDiagnostic, from_check_error,
    from_frontend_diagnostics, from_parse_error, from_transform_error,
};
use jv_pm::JavaTarget;

mod embedded_stdlib;
mod java_type_names;
mod sequence_warnings;

pub mod commands;
pub mod logging_overrides;
#[derive(Parser, Debug, Clone)]
#[command(name = "jv")]
#[command(
    about = "A Java Sugar Language compiler",
    long_about = r#"KotlinスタイルのコレクションAPIと遅延Sequenceパイプラインを備えたJavaシンタックスシュガーコンパイラです。

主なハイライト:
- Iterable/配列に対する map/filter/flatMap 呼び出しで暗黙Sequenceチェーンが開始され、終端操作 (toList/toSet/count/sum 等) まで遅延評価を維持します。
- Java 25 を優先ターゲットとしつつ、Collectors ベースの Java 21 フォールバックも自動生成します。AutoCloseable ソースは try-with-resources で保護されます。
- ラムダ式の引数は明示必須 ({ value -> ... } 形式)。暗黙の it パラメータは使用できません。
- Sequence から Java Stream へブリッジした後は再利用できないため、必要に応じて toList()/toSet() で具現化してください。

詳細は README と docs/stdlib/collections.md を参照してください。"#
)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(clap::Subcommand, Debug, Clone)]
pub enum Commands {
    /// Initialize a new jv project
    Init {
        /// Project name
        #[arg(default_value = ".")]
        name: String,
    },
    /// Build jv source to Java and compile with javac
    Build {
        /// Input .jv file (optional; defaults to manifest entrypoint)
        #[arg(value_name = "entrypoint")]
        input: Option<String>,
        /// Output directory for .java files
        #[arg(short, long)]
        output: Option<String>,
        /// Skip javac compilation
        #[arg(long)]
        java_only: bool,
        /// Enable type checking
        #[arg(long)]
        check: bool,
        /// Format output Java code
        #[arg(long)]
        format: bool,
        /// Clean output directory before building
        #[arg(long)]
        clean: bool,
        /// Capture AST→IR performance metrics and persist a report
        #[arg(long)]
        perf: bool,
        /// Emit type inference facts as JSON (implies --check)
        #[arg(long)]
        emit_types: bool,
        /// Emit inference telemetry summary to stdout
        #[arg(long)]
        emit_telemetry: bool,
        /// Print resolved imports and additional context
        #[arg(long)]
        verbose: bool,
        /// Enable module-level parallel type inference
        #[arg(long)]
        parallel_inference: bool,
        /// Override worker threads used for inference (requires --parallel-inference)
        #[arg(long, value_name = "threads")]
        inference_workers: Option<usize>,
        /// Override constraint batch size for inference
        #[arg(long, value_name = "batch")]
        constraint_batch: Option<usize>,
        /// ログレベルを上書きする
        #[arg(long = "log-level", value_name = "level")]
        log_level: Option<String>,
        /// ロギングフレームワークを上書きする
        #[arg(long = "log-framework", value_name = "framework")]
        log_framework: Option<String>,
        /// 既定のログレベルを上書きする
        #[arg(long = "log-default-level", value_name = "level")]
        log_default_level: Option<String>,
        /// OpenTelemetry の有効・無効を上書きする
        #[arg(long = "otel-enabled", value_name = "bool")]
        otel_enabled: Option<String>,
        /// OpenTelemetry Collector のエンドポイントを指定する
        #[arg(long = "otel-endpoint", value_name = "url")]
        otel_endpoint: Option<String>,
        /// OpenTelemetry のプロトコルを指定する
        #[arg(long = "otel-protocol", value_name = "protocol")]
        otel_protocol: Option<String>,
        /// TraceContext ヘッダ注入の可否を指定する
        #[arg(long = "otel-trace-context", value_name = "bool")]
        otel_trace_context: Option<String>,
        /// Produce a single-file binary artifact: 'jar' or 'native'
        #[arg(long, value_parser = ["jar", "native"])]
        binary: Option<String>,
        /// Output name (without extension) for --binary; default: 'app'
        #[arg(long, default_value = "app")]
        bin_name: String,
        /// Override the Java target (e.g., 21 or 25)
        #[arg(long, value_name = "java-target")]
        target: Option<JavaTarget>,
        /// Enable javac annotation processing (APT)
        #[arg(long, default_value_t = false)]
        apt: bool,
        /// Comma-separated annotation processors (e.g., org.example.Proc1,Proc2)
        #[arg(long, value_name = "list")]
        processors: Option<String>,
        /// Processor path entries (jar/dir). Use platform separator if multiple.
        #[arg(long, value_name = "path")]
        processorpath: Option<String>,
        /// Repeated processor option to pass as -A<k=v>
        #[arg(long = "apt-option", value_name = "k=v")]
        apt_options: Vec<String>,
    },
    /// Run a compiled jv program
    Run {
        /// Input .jv file to compile and run
        input: String,
        /// Arguments to pass to the program
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
    /// Format jv source files
    Fmt {
        /// Input .jv files
        files: Vec<String>,
    },
    /// Check jv source for errors without compiling
    Check {
        /// Input .jv file
        input: String,
    },
    /// Provide an in-depth explanation for a diagnostic code
    Explain {
        /// Diagnostic code (e.g., JV2001)
        code: String,
    },
    /// Show version information  
    Version,
    /// Start interactive REPL
    Repl,
    /// Launch the interactive language tour experience
    Tour,
    /// Inspect compiler artifacts for debugging
    Debug(commands::debug::DebugArgs),
    /// OpenTelemetry の設定検証と疎通確認を実行する
    Otel(commands::otel::OtelCommand),
}

pub fn tooling_failure(path: &Path, diagnostic: EnhancedDiagnostic) -> anyhow::Error {
    anyhow::anyhow!(format_tooling_diagnostic(path, &diagnostic))
}

pub fn format_tooling_diagnostic(path: &Path, diagnostic: &EnhancedDiagnostic) -> String {
    let location = diagnostic
        .span
        .as_ref()
        .map(|span| {
            format!(
                " (L{}C{}-L{}C{})",
                span.start_line, span.start_column, span.end_line, span.end_column
            )
        })
        .unwrap_or_default();

    format!(
        "[{severity:?}] {code}: {title}{location}\n  戦略: {strategy:?}\n  ファイル: {file}\n  詳細: {detail}\n  対処: {help}{suggestions}{hint}",
        severity = diagnostic.severity,
        code = diagnostic.code,
        title = diagnostic.title,
        strategy = diagnostic.strategy,
        file = path.display(),
        detail = diagnostic.message,
        help = diagnostic.help,
        suggestions = format_suggestions(&diagnostic.suggestions),
        hint = format_learning_hint(diagnostic.learning_hints.as_deref()),
    )
}

fn format_suggestions(suggestions: &[String]) -> String {
    if suggestions.is_empty() {
        return String::new();
    }

    let joined = suggestions
        .iter()
        .map(|suggestion| format!("\n  提案: {suggestion}"))
        .collect::<String>();
    joined
}

fn format_learning_hint(hint: Option<&str>) -> String {
    match hint {
        Some(value) => format!("\n  学習ヒント: {value}"),
        None => String::new(),
    }
}

pub fn resolved_imports_header(has_entries: bool) -> String {
    let key = if has_entries {
        "imports.plan.header"
    } else {
        "imports.plan.header_none"
    };
    let args = HashMap::new();
    let fallback = if has_entries {
        (
            "解決済み import 一覧".to_string(),
            "Resolved import list".to_string(),
        )
    } else {
        (
            "解決済み import はありません".to_string(),
            "No resolved imports found".to_string(),
        )
    };
    bilingual_line_or(key, &args, fallback)
}

pub fn format_resolved_import(import: &IrImport) -> String {
    let statement = render_import_statement(import);
    let summary = match &import.detail {
        IrImportDetail::Type { fqcn } => {
            let mut args = HashMap::new();
            args.insert("fqcn", fqcn.clone());
            bilingual_line_or(
                "imports.plan.type.summary",
                &args,
                (format!("型 import: {fqcn}"), format!("Type import: {fqcn}")),
            )
        }
        IrImportDetail::Package { name } => {
            let mut args = HashMap::new();
            args.insert("name", name.clone());
            bilingual_line_or(
                "imports.plan.package.summary",
                &args,
                (
                    format!("パッケージ import: {name}.*"),
                    format!("Package import: {name}.*"),
                ),
            )
        }
        IrImportDetail::Static { owner, member } => {
            let mut args = HashMap::new();
            args.insert("owner", owner.clone());
            args.insert("member", member.clone());
            bilingual_line_or(
                "imports.plan.static.summary",
                &args,
                (
                    format!("静的 import: {owner}.{member}"),
                    format!("Static import: {owner}.{member}"),
                ),
            )
        }
        IrImportDetail::Module { name } => {
            let mut args = HashMap::new();
            args.insert("name", name.clone());
            bilingual_line_or(
                "imports.plan.module.summary",
                &args,
                (
                    format!("モジュール import: {name}"),
                    format!("Module import: {name}"),
                ),
            )
        }
    };

    let extras = import_extras(import);
    format!("  - {statement} → {summary}{extras}")
}

fn import_extras(import: &IrImport) -> String {
    let mut entries = Vec::new();

    if let Some(alias) = import.alias.as_ref() {
        let mut args = HashMap::new();
        args.insert("alias", alias.clone());
        entries.push(bilingual_line_or(
            "imports.plan.alias.summary",
            &args,
            (format!("別名: {alias}"), format!("Alias: {alias}")),
        ));
    }

    if let Some(module) = import.module_dependency.as_ref() {
        let mut args = HashMap::new();
        args.insert("module", module.clone());
        entries.push(bilingual_line_or(
            "imports.plan.module_dependency.summary",
            &args,
            (
                format!("モジュール依存: {module}"),
                format!("Module dependency: {module}"),
            ),
        ));
    }

    if entries.is_empty() {
        String::new()
    } else {
        format!(" ({})", entries.join("; "))
    }
}

fn render_import_statement(import: &IrImport) -> String {
    match &import.detail {
        IrImportDetail::Type { fqcn } => format!("import {fqcn}"),
        IrImportDetail::Package { name } => format!("import {name}.*"),
        IrImportDetail::Static { owner, member } => {
            format!("import static {owner}.{member}")
        }
        IrImportDetail::Module { name } => format!("import module {name}"),
    }
}

fn bilingual_line_or(
    key: &str,
    args: &HashMap<&str, String>,
    fallback: (String, String),
) -> String {
    let (ja, en) = render_bilingual(key, args).unwrap_or(fallback);
    format!("{ja} / {en}")
}

fn render_bilingual(key: &str, args: &HashMap<&str, String>) -> Option<(String, String)> {
    let ja = catalog(LocaleCode::Ja).render(key, args)?;
    let en = catalog(LocaleCode::En).render(key, args)?;
    Some((ja, en))
}

pub fn init_project(name: &str) -> Result<String> {
    let project_dir = if name == "." {
        std::env::current_dir()?
    } else {
        let dir = Path::new(name);
        fs::create_dir_all(dir)?;
        dir.to_path_buf()
    };

    let project_name = project_dir
        .file_name()
        .unwrap_or_default()
        .to_string_lossy()
        .to_string();

    // Create jv.toml
    let default_target = JavaTarget::default();
    let jv_toml = format!(
        r#"[package]
name = "{}"
version = "0.1.0"

[build]
java_version = "{}"
"#,
        project_name, default_target
    );

    fs::write(project_dir.join("jv.toml"), jv_toml)?;

    // Create src directory and example file
    let src_dir = project_dir.join("src");
    fs::create_dir_all(&src_dir)?;

    let main_jv = r#"fun main() {
    greeting = "Hello, jv!"
    numbers = [1 2 3]
    println(greeting)
    println(numbers)
}
"#;
    fs::write(src_dir.join("main.jv"), main_jv)?;

    Ok(project_name)
}

pub fn validate_file_exists(path: &str) -> Result<()> {
    if !Path::new(path).exists() {
        anyhow::bail!("File '{}' not found", path);
    }
    Ok(())
}

/// Retrieve the CLI version string.
///
/// ```
/// let banner = jv_cli::get_version();
/// assert!(banner.starts_with("jv "));
/// ```
pub fn get_version() -> String {
    format!(
        "jv {} - Java Sugar Language compiler",
        env!("CARGO_PKG_VERSION")
    )
}

pub mod tour;

pub mod pipeline {
    pub mod compat {
        include!("pipeline/compat.rs");
    }

    pub mod report {
        include!("pipeline/report.rs");
    }

    pub mod perf {
        include!("pipeline/perf.rs");
    }

    pub mod generics {
        include!("pipeline/generics.rs");
    }

    pub mod type_facts_bridge {
        include!("pipeline/type_facts_bridge.rs");
    }

    pub mod project {
        pub mod locator {
            include!("pipeline/project/locator.rs");
        }

        pub mod manifest {
            include!("pipeline/project/manifest.rs");
        }

        pub mod layout {
            include!("pipeline/project/layout.rs");
        }

        pub mod output {
            include!("pipeline/project/output.rs");
        }
    }

    pub mod build_plan {
        include!("pipeline/build_plan.rs");
    }

    pub use build_plan::{BuildOptions, BuildOptionsFactory, BuildPlan, CliOverrides};
    pub use perf::{PerfCapture, persist_single_run_report};
    pub use project::output::{OutputManager, PreparedOutput};

    use super::*;
    use anyhow::{Context, anyhow, bail};
    use generics::apply_type_facts;
    use jv_ast::{Argument, CallArgumentMetadata, Expression, Literal, Span, Statement};
    use jv_build::BuildSystem;
    use jv_build::metadata::{
        BuildContext as SymbolBuildContext, SymbolIndexBuilder, SymbolIndexCache,
    };
    use jv_checker::binding::BindingUsageSummary;
    use jv_checker::compat::diagnostics as compat_diagnostics;
    use jv_checker::imports::{
        ImportResolutionService, ResolvedImport, ResolvedImportKind,
        diagnostics as import_diagnostics,
    };
    use jv_checker::inference::{AppliedConversion, HelperSpec, NullableGuardReason};
    use jv_checker::{InferenceSnapshot, InferenceTelemetry, TypeChecker, TypeKind};
    use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
    use jv_fmt::JavaFormatter;
    use jv_inference::types::TypeVariant;
    use jv_ir::TransformContext;
    use jv_ir::context::WhenStrategyRecord;
    use jv_ir::types::{IrImport, IrImportDetail, LogLevel as IrLogLevel, LoggingFrameworkKind};
    use jv_ir::{
        TransformPools, TransformProfiler, transform_program_with_context,
        transform_program_with_context_profiled,
    };
    use jv_parser_frontend::ParserPipeline;
    use jv_parser_rowan::frontend::RowanPipeline;
    use jv_pm::{LogLevel, LoggingConfig, LoggingFramework};
    use serde_json::json;
    use std::collections::{BTreeMap, HashSet};
    use std::ffi::OsStr;
    use std::path::{Path, PathBuf};
    use std::process::Command;
    use std::sync::Arc;
    use std::time::Instant;
    use tracing::debug;
    use type_facts_bridge::preload_type_facts_into_context;

    /// Resulting artifacts and diagnostics from the build pipeline.
    #[derive(Debug, Default, Clone)]
    pub struct BuildArtifacts {
        pub java_files: Vec<PathBuf>,
        pub metadata_files: Vec<PathBuf>,
        pub class_files: Vec<PathBuf>,
        pub javac_version: Option<String>,
        pub warnings: Vec<String>,
        pub compatibility: Option<report::RenderedCompatibilityReport>,
        pub compatibility_diagnostics: Vec<EnhancedDiagnostic>,
        pub inference: Option<InferenceSnapshot>,
        pub perf_capture: Option<PerfCapture>,
        pub when_strategies: Vec<StrategySummary>,
        pub script_main_class: String,
        pub binding_usage: BindingUsageSummary,
        pub resolved_imports: Vec<IrImport>,
    }

    /// Aggregated summary of lowering strategies selected for `when` expressions.
    #[derive(Debug, Default, Clone, PartialEq, Eq)]
    pub struct StrategySummary {
        pub description: String,
        pub count: usize,
    }

    /// Compute the Java class name used to wrap script statements for execution.
    pub fn compute_script_main_class(package_name: &str, entrypoint: &Path) -> String {
        let mut base = to_pascal_case(package_name.trim());
        let mut derived_from_package = !base.is_empty();

        if base.is_empty() {
            if let Some(stem) = entrypoint.file_stem().and_then(|s| s.to_str()) {
                base = to_pascal_case(stem);
            }
            derived_from_package = false;
        }

        if base.is_empty() {
            base = "Generated".to_string();
        }

        if let Some(first) = base.chars().next() {
            if !first.is_ascii_alphabetic() && first != '_' {
                base = format!("J{base}");
            }
        } else {
            base = "Generated".to_string();
        }

        if derived_from_package && !base.to_ascii_lowercase().ends_with("main") {
            base.push_str("Main");
        }

        base
    }

    fn script_main_class(plan: &BuildPlan) -> String {
        compute_script_main_class(&plan.settings.manifest.package.name, plan.entrypoint())
    }

    fn apply_logging_config_to_context(context: &mut TransformContext, config: &LoggingConfig) {
        context.set_logging_framework(map_framework(&config.framework));
        {
            let options = context.logging_options_mut();
            options.active_level = map_log_level(config.log_level);
            options.default_level = map_log_level(config.default_level);
        }
        let trace_enabled = config.opentelemetry.enabled && config.opentelemetry.trace_context;
        context.set_trace_context_enabled(trace_enabled);
    }

    fn map_log_level(level: LogLevel) -> IrLogLevel {
        match level {
            LogLevel::Trace => IrLogLevel::Trace,
            LogLevel::Debug => IrLogLevel::Debug,
            LogLevel::Info => IrLogLevel::Info,
            LogLevel::Warn => IrLogLevel::Warn,
            LogLevel::Error => IrLogLevel::Error,
        }
    }

    fn map_framework(framework: &LoggingFramework) -> LoggingFrameworkKind {
        match framework {
            LoggingFramework::Slf4j => LoggingFrameworkKind::Slf4j,
            LoggingFramework::Log4j2 => LoggingFrameworkKind::Log4j2,
            LoggingFramework::JbossLogging => LoggingFrameworkKind::JbossLogging,
            LoggingFramework::CommonsLogging => LoggingFrameworkKind::CommonsLogging,
            LoggingFramework::Jul => LoggingFrameworkKind::Jul,
            LoggingFramework::Custom(custom) => LoggingFrameworkKind::Custom {
                identifier: custom.identifier.clone(),
            },
        }
    }

    fn to_pascal_case(input: &str) -> String {
        let mut result = String::new();
        let mut capitalize_next = true;
        let mut digit_buffer = String::new();

        for ch in input.chars() {
            if ch.is_ascii_digit() {
                digit_buffer.push(ch);
                capitalize_next = true;
                continue;
            }

            if !digit_buffer.is_empty() {
                append_digit_words(&mut result, &digit_buffer);
                digit_buffer.clear();
            }

            if ch.is_ascii_alphanumeric() {
                if capitalize_next {
                    for upper in ch.to_uppercase() {
                        result.push(upper);
                    }
                    capitalize_next = false;
                } else {
                    for lower in ch.to_lowercase() {
                        result.push(lower);
                    }
                }
            } else {
                capitalize_next = true;
            }
        }

        if !digit_buffer.is_empty() {
            append_digit_words(&mut result, &digit_buffer);
        }

        result
    }

    fn append_digit_words(target: &mut String, digits: &str) {
        for digit in digits.chars() {
            target.push_str(digit_to_word(digit));
        }
    }

    fn digit_to_word(digit: char) -> &'static str {
        match digit {
            '0' => "Zero",
            '1' => "One",
            '2' => "Two",
            '3' => "Three",
            '4' => "Four",
            '5' => "Five",
            '6' => "Six",
            '7' => "Seven",
            '8' => "Eight",
            '9' => "Nine",
            _ => "",
        }
    }

    /// Compile a `.jv` file end-to-end into Java (and optionally `.class`) outputs.
    pub fn compile(plan: &BuildPlan) -> Result<BuildArtifacts> {
        let options = &plan.options;
        let entrypoint = options.entrypoint.as_path();

        if !entrypoint.exists() {
            bail!("Input file '{}' not found", entrypoint.display());
        }

        let mut warnings = Vec::new();
        let mut build_config = plan.build_config.clone();
        build_config.output_dir = options.output_dir.to_string_lossy().into_owned();

        let compatibility_report =
            compat::preflight(&BuildSystem::new(build_config.clone()), entrypoint)?;
        let target_label = format!("Java{}", compatibility_report.target);
        let compatibility_diagnostics = compatibility_report
            .warnings
            .iter()
            .map(|warning| compat_diagnostics::fallback_applied(&target_label, warning))
            .collect::<Vec<_>>();

        let source = fs::read_to_string(entrypoint)
            .with_context(|| format!("Failed to read file: {}", entrypoint.display()))?;

        let parse_start = Instant::now();
        let frontend_output = match RowanPipeline::default().parse(&source) {
            Ok(output) => output,
            Err(error) => {
                if let Some(diagnostic) = from_parse_error(&error) {
                    return Err(tooling_failure(
                        entrypoint,
                        diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                    ));
                }
                return Err(anyhow!("Parser error: {:?}", error));
            }
        };
        let frontend_diagnostics =
            from_frontend_diagnostics(frontend_output.diagnostics().final_diagnostics());
        if !frontend_diagnostics.is_empty() {
            for diagnostic in &frontend_diagnostics {
                let rendered = diagnostic
                    .clone()
                    .with_strategy(DiagnosticStrategy::Deferred);
                warnings.push(format_tooling_diagnostic(entrypoint, &rendered));
            }
            if let Some(error_diag) = frontend_diagnostics
                .iter()
                .find(|diag| diag.severity == DiagnosticSeverity::Error)
            {
                return Err(tooling_failure(
                    entrypoint,
                    error_diag
                        .clone()
                        .with_strategy(DiagnosticStrategy::Deferred),
                ));
            }
        }
        let mut program = frontend_output.into_program();

        embedded_stdlib::rewrite_collection_property_access(&mut program);
        let parse_duration = parse_start.elapsed();

        warnings.extend(sequence_warnings::collect_sequence_warnings(&program));

        let import_cache_dir = plan
            .root
            .root_dir()
            .join("target")
            .join("jv")
            .join("symbol-index");
        let index_cache = SymbolIndexCache::new(import_cache_dir);
        let build_context = SymbolBuildContext::from_config(&plan.build_config);
        let builder = SymbolIndexBuilder::new(&build_context);
        let mut symbol_index = builder
            .build_with_cache(&index_cache)
            .map_err(|error| anyhow!("failed to build symbol index: {error}"))?;

        // Populate stdlib types into symbol index before import resolution
        let stdlib_catalog = embedded_stdlib::stdlib_catalog()?;
        for fqcn in stdlib_catalog.fully_qualified_type_names() {
            if symbol_index.lookup_type(fqcn).is_some() {
                continue;
            }
            if let Some((package, _type_name)) = fqcn.rsplit_once('.') {
                use jv_build::metadata::TypeEntry;
                let entry = TypeEntry::new(fqcn.to_string(), package.to_string(), None);
                symbol_index.add_type(entry);
            }
        }

        let symbol_index = Arc::new(symbol_index);

        let import_service =
            ImportResolutionService::new(Arc::clone(&symbol_index), plan.build_config.target);
        let mut resolved_imports = Vec::new();
        for import_stmt in &program.imports {
            match import_service.resolve(import_stmt) {
                Ok(resolved) => resolved_imports.push(resolved),
                Err(error) => {
                    if let Some(diagnostic) = import_diagnostics::from_error(&error) {
                        return Err(tooling_failure(
                            entrypoint,
                            diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                        ));
                    }
                    return Err(anyhow!("failed to resolve import: {error}"));
                }
            }
        }

        let mut stdlib_usage =
            embedded_stdlib::StdlibUsage::from_resolved_imports(&resolved_imports, stdlib_catalog);
        stdlib_usage.record_program_usage(&program, stdlib_catalog);
        let import_plan = lowered_import_plan(&resolved_imports);
        let module_count = import_plan
            .iter()
            .filter(|entry| matches!(entry.detail, IrImportDetail::Module { .. }))
            .count();
        debug!(
            target: "jv::imports",
            resolved_count = resolved_imports.len(),
            module_count,
            "resolved import plan for {}",
            entrypoint.display()
        );

        let mut type_checker = TypeChecker::with_parallel_config(options.parallel_config);
        type_checker.set_java_target(plan.build_config.target);
        if !resolved_imports.is_empty() {
            type_checker.set_imports(Arc::clone(&symbol_index), resolved_imports.clone());
        }
        let (inference_snapshot, type_facts_snapshot, telemetry_snapshot) = match type_checker
            .check_program(&program)
        {
            Ok(()) => {
                if options.check {
                    let null_warnings = if let Some(normalized) = type_checker.normalized_program()
                    {
                        let cloned = normalized.clone();
                        type_checker.check_null_safety(&cloned, None)
                    } else {
                        type_checker.check_null_safety(&program, None)
                    };
                    warnings.extend(null_warnings.into_iter().map(|warning| warning.to_string()));
                }
                let telemetry = type_checker.telemetry().clone();
                let snapshot = type_checker.take_inference_snapshot();
                let facts_snapshot = snapshot.as_ref().map(|snap| snap.type_facts().clone());
                (snapshot, facts_snapshot, Some(telemetry))
            }
            Err(errors) => {
                if let Some(diagnostic) = errors.iter().find_map(from_check_error) {
                    if options.check {
                        return Err(tooling_failure(
                            entrypoint,
                            diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                        ));
                    }
                }
                if options.check {
                    let details = errors
                        .iter()
                        .map(|err| err.to_string())
                        .collect::<Vec<_>>()
                        .join(
                            "
- ",
                        );
                    bail!(
                        "Type checking failed:
- {}",
                        details
                    );
                } else {
                    let details = errors
                        .iter()
                        .map(|err| err.to_string())
                        .collect::<Vec<_>>()
                        .join("; ");
                    warnings.push(format!(
                        "Type checking skipped due to unresolved inference: {}",
                        details
                    ));
                    let snapshot = type_checker.take_inference_snapshot();
                    let facts_snapshot = snapshot
                        .as_ref()
                        .map(|snap| snap.type_facts().clone())
                        .or_else(|| type_checker.type_facts().cloned());
                    (snapshot, facts_snapshot, None)
                }
            }
        };

        let binding_usage = type_checker.binding_usage().clone();

        let mut type_facts_snapshot = type_facts_snapshot;
        let requires_probe = type_facts_snapshot
            .as_ref()
            .and_then(|facts| facts.function_signature("render"))
            .map(|signature| match signature.body.variant() {
                TypeVariant::Function(params, _) => params
                    .iter()
                    .any(|param| matches!(param.variant(), TypeVariant::Variable(_))),
                _ => false,
            })
            .unwrap_or(true);
        if requires_probe {
            let mut trimmed_program = program.clone();
            let original_len = trimmed_program.statements.len();
            trimmed_program.statements.retain(|statement| {
                !matches!(
                    statement,
                    Statement::FunctionDeclaration { name, .. } if name == "main"
                )
            });
            if trimmed_program.statements.len() < original_len {
                let probe_span = Span::dummy();
                let module_call = Expression::Call {
                    function: Box::new(Expression::Identifier(
                        "Module".to_string(),
                        probe_span.clone(),
                    )),
                    args: vec![
                        Argument::Positional(Expression::Literal(
                            Literal::String("probe".to_string()),
                            probe_span.clone(),
                        )),
                        Argument::Positional(Expression::Literal(
                            Literal::Boolean(true),
                            probe_span.clone(),
                        )),
                    ],
                    type_arguments: Vec::new(),
                    argument_metadata: CallArgumentMetadata::default(),
                    span: probe_span.clone(),
                };
                let render_probe = Expression::Call {
                    function: Box::new(Expression::Identifier(
                        "render".to_string(),
                        probe_span.clone(),
                    )),
                    args: vec![Argument::Positional(module_call)],
                    type_arguments: Vec::new(),
                    argument_metadata: CallArgumentMetadata::default(),
                    span: probe_span.clone(),
                };
                trimmed_program.statements.push(Statement::Expression {
                    expr: render_probe,
                    span: probe_span.clone(),
                });

                let mut fallback_checker =
                    TypeChecker::with_parallel_config(options.parallel_config);
                fallback_checker.set_java_target(plan.build_config.target);
                if !resolved_imports.is_empty() {
                    fallback_checker
                        .set_imports(Arc::clone(&symbol_index), resolved_imports.clone());
                }
                if fallback_checker.check_program(&trimmed_program).is_ok() {
                    let snapshot = fallback_checker.take_inference_snapshot();
                    type_facts_snapshot = snapshot
                        .as_ref()
                        .map(|snap| snap.type_facts().clone())
                        .or_else(|| fallback_checker.type_facts().cloned());
                }
            }
        }

        let mut perf_capture: Option<PerfCapture> = None;
        let when_strategy_records: Vec<WhenStrategyRecord>;
        let mut program_holder = Some(type_checker.take_normalized_program().unwrap_or(program));
        let mut ir_program = if options.perf {
            let pools = TransformPools::with_chunk_capacity(256 * 1024);
            let mut context = TransformContext::with_pools(pools);
            apply_logging_config_to_context(&mut context, &plan.logging_config);
            if let Some(facts) = type_facts_snapshot.as_ref() {
                preload_type_facts_into_context(&mut context, facts);
            }
            if !import_plan.is_empty() {
                context.set_resolved_imports(import_plan.clone());
            }
            let mut profiler = TransformProfiler::new();
            let lowering_result = transform_program_with_context_profiled(
                program_holder
                    .take()
                    .expect("program should be available for perf lowering"),
                &mut context,
                &mut profiler,
            );

            match lowering_result {
                Ok((ir, metrics)) => {
                    let capture = persist_single_run_report(
                        plan.root.root_dir(),
                        entrypoint,
                        parse_duration,
                        &metrics,
                    )?;
                    perf_capture = Some(capture);
                    when_strategy_records = context.take_when_strategies();
                    ir
                }
                Err(error) => {
                    if let Some(diagnostic) = from_transform_error(&error) {
                        return Err(tooling_failure(
                            entrypoint,
                            diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                        ));
                    }
                    return Err(anyhow!("IR transformation error: {:?}", error));
                }
            }
        } else {
            let mut context = TransformContext::new();
            apply_logging_config_to_context(&mut context, &plan.logging_config);
            if let Some(facts) = type_facts_snapshot.as_ref() {
                preload_type_facts_into_context(&mut context, facts);
            }
            if !import_plan.is_empty() {
                context.set_resolved_imports(import_plan.clone());
            }
            let lowering_result = transform_program_with_context(
                program_holder
                    .take()
                    .expect("program should be available for lowering"),
                &mut context,
            );

            match lowering_result {
                Ok(ir) => {
                    when_strategy_records = context.take_when_strategies();
                    ir
                }
                Err(error) => {
                    if let Some(diagnostic) = from_transform_error(&error) {
                        return Err(tooling_failure(
                            entrypoint,
                            diagnostic.with_strategy(DiagnosticStrategy::Deferred),
                        ));
                    }
                    return Err(anyhow!("IR transformation error: {:?}", error));
                }
            }
        };

        if let Some(facts) = type_facts_snapshot.as_ref() {
            apply_type_facts(&mut ir_program, facts);
        }

        sequence_pipeline::enforce_list_terminals(&mut ir_program);

        let when_strategy_summary = summarize_when_strategies(&when_strategy_records);

        let script_main_class = script_main_class(plan);

        let mut codegen_config = JavaCodeGenConfig::for_target(build_config.target);
        codegen_config.script_main_class = script_main_class.clone();

        let mut code_generator = JavaCodeGenerator::with_config(codegen_config);
        code_generator.set_symbol_index(Some(Arc::clone(&symbol_index)));
        let java_unit = code_generator
            .generate_compilation_unit(&ir_program)
            .map_err(|e| anyhow!("Code generation error: {:?}", e))?;
        stdlib_usage
            .extend_with_java_imports(java_unit.imports.iter().map(|s| s.as_str()), stdlib_catalog);

        fs::create_dir_all(&options.output_dir).with_context(|| {
            format!(
                "Failed to create output directory: {}",
                options.output_dir.display()
            )
        })?;

        let mut java_files = Vec::new();
        let mut emitted_java_filenames = HashSet::new();
        for (index, type_decl) in java_unit.type_declarations.iter().enumerate() {
            let preferred_name = if index == 0 {
                script_main_class.clone()
            } else {
                crate::java_type_names::derive_type_name(type_decl)
                    .unwrap_or_else(|| format!("{}{}", script_main_class, index))
            };

            let mut java_filename = format!("{}.java", preferred_name);
            if !emitted_java_filenames.insert(java_filename.clone()) {
                let mut suffix = 1usize;
                loop {
                    let candidate = format!("{}_{suffix}.java", preferred_name);
                    if emitted_java_filenames.insert(candidate.clone()) {
                        java_filename = candidate;
                        break;
                    }
                    suffix += 1;
                }
            }

            let java_path = options.output_dir.join(&java_filename);

            let mut java_content = String::new();
            if let Some(package) = &java_unit.package_declaration {
                java_content.push_str(&format!("package {};\n\n", package));
            }
            for import in &java_unit.imports {
                java_content.push_str(&format!("import {};\n", import));
            }
            java_content.push('\n');
            java_content.push_str(type_decl);
            stdlib_usage.scan_java_source(type_decl, stdlib_catalog);

            if options.format {
                let formatter = JavaFormatter::default();
                java_content = formatter
                    .format_compilation_unit(&java_content)
                    .unwrap_or(java_content);
            }

            fs::write(&java_path, java_content)
                .with_context(|| format!("Failed to write Java file: {}", java_path.display()))?;

            java_files.push(java_path);
        }

        let embedded_stdlib::StdlibCompilationArtifacts {
            java_files: stdlib_java_files,
            metadata_files: stdlib_metadata_files,
        } = embedded_stdlib::compile_stdlib_modules(
            &options.output_dir,
            plan.build_config.target,
            options.format,
            options.parallel_config,
            &stdlib_usage,
            Arc::clone(&symbol_index),
        )?;
        java_files.extend(stdlib_java_files);

        if options.emit_telemetry {
            if let Some(telemetry) = telemetry_snapshot.as_ref() {
                print_inference_telemetry(
                    entrypoint,
                    telemetry,
                    &when_strategy_summary,
                    &binding_usage,
                );
            } else {
                print_strategy_telemetry(entrypoint, &when_strategy_summary, &binding_usage);
            }
        }

        let mut artifacts = BuildArtifacts {
            java_files,
            class_files: Vec::new(),
            javac_version: None,
            warnings,
            compatibility: None,
            compatibility_diagnostics,
            inference: inference_snapshot,
            perf_capture,
            when_strategies: when_strategy_summary,
            script_main_class,
            binding_usage,
            resolved_imports: import_plan.clone(),
            metadata_files: stdlib_metadata_files,
        };

        if !options.java_only {
            let build_system = BuildSystem::new(build_config.clone());

            let javac_version = build_system
                .check_javac_availability()
                .map_err(|err| anyhow!("JDK not available: {}", err))?;
            artifacts.javac_version = Some(javac_version);

            let java_paths: Vec<&Path> = artifacts.java_files.iter().map(|p| p.as_path()).collect();
            if !java_paths.is_empty() {
                build_system
                    .compile_java_files(java_paths)
                    .map_err(|e| anyhow!("Java compilation failed: {}", e))?;

                let entries = fs::read_dir(&options.output_dir).with_context(|| {
                    format!(
                        "Failed to enumerate output directory: {}",
                        options.output_dir.display()
                    )
                })?;

                let mut class_files = Vec::new();
                for entry in entries {
                    let path = entry?.path();
                    if path.extension().and_then(OsStr::to_str) == Some("class") {
                        class_files.push(path);
                    }
                }
                artifacts.class_files = class_files;
            }
        }

        let rendered_report = report::render(&compatibility_report, &options.output_dir)
            .with_context(|| {
                format!(
                    "Failed to render compatibility report into {}",
                    options.output_dir.display()
                )
            })?;
        artifacts.compatibility = Some(rendered_report);

        Ok(artifacts)
    }

    fn lowered_import_plan(resolved_imports: &[ResolvedImport]) -> Vec<IrImport> {
        let mut imports = Vec::new();
        let mut module_sources: BTreeMap<String, Span> = BTreeMap::new();
        let mut explicit_modules = HashSet::new();

        for resolved in resolved_imports {
            let detail = match &resolved.kind {
                ResolvedImportKind::Type { fqcn } => IrImportDetail::Type { fqcn: fqcn.clone() },
                ResolvedImportKind::Package { name } => {
                    IrImportDetail::Package { name: name.clone() }
                }
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

    fn print_inference_telemetry(
        entrypoint: &Path,
        telemetry: &InferenceTelemetry,
        strategies: &[StrategySummary],
        binding_usage: &BindingUsageSummary,
    ) {
        println!(
            "Telemetry ({}):\n  constraints_emitted: {}\n  bindings_resolved: {}\n  inference_duration_ms: {:.3}\n  preserved_constraints: {}\n  cache_hit_rate: {}\n  invalidation_cascade_depth: {}\n  pattern_cache_hits: {}\n  pattern_cache_misses: {}\n  pattern_bridge_ms: {:.3}\n  generic_constraints_emitted: {}\n  bound_checks: {}\n  variance_conflicts: {}\n  sealed_hierarchy_checks: {}\n  generic_solver_ms: {:.3}\n  variance_analysis_ms: {:.3}\n  kind_checks_count: {}\n  kind_cache_hit_rate: {}\n  const_evaluations: {}\n  type_level_cache_size: {}",
            entrypoint.display(),
            telemetry.constraints_emitted,
            telemetry.bindings_resolved,
            telemetry.inference_duration_ms,
            telemetry.preserved_constraints,
            telemetry
                .cache_hit_rate
                .map(|rate| format!("{:.2}%", rate * 100.0))
                .unwrap_or_else(|| "n/a".to_string()),
            telemetry.invalidation_cascade_depth,
            telemetry.pattern_cache_hits,
            telemetry.pattern_cache_misses,
            telemetry.pattern_bridge_ms,
            telemetry.generic_constraints_emitted,
            telemetry.bound_checks,
            telemetry.variance_conflicts,
            telemetry.sealed_hierarchy_checks,
            telemetry.generic_solver_ms,
            telemetry.variance_analysis_ms,
            telemetry.kind_checks_count,
            telemetry
                .kind_cache_hit_rate
                .map(|rate| format!("{:.2}%", rate * 100.0))
                .unwrap_or_else(|| "n/a".to_string()),
            telemetry.const_evaluations,
            telemetry.type_level_cache_size
        );

        println!(
            "  binding_usage: explicit_val={} implicit_val={} implicit_typed={} var={}",
            binding_usage.explicit,
            binding_usage.implicit,
            binding_usage.implicit_typed,
            binding_usage.vars
        );

        print_conversion_events(telemetry);
        print_nullable_guards(telemetry);
        print_catalog_hits(telemetry);

        if strategies.is_empty() {
            println!(
                "  when_strategies: (none recorded)\n  pattern_diagnostic_hint: JV3199 surfaces when destructuring depth exceeds supported limits or when guards require Phase 5 features. Use --explain JV3199 for guidance."
            );
        } else {
            println!("  when_strategies:");
            for summary in strategies {
                println!(
                    "    - {} ({} occurrence{})",
                    summary.description,
                    summary.count,
                    if summary.count == 1 { "" } else { "s" }
                );
            }
            println!(
                "  pattern_diagnostic_hint: Deep destructuring (depth ≥ 11) and complex guards yield JV3199. Run --explain JV3199 to review mitigation paths."
            );
        }

        emit_binding_usage_json(entrypoint, Some(telemetry), strategies, binding_usage);
    }

    fn print_conversion_events(telemetry: &InferenceTelemetry) {
        if telemetry.conversion_events.is_empty() {
            println!("  conversion_events: (none recorded)");
            return;
        }

        println!("  conversion_events:");
        for event in &telemetry.conversion_events {
            println!("    - {}", format_conversion_event(event));
        }
    }

    fn print_nullable_guards(telemetry: &InferenceTelemetry) {
        if telemetry.nullable_guards.is_empty() {
            println!("  nullable_guards: (none recorded)");
            return;
        }

        println!("  nullable_guards:");
        for guard in &telemetry.nullable_guards {
            println!("    - {}", format_nullable_guard_reason(guard.reason));
        }
    }

    fn print_catalog_hits(telemetry: &InferenceTelemetry) {
        if telemetry.catalog_hits.is_empty() {
            println!("  catalog_hits: (none recorded)");
            return;
        }

        println!("  catalog_hits:");
        for helper in &telemetry.catalog_hits {
            println!("    - {}", format_helper_spec(helper));
        }
    }

    fn print_strategy_telemetry(
        entrypoint: &Path,
        strategies: &[StrategySummary],
        binding_usage: &BindingUsageSummary,
    ) {
        println!("Telemetry ({})", entrypoint.display());
        println!("  pattern_cache_hits: n/a");
        println!("  pattern_cache_misses: n/a");
        println!("  pattern_bridge_ms: n/a");
        if strategies.is_empty() {
            println!("  when_strategies: (none recorded)");
        } else {
            println!("  when_strategies:");
            for summary in strategies {
                println!(
                    "    - {} ({} occurrence{})",
                    summary.description,
                    summary.count,
                    if summary.count == 1 { "" } else { "s" }
                );
            }
        }
        println!(
            "  pattern_diagnostic_hint: Deep destructuring beyond depth 10 or hybrid guards will surface JV3199. Use --explain JV3199 for remediation."
        );
        println!(
            "  binding_usage: explicit_val={} implicit_val={} implicit_typed={} var={}",
            binding_usage.explicit,
            binding_usage.implicit,
            binding_usage.implicit_typed,
            binding_usage.vars
        );

        emit_binding_usage_json(entrypoint, None, strategies, binding_usage);
    }

    fn summarize_when_strategies(records: &[WhenStrategyRecord]) -> Vec<StrategySummary> {
        use std::collections::BTreeMap;

        let mut counts: BTreeMap<String, usize> = BTreeMap::new();
        for record in records {
            let key = record.description.trim().to_string();
            *counts.entry(key).or_insert(0) += 1;
        }

        counts
            .into_iter()
            .map(|(description, count)| StrategySummary { description, count })
            .collect()
    }

    fn format_conversion_event(event: &AppliedConversion) -> String {
        let mut details = format!(
            "{} -> {} [{}]",
            format_type_kind(&event.from_type),
            format_type_kind(&event.to_type),
            event.kind.label()
        );

        if let Some(helper) = event.helper_method.as_ref() {
            details.push_str(&format!(" via {}", format_helper_spec(helper)));
        }

        if event.warned {
            details.push_str(" (warn)");
        }

        if let Some(span) = event.source_span.as_ref() {
            details.push_str(&format!(" @{}", format_span(span)));
        }

        if let Some(hint) = event.java_span_hint.as_ref() {
            details.push_str(&format!(" -> {}", hint));
        }

        details
    }

    fn format_helper_spec(helper: &HelperSpec) -> String {
        let qualifier = if helper.is_static { "static " } else { "" };
        format!("{qualifier}{}::{}", helper.owner, helper.method)
    }

    fn format_nullable_guard_reason(reason: NullableGuardReason) -> &'static str {
        match reason {
            NullableGuardReason::OptionalLift => "optional-lift",
            NullableGuardReason::Unboxing => "unboxing",
        }
    }

    fn format_span(span: &Span) -> String {
        format!(
            "L{}C{}-L{}C{}",
            span.start_line, span.start_column, span.end_line, span.end_column
        )
    }

    fn format_type_kind(kind: &TypeKind) -> String {
        match kind {
            TypeKind::Primitive(primitive) => primitive.java_name().to_string(),
            TypeKind::Boxed(primitive) => primitive.boxed_fqcn().to_string(),
            TypeKind::Reference(name) => name.clone(),
            TypeKind::Optional(inner) => format!("{}?", format_type_kind(inner)),
            TypeKind::Variable(id) => format!("t{}", id.to_raw()),
            TypeKind::Function(params, result) => {
                let param_repr = params
                    .iter()
                    .map(format_type_kind)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({}) -> {}", param_repr, format_type_kind(result))
            }
            TypeKind::Unknown => "unknown".to_string(),
        }
    }

    fn emit_binding_usage_json(
        entrypoint: &Path,
        telemetry: Option<&InferenceTelemetry>,
        strategies: &[StrategySummary],
        binding_usage: &BindingUsageSummary,
    ) {
        let strategies_json = strategies
            .iter()
            .map(|summary| {
                json!({
                    "description": summary.description,
                    "count": summary.count,
                })
            })
            .collect::<Vec<_>>();

        let telemetry_json = telemetry.map(|telemetry| {
            json!({
                "constraints_emitted": telemetry.constraints_emitted,
                "bindings_resolved": telemetry.bindings_resolved,
                "inference_duration_ms": telemetry.inference_duration_ms,
                "preserved_constraints": telemetry.preserved_constraints,
                "cache_hit_rate": telemetry.cache_hit_rate,
                "invalidation_cascade_depth": telemetry.invalidation_cascade_depth,
                "pattern_cache_hits": telemetry.pattern_cache_hits,
                "pattern_cache_misses": telemetry.pattern_cache_misses,
                "pattern_bridge_ms": telemetry.pattern_bridge_ms,
                "generic_constraints_emitted": telemetry.generic_constraints_emitted,
                "bound_checks": telemetry.bound_checks,
                "variance_conflicts": telemetry.variance_conflicts,
                "sealed_hierarchy_checks": telemetry.sealed_hierarchy_checks,
                "generic_solver_ms": telemetry.generic_solver_ms,
                "variance_analysis_ms": telemetry.variance_analysis_ms,
                "widening_conversions": telemetry.widening_conversions,
                "boxing_conversions": telemetry.boxing_conversions,
                "unboxing_conversions": telemetry.unboxing_conversions,
                "string_conversions": telemetry.string_conversions,
                "nullable_guards_generated": telemetry.nullable_guards_generated,
                "method_invocation_conversions": telemetry.method_invocation_conversions,
                "conversion_catalog_hits": telemetry.conversion_catalog_hits,
                "conversion_catalog_misses": telemetry.conversion_catalog_misses,
                "conversion_events": telemetry
                    .conversion_events
                    .iter()
                    .map(|event| {
                        json!({
                            "from": format_type_kind(&event.from_type),
                            "to": format_type_kind(&event.to_type),
                            "kind": event.kind.label(),
                            "helper": event
                                .helper_method
                                .as_ref()
                                .map(|helper| format_helper_spec(helper)),
                            "warned": event.warned,
                            "source_span": event
                                .source_span
                                .as_ref()
                                .map(|span| format_span(span)),
                            "java_span_hint": event.java_span_hint.clone(),
                            "nullable_guard": event
                                .nullable_guard
                                .map(|guard| format_nullable_guard_reason(guard.reason)),
                        })
                    })
                    .collect::<Vec<_>>(),
                "nullable_guards": telemetry
                    .nullable_guards
                    .iter()
                    .map(|guard| format_nullable_guard_reason(guard.reason))
                    .collect::<Vec<_>>(),
                "catalog_hits": telemetry
                    .catalog_hits
                    .iter()
                    .map(|helper| format_helper_spec(helper))
                    .collect::<Vec<_>>(),
            })
        });

        let payload = json!({
            "entrypoint": entrypoint.display().to_string(),
            "binding_usage": {
                "explicit_val": binding_usage.explicit,
                "implicit_val": binding_usage.implicit,
                "implicit_typed": binding_usage.implicit_typed,
                "vars": binding_usage.vars,
            },
            "when_strategies": strategies_json,
            "telemetry": telemetry_json,
        });

        match serde_json::to_string(&payload) {
            Ok(json_line) => println!("{}", json_line),
            Err(error) => eprintln!("Failed to serialize telemetry payload: {}", error),
        }
    }

    /// Compile and execute a `.jv` program using the Java runtime.
    pub fn run_program(plan: &BuildPlan, args: &[String]) -> Result<()> {
        let mut prepared_output = OutputManager::prepare(plan.clone())
            .map_err(|diagnostic| tooling_failure(plan.entrypoint(), diagnostic))?;

        let target_dir = prepared_output.target_dir().to_path_buf();
        println!(
            "出力ディレクトリ: {} (Java{})\nOutput directory: {}",
            target_dir.display(),
            prepared_output.plan().build_config.target,
            target_dir.display()
        );
        if prepared_output.clean_applied() {
            println!("クリーンビルド: 実行しました / Clean build: applied");
        }

        let compile_result = match compile(prepared_output.plan()) {
            Ok(result) => result,
            Err(err) => return Err(err),
        };

        if !compile_result.warnings.is_empty() {
            for warning in &compile_result.warnings {
                eprintln!("warning: {}", warning);
            }
        }

        if compile_result.class_files.is_empty() {
            if compile_result.javac_version.is_none() {
                bail!(
                    "Java コンパイラ (javac) が見つかりません。JDK をインストールして PATH に追加してください。"
                );
            }
            bail!("Java compilation step did not produce class files; cannot execute program");
        }

        let generated_main = target_dir.join(format!("{}.class", compile_result.script_main_class));
        if !generated_main.exists() {
            bail!(
                "No compiled .class file found at {}. Make sure javac is available for execution.",
                generated_main.display()
            );
        }

        let classpath = target_dir.to_string_lossy().to_string();
        let mut cmd = Command::new("java");
        cmd.arg("-cp");
        cmd.arg(&classpath);
        cmd.arg(&compile_result.script_main_class);
        for arg in args {
            cmd.arg(arg);
        }

        let status = cmd
            .status()
            .with_context(|| "Failed to run java - is it installed?")?;

        if !status.success() {
            bail!("Program execution failed");
        }

        prepared_output.mark_success();
        Ok(())
    }

    /// Package generated outputs into a JAR or native binary.
    pub fn produce_binary(
        output_dir: &Path,
        bin_name: &str,
        kind: &str,
        main_class: &str,
    ) -> Result<PathBuf> {
        match kind {
            "jar" => {
                let jar_path = output_dir.join(format!("{}.jar", bin_name));
                let status = Command::new("jar")
                    .args([
                        "--create",
                        "--file",
                        jar_path
                            .to_str()
                            .ok_or_else(|| anyhow!("Non-UTF8 path encountered for jar output"))?,
                        "--main-class",
                        main_class,
                        "-C",
                        output_dir
                            .to_str()
                            .ok_or_else(|| anyhow!("Non-UTF8 output directory"))?,
                        ".",
                    ])
                    .status();

                match status {
                    Ok(code) if code.success() => Ok(jar_path),
                    Ok(code) => bail!("jar failed with status: {}", code),
                    Err(err) => bail!("'jar' tool not found or failed: {}", err),
                }
            }
            "native" => {
                let jar_artifact = produce_binary(output_dir, bin_name, "jar", main_class)?;
                let native_out = output_dir.join(bin_name);
                let status = Command::new("native-image")
                    .arg("-jar")
                    .arg(&jar_artifact)
                    .arg(&native_out)
                    .status();

                match status {
                    Ok(code) if code.success() => Ok(native_out),
                    Ok(_) => {
                        bail!("native-image failed. Ensure GraalVM native-image is installed.")
                    }
                    Err(_) => bail!(
                        "native-image not found. Install GraalVM native-image or use --binary jar"
                    ),
                }
            }
            other => bail!("Unsupported binary target '{}'.", other),
        }
    }
}

#[cfg(test)]
mod tests;
