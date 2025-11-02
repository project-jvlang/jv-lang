// jv CLI entry point
use anyhow::{Context, Result};
use clap::Parser;
use std::collections::HashMap;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use jv_checker::diagnostics::{
    DiagnosticSeverity, DiagnosticStrategy, from_frontend_diagnostics, from_parse_error,
    from_transform_error,
};
use jv_fmt::JavaFormatter;
use jv_ir::transform_program;
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;

use jv_cli::commands;
use jv_cli::pipeline::project::{
    layout::ProjectLayout,
    locator::{ProjectLocator, ProjectRoot},
    manifest::{ManifestLoader, OutputConfig, ProjectSettings, SourceConfig},
};
use jv_cli::pipeline::{
    BuildOptionsFactory, CliOverrides, OutputManager, compile, produce_binary,
    report::render_logging_overview, run_program,
};
use jv_cli::tour::TourOrchestrator;
use jv_cli::{
    Cli, Commands, format_resolved_import, get_version, init_project as cli_init_project,
    resolved_imports_header, tooling_failure,
};
use jv_pm::{
    LogLevel, LoggingConfig, LoggingConfigLayer, LoggingFramework, Manifest, OpenTelemetryLayer,
    OtelProtocol, PackageInfo, ProjectSection,
};

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Init { name }) => {
            let project_name = cli_init_project(&name)?;
            let project_dir = if name == "." {
                std::env::current_dir()?
            } else {
                PathBuf::from(&name)
            };

            println!(
                "Created jv project '{}' in {}",
                project_name,
                project_dir.display()
            );
            println!("Next steps:");
            println!("  cd {}", if name == "." { "." } else { &name });
            println!("  jv build src/main.jv");
            println!("  jv run src/main.jv");
        }
        Some(Commands::Build {
            input,
            output,
            java_only,
            check,
            format,
            clean,
            perf,
            emit_types,
            emit_telemetry,
            verbose,
            parallel_inference,
            inference_workers,
            constraint_batch,
            log_level,
            log_framework,
            log_default_level,
            otel_enabled,
            otel_endpoint,
            otel_protocol,
            otel_trace_context,
            binary,
            bin_name,
            target,
            apt,
            processors,
            processorpath,
            apt_options,
        }) => {
            let cwd = std::env::current_dir()?;
            let start_path = input
                .as_ref()
                .map(|value| {
                    let candidate = PathBuf::from(value);
                    if candidate.is_absolute() {
                        candidate
                    } else {
                        cwd.join(candidate)
                    }
                })
                .unwrap_or_else(|| cwd.clone());

            let locator = ProjectLocator::new();
            let (project_root, settings, diagnostic_path) = match locator.locate(&start_path) {
                Ok(root) => {
                    let manifest_path = root.manifest_path().to_path_buf();
                    let settings = ManifestLoader::load(&manifest_path)
                        .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;
                    (root, settings, manifest_path)
                }
                Err(diagnostic) => {
                    if let Some((root, settings)) = build_ephemeral_run_settings(&start_path) {
                        (root, settings, start_path.clone())
                    } else {
                        return Err(tooling_failure(&start_path, diagnostic));
                    }
                }
            };

            let entrypoint_override = resolve_cli_entrypoint_override(input.is_some(), &start_path);

            let layout = ProjectLayout::from_settings(&project_root, &settings)
                .map_err(|diagnostic| tooling_failure(&diagnostic_path, diagnostic))?;

            let mut check = check;
            if emit_types {
                check = true;
            }

            let env_logging_layer = read_env_logging_layer()?;
            let cli_logging_layer = build_cli_logging_layer(
                log_framework.as_deref(),
                log_level.as_deref(),
                log_default_level.as_deref(),
                otel_enabled.as_deref(),
                otel_endpoint.as_deref(),
                otel_protocol.as_deref(),
                otel_trace_context.as_deref(),
            )?;

            let overrides = CliOverrides {
                entrypoint: entrypoint_override,
                output: output.clone().map(PathBuf::from),
                java_only,
                check,
                format,
                target,
                clean,
                perf,
                emit_types,
                verbose,
                emit_telemetry,
                parallel_inference,
                inference_workers,
                constraint_batch,
                // APT
                apt_enabled: apt,
                apt_processors: processors.clone(),
                apt_processorpath: processorpath.clone(),
                apt_options: apt_options.clone(),
                logging_cli: cli_logging_layer,
                logging_env: env_logging_layer,
            };

            let plan = BuildOptionsFactory::compose(project_root, settings, layout, overrides)
                .map_err(|diagnostic| tooling_failure(&diagnostic_path, diagnostic))?;

            let manifest_path_for_output = diagnostic_path.clone();
            let mut prepared_output = OutputManager::prepare(plan)
                .map_err(|diagnostic| tooling_failure(&manifest_path_for_output, diagnostic))?;
            let plan = prepared_output.plan();

            println!(
                "出力ディレクトリ: {} (Java{})\nOutput directory: {}",
                prepared_output.target_dir().display(),
                plan.build_config.target,
                prepared_output.target_dir().display()
            );
            if prepared_output.clean_applied() {
                println!("クリーンビルド: 実行しました / Clean build: applied");
            }

            let artifacts = compile(plan)
                .with_context(|| format!("Failed to compile {}", plan.entrypoint().display()))?;

            for java_file in &artifacts.java_files {
                println!("Generated: {}", java_file.display());
            }

            if let Some(version) = &artifacts.javac_version {
                println!("Using javac: {}", version);
            }

            for diagnostic in &artifacts.compatibility_diagnostics {
                println!(
                    "{}",
                    jv_cli::format_tooling_diagnostic(plan.entrypoint(), diagnostic)
                );
            }

            for warning in &artifacts.warnings {
                println!("Warning: {}", warning);
            }

            if plan.options.verbose {
                let has_entries = !artifacts.resolved_imports.is_empty();
                println!("{}", resolved_imports_header(has_entries));
                if has_entries {
                    for import in &artifacts.resolved_imports {
                        println!("{}", format_resolved_import(import));
                    }
                }
            }

            let usage = &artifacts.binding_usage;
            println!(
                "バインディング統計 / Binding usage: explicit val={} implicit val={} implicit typed={} var={}",
                usage.explicit, usage.implicit, usage.implicit_typed, usage.vars
            );
            println!("{}", render_logging_overview(&plan.logging_config));

            if let Some(perf) = &artifacts.perf_capture {
                let summary = &perf.report.summary;
                let checks = &perf.report.checks;
                let reuse_display = if summary.warm_sessions == 0 {
                    "--".to_string()
                } else {
                    format!("{:.3}", summary.reuse_ratio)
                };
                println!(
                    "性能: total={:.2}ms lowering={:.2}ms reuse={} (sessions {}/{})",
                    summary.cold_total_ms,
                    summary.warm_average_ms,
                    reuse_display,
                    summary.warm_sessions,
                    summary.sessions
                );

                if !perf.report.pass {
                    println!(
                        "警告: AST→IR性能予算を超過しています (cold={}, warm={}, reuse={}, peak={:?})",
                        checks.cold_within_budget,
                        checks.warm_within_budget,
                        checks.reuse_ratio_ok,
                        checks.peak_rss_ok
                    );
                }

                println!("Perf report saved to {}", perf.report_path.display());
            }

            if !java_only {
                if artifacts.class_files.is_empty() {
                    println!("Java compilation skipped.");
                } else {
                    println!("Java compilation successful!");
                }
            }

            if let Some(kind) = binary {
                let artifact_path = produce_binary(
                    plan.output_dir(),
                    &bin_name,
                    &kind,
                    &artifacts.script_main_class,
                )?;
                println!("Produced {} artifact at {}", kind, artifact_path.display());
            }

            if let Some(compat) = &artifacts.compatibility {
                println!();
                println!("{}", compat.summary);
                println!("{}", compat.table);
                println!(
                    "互換性レポート出力先 / Compatibility report: {}",
                    compat.json_path.display()
                );
            }

            if plan.options.emit_types {
                if let Some(snapshot) = &artifacts.inference {
                    match snapshot.type_facts().to_pretty_json() {
                        Ok(json) => println!("{}", json),
                        Err(error) => eprintln!("Failed to serialize type facts: {}", error),
                    }
                } else {
                    eprintln!(
                        "Type facts unavailable: type checking did not produce an inference snapshot",
                    );
                }
            }

            prepared_output.mark_success();
        }
        Some(Commands::Run { input, args }) => {
            let cwd = std::env::current_dir()?;
            let start_path = {
                let candidate = PathBuf::from(&input);
                if candidate.is_absolute() {
                    candidate
                } else {
                    cwd.join(candidate)
                }
            };

            let locator = ProjectLocator::new();
            let (project_root, settings, error_path) = match locator.locate(&start_path) {
                Ok(root) => {
                    let manifest_path = root.manifest_path().to_path_buf();
                    let settings = ManifestLoader::load(&manifest_path)
                        .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;
                    (root, settings, manifest_path)
                }
                Err(diagnostic) => {
                    if let Some((root, settings)) = build_ephemeral_run_settings(&start_path) {
                        (root, settings, start_path.clone())
                    } else {
                        return Err(tooling_failure(&start_path, diagnostic));
                    }
                }
            };

            let entrypoint_override = resolve_cli_entrypoint_override(true, &start_path);

            let layout = ProjectLayout::from_settings(&project_root, &settings)
                .map_err(|diagnostic| tooling_failure(&error_path, diagnostic))?;

            let env_logging_layer = read_env_logging_layer()?;

            let overrides = CliOverrides {
                entrypoint: entrypoint_override,
                output: None,
                java_only: false,
                check: false,
                format: false,
                target: None,
                clean: false,
                perf: false,
                emit_types: false,
                verbose: false,
                emit_telemetry: false,
                parallel_inference: false,
                inference_workers: None,
                constraint_batch: None,
                // APT defaults disabled
                apt_enabled: false,
                apt_processors: None,
                apt_processorpath: None,
                apt_options: Vec::new(),
                logging_cli: LoggingConfigLayer::default(),
                logging_env: env_logging_layer,
            };

            let plan = BuildOptionsFactory::compose(project_root, settings, layout, overrides)
                .map_err(|diagnostic| tooling_failure(&error_path, diagnostic))?;

            run_program(&plan, &args)
                .with_context(|| format!("Failed to run {}", plan.entrypoint().display()))?;
        }
        Some(Commands::Fmt { files }) => {
            format_jv_files(files)?;
        }
        Some(Commands::Check { input }) => {
            commands::check::run(&input)?;
        }
        Some(Commands::Tour) => {
            TourOrchestrator::default()
                .run()
                .context("Failed to launch jv language tour")?;
        }
        Some(Commands::Explain { code }) => {
            commands::explain::run(&code).context("Failed to render explanation")?;
        }
        Some(Commands::Version) => {
            println!("{}", get_version());
        }
        Some(Commands::Debug(args)) => {
            commands::debug::run(args).context("Failed to run debug command")?;
        }
        Some(Commands::Repl) | None => {
            repl()?;
        }
    }

    Ok(())
}

fn repl() -> Result<()> {
    println!("jv REPL (type :help for help, :quit to exit)");
    let pipeline = RowanPipeline::default();

    let mut buffer = String::new();
    loop {
        buffer.clear();
        print!("jv> ");
        io::stdout().flush().ok();

        if io::stdin().read_line(&mut buffer)? == 0 {
            println!("\nBye");
            break;
        }
        let line = buffer.trim();
        if line.is_empty() {
            continue;
        }

        match line {
            ":q" | ":quit" | ":exit" => {
                println!("Bye");
                break;
            }
            ":h" | ":help" => {
                println!(
                    "Commands:\n  :help  Show help\n  :quit  Exit\n\nEnter jv statements or declarations (e.g., 'val x = 1')."
                );
                continue;
            }
            _ => {}
        }

        match pipeline.parse(line) {
            Ok(output) => {
                let stmt_count = output.program().statements().len();
                println!("Parsed ✓ (statements: {})", stmt_count);
            }
            Err(e) => {
                println!("Parse error: {}", e);
            }
        }
    }

    Ok(())
}

fn resolve_cli_entrypoint_override(explicit_input: bool, start_path: &Path) -> Option<PathBuf> {
    if !explicit_input {
        return None;
    }

    match fs::metadata(start_path) {
        Ok(metadata) => {
            if metadata.is_file() {
                Some(start_path.to_path_buf())
            } else {
                None
            }
        }
        Err(_) => Some(start_path.to_path_buf()),
    }
}

fn build_ephemeral_run_settings(start_path: &Path) -> Option<(ProjectRoot, ProjectSettings)> {
    let entrypoint_abs = fs::canonicalize(start_path).ok()?;
    if !entrypoint_abs.is_file() {
        return None;
    }

    let root_dir = entrypoint_abs.parent()?.to_path_buf();

    let relative = entrypoint_abs.strip_prefix(&root_dir).ok()?.to_path_buf();
    let rel_string = relative.to_string_lossy().to_string();

    let mut project_section = ProjectSection::default();
    project_section.sources.include = vec!["**/*.jv".to_string()];
    project_section.sources.exclude.clear();
    project_section.entrypoint = Some(rel_string.clone());

    let manifest = Manifest {
        package: PackageInfo {
            // Empty package name signals the script runner to derive the class from the entrypoint.
            name: String::new(),
            version: "0.0.0".to_string(),
            description: None,
            dependencies: HashMap::new(),
        },
        project: project_section,
        build: None,
        logging: LoggingConfig::default(),
    };

    let settings = ProjectSettings {
        manifest,
        sources: SourceConfig {
            include: vec!["**/*.jv".to_string()],
            exclude: Vec::new(),
        },
        output: OutputConfig {
            directory: PathBuf::from("target"),
            clean: false,
        },
        entrypoint: Some(relative),
    };

    let project_root = ProjectRoot::new(root_dir.clone(), root_dir.join("jv.toml"));
    Some((project_root, settings))
}

fn build_cli_logging_layer(
    framework: Option<&str>,
    log_level: Option<&str>,
    default_level: Option<&str>,
    otel_enabled: Option<&str>,
    otel_endpoint: Option<&str>,
    otel_protocol: Option<&str>,
    otel_trace_context: Option<&str>,
) -> Result<LoggingConfigLayer> {
    let mut layer = LoggingConfigLayer::default();

    if let Some(value) = framework {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LoggingFramework::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "--log-framework に無効な値 '{}' が指定されました: {}",
                    trimmed,
                    error
                )
            })?;
            layer.framework = Some(parsed);
        }
    }

    if let Some(value) = log_level {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LogLevel::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "--log-level に無効な値 '{}' が指定されました: {}",
                    trimmed,
                    error
                )
            })?;
            layer.log_level = Some(parsed);
        }
    }

    if let Some(value) = default_level {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LogLevel::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "--log-default-level に無効な値 '{}' が指定されました: {}",
                    trimmed,
                    error
                )
            })?;
            layer.default_level = Some(parsed);
        }
    }

    let mut otel_layer = OpenTelemetryLayer::default();
    let mut has_otel_override = false;

    if let Some(value) = otel_enabled {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            otel_layer.enabled = Some(parse_bool_flag(trimmed, "--otel-enabled")?);
            has_otel_override = true;
        }
    }

    if let Some(value) = otel_endpoint {
        let endpoint = if value.trim().is_empty() {
            None
        } else {
            Some(value.trim().to_string())
        };
        otel_layer.endpoint = Some(endpoint);
        has_otel_override = true;
    }

    if let Some(value) = otel_protocol {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = OtelProtocol::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "--otel-protocol に無効な値 '{}' が指定されました: {}",
                    trimmed,
                    error
                )
            })?;
            otel_layer.protocol = Some(parsed);
            has_otel_override = true;
        }
    }

    if let Some(value) = otel_trace_context {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            otel_layer.trace_context = Some(parse_bool_flag(trimmed, "--otel-trace-context")?);
            has_otel_override = true;
        }
    }

    if has_otel_override {
        layer.opentelemetry = Some(otel_layer);
    }

    Ok(layer)
}

fn read_env_logging_layer() -> Result<LoggingConfigLayer> {
    let mut layer = LoggingConfigLayer::default();

    if let Ok(value) = std::env::var("JV_LOG_FRAMEWORK") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LoggingFramework::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "環境変数 JV_LOG_FRAMEWORK の値 '{}' は無効です: {}",
                    trimmed,
                    error
                )
            })?;
            layer.framework = Some(parsed);
        }
    }

    if let Ok(value) = std::env::var("JV_LOG_LEVEL") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LogLevel::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "環境変数 JV_LOG_LEVEL の値 '{}' は無効です: {}",
                    trimmed,
                    error
                )
            })?;
            layer.log_level = Some(parsed);
        }
    }

    if let Ok(value) = std::env::var("JV_LOG_DEFAULT_LEVEL") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = LogLevel::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "環境変数 JV_LOG_DEFAULT_LEVEL の値 '{}' は無効です: {}",
                    trimmed,
                    error
                )
            })?;
            layer.default_level = Some(parsed);
        }
    }

    let mut otel_layer = OpenTelemetryLayer::default();
    let mut has_otel_override = false;

    if let Ok(value) = std::env::var("JV_OTEL_ENABLED") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            otel_layer.enabled = Some(parse_bool_flag(trimmed, "JV_OTEL_ENABLED")?);
            has_otel_override = true;
        }
    }

    if let Ok(value) = std::env::var("JV_OTEL_ENDPOINT") {
        let trimmed = value.trim();
        otel_layer.endpoint = Some(if trimmed.is_empty() {
            None
        } else {
            Some(trimmed.to_string())
        });
        has_otel_override = true;
    }

    if let Ok(value) = std::env::var("JV_OTEL_PROTOCOL") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            let parsed = OtelProtocol::from_str(trimmed).map_err(|error| {
                anyhow::anyhow!(
                    "環境変数 JV_OTEL_PROTOCOL の値 '{}' は無効です: {}",
                    trimmed,
                    error
                )
            })?;
            otel_layer.protocol = Some(parsed);
            has_otel_override = true;
        }
    }

    if let Ok(value) = std::env::var("JV_OTEL_TRACE_CONTEXT") {
        let trimmed = value.trim();
        if !trimmed.is_empty() {
            otel_layer.trace_context = Some(parse_bool_flag(trimmed, "JV_OTEL_TRACE_CONTEXT")?);
            has_otel_override = true;
        }
    }

    if has_otel_override {
        layer.opentelemetry = Some(otel_layer);
    }

    Ok(layer)
}

fn parse_bool_flag(value: &str, context: &str) -> Result<bool> {
    let trimmed = value.trim();
    let normalized = trimmed.to_ascii_lowercase();
    match normalized.as_str() {
        "true" | "1" | "yes" | "on" => Ok(true),
        "false" | "0" | "no" | "off" => Ok(false),
        _ => anyhow::bail!(
            "{} には true/false のいずれかを指定してください (指定値: '{}')",
            context,
            trimmed
        ),
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn build_ephemeral_settings_detects_script_root() {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let base = std::env::temp_dir().join(format!("jv-ephemeral-run-{timestamp}"));
        fs::create_dir_all(&base).expect("create temp script dir");
        let script_path = base.join("script.jv");
        fs::write(&script_path, "fun main() {}\n").expect("write script");

        let (project_root, settings) =
            build_ephemeral_run_settings(&script_path).expect("ephemeral project to be created");

        let canonical_base = fs::canonicalize(&base).expect("canonicalise base dir");
        assert_eq!(project_root.root_dir(), canonical_base);
        assert_eq!(settings.entrypoint.as_deref(), Some(Path::new("script.jv")));
        assert!(
            settings.manifest.package.name.is_empty(),
            "ephemeral manifest package name should be empty"
        );
        assert_eq!(settings.output.directory, PathBuf::from("target"));

        fs::remove_dir_all(&base).expect("cleanup temp dir");
    }
}

fn format_jv_files(files: Vec<String>) -> Result<()> {
    println!("Formatting {} file(s)...", files.len());
    let pipeline = RowanPipeline::default();

    for file in &files {
        if !Path::new(file).exists() {
            println!("Warning: File '{}' not found", file);
            continue;
        }

        let source =
            fs::read_to_string(file).with_context(|| format!("Failed to read file: {}", file))?;

        if file.ends_with(".jv") {
            match pipeline.parse(&source) {
                Ok(frontend_output) => {
                    let frontend_diagnostics = from_frontend_diagnostics(
                        frontend_output.diagnostics().final_diagnostics(),
                    );
                    let mut should_skip = false;
                    for diagnostic in frontend_diagnostics {
                        let rendered = diagnostic
                            .clone()
                            .with_strategy(DiagnosticStrategy::Deferred);
                        println!(
                            "{}",
                            jv_cli::format_tooling_diagnostic(Path::new(file), &rendered)
                        );
                        if diagnostic.severity == DiagnosticSeverity::Error {
                            should_skip = true;
                        }
                    }
                    if should_skip {
                        continue;
                    }
                    if let Err(error) = transform_program(frontend_output.into_program()) {
                        if let Some(diagnostic) = from_transform_error(&error) {
                            println!(
                                "{}",
                                jv_cli::format_tooling_diagnostic(Path::new(file), &diagnostic)
                            );
                            continue;
                        }
                        println!("IR transformation error for {}: {:?}", file, error);
                        continue;
                    }
                }
                Err(error) => {
                    if let Some(diagnostic) = from_parse_error(&error) {
                        println!(
                            "{}",
                            jv_cli::format_tooling_diagnostic(Path::new(file), &diagnostic)
                        );
                        continue;
                    }
                    println!("Parser error for {}: {:?}", file, error);
                    continue;
                }
            }
        }

        let formatter = JavaFormatter::default();
        let formatted = formatter.format_compilation_unit(&source).unwrap_or(source);

        fs::write(file, formatted)
            .with_context(|| format!("Failed to write formatted file: {}", file))?;

        println!("Formatted: {}", file);
    }

    Ok(())
}
