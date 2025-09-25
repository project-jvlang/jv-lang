// jv CLI entry point
use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use jv_checker::diagnostics::{from_parse_error, from_transform_error};
use jv_checker::TypeChecker;
use jv_fmt::JavaFormatter;
use jv_ir::transform_program;
use jv_parser::Parser as JvParser;

use jv_cli::pipeline::project::{
    layout::ProjectLayout, locator::ProjectLocator, manifest::ManifestLoader,
};
use jv_cli::pipeline::{compile, produce_binary, run_program, BuildOptionsFactory, CliOverrides};
use jv_cli::tour::TourOrchestrator;
use jv_cli::{get_version, init_project as cli_init_project, Cli, Commands};

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
            binary,
            bin_name,
            target,
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

            let project_root = ProjectLocator::new()
                .locate(&start_path)
                .map_err(|diagnostic| tooling_failure(&start_path, diagnostic))?;
            let manifest_path = project_root.manifest_path().to_path_buf();

            let settings = ManifestLoader::load(&manifest_path)
                .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;
            let layout = ProjectLayout::from_settings(&project_root, &settings)
                .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;

            let overrides = CliOverrides {
                entrypoint: input.clone().map(PathBuf::from),
                output: output.clone().map(PathBuf::from),
                java_only,
                check,
                format,
                target,
            };

            let plan = BuildOptionsFactory::compose(project_root, settings, layout, overrides)
                .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;

            let artifacts = compile(&plan)
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

            if !java_only {
                if artifacts.class_files.is_empty() {
                    println!("Java compilation skipped.");
                } else {
                    println!("Java compilation successful!");
                }
            }

            if let Some(kind) = binary {
                let artifact_path = produce_binary(plan.output_dir(), &bin_name, &kind)?;
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

            let project_root = ProjectLocator::new()
                .locate(&start_path)
                .map_err(|diagnostic| tooling_failure(&start_path, diagnostic))?;
            let manifest_path = project_root.manifest_path().to_path_buf();

            let settings = ManifestLoader::load(&manifest_path)
                .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;
            let layout = ProjectLayout::from_settings(&project_root, &settings)
                .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;

            let overrides = CliOverrides {
                entrypoint: Some(PathBuf::from(&input)),
                output: None,
                java_only: false,
                check: false,
                format: false,
                target: None,
            };

            let plan = BuildOptionsFactory::compose(project_root, settings, layout, overrides)
                .map_err(|diagnostic| tooling_failure(&manifest_path, diagnostic))?;

            run_program(&plan, &args)
                .with_context(|| format!("Failed to run {}", plan.entrypoint().display()))?;
        }
        Some(Commands::Fmt { files }) => {
            format_jv_files(files)?;
        }
        Some(Commands::Check { input }) => {
            check_jv_file(&input)?;
        }
        Some(Commands::Tour) => {
            TourOrchestrator::default()
                .run()
                .context("Failed to launch jv language tour")?;
        }
        Some(Commands::Version) => {
            println!("{}", get_version());
        }
        Some(Commands::Repl) | None => {
            repl()?;
        }
    }

    Ok(())
}

fn repl() -> Result<()> {
    println!("jv REPL (type :help for help, :quit to exit)");

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

        match JvParser::parse(line) {
            Ok(program) => {
                let stmt_count = program.statements.len();
                println!("Parsed ✓ (statements: {})", stmt_count);
            }
            Err(e) => {
                println!("Parse error: {}", e);
            }
        }
    }

    Ok(())
}

fn format_jv_files(files: Vec<String>) -> Result<()> {
    println!("Formatting {} file(s)...", files.len());

    for file in &files {
        if !Path::new(file).exists() {
            println!("Warning: File '{}' not found", file);
            continue;
        }

        let source =
            fs::read_to_string(file).with_context(|| format!("Failed to read file: {}", file))?;

        if file.ends_with(".jv") {
            match JvParser::parse(&source) {
                Ok(program) => {
                    if let Err(error) = transform_program(program) {
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

fn check_jv_file(input: &str) -> Result<()> {
    println!("Checking: {}", input);

    if !Path::new(input).exists() {
        anyhow::bail!("Input file '{}' not found", input);
    }

    let source =
        fs::read_to_string(input).with_context(|| format!("Failed to read file: {}", input))?;

    let program = match JvParser::parse(&source) {
        Ok(program) => program,
        Err(error) => {
            if let Some(diagnostic) = from_parse_error(&error) {
                return Err(jv_cli::tooling_failure(Path::new(input), diagnostic));
            }
            return Err(anyhow::anyhow!("Parser error: {:?}", error));
        }
    };

    if let Err(error) = transform_program(program.clone()) {
        if let Some(diagnostic) = from_transform_error(&error) {
            return Err(jv_cli::tooling_failure(Path::new(input), diagnostic));
        }
        return Err(anyhow::anyhow!("IR transformation error: {:?}", error));
    }

    let type_checker = TypeChecker::new();
    match type_checker.check_program(&program) {
        Ok(_) => {
            println!("✓ No errors found");
        }
        Err(errors) => {
            println!("Found {} error(s):", errors.len());
            for error in &errors {
                println!("  Error: {}", error);
            }
            return Err(anyhow::anyhow!("Type checking failed"));
        }
    }

    let warnings = type_checker.check_null_safety(&program);
    if !warnings.is_empty() {
        println!("Null safety warnings:");
        for warning in &warnings {
            println!("  Warning: {}", warning);
        }
    }

    println!("Check completed successfully!");
    Ok(())
}
