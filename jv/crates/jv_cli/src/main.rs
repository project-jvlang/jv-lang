// jv CLI entry point
use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use jv_checker::TypeChecker;
use jv_fmt::JavaFormatter;
use jv_parser::Parser as JvParser;

use jv_cli::pipeline::{compile, produce_binary, run_program, BuildOptions};
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
        }) => {
            let mut options = BuildOptions::new(input.as_str(), output.as_str());
            options.java_only = java_only;
            options.check = check;
            options.format = format;

            let artifacts =
                compile(&options).with_context(|| format!("Failed to compile {}", input))?;

            for java_file in &artifacts.java_files {
                println!("Generated: {}", java_file.display());
            }

            if let Some(version) = &artifacts.javac_version {
                println!("Using javac: {}", version);
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
                let artifact_path = produce_binary(&options.output_dir, &bin_name, &kind)?;
                println!("Produced {} artifact at {}", kind, artifact_path.display());
            }
        }
        Some(Commands::Run { input, args }) => {
            run_program(input.as_str(), &args)
                .with_context(|| format!("Failed to run {}", input))?;
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

    let program = JvParser::parse(&source).map_err(|e| anyhow::anyhow!("Parser error: {:?}", e))?;

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
