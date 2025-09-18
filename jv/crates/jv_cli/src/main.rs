// jv CLI entry point
use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::Path;
use std::process::Command;

use jv_build::{BuildConfig, BuildSystem};
use jv_checker::TypeChecker;
use jv_codegen_java::JavaCodeGenerator;
use jv_fmt::JavaFormatter;
use jv_ir::transform_program;
use jv_mapper::{SourceMapBuilder, SourceMapManager};
use jv_parser::Parser as JvParser;
use std::io::{self, Write};
use std::process::Command as ProcCommand;

#[derive(Parser)]
#[command(name = "jv")]
#[command(about = "A Java Sugar Language compiler")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(clap::Subcommand)]
enum Commands {
    /// Initialize a new jv project
    Init {
        /// Project name
        #[arg(default_value = ".")]
        name: String,
    },
    /// Build jv source to Java and compile with javac
    Build {
        /// Input .jv file
        input: String,
        /// Output directory for .java files
        #[arg(short, long, default_value = "./out")]
        output: String,
        /// Skip javac compilation
        #[arg(long)]
        java_only: bool,
        /// Enable type checking
        #[arg(long)]
        check: bool,
        /// Format output Java code
        #[arg(long)]
        format: bool,
        /// Produce a single-file binary artifact: 'jar' or 'native'
        #[arg(long, value_parser = ["jar", "native"])]
        binary: Option<String>,
        /// Output name (without extension) for --binary; default: 'app'
        #[arg(long, default_value = "app")]
        bin_name: String,
    },
    /// Run a compiled jv program
    Run {
        /// Input .jv file to compile and run
        input: String,
        /// Arguments to pass to the program
        #[arg(last = true)]
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
    /// Show version information  
    Version,
    /// Start interactive REPL
    Repl,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Init { name }) => {
            init_project(&name)?;
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
            compile_jv_file(&input, &output, java_only, check, format)
                .with_context(|| format!("Failed to compile {}", input))?;

            if let Some(kind) = binary {
                produce_binary(&output, &bin_name, &kind)?;
            }
        }
        Some(Commands::Run { input, args }) => {
            run_jv_file(&input, args)?;
        }
        Some(Commands::Fmt { files }) => {
            format_jv_files(files)?;
        }
        Some(Commands::Check { input }) => {
            check_jv_file(&input)?;
        }
        Some(Commands::Version) => {
            println!(
                "jv {} - Java Sugar Language compiler",
                env!("CARGO_PKG_VERSION")
            );
        }
        Some(Commands::Repl) => {
            repl()?;
        }
        None => {
            // No args: launch REPL by default
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
            // EOF
            println!("\nBye");
            break;
        }
        let line = buffer.trim();
        if line.is_empty() {
            continue;
        }

        // REPL meta commands
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

        // Try parsing the input as a small program
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

fn init_project(name: &str) -> Result<()> {
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
    let jv_toml = format!(
        r#"[package]
name = "{}"
version = "0.1.0"

[build]
java_version = "25"
"#,
        project_name
    );

    fs::write(project_dir.join("jv.toml"), jv_toml)?;

    // Create src directory and example file
    let src_dir = project_dir.join("src");
    fs::create_dir_all(&src_dir)?;

    let main_jv = r#"fun main() {
    val greeting = "Hello, jv!"
    println(greeting)
}
"#;
    fs::write(src_dir.join("main.jv"), main_jv)?;

    println!(
        "Created jv project '{}' in {}",
        project_name,
        project_dir.display()
    );
    println!("Next steps:");
    println!("  cd {}", if name == "." { "." } else { name });
    println!("  jv build src/main.jv");
    println!("  jv run src/main.jv");

    Ok(())
}

fn compile_jv_file(
    input: &str,
    output_dir: &str,
    java_only: bool,
    check: bool,
    format: bool,
) -> Result<()> {
    println!("Compiling: {} -> {}/", input, output_dir);

    // Ensure input file exists
    if !Path::new(input).exists() {
        anyhow::bail!("Input file '{}' not found", input);
    }

    // Read source code
    let source =
        fs::read_to_string(input).with_context(|| format!("Failed to read file: {}", input))?;

    // Parse to AST (which internally does lexical analysis)
    let program = JvParser::parse(&source).map_err(|e| anyhow::anyhow!("Parser error: {:?}", e))?;

    // Optional: Type checking
    if check {
        let type_checker = TypeChecker::new();
        if let Err(errors) = type_checker.check_program(&program) {
            println!("Type checking failed with {} errors:", errors.len());
            for error in &errors {
                println!("  Error: {}", error);
            }
            return Err(anyhow::anyhow!("Type checking failed"));
        }
        println!("Type checking passed ✓");
    }

    // Transform AST to IR
    let ir_program = transform_program(program)
        .map_err(|e| anyhow::anyhow!("IR transformation error: {:?}", e))?;

    // Generate Java code
    let mut code_generator = JavaCodeGenerator::new();
    let java_unit = code_generator
        .generate_compilation_unit(&ir_program)
        .map_err(|e| anyhow::anyhow!("Code generation error: {:?}", e))?;

    // Create output directory
    fs::create_dir_all(output_dir)
        .with_context(|| format!("Failed to create output directory: {}", output_dir))?;

    // Write Java files
    for (i, type_decl) in java_unit.type_declarations.iter().enumerate() {
        let java_filename = format!(
            "GeneratedMain{}.java",
            if i == 0 {
                "".to_string()
            } else {
                i.to_string()
            }
        );
        let java_path = Path::new(output_dir).join(&java_filename);

        let mut java_content = String::new();
        if let Some(package) = &java_unit.package_declaration {
            java_content.push_str(&format!("package {};\n\n", package));
        }
        for import in &java_unit.imports {
            java_content.push_str(&format!("import {};\n", import));
        }
        java_content.push('\n');
        java_content.push_str(type_decl);

        // Optional: Format generated Java code
        if format {
            let formatter = JavaFormatter::default();
            java_content = formatter
                .format_compilation_unit(&java_content)
                .unwrap_or(java_content);
        }

        fs::write(&java_path, java_content)
            .with_context(|| format!("Failed to write Java file: {:?}", java_path))?;

        println!("Generated: {:?}", java_path);
    }

    // Compile with javac if requested
    if !java_only {
        let build_config = BuildConfig {
            output_dir: output_dir.to_string(),
            ..BuildConfig::default()
        };
        let build_system = BuildSystem::new(build_config);

        // Check if javac is available
        match build_system.check_javac_availability() {
            Ok(version) => println!("Using javac: {}", version),
            Err(e) => {
                println!("Warning: {}", e);
                println!("Skipping Java compilation - install JDK 25 to compile to .class files");
                return Ok(());
            }
        }

        // Collect all java files for compilation
        let java_paths: Vec<std::path::PathBuf> = std::fs::read_dir(output_dir)?
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "java"))
            .map(|entry| entry.path())
            .collect();

        let java_files: Vec<&Path> = java_paths.iter().map(|p| p.as_path()).collect();

        if !java_files.is_empty() {
            build_system
                .compile_java_files(java_files)
                .map_err(|e| anyhow::anyhow!("Java compilation failed: {}", e))?;
            println!("Java compilation successful!");
        }
    }

    println!("Compilation completed successfully!");
    Ok(())
}

fn run_jv_file(input: &str, args: Vec<String>) -> Result<()> {
    // First compile the file
    let temp_output = "./temp_out";
    compile_jv_file(input, temp_output, false, false, false)?;

    // Find the main class file
    let main_class_file = format!("{}/GeneratedMain.class", temp_output);
    if !Path::new(&main_class_file).exists() {
        anyhow::bail!("No compiled .class file found. Make sure javac is available.");
    }

    println!("Running compiled program...");

    // Run with java command
    let mut cmd = Command::new("java");
    cmd.args(["-cp", temp_output, "GeneratedMain"]);
    cmd.args(&args);

    let status = cmd
        .status()
        .context("Failed to run java - is it installed?")?;

    // Cleanup temp directory
    if Path::new(temp_output).exists() {
        fs::remove_dir_all(temp_output).ok();
    }

    if !status.success() {
        anyhow::bail!("Program execution failed");
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

        // For now, jv files use the same format as the generated Java
        // In the future, this would have jv-specific formatting rules
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

    // Read and parse source
    let source =
        fs::read_to_string(input).with_context(|| format!("Failed to read file: {}", input))?;

    let program = JvParser::parse(&source).map_err(|e| anyhow::anyhow!("Parser error: {:?}", e))?;

    // Run type checker
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

    // Check null safety
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

fn produce_binary(output_dir: &str, bin_name: &str, kind: &str) -> Result<()> {
    match kind {
        "jar" => {
            let jar_path = format!("{}/{}.jar", output_dir, bin_name);
            // Use JDK 'jar' tool to package classes in output_dir
            // Set Main-Class to GeneratedMain (current default)
            let status = ProcCommand::new("jar")
                .args([
                    "--create",
                    "--file",
                    &jar_path,
                    "--main-class",
                    "GeneratedMain",
                    "-C",
                    output_dir,
                    ".",
                ])
                .status();
            match status {
                Ok(s) if s.success() => {
                    println!("Created JAR: {}", jar_path);
                    Ok(())
                }
                Ok(s) => {
                    anyhow::bail!("jar failed with status: {}", s);
                }
                Err(e) => {
                    anyhow::bail!("'jar' tool not found or failed: {}", e);
                }
            }
        }
        "native" => {
            // Best-effort: require GraalVM native-image; package via jar first
            let jar_path = format!("{}/{}.jar", output_dir, bin_name);
            let _ = ProcCommand::new("jar")
                .args([
                    "--create",
                    "--file",
                    &jar_path,
                    "--main-class",
                    "GeneratedMain",
                    "-C",
                    output_dir,
                    ".",
                ])
                .status();

            let native_out = format!("{}/{}", output_dir, bin_name);
            match ProcCommand::new("native-image")
                .args(["-jar", &jar_path, &native_out])
                .status()
            {
                Ok(s) if s.success() => {
                    println!("Created native binary: {}", native_out);
                    Ok(())
                }
                Ok(_) => {
                    anyhow::bail!("native-image failed. Ensure GraalVM native-image is installed.");
                }
                Err(_) => {
                    anyhow::bail!(
                        "native-image not found. Install GraalVM native-image or use --binary jar"
                    );
                }
            }
        }
        _ => unreachable!(),
    }
}

// exec_quick removed: prefer `jv run <file>` or standalone `jvx` for quick execution
