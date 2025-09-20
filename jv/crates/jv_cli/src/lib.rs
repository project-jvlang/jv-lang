// jv_cli - CLI functionality (library interface for testing)
use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Parser, Debug, Clone)]
#[command(name = "jv")]
#[command(about = "A Java Sugar Language compiler")]
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
    /// Show version information  
    Version,
    /// Start interactive REPL
    Repl,
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

pub mod pipeline {
    use super::*;
    use anyhow::{anyhow, bail};
    use jv_build::{BuildConfig, BuildSystem};
    use jv_checker::TypeChecker;
    use jv_codegen_java::JavaCodeGenerator;
    use jv_fmt::JavaFormatter;
    use jv_ir::transform_program;
    use jv_parser::Parser as JvParser;
    use std::ffi::OsStr;
    use std::process::Command;
    use std::time::{SystemTime, UNIX_EPOCH};

    /// Options that control how the CLI build pipeline executes.
    #[derive(Debug, Clone)]
    pub struct BuildOptions {
        pub input: PathBuf,
        pub output_dir: PathBuf,
        pub java_only: bool,
        pub check: bool,
        pub format: bool,
    }

    impl BuildOptions {
        pub fn new(input: impl Into<PathBuf>, output_dir: impl Into<PathBuf>) -> Self {
            Self {
                input: input.into(),
                output_dir: output_dir.into(),
                java_only: false,
                check: false,
                format: false,
            }
        }
    }

    /// Resulting artifacts and diagnostics from the build pipeline.
    #[derive(Debug, Default, Clone)]
    pub struct BuildArtifacts {
        pub java_files: Vec<PathBuf>,
        pub class_files: Vec<PathBuf>,
        pub javac_version: Option<String>,
        pub warnings: Vec<String>,
    }

    /// Compile a `.jv` file end-to-end into Java (and optionally `.class`) outputs.
    pub fn compile(options: &BuildOptions) -> Result<BuildArtifacts> {
        if !options.input.exists() {
            bail!("Input file '{}' not found", options.input.display());
        }

        let source = fs::read_to_string(&options.input)
            .with_context(|| format!("Failed to read file: {}", options.input.display()))?;

        let program = JvParser::parse(&source).map_err(|e| anyhow!("Parser error: {:?}", e))?;

        let mut warnings = Vec::new();

        if options.check {
            let type_checker = TypeChecker::new();
            if let Err(errors) = type_checker.check_program(&program) {
                let details = errors
                    .iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n  - ");
                bail!("Type checking failed:\n  - {}", details);
            }

            warnings.extend(type_checker.check_null_safety(&program));
        }

        let ir_program =
            transform_program(program).map_err(|e| anyhow!("IR transformation error: {:?}", e))?;

        let mut code_generator = JavaCodeGenerator::new();
        let java_unit = code_generator
            .generate_compilation_unit(&ir_program)
            .map_err(|e| anyhow!("Code generation error: {:?}", e))?;

        fs::create_dir_all(&options.output_dir).with_context(|| {
            format!(
                "Failed to create output directory: {}",
                options.output_dir.display()
            )
        })?;

        let mut java_files = Vec::new();
        for (index, type_decl) in java_unit.type_declarations.iter().enumerate() {
            let java_filename = if index == 0 {
                "GeneratedMain.java".to_string()
            } else {
                format!("GeneratedMain{}.java", index)
            };
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

        let mut artifacts = BuildArtifacts {
            java_files,
            class_files: Vec::new(),
            javac_version: None,
            warnings,
        };

        if !options.java_only {
            let build_config = BuildConfig {
                output_dir: options.output_dir.to_string_lossy().into_owned(),
                ..BuildConfig::default()
            };
            let build_system = BuildSystem::new(build_config);

            match build_system.check_javac_availability() {
                Ok(version) => {
                    artifacts.javac_version = Some(version.clone());
                    let java_paths: Vec<&Path> =
                        artifacts.java_files.iter().map(|p| p.as_path()).collect();
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
                Err(err) => {
                    artifacts
                        .warnings
                        .push(format!("Skipping Java compilation: {}", err));
                }
            }
        }

        Ok(artifacts)
    }

    /// Compile and execute a `.jv` program using the Java runtime.
    pub fn run_program(input: impl AsRef<Path>, args: &[String]) -> Result<()> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let temp_dir = std::env::temp_dir().join(format!("jv-cli-{}", timestamp));
        fs::create_dir_all(&temp_dir)?;

        let mut options = BuildOptions::new(input.as_ref(), &temp_dir);
        options.java_only = false;
        options.check = false;
        options.format = false;

        let compile_result = match compile(&options) {
            Ok(result) => result,
            Err(err) => {
                let _ = fs::remove_dir_all(&temp_dir);
                return Err(err);
            }
        };

        let main_class = temp_dir.join("GeneratedMain.class");
        if !main_class.exists() {
            let _ = fs::remove_dir_all(&temp_dir);
            bail!("No compiled .class file found. Make sure javac is available for execution.");
        }

        let classpath = temp_dir.to_string_lossy().to_string();
        let mut cmd = Command::new("java");
        cmd.arg("-cp");
        cmd.arg(&classpath);
        cmd.arg("GeneratedMain");
        for arg in args {
            cmd.arg(arg);
        }

        let status = cmd
            .status()
            .with_context(|| "Failed to run java - is it installed?")?;
        let _ = fs::remove_dir_all(&temp_dir);

        if !status.success() {
            bail!("Program execution failed");
        }

        // Warn the caller if javac step was skipped
        if compile_result.class_files.is_empty() {
            bail!("Java compilation step did not produce class files; cannot execute program");
        }

        Ok(())
    }

    /// Package generated outputs into a JAR or native binary.
    pub fn produce_binary(output_dir: &Path, bin_name: &str, kind: &str) -> Result<PathBuf> {
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
                        "GeneratedMain",
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
                let jar_artifact = produce_binary(output_dir, bin_name, "jar")?;
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
