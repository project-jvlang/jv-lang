// jv_cli - CLI functionality (library interface for testing)
use anyhow::Result;
use clap::Parser;
use std::fs;
use std::path::Path;

use jv_checker::diagnostics::{from_parse_error, from_transform_error, ToolingDiagnostic};
use jv_pm::JavaTarget;

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
        /// Produce a single-file binary artifact: 'jar' or 'native'
        #[arg(long, value_parser = ["jar", "native"])]
        binary: Option<String>,
        /// Output name (without extension) for --binary; default: 'app'
        #[arg(long, default_value = "app")]
        bin_name: String,
        /// Override the Java target (e.g., 21 or 25)
        #[arg(long, value_name = "java-target")]
        target: Option<JavaTarget>,
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
    /// Launch the interactive language tour experience
    Tour,
}

pub fn tooling_failure(path: &Path, diagnostic: ToolingDiagnostic) -> anyhow::Error {
    anyhow::anyhow!(format_tooling_diagnostic(path, &diagnostic))
}

pub fn format_tooling_diagnostic(path: &Path, diagnostic: &ToolingDiagnostic) -> String {
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
        "{code}: {title}{location}\n  ファイル: {file}\n  詳細: {detail}\n  対処: {help}",
        code = diagnostic.code,
        title = diagnostic.title,
        file = path.display(),
        detail = diagnostic.message,
        help = diagnostic.help
    )
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

pub mod tour;

pub mod pipeline {
    pub mod compat {
        include!("pipeline/compat.rs");
    }

    pub mod report {
        include!("pipeline/report.rs");
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
    pub use project::output::{OutputManager, PreparedOutput};

    use super::*;
    use anyhow::{anyhow, bail, Context};
    use jv_build::BuildSystem;
    use jv_checker::compat::diagnostics as compat_diagnostics;
    use jv_checker::{InferenceSnapshot, TypeChecker};
    use jv_codegen_java::{JavaCodeGenConfig, JavaCodeGenerator};
    use jv_fmt::JavaFormatter;
    use jv_ir::transform_program;
    use jv_parser::Parser as JvParser;
    use std::ffi::OsStr;
    use std::path::{Path, PathBuf};
    use std::process::Command;
    use std::time::{SystemTime, UNIX_EPOCH};

    /// Resulting artifacts and diagnostics from the build pipeline.
    #[derive(Debug, Default, Clone)]
    pub struct BuildArtifacts {
        pub java_files: Vec<PathBuf>,
        pub class_files: Vec<PathBuf>,
        pub javac_version: Option<String>,
        pub warnings: Vec<String>,
        pub compatibility: Option<report::RenderedCompatibilityReport>,
        pub compatibility_diagnostics: Vec<ToolingDiagnostic>,
        pub inference: Option<InferenceSnapshot>,
    }

    /// Compile a `.jv` file end-to-end into Java (and optionally `.class`) outputs.
    pub fn compile(plan: &BuildPlan) -> Result<BuildArtifacts> {
        let options = &plan.options;
        let entrypoint = options.entrypoint.as_path();

        if !entrypoint.exists() {
            bail!("Input file '{}' not found", entrypoint.display());
        }

        let mut warnings = Vec::new();
        let mut inference_snapshot: Option<InferenceSnapshot> = None;
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

        let program = match JvParser::parse(&source) {
            Ok(program) => program,
            Err(error) => {
                if let Some(diagnostic) = from_parse_error(&error) {
                    return Err(tooling_failure(entrypoint, diagnostic));
                }
                return Err(anyhow!("Parser error: {:?}", error));
            }
        };

        if options.check {
            let mut type_checker = TypeChecker::new();
            if let Err(errors) = type_checker.check_program(&program) {
                let details = errors
                    .iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n  - ");
                bail!("Type checking failed:\n  - {}", details);
            }

            warnings.extend(
                type_checker
                    .check_null_safety(&program)
                    .into_iter()
                    .map(|warning| warning.to_string()),
            );
            inference_snapshot = type_checker.take_inference_snapshot();
        }

        let ir_program = match transform_program(program) {
            Ok(ir) => ir,
            Err(error) => {
                if let Some(diagnostic) = from_transform_error(&error) {
                    return Err(tooling_failure(entrypoint, diagnostic));
                }
                return Err(anyhow!("IR transformation error: {:?}", error));
            }
        };

        let mut code_generator =
            JavaCodeGenerator::with_config(JavaCodeGenConfig::for_target(build_config.target));
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
            compatibility: None,
            compatibility_diagnostics,
            inference: inference_snapshot,
        };

        if !options.java_only {
            let build_system = BuildSystem::new(build_config.clone());

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

    /// Compile and execute a `.jv` program using the Java runtime.
    pub fn run_program(plan: &BuildPlan, args: &[String]) -> Result<()> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis();
        let temp_dir = std::env::temp_dir().join(format!("jv-cli-{}", timestamp));
        fs::create_dir_all(&temp_dir)?;

        let mut temp_plan = plan.with_output_dir(temp_dir.clone());
        temp_plan.options.java_only = false;
        temp_plan.options.check = false;
        temp_plan.options.format = false;

        let compile_result = match compile(&temp_plan) {
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
