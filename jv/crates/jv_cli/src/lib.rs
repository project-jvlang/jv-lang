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

        let program = JvParser::parse(&source)
            .map_err(|e| anyhow!("Parser error: {:?}", e))?;

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

        let ir_program = transform_program(program)
            .map_err(|e| anyhow!("IR transformation error: {:?}", e))?;

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

            fs::write(&java_path, java_content).with_context(|| {
                format!("Failed to write Java file: {}", java_path.display())
            })?;

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

                        let entries = fs::read_dir(&options.output_dir)
                            .with_context(|| {
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
            bail!(
                "No compiled .class file found. Make sure javac is available for execution."
            );
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
            bail!(
                "Java compilation step did not produce class files; cannot execute program"
            );
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
                        jar_path.to_str().ok_or_else(|| {
                            anyhow!("Non-UTF8 path encountered for jar output")
                        })?,
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
                    Ok(_) => bail!(
                        "native-image failed. Ensure GraalVM native-image is installed."
                    ),
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
mod tests {
    use super::*;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    struct TempDirGuard {
        path: PathBuf,
    }

    impl TempDirGuard {
        fn new(prefix: &str) -> Self {
            let timestamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis();
            let path = std::env::temp_dir().join(format!("jv-cli-{}-{}", prefix, timestamp));
            fs::create_dir_all(&path).unwrap();
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }
    }

    impl Drop for TempDirGuard {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    #[test]
    fn test_cli_parsing() {
        // Test version command
        let version_args = vec!["jv", "version"];
        let cli = Cli::try_parse_from(version_args).unwrap();
        assert!(matches!(cli.command, Some(Commands::Version)));

        // Test init command
        let init_args = vec!["jv", "init", "my-project"];
        let cli = Cli::try_parse_from(init_args).unwrap();
        match cli.command {
            Some(Commands::Init { name }) => assert_eq!(name, "my-project"),
            _ => panic!("Expected Init command"),
        }
    }

    #[test]
    fn test_build_command_parsing() {
        let build_args = vec![
            "jv", "build", "test.jv", "-o", "output", "--check", "--format",
        ];
        let cli = Cli::try_parse_from(build_args).unwrap();

        match cli.command {
            Some(Commands::Build {
                input,
                output,
                java_only,
                check,
                format,
                ..
            }) => {
                assert_eq!(input, "test.jv");
                assert_eq!(output, "output");
                assert!(!java_only);
                assert!(check);
                assert!(format);
            }
            _ => panic!("Expected Build command"),
        }
    }

    #[test]
    fn test_run_command_parsing() {
        let run_args = vec!["jv", "run", "test.jv", "arg1", "arg2"];
        let cli = Cli::try_parse_from(run_args).unwrap();

        match cli.command {
            Some(Commands::Run { input, args }) => {
                assert_eq!(input, "test.jv");
                assert_eq!(args, vec!["arg1", "arg2"]);
            }
            _ => panic!("Expected Run command"),
        }
    }

    #[test]
    fn test_fmt_command_parsing() {
        let fmt_args = vec!["jv", "fmt", "file1.jv", "file2.jv"];
        let cli = Cli::try_parse_from(fmt_args).unwrap();

        match cli.command {
            Some(Commands::Fmt { files }) => {
                assert_eq!(files, vec!["file1.jv", "file2.jv"]);
            }
            _ => panic!("Expected Fmt command"),
        }
    }

    #[test]
    fn test_check_command_parsing() {
        let check_args = vec!["jv", "check", "test.jv"];
        let cli = Cli::try_parse_from(check_args).unwrap();

        match cli.command {
            Some(Commands::Check { input }) => {
                assert_eq!(input, "test.jv");
            }
            _ => panic!("Expected Check command"),
        }
    }

    #[test]
    fn test_no_command() {
        let no_command_args = vec!["jv"];
        let cli = Cli::try_parse_from(no_command_args).unwrap();
        assert!(cli.command.is_none());
    }

    #[test]
    fn test_init_project_in_temp_dir() {
        let temp_dir = TempDirGuard::new("init-temp");
        let temp_path = temp_dir.path().to_path_buf();

        // Change to temp directory
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(&temp_path).unwrap();

        // Test init in current directory
        let result = init_project(".");
        assert!(result.is_ok());

        // Check that files were created
        assert!(temp_path.join("jv.toml").exists());
        assert!(temp_path.join("src").exists());
        assert!(temp_path.join("src/main.jv").exists());

        // Check jv.toml content
        let jv_toml_content = fs::read_to_string(temp_path.join("jv.toml")).unwrap();
        assert!(jv_toml_content.contains("[package]"));
        assert!(jv_toml_content.contains("java_version = \"25\""));

        // Check main.jv content
        let main_jv_content = fs::read_to_string(temp_path.join("src/main.jv")).unwrap();
        assert!(main_jv_content.contains("fun main()"));
        assert!(main_jv_content.contains("Hello, jv!"));

        // Restore original directory
        std::env::set_current_dir(&original_dir).unwrap();
    }

    #[test]
    fn test_init_project_with_name() {
        let temp_dir = TempDirGuard::new("init-named");
        let temp_path = temp_dir.path().to_path_buf();

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(&temp_path).unwrap();

        let project_name = "test-project";
        let result = init_project(project_name);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), project_name);

        let project_path = temp_path.join(project_name);
        assert!(project_path.exists());
        assert!(project_path.join("jv.toml").exists());
        assert!(project_path.join("src/main.jv").exists());

        std::env::set_current_dir(&original_dir).unwrap();
    }

    #[test]
    fn test_validate_file_exists() {
        let temp_dir = TempDirGuard::new("validate");
        let temp_file = temp_dir.path().join("test.jv");

        // Test non-existent file
        let result = validate_file_exists(temp_file.to_str().unwrap());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not found"));

        // Create the file and test again
        fs::write(&temp_file, "val x = 42").unwrap();
        let result = validate_file_exists(temp_file.to_str().unwrap());
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_version() {
        let version = get_version();
        assert!(version.contains("jv"));
        assert!(version.contains("Java Sugar Language compiler"));
        assert!(version.contains(env!("CARGO_PKG_VERSION")));
    }

    #[test]
    fn test_build_command_defaults() {
        let build_args = vec!["jv", "build", "test.jv"];
        let cli = Cli::try_parse_from(build_args).unwrap();

        match cli.command {
            Some(Commands::Build {
                input,
                output,
                java_only,
                check,
                format,
                ..
            }) => {
                assert_eq!(input, "test.jv");
                assert_eq!(output, "./out");
                assert!(!java_only);
                assert!(!check);
                assert!(!format);
            }
            _ => panic!("Expected Build command"),
        }
    }

    #[test]
    fn test_init_command_default() {
        let init_args = vec!["jv", "init"];
        let cli = Cli::try_parse_from(init_args).unwrap();

        match cli.command {
            Some(Commands::Init { name }) => assert_eq!(name, "."),
            _ => panic!("Expected Init command"),
        }
    }

    #[test]
    fn test_invalid_command() {
        let invalid_args = vec!["jv", "invalid-command"];
        let result = Cli::try_parse_from(invalid_args);
        assert!(result.is_err());
    }

    #[test]
    fn test_missing_required_args() {
        // Test build without input file
        let invalid_build = vec!["jv", "build"];
        let result = Cli::try_parse_from(invalid_build);
        assert!(result.is_err());

        // Test check without input file
        let invalid_check = vec!["jv", "check"];
        let result = Cli::try_parse_from(invalid_check);
        assert!(result.is_err());
    }

    #[test]
    fn test_java_only_flag() {
        let build_args = vec!["jv", "build", "test.jv", "--java-only"];
        let cli = Cli::try_parse_from(build_args).unwrap();

        match cli.command {
            Some(Commands::Build { java_only, .. }) => assert!(java_only),
            _ => panic!("Expected Build command"),
        }
    }

    #[test]
    fn test_multiple_files_fmt() {
        let fmt_args = vec!["jv", "fmt", "a.jv", "b.jv", "c.jv"];
        let cli = Cli::try_parse_from(fmt_args).unwrap();

        match cli.command {
            Some(Commands::Fmt { files }) => {
                assert_eq!(files.len(), 3);
                assert!(files.contains(&"a.jv".to_string()));
                assert!(files.contains(&"b.jv".to_string()));
                assert!(files.contains(&"c.jv".to_string()));
            }
            _ => panic!("Expected Fmt command"),
        }
    }
}
