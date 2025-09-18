// jv_cli - CLI functionality (library interface for testing)
use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::Path;

#[derive(Parser)]
#[command(name = "jv")]
#[command(about = "A Java Sugar Language compiler")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(clap::Subcommand)]
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

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
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Change to temp directory
        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp_path).unwrap();

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
        std::env::set_current_dir(original_dir).unwrap();
    }

    #[test]
    fn test_init_project_with_name() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let original_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(temp_path).unwrap();

        let project_name = "test-project";
        let result = init_project(project_name);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), project_name);

        let project_path = temp_path.join(project_name);
        assert!(project_path.exists());
        assert!(project_path.join("jv.toml").exists());
        assert!(project_path.join("src/main.jv").exists());

        std::env::set_current_dir(original_dir).unwrap();
    }

    #[test]
    fn test_validate_file_exists() {
        let temp_dir = TempDir::new().unwrap();
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
