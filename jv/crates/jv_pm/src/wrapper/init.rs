//! Init command implementation for Maven wrapper mode.

use std::fs;
use std::path::PathBuf;

use dialoguer::{Input, Select};

use super::error::WrapperError;
use crate::cli::InitArgs;

/// Packaging type for Maven projects.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Packaging {
    #[default]
    Jar,
    War,
    Pom,
}

impl Packaging {
    /// Returns the Maven packaging value string.
    pub fn as_str(&self) -> &'static str {
        match self {
            Packaging::Jar => "jar",
            Packaging::War => "war",
            Packaging::Pom => "pom",
        }
    }

    /// Parse from string (case-insensitive).
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "jar" => Some(Packaging::Jar),
            "war" => Some(Packaging::War),
            "pom" => Some(Packaging::Pom),
            _ => None,
        }
    }

    /// Returns all packaging options for display.
    pub fn all() -> &'static [Packaging] {
        &[Packaging::Jar, Packaging::War, Packaging::Pom]
    }
}

impl std::fmt::Display for Packaging {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Java version for Maven projects.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum JavaVersion {
    Java17,
    Java21,
    #[default]
    Java25,
}

impl JavaVersion {
    /// Returns the Java version number string.
    pub fn as_str(&self) -> &'static str {
        match self {
            JavaVersion::Java17 => "17",
            JavaVersion::Java21 => "21",
            JavaVersion::Java25 => "25",
        }
    }

    /// Parse from string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "17" => Some(JavaVersion::Java17),
            "21" => Some(JavaVersion::Java21),
            "25" => Some(JavaVersion::Java25),
            _ => None,
        }
    }

    /// Returns all Java version options for display.
    pub fn all() -> &'static [JavaVersion] {
        &[
            JavaVersion::Java17,
            JavaVersion::Java21,
            JavaVersion::Java25,
        ]
    }
}

impl std::fmt::Display for JavaVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Java {}", self.as_str())
    }
}

/// Configuration for init command.
#[derive(Debug, Clone)]
pub struct InitConfig {
    pub group_id: String,
    pub artifact_id: String,
    pub version: String,
    pub packaging: Packaging,
    pub java_version: JavaVersion,
    pub target_dir: PathBuf,
}

impl InitConfig {
    /// Default group ID.
    const DEFAULT_GROUP_ID: &'static str = "com.example";
    /// Default version.
    const DEFAULT_VERSION: &'static str = "0.1.0-SNAPSHOT";

    /// Create InitConfig from CLI args in non-interactive mode.
    /// Uses defaults for any unspecified values.
    pub fn from_args_non_interactive(args: &InitArgs) -> anyhow::Result<Self> {
        let target_dir = args
            .directory
            .clone()
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        let artifact_id = args.artifact_id.clone().unwrap_or_else(|| {
            target_dir
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("my-app")
                .to_string()
        });

        let packaging = args
            .packaging
            .as_ref()
            .and_then(|s| Packaging::from_str(s))
            .unwrap_or_default();

        let java_version = args
            .java_version
            .as_ref()
            .and_then(|s| JavaVersion::from_str(s))
            .unwrap_or_default();

        Ok(InitConfig {
            group_id: args
                .group_id
                .clone()
                .unwrap_or_else(|| Self::DEFAULT_GROUP_ID.to_string()),
            artifact_id,
            version: args
                .version
                .clone()
                .unwrap_or_else(|| Self::DEFAULT_VERSION.to_string()),
            packaging,
            java_version,
            target_dir,
        })
    }

    /// Create InitConfig from CLI args in interactive mode.
    /// Prompts for any unspecified values.
    pub fn from_args_interactive(args: &InitArgs) -> anyhow::Result<Self> {
        let target_dir = args
            .directory
            .clone()
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        let default_artifact_id = target_dir
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("my-app")
            .to_string();

        // Group ID
        let group_id = if let Some(ref gid) = args.group_id {
            gid.clone()
        } else {
            Input::new()
                .with_prompt("groupId")
                .default(Self::DEFAULT_GROUP_ID.to_string())
                .interact_text()?
        };

        // Artifact ID
        let artifact_id = if let Some(ref aid) = args.artifact_id {
            aid.clone()
        } else {
            Input::new()
                .with_prompt("artifactId")
                .default(default_artifact_id)
                .interact_text()?
        };

        // Version
        let version = if let Some(ref v) = args.version {
            v.clone()
        } else {
            Input::new()
                .with_prompt("version")
                .default(Self::DEFAULT_VERSION.to_string())
                .interact_text()?
        };

        // Packaging
        let packaging = if let Some(ref p) = args.packaging {
            Packaging::from_str(p).unwrap_or_default()
        } else {
            let items: Vec<&str> = Packaging::all().iter().map(|p| p.as_str()).collect();
            let selection = Select::new()
                .with_prompt("packaging")
                .items(&items)
                .default(0)
                .interact()?;
            Packaging::all()[selection]
        };

        // Java Version
        let java_version = if let Some(ref jv) = args.java_version {
            JavaVersion::from_str(jv).unwrap_or_default()
        } else {
            let items: Vec<&str> = JavaVersion::all().iter().map(|v| v.as_str()).collect();
            let selection = Select::new()
                .with_prompt("Java version")
                .items(&items)
                .default(2) // Java 25 is default
                .interact()?;
            JavaVersion::all()[selection]
        };

        Ok(InitConfig {
            group_id,
            artifact_id,
            version,
            packaging,
            java_version,
            target_dir,
        })
    }

    /// Determine whether to use interactive mode based on args and terminal state.
    pub fn from_args(args: &InitArgs) -> anyhow::Result<Self> {
        // Non-interactive if explicitly requested or if stdin is not a terminal
        if args.non_interactive || !is_terminal::is_terminal(std::io::stdin()) {
            Self::from_args_non_interactive(args)
        } else {
            Self::from_args_interactive(args)
        }
    }
}

/// Summary of initialized project.
#[derive(Debug, Clone)]
pub struct InitSummary {
    pub pom_path: PathBuf,
    pub created_dirs: Vec<PathBuf>,
    pub created_files: Vec<PathBuf>,
    pub artifact_id: String,
}

/// Project initializer that validates context and generates project files.
pub struct ProjectInitializer {
    config: InitConfig,
}

impl ProjectInitializer {
    /// Create a new ProjectInitializer with the given config.
    pub fn new(config: InitConfig) -> Self {
        Self { config }
    }

    /// Validate the init context.
    /// Returns error if:
    /// - pom.xml already exists
    /// - jv.toml exists (native project)
    /// - target directory exists and is not empty
    pub fn validate(&self) -> Result<(), WrapperError> {
        let target = &self.config.target_dir;

        // Check for jv.toml (native project)
        let jv_toml = target.join("jv.toml");
        if jv_toml.exists() {
            return Err(WrapperError::NativeProjectDetected);
        }

        // Check for existing pom.xml
        let pom_path = target.join("pom.xml");
        if pom_path.exists() {
            return Err(WrapperError::ProjectAlreadyExists);
        }

        // If directory exists, check if it's empty
        if target.exists() {
            let entries: Vec<_> = fs::read_dir(target)
                .map_err(|_| WrapperError::PermissionDenied(target.clone()))?
                .filter_map(|e| e.ok())
                .collect();
            if !entries.is_empty() {
                return Err(WrapperError::DestinationNotEmpty(target.clone()));
            }
        }

        Ok(())
    }

    /// Generate project files based on config.
    pub fn generate(&self) -> Result<InitSummary, WrapperError> {
        let target = &self.config.target_dir;
        let mut created_dirs = Vec::new();
        let mut created_files = Vec::new();

        // Create target directory if it doesn't exist
        if !target.exists() {
            fs::create_dir_all(target)
                .map_err(|_| WrapperError::PermissionDenied(target.clone()))?;
            created_dirs.push(target.clone());
        }

        // Generate pom.xml
        let pom_path = target.join("pom.xml");
        let pom_content = self.generate_pom_xml();
        fs::write(&pom_path, pom_content)
            .map_err(|_| WrapperError::PermissionDenied(pom_path.clone()))?;
        created_files.push(pom_path.clone());

        // Generate .gitignore
        let gitignore_path = target.join(".gitignore");
        let gitignore_content = self.generate_gitignore();
        fs::write(&gitignore_path, gitignore_content)
            .map_err(|_| WrapperError::PermissionDenied(gitignore_path.clone()))?;
        created_files.push(gitignore_path);

        // Create directories based on packaging type
        let dirs_to_create = match self.config.packaging {
            Packaging::Jar => vec![
                "src/main/java",
                "src/main/resources",
                "src/test/java",
                "src/test/resources",
            ],
            Packaging::War => vec![
                "src/main/java",
                "src/main/resources",
                "src/main/webapp",
                "src/main/webapp/WEB-INF",
                "src/test/java",
                "src/test/resources",
            ],
            Packaging::Pom => vec![], // No source directories for pom packaging
        };

        for dir in dirs_to_create {
            let dir_path = target.join(dir);
            fs::create_dir_all(&dir_path)
                .map_err(|_| WrapperError::PermissionDenied(dir_path.clone()))?;
            created_dirs.push(dir_path);
        }

        Ok(InitSummary {
            pom_path,
            created_dirs,
            created_files,
            artifact_id: self.config.artifact_id.clone(),
        })
    }

    /// Generate pom.xml content.
    fn generate_pom_xml(&self) -> String {
        let mut pom = format!(
            r#"<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>{}</groupId>
    <artifactId>{}</artifactId>
    <version>{}</version>
    <packaging>{}</packaging>

    <name>{}</name>

    <properties>
        <maven.compiler.source>{}</maven.compiler.source>
        <maven.compiler.target>{}</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>
"#,
            self.config.group_id,
            self.config.artifact_id,
            self.config.version,
            self.config.packaging.as_str(),
            self.config.artifact_id,
            self.config.java_version.as_str(),
            self.config.java_version.as_str(),
        );

        // Add empty modules section for pom packaging
        if self.config.packaging == Packaging::Pom {
            pom.push_str(
                r#"
    <modules>
    </modules>
"#,
            );
        }

        pom.push_str("</project>\n");
        pom
    }

    /// Generate .gitignore content.
    fn generate_gitignore(&self) -> String {
        r#"# Compiled class files
*.class

# Log files
*.log

# Package files
*.jar
*.war
*.nar
*.ear
*.zip
*.tar.gz
*.rar

# Maven
target/
pom.xml.tag
pom.xml.releaseBackup
pom.xml.versionsBackup
pom.xml.next
release.properties
dependency-reduced-pom.xml
buildNumber.properties
.mvn/timing.properties
.mvn/wrapper/maven-wrapper.jar

# IDE
.idea/
*.iml
*.ipr
*.iws
.project
.classpath
.settings/
.vscode/
*.swp
*.swo
*~

# jv/jvpm
jv.lock
"#
        .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_packaging_from_str() {
        assert_eq!(Packaging::from_str("jar"), Some(Packaging::Jar));
        assert_eq!(Packaging::from_str("JAR"), Some(Packaging::Jar));
        assert_eq!(Packaging::from_str("war"), Some(Packaging::War));
        assert_eq!(Packaging::from_str("pom"), Some(Packaging::Pom));
        assert_eq!(Packaging::from_str("invalid"), None);
    }

    #[test]
    fn test_packaging_as_str() {
        assert_eq!(Packaging::Jar.as_str(), "jar");
        assert_eq!(Packaging::War.as_str(), "war");
        assert_eq!(Packaging::Pom.as_str(), "pom");
    }

    #[test]
    fn test_java_version_from_str() {
        assert_eq!(JavaVersion::from_str("17"), Some(JavaVersion::Java17));
        assert_eq!(JavaVersion::from_str("21"), Some(JavaVersion::Java21));
        assert_eq!(JavaVersion::from_str("25"), Some(JavaVersion::Java25));
        assert_eq!(JavaVersion::from_str("11"), None);
    }

    #[test]
    fn test_java_version_as_str() {
        assert_eq!(JavaVersion::Java17.as_str(), "17");
        assert_eq!(JavaVersion::Java21.as_str(), "21");
        assert_eq!(JavaVersion::Java25.as_str(), "25");
    }

    #[test]
    fn test_init_config_non_interactive_defaults() {
        let args = InitArgs {
            group_id: None,
            artifact_id: None,
            version: None,
            packaging: None,
            java_version: None,
            non_interactive: true,
            directory: Some(PathBuf::from("/tmp/my-project")),
        };

        let config = InitConfig::from_args_non_interactive(&args).unwrap();
        assert_eq!(config.group_id, "com.example");
        assert_eq!(config.artifact_id, "my-project");
        assert_eq!(config.version, "0.1.0-SNAPSHOT");
        assert_eq!(config.packaging, Packaging::Jar);
        assert_eq!(config.java_version, JavaVersion::Java25);
    }

    #[test]
    fn test_init_config_non_interactive_with_args() {
        let args = InitArgs {
            group_id: Some("org.test".to_string()),
            artifact_id: Some("test-app".to_string()),
            version: Some("1.0.0".to_string()),
            packaging: Some("war".to_string()),
            java_version: Some("21".to_string()),
            non_interactive: true,
            directory: None,
        };

        let config = InitConfig::from_args_non_interactive(&args).unwrap();
        assert_eq!(config.group_id, "org.test");
        assert_eq!(config.artifact_id, "test-app");
        assert_eq!(config.version, "1.0.0");
        assert_eq!(config.packaging, Packaging::War);
        assert_eq!(config.java_version, JavaVersion::Java21);
    }

    #[test]
    fn test_init_config_partial_args() {
        let args = InitArgs {
            group_id: Some("com.custom".to_string()),
            artifact_id: None,
            version: None,
            packaging: Some("pom".to_string()),
            java_version: None,
            non_interactive: true,
            directory: Some(PathBuf::from("/tmp/test-project")),
        };

        let config = InitConfig::from_args_non_interactive(&args).unwrap();
        assert_eq!(config.group_id, "com.custom");
        assert_eq!(config.artifact_id, "test-project"); // derived from directory
        assert_eq!(config.version, "0.1.0-SNAPSHOT"); // default
        assert_eq!(config.packaging, Packaging::Pom);
        assert_eq!(config.java_version, JavaVersion::Java25); // default
    }

    // ProjectInitializer::validate tests

    #[test]
    fn test_validate_empty_directory_succeeds() {
        let temp = tempdir().unwrap();
        let target = temp.path().join("new-project");

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "test".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::Jar,
            java_version: JavaVersion::Java25,
            target_dir: target,
        };

        let initializer = ProjectInitializer::new(config);
        assert!(initializer.validate().is_ok());
    }

    #[test]
    fn test_validate_existing_pom_fails() {
        let temp = tempdir().unwrap();
        let target = temp.path().to_path_buf();

        // Create existing pom.xml
        fs::write(target.join("pom.xml"), "<project/>").unwrap();

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "test".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::Jar,
            java_version: JavaVersion::Java25,
            target_dir: target,
        };

        let initializer = ProjectInitializer::new(config);
        let result = initializer.validate();
        assert!(matches!(result, Err(WrapperError::ProjectAlreadyExists)));
    }

    #[test]
    fn test_validate_existing_jv_toml_fails() {
        let temp = tempdir().unwrap();
        let target = temp.path().to_path_buf();

        // Create existing jv.toml (native project)
        fs::write(target.join("jv.toml"), "[package]").unwrap();

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "test".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::Jar,
            java_version: JavaVersion::Java25,
            target_dir: target,
        };

        let initializer = ProjectInitializer::new(config);
        let result = initializer.validate();
        assert!(matches!(result, Err(WrapperError::NativeProjectDetected)));
    }

    #[test]
    fn test_validate_non_empty_directory_fails() {
        let temp = tempdir().unwrap();
        let target = temp.path().to_path_buf();

        // Create some file (not pom.xml or jv.toml)
        fs::write(target.join("README.md"), "# Test").unwrap();

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "test".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::Jar,
            java_version: JavaVersion::Java25,
            target_dir: target.clone(),
        };

        let initializer = ProjectInitializer::new(config);
        let result = initializer.validate();
        assert!(matches!(result, Err(WrapperError::DestinationNotEmpty(p)) if p == target));
    }

    // ProjectInitializer::generate tests

    #[test]
    fn test_generate_jar_project() {
        let temp = tempdir().unwrap();
        let target = temp.path().join("jar-project");

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "my-jar-app".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::Jar,
            java_version: JavaVersion::Java21,
            target_dir: target.clone(),
        };

        let initializer = ProjectInitializer::new(config);
        let summary = initializer.generate().unwrap();

        // Verify pom.xml created
        assert!(summary.pom_path.exists());
        let pom_content = fs::read_to_string(&summary.pom_path).unwrap();
        assert!(pom_content.contains("<groupId>com.example</groupId>"));
        assert!(pom_content.contains("<artifactId>my-jar-app</artifactId>"));
        assert!(pom_content.contains("<version>1.0.0</version>"));
        assert!(pom_content.contains("<packaging>jar</packaging>"));
        assert!(pom_content.contains("<maven.compiler.source>21</maven.compiler.source>"));

        // Verify directories created
        assert!(target.join("src/main/java").exists());
        assert!(target.join("src/main/resources").exists());
        assert!(target.join("src/test/java").exists());
        assert!(target.join("src/test/resources").exists());

        // Verify .gitignore created
        assert!(target.join(".gitignore").exists());

        // Verify summary
        assert_eq!(summary.artifact_id, "my-jar-app");
    }

    #[test]
    fn test_generate_war_project() {
        let temp = tempdir().unwrap();
        let target = temp.path().join("war-project");

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "my-war-app".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::War,
            java_version: JavaVersion::Java17,
            target_dir: target.clone(),
        };

        let initializer = ProjectInitializer::new(config);
        let summary = initializer.generate().unwrap();

        // Verify pom.xml
        let pom_content = fs::read_to_string(&summary.pom_path).unwrap();
        assert!(pom_content.contains("<packaging>war</packaging>"));
        assert!(pom_content.contains("<maven.compiler.source>17</maven.compiler.source>"));

        // Verify war-specific directories
        assert!(target.join("src/main/java").exists());
        assert!(target.join("src/main/resources").exists());
        assert!(target.join("src/main/webapp").exists());
        assert!(target.join("src/main/webapp/WEB-INF").exists());
        assert!(target.join("src/test/java").exists());
        assert!(target.join("src/test/resources").exists());
    }

    #[test]
    fn test_generate_pom_project() {
        let temp = tempdir().unwrap();
        let target = temp.path().join("pom-project");

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "my-parent".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::Pom,
            java_version: JavaVersion::Java25,
            target_dir: target.clone(),
        };

        let initializer = ProjectInitializer::new(config);
        let summary = initializer.generate().unwrap();

        // Verify pom.xml with modules section
        let pom_content = fs::read_to_string(&summary.pom_path).unwrap();
        assert!(pom_content.contains("<packaging>pom</packaging>"));
        assert!(pom_content.contains("<modules>"));
        assert!(pom_content.contains("</modules>"));

        // Verify NO source directories for pom packaging
        assert!(!target.join("src/main/java").exists());
        assert!(!target.join("src/test/java").exists());
    }

    #[test]
    fn test_generate_gitignore_content() {
        let temp = tempdir().unwrap();
        let target = temp.path().join("gitignore-test");

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "test".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::Jar,
            java_version: JavaVersion::Java25,
            target_dir: target.clone(),
        };

        let initializer = ProjectInitializer::new(config);
        initializer.generate().unwrap();

        let gitignore = fs::read_to_string(target.join(".gitignore")).unwrap();
        assert!(gitignore.contains("*.class"));
        assert!(gitignore.contains("target/"));
        assert!(gitignore.contains(".idea/"));
        assert!(gitignore.contains("jv.lock"));
    }

    #[test]
    fn test_generate_creates_parent_directories() {
        let temp = tempdir().unwrap();
        let target = temp.path().join("deep/nested/project");

        let config = InitConfig {
            group_id: "com.example".to_string(),
            artifact_id: "nested-app".to_string(),
            version: "1.0.0".to_string(),
            packaging: Packaging::Jar,
            java_version: JavaVersion::Java25,
            target_dir: target.clone(),
        };

        let initializer = ProjectInitializer::new(config);
        let summary = initializer.generate().unwrap();

        // Verify nested directory created
        assert!(target.exists());
        assert!(summary.pom_path.exists());
    }
}
