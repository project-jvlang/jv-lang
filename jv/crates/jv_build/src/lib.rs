// jv_build - Build system and javac integration
mod config;

pub use config::{
    BuildConfig, CliRequirement, NetworkPolicy, SampleCliDependencies, SampleConfig,
    SampleConfigError, SampleDependency, SampleProtocol,
};

use anyhow::Result;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("Build configuration error: {0}")]
    ConfigError(String),
    #[error("Javac compilation error: {0}")]
    JavacError(String),
    #[error("JDK not found: {0}")]
    JdkNotFound(String),
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("Failed to spawn CLI '{command}': {source}")]
    CliSpawnError {
        command: String,
        #[source]
        source: std::io::Error,
    },
    #[error("CLI '{command}' execution failed (status={status:?}): {stderr}")]
    CliExecutionError {
        command: String,
        status: Option<i32>,
        stderr: String,
    },
}

pub struct BuildSystem {
    config: BuildConfig,
}

#[derive(Debug, Clone)]
pub struct CliCommandOutput {
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct ResolvedCli {
    pub command: String,
    pub path: PathBuf,
}

impl BuildSystem {
    pub fn new(config: BuildConfig) -> Self {
        Self { config }
    }

    /// Compile Java files using javac
    pub fn compile_java_files(&self, java_files: Vec<&Path>) -> Result<(), BuildError> {
        if java_files.is_empty() {
            return Ok(());
        }

        // Build javac command
        let mut cmd = Command::new("javac");

        // Add compiler options
        for option in &self.config.compiler_options {
            cmd.arg(option);
        }

        // Add output directory
        cmd.args(["-d", &self.config.output_dir]);

        // Add classpath if specified
        if !self.config.classpath.is_empty() {
            cmd.args(["-cp", &self.config.classpath.join(":")]);
        }

        // Add source files
        for file in java_files {
            cmd.arg(file);
        }

        // Execute command
        let output = cmd
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .map_err(BuildError::IoError)?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(BuildError::JavacError(stderr.to_string()));
        }

        Ok(())
    }

    /// Check if javac is available and get version
    pub fn check_javac_availability(&self) -> Result<String, BuildError> {
        let output = Command::new("javac")
            .arg("-version")
            .output()
            .map_err(|_| BuildError::JdkNotFound("javac command not found".to_string()))?;

        if output.status.success() {
            let version = String::from_utf8_lossy(&output.stderr);
            Ok(version.trim().to_string())
        } else {
            Err(BuildError::JdkNotFound("javac not available".to_string()))
        }
    }

    /// Create output directory if it doesn't exist
    pub fn ensure_output_dir(&self) -> Result<(), BuildError> {
        std::fs::create_dir_all(&self.config.output_dir)?;
        Ok(())
    }

    /// Clean output directory
    pub fn clean(&self) -> Result<(), BuildError> {
        if Path::new(&self.config.output_dir).exists() {
            std::fs::remove_dir_all(&self.config.output_dir)?;
        }
        Ok(())
    }

    /// Resolve the configured CLI for a given @Sample dependency.
    pub fn resolve_sample_dependency(
        &self,
        dependency: SampleDependency,
    ) -> Result<ResolvedCli, BuildError> {
        let requirement = self.config.sample.cli.dependency(dependency);
        let path = requirement
            .resolve()
            .map_err(|error| BuildError::ConfigError(error.to_string()))?;

        Ok(ResolvedCli {
            command: requirement.command.clone(),
            path,
        })
    }

    /// Ensure required CLIs are available for the given protocol.
    pub fn ensure_protocol_dependencies(&self, protocol: SampleProtocol) -> Result<(), BuildError> {
        match protocol {
            SampleProtocol::S3 => {
                let _ = self.resolve_sample_dependency(SampleDependency::AwsCli)?;
            }
            SampleProtocol::GitSsh => {
                let _ = self.resolve_sample_dependency(SampleDependency::GitCli)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Execute a CLI associated with the @Sample feature and capture its output.
    pub fn execute_sample_command(
        &self,
        dependency: SampleDependency,
        args: &[&str],
        current_dir: Option<&Path>,
    ) -> Result<CliCommandOutput, BuildError> {
        let resolved = self.resolve_sample_dependency(dependency)?;

        let mut command = Command::new(&resolved.path);
        command.args(args);

        if let Some(dir) = current_dir {
            command.current_dir(dir);
        }

        let output = command
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .map_err(|error| BuildError::CliSpawnError {
                command: resolved.command.clone(),
                source: error,
            })?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
            return Err(BuildError::CliExecutionError {
                command: resolved.command,
                status: output.status.code(),
                stderr,
            });
        }

        Ok(CliCommandOutput {
            stdout: output.stdout,
            stderr: output.stderr,
        })
    }
}

#[cfg(test)]
mod tests;
