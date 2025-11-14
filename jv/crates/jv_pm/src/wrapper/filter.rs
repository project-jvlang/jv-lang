use std::env;
use std::path::Path;

use crate::cli::Commands;

use super::error::WrapperError;

/// Represents which CLI entrypoint the user invoked.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CliMode {
    Native,
    Wrapper,
}

impl CliMode {
    /// Detects the mode by inspecting how the binary was launched.
    pub fn detect() -> Self {
        env::args_os()
            .next()
            .and_then(|path| {
                Path::new(&path)
                    .file_stem()
                    .map(|stem| stem.to_string_lossy().to_ascii_lowercase())
            })
            .as_deref()
            .and_then(|name| {
                if name == "jvpm" {
                    Some(Self::Wrapper)
                } else {
                    None
                }
            })
            .unwrap_or(Self::Native)
    }

    pub fn is_wrapper(self) -> bool {
        matches!(self, CliMode::Wrapper)
    }
}

/// Guard that prevents `jvpm` wrapper mode from executing JV-specific commands.
pub struct WrapperCommandFilter;

impl WrapperCommandFilter {
    pub fn validate(command: &Commands, mode: CliMode) -> Result<(), WrapperError> {
        if !mode.is_wrapper() {
            return Ok(());
        }

        match command {
            Commands::Resolver(_) | Commands::Repo(_) => {
                Err(WrapperError::OperationFailed(format!(
                    "JV専用コマンド `{}` はラッパーモードでは利用できません。`jv {}` を使用してください。",
                    command_name(command),
                    command_name(command)
                )))
            }
            _ => Ok(()),
        }
    }
}

fn command_name(command: &Commands) -> &'static str {
    match command {
        Commands::Add(_) => "add",
        Commands::Remove(_) => "remove",
        Commands::Resolver(_) => "resolver",
        Commands::Repo(_) => "repo",
        Commands::Maven(_) => "maven",
    }
}
