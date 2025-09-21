use anyhow::Result;

pub mod cli;
pub mod environment;
pub mod portfolio;
pub mod progress;
pub mod projects;
pub mod sections;

pub use cli::{MenuAction, MenuEntry, SectionId, TourCli};
pub use environment::{
    BuildSystemProbe, EnvironmentManager, EnvironmentReport, JdkProbe, JdkStatus, OperatingSystem,
    SetupGuide,
};
pub use portfolio::{PortfolioArtifact, PortfolioConfig, PortfolioGenerator, PortfolioMetadata};
pub use progress::{ProgressSummary, ProgressTracker, SectionDisplay, SectionStatus};
pub use projects::{Feature, JarArtifact, MiniProject, Project, ProjectStep, ProjectType};

/// Coordinates the language tour experience with sensible defaults.
#[derive(Debug, Clone)]
pub struct TourOrchestrator {
    cli: TourCli,
}

impl TourOrchestrator {
    /// Create an orchestrator backed by the default CLI implementation.
    pub fn new() -> Self {
        Self { cli: TourCli::new() }
    }

    /// Inject a pre-configured CLI facade (useful for testing scenarios).
    pub fn with_cli(cli: TourCli) -> Self {
        Self { cli }
    }

    /// Run the interactive tour end-to-end.
    pub fn run(&self) -> Result<()> {
        self.cli.run()
    }
}

impl Default for TourOrchestrator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience helper to launch the tour without constructing an orchestrator manually.
pub fn run_tour() -> Result<()> {
    TourOrchestrator::default().run()
}
