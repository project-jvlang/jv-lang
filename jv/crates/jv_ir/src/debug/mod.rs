//! Debug utilities for reconstructing AST structures from IR artifacts.
//!
//! This module is gated behind the `debug-ir` feature flag. Enable the feature
//! to access the reconstruction APIs that power the IR→AST debugging workflow.

use std::fmt;
use std::time::{Duration, Instant};

mod diagnostics;
mod emit;
pub mod loader;
mod reconstruct;

use crate::types::IrProgram;
use jv_ast::{Program, Span};
use thiserror::Error;

/// Configuration flags that control how IR artifacts should be reconstructed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReconstructionOptions {
    pub preserve_layout: bool,
    pub annotate_origins: bool,
    pub allow_placeholders: bool,
    pub warning_threshold: usize,
}

impl Default for ReconstructionOptions {
    fn default() -> Self {
        Self {
            preserve_layout: true,
            annotate_origins: true,
            allow_placeholders: true,
            warning_threshold: usize::MAX,
        }
    }
}

/// Summary statistics describing a reconstruction run.
#[derive(Debug, Clone, PartialEq)]
pub struct ReconstructionStats {
    pub total_nodes: usize,
    pub reconstructed_nodes: usize,
    pub placeholder_nodes: usize,
    pub elapsed: Duration,
}

impl Default for ReconstructionStats {
    fn default() -> Self {
        Self {
            total_nodes: 0,
            reconstructed_nodes: 0,
            placeholder_nodes: 0,
            elapsed: Duration::default(),
        }
    }
}

/// Enumerates the warning categories emitted while rebuilding the AST.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WarningKind {
    MissingMetadata,
    UnsupportedNode,
    PlaceholderInjected,
    ThresholdExceeded,
}

impl WarningKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            WarningKind::MissingMetadata => "missing-metadata",
            WarningKind::UnsupportedNode => "unsupported-node",
            WarningKind::PlaceholderInjected => "placeholder-injected",
            WarningKind::ThresholdExceeded => "threshold-exceeded",
        }
    }
}

impl fmt::Display for WarningKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Structured warning produced when reconstruction encounters unexpected input.
#[derive(Debug, Clone, PartialEq)]
pub struct ReconstructionWarning {
    pub span: Option<Span>,
    pub node_path: String,
    pub kind: WarningKind,
    pub message: String,
}

impl ReconstructionWarning {
    pub fn new(
        span: Option<Span>,
        node_path: impl Into<String>,
        kind: WarningKind,
        message: impl Into<String>,
    ) -> Self {
        Self {
            span,
            node_path: node_path.into(),
            kind,
            message: message.into(),
        }
    }
}

/// Aggregated output of a reconstruction pass.
#[derive(Debug, Clone, PartialEq)]
pub struct ReconstructedAst {
    pub program: Program,
    pub warnings: Vec<ReconstructionWarning>,
    pub stats: ReconstructionStats,
}

impl ReconstructedAst {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            warnings: Vec::new(),
            stats: ReconstructionStats::default(),
        }
    }
}

/// Result alias for reconstruction operations.
pub type ReconstructionResult<T = ReconstructedAst> = Result<T, ReconstructionError>;

pub use diagnostics::{DiagnosticsSummary, WarningSummary};
pub use emit::AstArtifactWriter;
pub use loader::{load_ir_program, IrArtifactSource};

/// Errors that can occur while attempting to rebuild an AST from IR data.
#[derive(Debug, Error)]
pub enum ReconstructionError {
    #[error("IR metadata was insufficient for reconstruction")]
    InsufficientMetadata,
    #[error("IR artifact format is unsupported: {0}")]
    UnsupportedFormat(String),
    #[error("IR→AST reconstruction is not yet implemented")]
    Unimplemented,
}

/// Central coordinator for rebuilding AST artifacts from in-memory IR programs.
#[derive(Debug, Clone)]
pub struct IrAstRebuilder {
    opts: ReconstructionOptions,
}

impl IrAstRebuilder {
    /// Create a new rebuilder with the provided options.
    pub fn new(opts: ReconstructionOptions) -> Self {
        Self { opts }
    }

    /// Access the reconstruction options in use by this rebuilder.
    pub fn options(&self) -> &ReconstructionOptions {
        &self.opts
    }

    /// Rebuild an entire program from IR into an AST representation.
    pub fn reconstruct_program(&self, program: &IrProgram) -> ReconstructionResult {
        let start = Instant::now();
        let reconstruct::ReconstructionOutput {
            program: ast_program,
            warnings,
            stats,
        } = reconstruct::reconstruct_program(&self.opts, program)?;

        let mut reconstructed = ReconstructedAst::new(ast_program);
        let mut stats = stats;
        stats.elapsed = start.elapsed();

        if warnings.len() > self.opts.warning_threshold {
            return Err(ReconstructionError::InsufficientMetadata);
        }

        reconstructed.warnings = warnings;
        reconstructed.stats = stats;

        Ok(reconstructed)
    }

    /// Rebuild a single module within an IR program.
    pub fn reconstruct_module(&self, module: &IrProgram) -> ReconstructionResult {
        self.reconstruct_program(module)
    }
}

impl Default for IrAstRebuilder {
    fn default() -> Self {
        Self::new(ReconstructionOptions::default())
    }
}

#[cfg(test)]
mod tests;
