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

use crate::types::{IrProgram, Span};
use jv_ast::Program;
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
mod tests {
    use super::*;
    use crate::types::{
        IrExpression, IrModifiers, IrProgram, IrStatement, IrVisibility, JavaType, Span,
    };
    use jv_ast::{Expression as AstExpression, Literal, Statement as AstStatement};

    fn span() -> Span {
        Span::new(1, 0, 1, 1)
    }

    #[test]
    fn reconstructs_basic_program_with_variable_and_expression() {
        let program = IrProgram {
            package: Some("com.example".to_string()),
            imports: vec![IrStatement::Import {
                path: "java.util.List".to_string(),
                is_static: false,
                is_wildcard: false,
                span: span(),
            }],
            type_declarations: vec![
                IrStatement::VariableDeclaration {
                    name: "answer".to_string(),
                    java_type: JavaType::Primitive("int".into()),
                    initializer: Some(IrExpression::Literal(Literal::Number("42".into()), span())),
                    is_final: true,
                    modifiers: IrModifiers {
                        visibility: IrVisibility::Public,
                        ..IrModifiers::default()
                    },
                    span: span(),
                },
                IrStatement::Expression {
                    expr: IrExpression::Identifier {
                        name: "answer".into(),
                        java_type: JavaType::Primitive("int".into()),
                        span: span(),
                    },
                    span: span(),
                },
            ],
            span: span(),
        };

        let rebuilder = IrAstRebuilder::default();
        let result = rebuilder
            .reconstruct_program(&program)
            .expect("should rebuild");

        assert_eq!(result.program.package.as_deref(), Some("com.example"));
        assert_eq!(result.program.imports.len(), 1);
        assert_eq!(result.program.statements.len(), 2);

        match &result.program.statements[0] {
            AstStatement::ValDeclaration {
                name, initializer, ..
            } => {
                assert_eq!(name, "answer");
                match initializer {
                    AstExpression::Literal(Literal::Number(value), _) => assert_eq!(value, "42"),
                    other => panic!("unexpected initializer: {other:?}"),
                }
            }
            other => panic!("expected val declaration, found {other:?}"),
        }

        assert!(result.warnings.is_empty(), "warnings were not expected");
        assert_eq!(
            result.stats.reconstructed_nodes,
            result.stats.total_nodes - result.stats.placeholder_nodes
        );
    }

    #[test]
    fn errors_when_placeholders_disallowed() {
        let program = IrProgram {
            package: None,
            imports: vec![],
            type_declarations: vec![IrStatement::VariableDeclaration {
                name: "missing".to_string(),
                java_type: JavaType::Primitive("int".into()),
                initializer: None,
                is_final: true,
                modifiers: IrModifiers::default(),
                span: span(),
            }],
            span: span(),
        };

        let rebuilder = IrAstRebuilder::new(ReconstructionOptions {
            allow_placeholders: false,
            ..ReconstructionOptions::default()
        });

        let err = rebuilder
            .reconstruct_program(&program)
            .expect_err("placeholders should be rejected");

        assert!(matches!(err, ReconstructionError::InsufficientMetadata));
    }

    #[test]
    fn produces_warning_for_unsupported_statement() {
        let program = IrProgram {
            package: None,
            imports: vec![],
            type_declarations: vec![IrStatement::While {
                condition: IrExpression::Literal(Literal::Boolean(true), span()),
                body: Box::new(IrStatement::Expression {
                    expr: IrExpression::Literal(Literal::Number("1".into()), span()),
                    span: span(),
                }),
                span: span(),
            }],
            span: span(),
        };

        let rebuilder = IrAstRebuilder::default();
        let result = rebuilder
            .reconstruct_program(&program)
            .expect("should rebuild with placeholder");

        assert_eq!(result.warnings.len(), 1);
        assert_eq!(result.stats.placeholder_nodes, 1);
        assert!(matches!(
            result.program.statements[0],
            AstStatement::Expression { .. }
        ));
        assert_eq!(result.warnings[0].kind, WarningKind::UnsupportedNode);
    }
}
