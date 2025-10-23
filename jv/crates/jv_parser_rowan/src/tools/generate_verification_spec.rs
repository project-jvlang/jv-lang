use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use jv_ast::Span;
use jv_lexer::Lexer;
use jv_parser_rowan::lowering::lower_program;
use jv_parser_rowan::parse;
use jv_parser_rowan::verification::{
    BlockExpectation, DiagnosticsExpectation, ExpectationSpec, SpanExpectation,
    StatementExpectation, StatementSummary,
};
use jv_parser_rowan::{JvLanguage, ParseBuilder};
use rowan::SyntaxNode;
use serde::Serialize;

#[derive(Serialize)]
struct FixtureFile {
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
    source: String,
    expect: ExpectationSpec,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = env::args().skip(1);
    let mut source: Option<PathBuf> = None;
    let mut output: Option<PathBuf> = None;
    let mut description: Option<String> = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--source" => {
                let value = args.next().ok_or("`--source` requires a path argument")?;
                source = Some(PathBuf::from(value));
            }
            "--output" => {
                let value = args.next().ok_or("`--output` requires a path argument")?;
                output = Some(PathBuf::from(value));
            }
            "--description" => {
                let value = args
                    .next()
                    .ok_or("`--description` requires a string argument")?;
                description = Some(value);
            }
            "--help" | "-h" => {
                eprintln!(
                    "Usage: rowan_spec_generate --source <path> [--output <path>] [--description <text>]"
                );
                return Ok(());
            }
            other => {
                return Err(format!("unknown argument `{other}`").into());
            }
        }
    }

    let source = source.ok_or("`--source` is required")?;

    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest
        .ancestors()
        .nth(3)
        .ok_or("failed to resolve workspace root")?;

    let resolved_source = if source.is_absolute() {
        source
    } else {
        workspace_root.join(&source)
    };

    let source_content = fs::read_to_string(&resolved_source)?;
    let tokens = Lexer::new(source_content)
        .tokenize()
        .map_err(|error| format!("lexing failed for {}: {error:?}", resolved_source.display()))?;
    let parse_output = parse(&tokens);
    let green = ParseBuilder::build_from_events(&parse_output.events, &tokens);
    let syntax: SyntaxNode<JvLanguage> = SyntaxNode::new_root(green);
    let lowering = lower_program(&syntax, &tokens);

    let mut expect = ExpectationSpec::default();
    expect.statement_count = Some(lowering.statements.len());
    expect.parser_diagnostics = diagnostics_for_parser(parse_output.diagnostics.len());

    let lowering_error_count = lowering
        .diagnostics
        .iter()
        .filter(|diag| {
            diag.severity == jv_parser_rowan::lowering::LoweringDiagnosticSeverity::Error
        })
        .count();
    expect.lowering_diagnostics = diagnostics_for_lowering(lowering_error_count);

    let mut statements = Vec::new();
    for (index, statement) in lowering.statements.iter().enumerate() {
        let summary = StatementSummary::from_statement(statement);
        statements.push(StatementExpectation {
            index,
            kind: summary.kind,
            name: summary.name.clone(),
            span: summary.span.as_ref().map(|span| span_expectation(span)),
            block: block_expectation(&summary),
        });
    }
    expect.statements = statements;

    let rel_source = relative_path(&resolved_source, workspace_root);
    let fixture = FixtureFile {
        description: description
            .or_else(|| Some(format!("Auto-generated expectation for {}", rel_source))),
        source: rel_source,
        expect,
    };

    let toml = toml::to_string_pretty(&fixture)?;

    if let Some(path) = output {
        let resolved_output = if path.is_absolute() {
            path
        } else {
            workspace_root.join(path)
        };
        if let Some(parent) = resolved_output.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&resolved_output, toml)?;
        println!("wrote {}", relative_path(&resolved_output, workspace_root));
    } else {
        println!("{toml}");
    }

    Ok(())
}

fn diagnostics_for_parser(count: usize) -> DiagnosticsExpectation {
    if count == 0 {
        DiagnosticsExpectation::default()
    } else {
        DiagnosticsExpectation {
            allowed: true,
            max: Some(count),
        }
    }
}

fn diagnostics_for_lowering(error_count: usize) -> DiagnosticsExpectation {
    if error_count == 0 {
        DiagnosticsExpectation::default()
    } else {
        DiagnosticsExpectation {
            allowed: true,
            max: Some(error_count),
        }
    }
}

fn span_expectation(span: &Span) -> SpanExpectation {
    SpanExpectation {
        start_line: Some(span.start_line),
        start_column: Some(span.start_column),
        end_line: Some(span.end_line),
        end_column: Some(span.end_column),
    }
}

fn block_expectation(summary: &StatementSummary) -> Option<BlockExpectation> {
    if summary.block_statement_kinds.is_empty() {
        None
    } else {
        Some(BlockExpectation {
            statement_kinds: summary.block_statement_kinds.clone(),
        })
    }
}

fn relative_path(path: &Path, workspace_root: &Path) -> String {
    path.strip_prefix(workspace_root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}
