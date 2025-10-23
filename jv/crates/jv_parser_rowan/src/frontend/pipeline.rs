use super::diagnostics;
use crate::lowering::{self, LoweringDiagnosticSeverity, LoweringResult};
use crate::parser::{self, DiagnosticSeverity as ParserDiagnosticSeverity};
use crate::ParseBuilder;
use jv_ast::{Program, Span, Statement};
use jv_lexer::Lexer;
use jv_parser_frontend::{ParseError, ParserPipeline, PipelineArtifacts};
use jv_parser_preprocess::PreprocessDiagnostic;
use jv_parser_semantics;
use rowan::SyntaxNode;

/// Rowan ベースのパイプライン実装。
pub struct RowanPipeline;

impl Default for RowanPipeline {
    fn default() -> Self {
        Self
    }
}

impl RowanPipeline {
    /// 新しいパイプラインインスタンスを生成する。
    pub fn new() -> Self {
        Self
    }

    fn assemble_program(&self, statements: Vec<Statement>) -> Program {
        let package = statements.iter().find_map(|statement| {
            if let Statement::Package { name, .. } = statement {
                Some(name.clone())
            } else {
                None
            }
        });

        let imports = statements
            .iter()
            .filter_map(|statement| match statement {
                Statement::Import { .. } => Some(statement.clone()),
                _ => None,
            })
            .collect();

        let span = statements
            .first()
            .and_then(|first| {
                statements
                    .last()
                    .map(|last| merge_statement_spans(first.span(), last.span()))
            })
            .unwrap_or_else(Span::dummy);

        Program {
            package,
            imports,
            statements,
            span,
        }
    }
}

impl ParserPipeline for RowanPipeline {
    fn execute(&self, source: &str) -> Result<PipelineArtifacts, ParseError> {
        let tokens = Lexer::new(source.to_string()).tokenize()?;

        let preprocess_result = jv_parser_preprocess::run(tokens);
        let (tokens, preprocess_diagnostics, preprocess_halted_stage) =
            preprocess_result.into_parts();

        if let Some(stage_name) = preprocess_halted_stage {
            return Err(preprocess_halt_error(stage_name, &preprocess_diagnostics));
        }

        let parse_output = parser::parse(&tokens);

        if let Some(diagnostic) = parse_output
            .diagnostics
            .iter()
            .find(|diagnostic| matches!(diagnostic.severity, ParserDiagnosticSeverity::Error))
        {
            let span = diagnostics::token_span_to_span(diagnostic.span, &tokens)
                .unwrap_or_else(Span::dummy);
            let message = format!(
                "[{}] {}",
                diagnostics::ROWAN_PARSER_STAGE,
                diagnostic.message
            );
            return Err(ParseError::Syntax { message, span });
        }

        let green_tree = ParseBuilder::build_from_events(&parse_output.events, &tokens);
        let syntax_node = SyntaxNode::<crate::JvLanguage>::new_root(green_tree);
        let lowering_result = lowering::lower_program(&syntax_node, &tokens);

        if let Some(diagnostic) = lowering_result
            .diagnostics
            .iter()
            .find(|diagnostic| matches!(diagnostic.severity, LoweringDiagnosticSeverity::Error))
        {
            let span = diagnostic.span.clone().unwrap_or_else(Span::dummy);
            let message = format!(
                "[{}] {}",
                diagnostics::ROWAN_LOWERING_STAGE,
                diagnostic.message.clone()
            );
            return Err(ParseError::Syntax { message, span });
        }

        let LoweringResult {
            statements,
            diagnostics: lowering_diagnostics,
        } = lowering_result;

        let program = self.assemble_program(statements);
        let semantics_result = jv_parser_semantics::run(&tokens, program);

        if let Some(stage_name) = semantics_result.halted_stage {
            let message = semantics_result
                .staged_diagnostics
                .first()
                .map(|diagnostic| format!("[{}] {}", stage_name, diagnostic.message()))
                .unwrap_or_else(|| format!("Stage 2 semantics halted at {}", stage_name));
            let span = semantics_result
                .staged_diagnostics
                .first()
                .and_then(|diagnostic| diagnostic.span().cloned())
                .unwrap_or_else(Span::dummy);
            return Err(ParseError::Syntax { message, span });
        }

        let jv_parser_semantics::SemanticsResult {
            program,
            staged_diagnostics: semantics_diagnostics,
            halted_stage: semantics_halted_stage,
        } = semantics_result;

        let frontend_diagnostics = diagnostics::compose_frontend_diagnostics(
            &tokens,
            &parse_output.diagnostics,
            &lowering_diagnostics,
            preprocess_diagnostics,
            preprocess_halted_stage,
            semantics_diagnostics,
            semantics_halted_stage,
        );

        Ok(PipelineArtifacts::new(
            program,
            tokens,
            frontend_diagnostics,
        ))
    }
}

fn preprocess_halt_error(
    stage_name: &'static str,
    diagnostics: &[PreprocessDiagnostic],
) -> ParseError {
    let message = diagnostics
        .first()
        .map(|diagnostic| format!("[{}] {}", stage_name, diagnostic.message()))
        .unwrap_or_else(|| format!("Stage 0 preprocessing halted at {}", stage_name));
    let span = diagnostics
        .first()
        .and_then(|diagnostic| diagnostic.span().cloned())
        .unwrap_or_else(Span::dummy);

    ParseError::Syntax { message, span }
}

fn merge_statement_spans(first: &Span, last: &Span) -> Span {
    Span::new(
        first.start_line,
        first.start_column,
        last.end_line,
        last.end_column,
    )
}
