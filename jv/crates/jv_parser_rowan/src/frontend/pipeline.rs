use super::diagnostics;
use crate::lowering::{self, LoweringDiagnostic, LoweringDiagnosticSeverity, LoweringResult};
use crate::parser::{self, DiagnosticSeverity as ParserDiagnosticSeverity, ParserDiagnostic};
use crate::ParseBuilder;
use jv_ast::{Program, Span, Statement};
use jv_lexer::Lexer;
use jv_parser_frontend::{ParseError, ParserPipeline, PipelineArtifacts};
use jv_parser_preprocess::PreprocessDiagnostic;
use jv_parser_semantics;
use rowan::SyntaxNode;

/// Rowan ベースのパイプライン実装。
pub struct RowanPipeline;

/// Rowanパイプラインのデバッグ用成果物。
pub struct RowanPipelineDebug {
    artifacts: PipelineArtifacts,
    parser_diagnostics: Vec<ParserDiagnostic>,
    parser_recovered: bool,
    lowering_diagnostics: Vec<LoweringDiagnostic>,
    lowered_statements: Vec<Statement>,
    pipeline_error: Option<ParseError>,
}

impl RowanPipelineDebug {
    /// パイプライン成果物への参照を取得する。
    pub fn artifacts(&self) -> &PipelineArtifacts {
        &self.artifacts
    }

    /// パイプライン成果物を消費して取り出す。
    pub fn into_artifacts(self) -> PipelineArtifacts {
        self.artifacts
    }

    /// パーサ診断の一覧を返す。
    pub fn parser_diagnostics(&self) -> &[ParserDiagnostic] {
        &self.parser_diagnostics
    }

    /// パーサがエラー回復を行ったかを返す。
    pub fn parser_recovered(&self) -> bool {
        self.parser_recovered
    }

    /// ローワリング診断の一覧を返す。
    pub fn lowering_diagnostics(&self) -> &[LoweringDiagnostic] {
        &self.lowering_diagnostics
    }

    /// ローワリング後のステートメント列を返す。
    pub fn statements(&self) -> &[Statement] {
        &self.lowered_statements
    }

    /// パイプラインがエラーを報告した場合はその内容を返す。
    pub fn pipeline_error(&self) -> Option<&ParseError> {
        self.pipeline_error.as_ref()
    }
}

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
        let debug = self.execute_with_debug(source)?;
        match debug.pipeline_error {
            Some(error) => Err(error),
            None => Ok(debug.into_artifacts()),
        }
    }
}

impl RowanPipeline {
    /// デバッグ用の成果物を含めてパイプラインを実行する。
    pub fn execute_with_debug(&self, source: &str) -> Result<RowanPipelineDebug, ParseError> {
        let tokens = Lexer::new(source.to_string()).tokenize()?;

        let preprocess_result = jv_parser_preprocess::run(tokens);
        let (tokens, preprocess_diagnostics, preprocess_halted_stage) =
            preprocess_result.into_parts();

        if let Some(stage_name) = preprocess_halted_stage {
            return Err(preprocess_halt_error(stage_name, &preprocess_diagnostics));
        }

        let parse_output = parser::parse(&tokens);
        let parser_diagnostics = parse_output.diagnostics.clone();
        let parser_recovered = parse_output.recovered;
        let parser_error = parse_output
            .diagnostics
            .iter()
            .find(|diagnostic| matches!(diagnostic.severity, ParserDiagnosticSeverity::Error))
            .map(|diagnostic| {
                let span = diagnostics::token_span_to_span(diagnostic.span, &tokens)
                    .unwrap_or_else(Span::dummy);
                let message = format!(
                    "[{}] {}",
                    diagnostics::ROWAN_PARSER_STAGE,
                    diagnostic.message.clone()
                );
                ParseError::Syntax { message, span }
            });

        let green_tree = ParseBuilder::build_from_events(&parse_output.events, &tokens);
        let syntax_node = SyntaxNode::<crate::JvLanguage>::new_root(green_tree);
        let lowering_result = lowering::lower_program(&syntax_node, &tokens);
        let lowering_diagnostics = lowering_result.diagnostics.clone();
        let lowering_error = lowering_result
            .diagnostics
            .iter()
            .find(|diagnostic| matches!(diagnostic.severity, LoweringDiagnosticSeverity::Error))
            .map(|diagnostic| {
                let span = diagnostic.span.clone().unwrap_or_else(Span::dummy);
                let message = format!(
                    "[{}] {}",
                    diagnostics::ROWAN_LOWERING_STAGE,
                    diagnostic.message.clone()
                );
                ParseError::Syntax { message, span }
            });

        let LoweringResult {
            statements,
            diagnostics: lowering_diagnostics_current,
        } = lowering_result;

        let lowered_statements = statements.clone();
        let program = self.assemble_program(statements);

        let (program, semantics_diagnostics, semantics_halted_stage, semantics_error) =
            if parser_error.is_none() && lowering_error.is_none() {
                let semantics_result = jv_parser_semantics::run(&tokens, program);
                let error = if let Some(stage_name) = semantics_result.halted_stage {
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
                    Some(ParseError::Syntax { message, span })
                } else {
                    None
                };
                (
                    semantics_result.program,
                    semantics_result.staged_diagnostics,
                    semantics_result.halted_stage,
                    error,
                )
            } else {
                (program, Vec::new(), None, None)
            };

        let pipeline_error = parser_error.or(lowering_error).or(semantics_error);

        let frontend_diagnostics = diagnostics::compose_frontend_diagnostics(
            &tokens,
            &parse_output.diagnostics,
            &lowering_diagnostics_current,
            preprocess_diagnostics,
            preprocess_halted_stage,
            semantics_diagnostics,
            semantics_halted_stage,
        );

        let artifacts = PipelineArtifacts::new(program, tokens, frontend_diagnostics);

        Ok(RowanPipelineDebug {
            artifacts,
            parser_diagnostics,
            parser_recovered,
            lowering_diagnostics,
            lowered_statements,
            pipeline_error,
        })
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
