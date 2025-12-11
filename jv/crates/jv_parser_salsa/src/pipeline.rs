//! Salsa パーサーパイプライン統合。

use crate::db::{Database, FileInput, PreprocessOutput, preprocess};
use crate::diagnostics::{self, SALSA_LOWERING_STAGE, SALSA_PARSER_STAGE};
use crate::lower::{LoweringDiagnosticSeverity, LoweringResult};
use crate::parser::cst::CstNode;
use crate::parser::{self, ParseEvent, ParseResult, ParserDiagnostic};
use crate::support::token_compat::{LineIndex, span_from_legacy_token, to_legacy_token_with_index};
use crate::support::trivia::TriviaMap;
use jv_ast::{Program, Span, Statement};
use jv_lexer::Token as LegacyToken;
use jv_parser_frontend::{ParseError, ParserPipeline, PipelineArtifacts};
use jv_parser_preprocess::PreprocessDiagnostic;
use std::sync::Arc;

/// パースオプション。CST/TriviaMap 生成の有無を切り替える。
#[derive(Debug, Clone, Copy)]
pub struct ParseOptions {
    pub generate_cst: bool,
    pub generate_trivia_map: bool,
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self {
            generate_cst: false,
            generate_trivia_map: false,
        }
    }
}

/// Salsa パイプラインの成果物（デバッグ情報付き）。
pub struct SalsaPipelineOutput {
    pub artifacts: PipelineArtifacts,
    pub cst: Option<CstNode>,
    pub trivia_map: Option<TriviaMap>,
    pub parser_diagnostics: Vec<ParserDiagnostic>,
    pub lowering_diagnostics: Vec<crate::lower::LoweringDiagnostic>,
    pub recovered: bool,
}

impl SalsaPipelineOutput {
    pub fn into_artifacts(self) -> PipelineArtifacts {
        self.artifacts
    }
}

/// Salsa ベースのパーサーパイプライン。
pub struct SalsaPipeline {
    db: Database,
}

impl Default for SalsaPipeline {
    fn default() -> Self {
        Self::new()
    }
}

impl SalsaPipeline {
    /// 新しいパイプラインを生成する。
    pub fn new() -> Self {
        Self {
            db: Database::new(),
        }
    }

    /// パーサーパイプラインを実行し、必要に応じて CST/TriviaMap を生成する。
    pub fn execute_with_options(
        &self,
        source: &str,
        options: ParseOptions,
    ) -> Result<SalsaPipelineOutput, ParseError> {
        let file = FileInput::new(
            &self.db,
            Arc::from("<memory>"),
            Arc::from(source.to_string()),
        );

        // 1. 前処理
        let preprocess_output = preprocess(&self.db, file);
        let preprocess_owned =
            Arc::try_unwrap(preprocess_output).unwrap_or_else(|arc| (*arc).clone());
        let PreprocessOutput {
            tokens: pre_tokens,
            diagnostics: preprocess_diagnostics,
            halted_stage: preprocess_halted_stage,
        } = preprocess_owned;

        if let Some(stage_name) = preprocess_halted_stage {
            return Err(preprocess_halt_error(stage_name, &preprocess_diagnostics));
        }

        // 2. パース
        let line_index = LineIndex::new(source);
        let mut parse_result = parser::parse(build_owned_tokens(pre_tokens, &line_index, source));
        let legacy_tokens = owned_tokens_to_legacy(&parse_result.tokens, &line_index);

        let parser_error = parse_result
            .errors
            .first()
            .map(|msg| ParseError::Syntax {
                message: format!("[{}] {}", SALSA_PARSER_STAGE, msg.clone()),
                span: Span::dummy(),
            })
            .or_else(|| first_parser_error(&parse_result, &line_index));
        let cst = if options.generate_cst {
            build_cst(&parse_result)
        } else {
            None
        };
        let trivia_map = if options.generate_trivia_map {
            Some(TriviaMap::from_tokens(&parse_result.tokens))
        } else {
            None
        };

        // 3. ローワリング
        let lowering = crate::lower::lower(source, &parse_result);
        let lowering_error = first_lowering_error(&lowering);

        // 4. Program 組み立て
        let program = assemble_program(lowering.statements.clone());

        // 5. セマンティクス
        let (program, semantics_diagnostics, semantics_halted_stage, semantics_error) =
            if parser_error.is_none() && lowering_error.is_none() {
                let semantics_result = jv_parser_semantics::run(&legacy_tokens, program);
                let error = semantics_result.halted_stage.map(|stage| {
                    let message = semantics_result
                        .staged_diagnostics
                        .first()
                        .map(|d| format!("[{}] {}", stage, d.message()))
                        .unwrap_or_else(|| format!("Stage 2 semantics halted at {}", stage));
                    let span = semantics_result
                        .staged_diagnostics
                        .first()
                        .and_then(|d| d.span().cloned())
                        .unwrap_or_else(Span::dummy);
                    ParseError::Syntax { message, span }
                });
                (
                    semantics_result.program,
                    semantics_result.staged_diagnostics,
                    semantics_result.halted_stage,
                    error,
                )
            } else {
                (program, Vec::new(), None, None)
            };

        // 6. パイプラインエラーの決定
        let pipeline_error = parser_error.or(lowering_error).or(semantics_error);

        // 7. 診断統合
        let frontend_diagnostics = diagnostics::compose_frontend_diagnostics(
            &parse_result.tokens,
            &parse_result.output.diagnostics,
            &lowering.diagnostics,
            preprocess_diagnostics,
            preprocess_halted_stage,
            semantics_diagnostics,
            semantics_halted_stage,
            &line_index,
        );

        // 8. 成果物構築
        let legacy_tokens = owned_tokens_to_legacy(&parse_result.tokens, &line_index);
        let parser_diagnostics = parse_result.output.diagnostics.clone();
        let lowering_diagnostics = lowering.diagnostics.clone();
        let recovered = parse_result.output.recovered;
        let artifacts = PipelineArtifacts::new(program, legacy_tokens, frontend_diagnostics);
        let output = SalsaPipelineOutput {
            artifacts,
            cst,
            trivia_map,
            parser_diagnostics,
            lowering_diagnostics,
            recovered,
        };

        // 余計なバッファを早期に解放してメモリフットプリントを抑える。
        let _ = std::mem::take(&mut parse_result.tokens);
        let _ = std::mem::take(&mut parse_result.output.events);
        let _ = std::mem::take(&mut parse_result.output.diagnostics);

        match pipeline_error {
            Some(err) => Err(err),
            None => Ok(output),
        }
    }
}

impl ParserPipeline for SalsaPipeline {
    fn execute(&self, source: &str) -> Result<PipelineArtifacts, ParseError> {
        self.execute_with_options(source, ParseOptions::default())
            .map(SalsaPipelineOutput::into_artifacts)
    }
}

fn owned_tokens_to_legacy(
    tokens: &[crate::parser::OwnedToken],
    index: &LineIndex,
) -> Vec<LegacyToken> {
    tokens
        .iter()
        .map(|token| to_legacy_token_with_index(token, index))
        .collect()
}

fn build_owned_tokens(
    tokens: Vec<LegacyToken>,
    index: &LineIndex,
    source: &str,
) -> Vec<crate::parser::OwnedToken> {
    tokens
        .into_iter()
        .map(|tok| {
            let span = span_from_legacy_token(&tok, index, source);
            crate::parser::OwnedToken {
                kind: crate::lexer::kind_from_token_type(&tok.token_type),
                span,
                lexeme: Arc::from(tok.lexeme),
                leading_trivia: tok.leading_trivia,
                metadata: tok.metadata,
                diagnostic: tok.diagnostic,
            }
        })
        .collect()
}

fn build_cst(parse: &ParseResult) -> Option<CstNode> {
    let mut builder = crate::parser::cst::CstBuilder::new();
    for event in &parse.output.events {
        match event {
            ParseEvent::StartNode { kind } => builder.start_node(kind.clone()),
            ParseEvent::FinishNode => builder.finish_node(),
            ParseEvent::Token { token_index, .. } => {
                if let Some(tok) = parse.tokens.get(*token_index).cloned() {
                    builder.push_token(tok);
                }
            }
            ParseEvent::Error { .. } => {}
        }
    }
    builder.build()
}

fn assemble_program(statements: Vec<Statement>) -> Program {
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

fn merge_statement_spans(first: &Span, last: &Span) -> Span {
    Span {
        start_line: first.start_line,
        start_column: first.start_column,
        end_line: last.end_line,
        end_column: last.end_column,
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

fn first_parser_error(parse: &ParseResult, index: &LineIndex) -> Option<ParseError> {
    parse
        .output
        .diagnostics
        .iter()
        .find(|d| matches!(d.severity, crate::parser::DiagnosticSeverity::Error))
        .map(|diagnostic| {
            let span = diagnostics::token_span_to_span(diagnostic.span, &parse.tokens, index)
                .unwrap_or_else(Span::dummy);
            let message = format!("[{}] {}", SALSA_PARSER_STAGE, diagnostic.message.clone());
            ParseError::Syntax { message, span }
        })
}

fn first_lowering_error(lowering: &LoweringResult) -> Option<ParseError> {
    lowering
        .diagnostics
        .iter()
        .find(|d| matches!(d.severity, LoweringDiagnosticSeverity::Error))
        .map(|diagnostic| {
            let message = format!("[{}] {}", SALSA_LOWERING_STAGE, diagnostic.message.clone());
            let span = diagnostic.span.clone().unwrap_or_else(Span::dummy);
            ParseError::Syntax { message, span }
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_parser_rowan::frontend::RowanPipeline;

    #[test]
    fn executes_pipeline_and_builds_artifacts() {
        let pipeline = SalsaPipeline::new();
        let source = "package main\nval x = 1\n";
        let output = pipeline
            .execute_with_options(
                source,
                ParseOptions {
                    generate_cst: true,
                    generate_trivia_map: true,
                },
            )
            .expect("パイプライン実行に成功する");

        let artifacts = &output.artifacts;
        assert_eq!(
            artifacts.program.package.as_deref(),
            Some("main"),
            "package 名が組み立てられる"
        );
        assert!(!artifacts.tokens.is_empty(), "トークン列が生成される");
        assert!(
            artifacts.diagnostics.final_diagnostics().is_empty(),
            "診断は空である"
        );
        assert!(output.cst.is_some(), "CST が生成される");
        assert!(output.trivia_map.is_some(), "TriviaMap が生成される");
    }

    #[test]
    fn reports_parser_error_for_invalid_constructs() {
        let pipeline = SalsaPipeline::new();
        let source = "if true { }";
        let error = pipeline
            .execute(source)
            .expect_err("構文エラーが発生するはず");

        match error {
            ParseError::Syntax { message, .. } => {
                assert!(
                    message.contains(SALSA_PARSER_STAGE) || message.contains("JV3103"),
                    "パーサー診断が昇格される: {}",
                    message
                );
            }
            other => panic!("Syntax エラーを期待したが {:?} を受け取った", other),
        }
    }

    #[test]
    fn matches_rowan_pipeline_on_simple_input() {
        let salsa = SalsaPipeline::new();
        let rowan = RowanPipeline::new();
        let source = "package sample\nval a = 1\n";

        let salsa_output = salsa
            .execute_with_options(source, ParseOptions::default())
            .expect("salsa パイプラインが成功する");
        let rowan_artifacts = rowan.execute(source).expect("rowan パイプラインが成功する");

        let salsa_artifacts = &salsa_output.artifacts;
        assert_eq!(
            salsa_artifacts.program.package, rowan_artifacts.program.package,
            "package が一致する"
        );
        assert_eq!(
            salsa_artifacts.program.statements.len(),
            rowan_artifacts.program.statements.len(),
            "ステートメント数が一致する"
        );
        assert_eq!(
            salsa_artifacts.tokens.len(),
            rowan_artifacts.tokens.len(),
            "トークン数が一致する"
        );
        assert_eq!(
            salsa_artifacts.diagnostics.final_diagnostics().len(),
            rowan_artifacts.diagnostics.final_diagnostics().len(),
            "診断件数が一致する"
        );
    }
}
