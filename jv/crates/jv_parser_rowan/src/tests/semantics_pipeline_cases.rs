use crate::frontend::RowanPipeline;
use jv_ast::statement::Statement;
use jv_ast::types::{GenericParameter, GenericSignature, Span};
use jv_ast::Program;
use jv_lexer::{Token, TokenType};
use jv_parser_semantics::{
    GenericDirectivePass, PrimitiveReturnPass, SemanticsPipeline, SemanticsPipelineBuilder,
};

fn load_pipeline_artifacts(source: &str) -> (Vec<Token>, Program) {
    let pipeline = RowanPipeline::default();
    let debug = pipeline
        .execute_with_debug(source)
        .expect("Rowan パイプラインの実行に成功すること");
    let tokens = debug.artifacts().tokens().to_vec();
    let program = debug.artifacts().program().clone();
    (tokens, program)
}

fn clear_primitive_metadata(program: &mut Program) {
    fn walk(statement: &mut Statement) {
        match statement {
            Statement::FunctionDeclaration {
                primitive_return, ..
            } => {
                *primitive_return = None;
            }
            Statement::ClassDeclaration { methods, .. }
            | Statement::InterfaceDeclaration { methods, .. } => {
                for method in methods {
                    walk(method);
                }
            }
            Statement::ExtensionFunction(extension) => {
                walk(&mut extension.function);
            }
            _ => {}
        }
    }

    for statement in &mut program.statements {
        walk(statement);
    }
}

fn clear_raw_directives(program: &mut Program) {
    fn walk(statement: &mut Statement) {
        match statement {
            Statement::FunctionDeclaration {
                generic_signature, ..
            } => {
                if let Some(signature) = generic_signature {
                    signature.raw_directives.clear();
                }
            }
            Statement::ClassDeclaration {
                generic_signature,
                methods,
                ..
            }
            | Statement::InterfaceDeclaration {
                generic_signature,
                methods,
                ..
            } => {
                if let Some(signature) = generic_signature {
                    signature.raw_directives.clear();
                }
                for method in methods {
                    walk(method);
                }
            }
            Statement::ExtensionFunction(extension) => {
                walk(&mut extension.function);
            }
            _ => {}
        }
    }

    for statement in &mut program.statements {
        walk(statement);
    }
}

fn semantics_pipeline_with_pass(
    pass: impl jv_parser_semantics::SemanticsPass + 'static,
) -> SemanticsPipeline {
    SemanticsPipelineBuilder::new().with_pass(pass).build()
}

#[test]
fn primitive_return_pass_restores_metadata() {
    let source = r#"
        fun int sum(values: Stream<Int>) {
            return 0
        }
    "#;

    let (tokens, mut program) = load_pipeline_artifacts(source);
    clear_primitive_metadata(&mut program);

    let pipeline = semantics_pipeline_with_pass(PrimitiveReturnPass::default());
    let result = pipeline.run(&tokens, program);
    let program = result.program;

    let statement = program.statements.first().expect("関数宣言が存在すること");

    match statement {
        Statement::FunctionDeclaration {
            name,
            primitive_return,
            ..
        } => {
            assert_eq!(name, "sum");
            let metadata = primitive_return
                .as_ref()
                .expect("プリミティブ戻り値メタデータが復元されること");
            assert_eq!(
                metadata.reference.raw_path,
                vec!["int".to_string()],
                "プリミティブの参照経路が保持されること"
            );
        }
        other => panic!("関数宣言が得られるはずが: {:?}", other),
    }
}

#[test]
fn generic_directive_pass_collects_comment_directives() {
    let source = r#"
        fun mapper<T /* jv:raw-allow com.example.RawType */>() {}
    "#;

    let (tokens, mut program) = load_pipeline_artifacts(source);
    let less_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Less))
        .expect("ジェネリック開始トークンが存在すること");
    let greater_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Greater))
        .expect("ジェネリック終了トークンが存在すること");
    let type_param_token = tokens
        .iter()
        .find(|token| matches!(token.token_type, TokenType::Identifier(ref name) if name == "T"))
        .expect("型パラメータ T が存在すること");

    let signature_span = Span::new(
        less_token.line,
        less_token.column,
        greater_token.line,
        greater_token.column + greater_token.lexeme.chars().count().max(1),
    );
    let param_span = Span::new(
        type_param_token.line,
        type_param_token.column,
        type_param_token.line,
        type_param_token.column + type_param_token.lexeme.chars().count().max(1),
    );

    let signature = GenericSignature {
        parameters: vec![GenericParameter {
            name: "T".to_string(),
            bounds: Vec::new(),
            variance: None,
            default: None,
            kind: None,
            span: param_span,
        }],
        const_parameters: Vec::new(),
        where_clause: None,
        raw_directives: Vec::new(),
        span: signature_span,
    };

    if let Some(Statement::FunctionDeclaration {
        type_parameters,
        generic_signature,
        ..
    }) = program.statements.first_mut()
    {
        *type_parameters = vec!["T".to_string()];
        *generic_signature = Some(signature);
    } else {
        panic!("関数宣言が存在すること");
    }

    let has_signature = program.statements.iter().any(|statement| match statement {
        Statement::FunctionDeclaration {
            generic_signature, ..
        } => generic_signature.is_some(),
        _ => false,
    });
    assert!(
        has_signature,
        "Rowan パイプラインがジェネリックシグネチャを構築できていること: {:?}",
        program.statements
    );

    clear_raw_directives(&mut program);

    let pipeline = semantics_pipeline_with_pass(GenericDirectivePass::default());
    let result = pipeline.run(&tokens, program);
    let program = result.program;

    let statement = program.statements.first().expect("関数宣言が存在すること");

    match statement {
        Statement::FunctionDeclaration {
            generic_signature, ..
        } => {
            let signature = generic_signature
                .as_ref()
                .expect("ジェネリックシグネチャが存在すること");
            let owners: Vec<Vec<String>> = signature
                .raw_directives
                .iter()
                .map(|directive| directive.owner.segments.clone())
                .collect();
            assert_eq!(
                owners,
                vec![vec![
                    "com".to_string(),
                    "example".to_string(),
                    "RawType".to_string()
                ]],
                "コメントディレクティブの収集結果が期待どおりであること"
            );
        }
        other => panic!("関数宣言が得られるはずが: {:?}", other),
    }
}

#[test]
fn default_pipeline_runs_all_passes_without_diagnostics() {
    let source = r#"
        fun int id(value: Int) {
            return value
        }
    "#;

    let (tokens, program) = load_pipeline_artifacts(source);

    let result = SemanticsPipeline::default().run(&tokens, program);
    assert!(
        result.staged_diagnostics.is_empty(),
        "診断は発生しない想定です: {:?}",
        result.staged_diagnostics
    );
    assert!(
        result.halted_stage.is_none(),
        "正常系でパイプラインが停止しないこと"
    );
}
