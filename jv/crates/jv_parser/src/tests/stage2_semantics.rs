use crate::semantics::{GenericDirectivePass, PrimitiveReturnPass, SemanticsPipeline};
use crate::{preprocess, Parser};
use jv_ast::{Program, Statement};
use jv_lexer::Lexer;

fn preprocess_tokens(source: &str) -> Vec<jv_lexer::Token> {
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize().expect("lexing should succeed");
    let (tokens, _, _) = preprocess::run(tokens).into_parts();
    tokens
}

fn parse_program(source: &str) -> Program {
    Parser::parse(source)
        .expect("parsing should succeed")
        .into_program()
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

#[test]
fn primitive_return_pass_restores_metadata() {
    let source = r#"
        fun int sum(values: Stream<Int>) {
            return 0
        }
    "#;

    let tokens = preprocess_tokens(source);
    let mut program = parse_program(source);
    clear_primitive_metadata(&mut program);

    let pipeline = SemanticsPipeline::builder()
        .with_pass(PrimitiveReturnPass::default())
        .build();

    let result = pipeline.run(&tokens, program);
    let program = result.program;

    let statement = program
        .statements
        .first()
        .expect("function declaration should exist");

    match statement {
        Statement::FunctionDeclaration {
            name,
            primitive_return,
            ..
        } => {
            assert_eq!(name, "sum");
            let metadata = primitive_return
                .as_ref()
                .expect("primitive metadata should be attached");
            assert_eq!(
                metadata.reference.raw_path,
                vec!["int".to_string()],
                "primitive path should preserve original segments"
            );
        }
        other => panic!("expected function declaration, found {:?}", other),
    }
}

#[test]
fn generic_directive_pass_collects_comment_directives() {
    let source = r#"
        fun mapper
        /* jv:raw-allow com.example.RawType */
        <T>() {}
    "#;

    let tokens = preprocess_tokens(source);
    let mut program = parse_program(source);
    clear_raw_directives(&mut program);

    let pipeline = SemanticsPipeline::builder()
        .with_pass(GenericDirectivePass::default())
        .build();
    let result = pipeline.run(&tokens, program);
    let program = result.program;

    let statement = program
        .statements
        .first()
        .expect("function declaration should exist");

    match statement {
        Statement::FunctionDeclaration {
            generic_signature, ..
        } => {
            let signature = generic_signature
                .as_ref()
                .expect("generic signature should be present");
            assert_eq!(
                signature
                    .raw_directives
                    .iter()
                    .map(|directive| directive.owner.segments.clone())
                    .collect::<Vec<_>>(),
                vec![vec![
                    "com".to_string(),
                    "example".to_string(),
                    "RawType".to_string()
                ]]
            );
        }
        other => panic!("expected function declaration, found {:?}", other),
    }
}

#[test]
fn default_pipeline_runs_all_passes_without_diagnostics() {
    let source = r#"
        fun int id(value: Int) {
            return value
        }
    "#;

    let tokens = preprocess_tokens(source);
    let program = parse_program(source);

    let result = SemanticsPipeline::default().run(&tokens, program);
    assert!(
        result.staged_diagnostics.is_empty(),
        "expected no staged diagnostics but found {:?}",
        result.staged_diagnostics
    );
    assert!(
        result.halted_stage.is_none(),
        "default pipeline should not halt on a well-formed program"
    );
}
