use crate::frontend::RowanPipeline;
use jv_ast::Statement;
use jv_parser_frontend::ParserPipeline;
use std::path::Path;

#[test]
fn sum_int_family_contains_when_expression() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/collections/sequence.jv");
    let source = std::fs::read_to_string(path).expect("read stdlib sequence module");
    let output = RowanPipeline::default()
        .parse(&source)
        .expect("stdlib parses");
    let program = output.into_program();

    let mut found = false;
    for statement in &program.statements {
        if let Statement::ExtensionFunction(extension) = statement {
            if let Statement::FunctionDeclaration { name, body, .. } = extension.function.as_ref() {
                if name == "sumIntFamily" {
                    let debug_body = format!("{:?}", body);
                    found = debug_body.contains("When");
                    break;
                }
            }
        }
    }

    assert!(found, "sumIntFamily should contain a when expression");
}
