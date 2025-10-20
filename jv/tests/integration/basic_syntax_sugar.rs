use jv_codegen_java::JavaCodeGenerator;
use jv_ir::transform::transform_program_with_context;
use jv_ir::TransformContext;
use jv_parser::Parser;

fn parse_and_lower(source: &str) -> jv_ir::types::IrProgram {
    let program = Parser::parse(source)
        .expect("source parses")
        .into_program();
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context).expect("lowering succeeds")
}

#[test]
fn inline_json_pipeline_emits_records_and_constructors() {
    let source = r#"
        val payload = {
            "user": {
                "name": "Alice",
                "age": 30
            },
            "tags": ["admin", "core"]
        }
    "#;

    let ir_program = parse_and_lower(source);
    assert_eq!(ir_program.type_declarations.len(), 1);

    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&ir_program)
        .expect("java generation succeeds");

    assert!(
        unit.type_declarations
            .iter()
            .any(|decl| decl.contains("record PayloadSample")),
        "generated Java should include root record"
    );
    assert!(
        unit.type_declarations
            .iter()
            .any(|decl| decl.contains("record PayloadUserSample")),
        "generated Java should include nested record"
    );
    assert!(
        unit.type_declarations
            .iter()
            .any(|decl| decl.contains("List<String> tags")),
        "list fields should be lowered into java.util.List"
    );
}
