use jv_codegen_java::JavaCodeGenerator;
use jv_ir::transform::transform_program_with_context;
use jv_ir::TransformContext;
use jv_parser::Parser;

fn parse_and_lower(source: &str) -> jv_ir::types::IrProgram {
    let program = Parser::parse(source).expect("fixture parses");
    let mut context = TransformContext::new();
    transform_program_with_context(program, &mut context).expect("lowering succeeds")
}

#[test]
fn annotations_with_arguments_and_imports_are_preserved() {
    let source = include_str!("../fixtures/java_annotations/pass_through.jv");
    let ir_program = parse_and_lower(source);
    let mut generator = JavaCodeGenerator::new();
    let unit = generator
        .generate_compilation_unit(&ir_program)
        .expect("Java generation succeeds");

    let service_class = unit
        .type_declarations
        .iter()
        .find(|decl| decl.contains("class Service"))
        .expect("Service class generated");

    assert!(service_class.contains("@Component"), "class annotation emitted");
    assert!(
        service_class.contains("@Autowired"),
        "field annotation emitted"
    );
    assert!(
        service_class.contains("@RequestMapping(path = {\"/ping\"}, produces = {\"application/json\"})"),
        "named and array arguments rendered"
    );
    assert!(
        service_class.contains("@Nullable"),
        "parameter annotation emitted"
    );

    let imports: Vec<_> = unit.imports.iter().map(String::as_str).collect();
    assert!(
        imports.contains(&"org.springframework.stereotype.Component"),
        "component import inferred"
    );
    assert!(
        imports.contains(&"org.springframework.beans.factory.annotation.Autowired"),
        "autowired import inferred"
    );
    assert!(
        imports.contains(&"org.springframework.web.bind.annotation.RequestMapping"),
        "request mapping import inferred"
    );
}
