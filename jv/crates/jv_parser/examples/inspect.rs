use jv_parser::Parser;

fn main() {
    let input = "val doubled = numbers.map { x -> x * 2 }";
    let program = Parser::parse(input).unwrap();
    println!("{:#?}", program);
}
