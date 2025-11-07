use std::fs;
use std::path::{Path, PathBuf};

use jv_ast::Expression;
use jv_ast::expression::{Argument, StringPart};
use jv_checker::TypeChecker;
use jv_cli::pipeline::{
    generics::apply_type_facts,
    type_facts_bridge::{preload_tuple_plans_into_context, preload_type_facts_into_context},
};
use jv_codegen_java::{generate_java_source_with_config, JavaCodeGenConfig};
use jv_ir::{TransformContext, transform::transform_program_with_context};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::frontend::RowanPipeline;
use jv_pm::JavaTarget;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
}

fn fixture_path(name: &str) -> PathBuf {
    workspace_root()
        .join("tests")
        .join("data")
        .join("tuple")
        .join(format!("{name}.jv"))
}

fn expected_path(name: &str, target: JavaTarget) -> PathBuf {
    workspace_root()
        .join("tests")
        .join("data")
        .join("tuple")
        .join("expected")
        .join(format!("java{}", target.as_str()))
        .join(format!("{name}.java"))
}

fn normalize(text: &str) -> String {
    text.replace("\r\n", "\n").trim_end().to_string()
}

fn generate_java_source_from_text(source: &str, target: JavaTarget) -> String {
    let program = RowanPipeline::default()
        .parse(source)
        .expect("ソース解析に失敗しました")
        .into_program();

    let mut checker = TypeChecker::new();
    checker.set_java_target(target);
    checker
        .check_program(&program)
        .expect("型チェックに失敗しました");

    let tuple_plans = checker.tuple_record_plans().to_vec();
    let type_facts_snapshot = checker.type_facts().cloned();

    let lowering_input = checker
        .take_normalized_program()
        .unwrap_or_else(|| program.clone());

    let mut context = TransformContext::new();
    if let Some(facts) = type_facts_snapshot.as_ref() {
        preload_type_facts_into_context(&mut context, facts);
    }
    preload_tuple_plans_into_context(&mut context, &tuple_plans);

    let mut ir_program = transform_program_with_context(lowering_input, &mut context)
        .expect("IR変換に失敗しました");
    ir_program.tuple_record_plans = tuple_plans;

    if let Some(facts) = type_facts_snapshot.as_ref() {
        apply_type_facts(&mut ir_program, facts);
    }

    let config = JavaCodeGenConfig::for_target(target);
    generate_java_source_with_config(&ir_program, &config)
        .expect("Javaコード生成に失敗しました")
}

fn generate_java_source(name: &str, target: JavaTarget) -> String {
    let source =
        fs::read_to_string(fixture_path(name)).expect("フィクスチャの読み込みに失敗しました");
    generate_java_source_from_text(&source, target)
}

#[test]
fn tuple_fixtures_match_expected_java() {
    let fixtures = ["divmod", "find_user", "calculate_stats", "function_return"];
    for target in [JavaTarget::Java25, JavaTarget::Java21] {
        for fixture in fixtures {
            let actual = generate_java_source(fixture, target);
            let expected_file = expected_path(fixture, target);
            let expected = fs::read_to_string(&expected_file).unwrap_or_else(|_| {
                panic!(
                    "ゴールデンファイルが存在しません: {}\n--- 実際の出力 ---\n{}",
                    expected_file.display(),
                    actual
                )
            });

            assert_eq!(
                normalize(&actual),
                normalize(&expected),
                "ゴールデン比較が一致しません (fixture: {}, target: java{})",
                fixture,
                target.as_str()
            );
        }
    }
}

#[test]
fn tuple_demo_example_preserves_interpolation_calls() {
    let example_path = workspace_root().join("examples").join("tuple_demo.jv");
    let source =
        fs::read_to_string(&example_path).expect("tuple_demo.jv の読み込みに失敗しました");

    let program = RowanPipeline::default()
        .parse(&source)
        .expect("tuple_demo.jv の解析に失敗しました")
        .into_program();

    fn expression_uses_tuple_component(expr: &Expression) -> bool {
        match expr {
            Expression::StringInterpolation { parts, .. } => parts.iter().any(|part| match part {
                StringPart::Expression(inner) => expression_uses_tuple_component(inner),
                _ => false,
            }),
            Expression::Call { function, args, .. } => {
                if let Expression::MemberAccess { property, .. } = function.as_ref() {
                    if property == "_3" {
                        return true;
                    }
                }
                if expression_uses_tuple_component(function) {
                    return true;
                }
                args.iter().any(|arg| match arg {
                    Argument::Positional(inner) => expression_uses_tuple_component(inner),
                    Argument::Named { value, .. } => expression_uses_tuple_component(value),
                })
            }
            Expression::MemberAccess { object, .. } => expression_uses_tuple_component(object),
            Expression::Binary { left, right, .. } => {
                expression_uses_tuple_component(left)
                    || expression_uses_tuple_component(right)
            }
            Expression::Block { statements, .. } => statements.iter().any(|stmt| match stmt {
                jv_ast::Statement::Expression { expr, .. } => expression_uses_tuple_component(expr),
                jv_ast::Statement::ValDeclaration { initializer, .. } => {
                    expression_uses_tuple_component(initializer)
                }
                jv_ast::Statement::VarDeclaration {
                    initializer: Some(expr),
                    ..
                } => expression_uses_tuple_component(expr),
                jv_ast::Statement::Return {
                    value: Some(expr), ..
                } => expression_uses_tuple_component(expr),
                _ => false,
            }),
            Expression::Tuple { elements, .. } => {
                elements.iter().any(expression_uses_tuple_component)
            }
            Expression::Array { elements, .. } => {
                elements.iter().any(expression_uses_tuple_component)
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                expression_uses_tuple_component(condition)
                    || expression_uses_tuple_component(then_branch)
                    || else_branch
                        .as_ref()
                        .map_or(false, |expr| expression_uses_tuple_component(expr))
            }
            Expression::When {
                expr,
                arms,
                else_arm,
                ..
            } => {
                expr.as_ref()
                    .map_or(false, |expr| expression_uses_tuple_component(expr))
                    || arms.iter().any(|arm| {
                        expression_uses_tuple_component(&arm.body)
                            || arm
                                .guard
                                .as_ref()
                                .map_or(false, |guard| expression_uses_tuple_component(guard))
                    })
                    || else_arm
                        .as_ref()
                        .map_or(false, |expr| expression_uses_tuple_component(expr))
            }
            Expression::Lambda { body, .. } => expression_uses_tuple_component(body),
            Expression::Unary { operand, .. } => expression_uses_tuple_component(operand),
            _ => false,
        }
    }

    let main_body = program
        .statements
        .iter()
        .find_map(|statement| match statement {
            jv_ast::Statement::FunctionDeclaration { name, body, .. } if name == "main" => {
                Some(body.as_ref())
            }
            _ => None,
        })
        .expect("main 関数が見つかりません");

    let interpolation_found = match main_body {
        Expression::Block { statements, .. } => statements.iter().any(|statement| match statement {
            jv_ast::Statement::Expression { expr, .. } => expression_uses_tuple_component(expr),
            _ => false,
        }),
        other => panic!("main 関数の本体がブロック式ではありません: {:?}", other),
    };

    assert!(
        interpolation_found,
        "Expected tuple_demo.jv to contain string interpolation calling tuple._3()"
    );
}
