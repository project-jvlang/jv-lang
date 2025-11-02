//! 簡潔な正規表現コマンド構文のパーサ統合テスト。

use jv_ast::{
    Expression, RegexCommand, RegexCommandMode, RegexCommandModeOrigin, RegexFlag, Statement,
};
use jv_parser_frontend::ParserPipeline;
use jv_parser_rowan::{frontend::RowanPipeline, syntax::TokenKind};

fn extract_regex_command(source: &str) -> RegexCommand {
    let program = RowanPipeline::default()
        .parse(source)
        .expect("ソースコードが構文解析できること")
        .into_program();

    for statement in program.statements {
        let candidate = match statement {
            Statement::ValDeclaration { initializer, .. }
            | Statement::Expression {
                expr: initializer, ..
            } => initializer,
            Statement::VarDeclaration {
                initializer: Some(initializer),
                ..
            } => initializer,
            _ => continue,
        };
        if let Expression::RegexCommand(command) = candidate {
            return (*command).clone();
        }
    }

    panic!("対象となる式を検出できませんでした: {source}");
}

fn expression_variant(expr: &Expression) -> &'static str {
    match expr {
        Expression::Literal(_, _) => "Literal",
        Expression::RegexLiteral(_) => "RegexLiteral",
        Expression::RegexCommand(_) => "RegexCommand",
        Expression::Identifier(_, _) => "Identifier",
        Expression::Binary { .. } => "Binary",
        Expression::Unary { .. } => "Unary",
        Expression::Call { .. } => "Call",
        Expression::MemberAccess { .. } => "MemberAccess",
        Expression::NullSafeMemberAccess { .. } => "NullSafeMemberAccess",
        Expression::IndexAccess { .. } => "IndexAccess",
        Expression::NullSafeIndexAccess { .. } => "NullSafeIndexAccess",
        Expression::TypeCast { .. } => "TypeCast",
        Expression::StringInterpolation { .. } => "StringInterpolation",
        Expression::MultilineString(_) => "MultilineString",
        Expression::JsonLiteral(_) => "JsonLiteral",
        Expression::When { .. } => "When",
        Expression::If { .. } => "If",
        Expression::Block { .. } => "Block",
        Expression::Array { .. } => "Array",
        Expression::Lambda { .. } => "Lambda",
        Expression::Try { .. } => "Try",
        Expression::This(_) => "This",
        Expression::Super(_) => "Super",
    }
}

#[test]
fn default_match_mode_is_inferred() {
    let source = r#"
val subject = "value123"
val matched = /subject/\d+/
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::Match);
    assert!(matches!(
        command.mode_origin,
        RegexCommandModeOrigin::DefaultMatch
    ));
    assert!(command.replacement.is_none());
    assert!(command.flags.is_empty());
}

#[test]
fn short_mode_with_replacement_and_flags_parses() {
    let source = r#"
val replaced = a/sub/\d+/"X"/ims
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::All);
    assert!(matches!(
        command.mode_origin,
        RegexCommandModeOrigin::ShortMode
    ));
    let replacement = command
        .replacement
        .as_ref()
        .expect("置換情報が存在すること");
    match replacement {
        jv_ast::RegexReplacement::Literal(literal) => {
            assert_eq!(literal.normalized, "X");
        }
        other => panic!("リテラル置換を期待しました: {other:?}"),
    }

    let flags: Vec<RegexFlag> = command.flags.clone();
    assert!(flags.contains(&RegexFlag::CaseInsensitive));
    assert!(flags.contains(&RegexFlag::Multiline));
    assert!(flags.contains(&RegexFlag::DotAll));
}

#[test]
fn first_mode_literal_replacement_is_preserved() {
    let source = r#"
val once = f/text/\d+/"first"/
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::First);
    assert!(matches!(
        command.mode_origin,
        RegexCommandModeOrigin::ShortMode
    ));

    let replacement = command
        .replacement
        .as_ref()
        .expect("first モードでも置換部が存在すること");
    match replacement {
        jv_ast::RegexReplacement::Literal(literal) => {
            assert_eq!(literal.normalized, "first");
            assert_eq!(literal.raw, "\"first\"");
        }
        other => panic!("リテラル置換を期待しました: {other:?}"),
    }
    assert!(command.flags.is_empty(), "明示フラグが無い場合は空ベクタ");
}

#[test]
fn split_mode_without_replacement_has_no_replacement() {
    let source = r#"
val text = "a,b,c"
val parts = s/text/\s+/
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::Split);
    assert!(
        command.replacement.is_none(),
        "Split モードでは置換が不要です"
    );
    assert_eq!(
        command.pattern.pattern, "\\s+",
        "パターンの正規化結果が保持されること"
    );
    assert!(
        command.flags.is_empty(),
        "明示フラグが無い場合は空集合として扱う想定です: {:?}",
        command.flags
    );
}

#[test]
fn iterate_mode_lambda_replacement_is_captured() {
    let source = r#"
val text = "value123"
val joined = i/text/\w+/{ match -> match.group(1).toUpperCase() }/
"#;

    let command = extract_regex_command(source);

    assert_eq!(command.mode, RegexCommandMode::Iterate);
    let replacement = command
        .replacement
        .as_ref()
        .expect("ラムダ置換が保持されること");
    match replacement {
        jv_ast::RegexReplacement::Lambda(lambda) => {
            assert_eq!(
                lambda.params.len(),
                1,
                "ラムダ置換には1つのパラメータが存在する想定です: {:?}",
                lambda.params
            );
            if let Some(param) = lambda.params.first() {
                assert_eq!(
                    param.name, "match",
                    "先頭パラメータ名が match である想定です: {:?}",
                    param
                );
            }
            assert!(
                matches!(
                    lambda.body.as_ref(),
                    Expression::Call { .. }
                        | Expression::MemberAccess { .. }
                        | Expression::Binary { .. }
                        | Expression::Identifier(_, _)
                ),
                "ラムダ本体が式として保持される必要があります: {:?}",
                lambda.body
            );
        }
        other => panic!("ラムダ置換を期待しました: {other:?}"),
    }
}

#[test]
fn unit_main_allows_consecutive_regex_commands() {
    let source = r#"
fun main(): Unit {
    val text = "abc"
    val masked = a/text/'a'/"b"/
    val first = f/text/'a'/"c"/
}
"#;

    let debug = RowanPipeline::default()
        .execute_with_debug(source)
        .expect("正規表現コマンド付き関数が構文解析できること");

    let tokens_snapshot: Vec<(usize, usize, TokenKind, String)> = debug
        .artifacts()
        .tokens()
        .iter()
        .filter(|token| (3..=5).contains(&token.line) && !TokenKind::from_token(token).is_trivia())
        .map(|token| {
            (
                token.line,
                token.column,
                TokenKind::from_token(token),
                token.lexeme.clone(),
            )
        })
        .collect();

    let lowered_summary: Vec<String> = debug
        .statements()
        .iter()
        .map(|statement| format!("{statement:?}"))
        .collect();

    assert!(
        debug.parser_diagnostics().is_empty(),
        "parser diagnostics: {:?}",
        debug.parser_diagnostics()
    );

    let artifacts = debug.into_artifacts();
    let program = artifacts.program;

    let body_expr = program
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionDeclaration { name, body, .. } if name == "main" => Some(body),
            _ => None,
        })
        .expect("main 関数を検出できること");

    let Expression::Block { statements, .. } = body_expr.as_ref() else {
        panic!("関数本体はブロック式である想定です: {body_expr:?}");
    };

    let initializer_summary: Vec<(String, &'static str)> = statements
        .iter()
        .filter_map(|statement| match statement {
            Statement::ValDeclaration {
                name, initializer, ..
            } => Some((name.clone(), expression_variant(initializer))),
            _ => None,
        })
        .collect();

    let statement_summary: Vec<String> = statements
        .iter()
        .map(|statement| match statement {
            Statement::ValDeclaration {
                name, initializer, ..
            } => {
                format!("val {name} = {}", expression_variant(initializer))
            }
            other => format!("{other:?}"),
        })
        .collect();

    let commands: Vec<&RegexCommand> = statements
        .iter()
        .filter_map(|statement| match statement {
            Statement::ValDeclaration { initializer, .. } => match initializer {
                Expression::RegexCommand(command) => Some(command.as_ref()),
                _ => None,
            },
            _ => None,
        })
        .collect();

    assert_eq!(
        commands.len(),
        2,
        "連続する RegexCommand が2件とも検出されること: {initializer_summary:?} / {statement_summary:?} / {tokens_snapshot:?} / {lowered_summary:?}"
    );

    assert!(
        matches!(commands[0].mode, RegexCommandMode::All),
        "先頭の短縮モード `a/` は RegexCommandMode::All へ解決される想定です: {:?}",
        commands[0].mode
    );
    assert!(
        matches!(commands[1].mode, RegexCommandMode::First),
        "2件目の `f/` は RegexCommandMode::First として扱われる想定です: {:?}",
        commands[1].mode
    );
}

#[test]
fn script_allows_explicit_match_after_short_mode() {
    let source = r#"
val auditLog = """
USER=Akira ACTION=login
USER=Tom ACTION=LOGIN
""".trim()

val all_login_upper = m/auditLog/'^USER=\w+\sACTION=LOGIN$'/
val has_result = [match]/auditLog/'ACTION=RESULT'/
"#;

    let debug = RowanPipeline::default()
        .execute_with_debug(source)
        .expect("スクリプト全体が構文解析できること");

    assert!(
        debug.parser_diagnostics().is_empty(),
        "parser diagnostics: {:?}",
        debug.parser_diagnostics()
    );

    let tokens_snapshot: Vec<(usize, usize, TokenKind, String)> = debug
        .artifacts()
        .tokens()
        .iter()
        .filter(|token| !TokenKind::from_token(token).is_trivia())
        .map(|token| {
            (
                token.line,
                token.column,
                TokenKind::from_token(token),
                token.lexeme.clone(),
            )
        })
        .collect();
    println!("tokens_snapshot = {:?}", tokens_snapshot);
    let lowering_summary: Vec<String> = debug
        .statements()
        .iter()
        .map(|statement| format!("{statement:?}"))
        .collect();
    println!("lowering_summary = {:?}", lowering_summary);

    let artifacts = debug.into_artifacts();
    let program = artifacts.program;

    let statements_summary: Vec<String> = program
        .statements
        .iter()
        .map(|statement| format!("{statement:?}"))
        .collect();
    println!("statements_summary = {:?}", statements_summary);

    let mut modes_by_name = Vec::new();
    for statement in program.statements {
        if let Statement::ValDeclaration {
            name, initializer, ..
        } = statement
        {
            if let Expression::RegexCommand(command) = initializer {
                modes_by_name.push((name, command.mode));
            }
        }
    }

    let statement_summary: Vec<String> = modes_by_name
        .iter()
        .map(|(name, mode)| format!("val {name} = {:?}", mode))
        .collect();
    println!("regex_command_val_summary = {:?}", statement_summary);

    assert_eq!(
        modes_by_name.len(),
        2,
        "連続する RegexCommand が val 宣言として検出される想定です: {:?}",
        modes_by_name
    );

    assert!(
        modes_by_name.iter().any(
            |(name, mode)| name == "all_login_upper" && matches!(mode, RegexCommandMode::Match)
        ),
        "短縮 `m/` モードは RegexCommandMode::Match へ正しく解決される必要があります: {:?}",
        modes_by_name
    );

    assert!(
        modes_by_name
            .iter()
            .any(|(name, mode)| name == "has_result" && matches!(mode, RegexCommandMode::Match)),
        "`[match]` モードの RegexCommand が RegexCommandMode::Match として検出される必要があります: {:?}",
        modes_by_name
    );
}

#[test]
fn script_modes_sample_parses() {
    let source = r#"
val auditLog = """
USER=Akira ACTION=login ip=192.168.0.10
USER=Tom ACTION=LOGIN ip=10.0.0.5
USER=Chie ACTION=RESULT ip=172.16.0.77
""".trim()

val maskedAll = a/auditLog/'ip=\d{1,3}(\.\d{1,3}){3}'/'ip=***.***.***.***'/ims

val firstTagged = f/auditLog/'USER=\w+'/{ match ->
    when (match.group()) {
        null -> ""
        else -> "[FIRST-${match.group()}]"
    }
}/

val canonicalLog = /auditLog/'ACTION=LOGIN'/'ACTION=login'/i

val containsAkira = m/auditLog/'.*USER=Akira.*'/

val hasResult = [match]/auditLog/'ACTION=RESULT'/

val splitLines = s/auditLog/'\n'/

val iteratedPairs = i/auditLog/'USER=(\w+)\sACTION=(\w+)'/{ match ->
    "${match.group(1)}:${match.group(2)}"
}/

println("=== replaceAll ===")
println(maskedAll)

println("=== replaceFirst ===")
println(firstTagged)

println("=== implicit replace ===")
println(canonicalLog)

println("=== match implicit ===")
println(containsAkira)

println("=== match explicit ===")
println(hasResult)

println("=== split ===")
for (part in splitLines) {
    println(" • ${part}")
}

println("=== iterate lambda ===")
println(iteratedPairs)
"#;

    let debug = RowanPipeline::default()
        .execute_with_debug(source)
        .expect("複数モードの RegexCommand スクリプトが構文解析できること");

    let tokens_with_index: Vec<(usize, usize, usize, TokenKind, String)> = debug
        .artifacts()
        .tokens()
        .iter()
        .enumerate()
        .filter(|(_, token)| !TokenKind::from_token(token).is_trivia())
        .map(|(index, token)| {
            (
                index,
                token.line,
                token.column,
                TokenKind::from_token(token),
                token.lexeme.clone(),
            )
        })
        .collect();
    println!("tokens = {:?}", tokens_with_index);

    let statements: Vec<String> = debug
        .statements()
        .iter()
        .map(|statement| format!("{statement:?}"))
        .collect();
    println!("statements = {:?}", statements);

    assert!(
        debug.parser_diagnostics().is_empty(),
        "parser diagnostics: {:?}",
        debug.parser_diagnostics()
    );
}

#[test]
fn unit_main_allows_regex_commands_with_lambda_replacement() {
    let source = r#"
fun main(): Unit {
    val auditLog = """
USER=Akira ACTION=login ip=192.168.0.10
USER=Tom ACTION=LOGIN ip=10.0.0.5
""".trim()

    val masked = a/auditLog/'ip=\d{1,3}(\.\d{1,3}){3}'/"ip=***.***.***.***"/ims
    val firstTagged = f/auditLog/'USER=\w+'/{ match ->
        match.group() ?: ""
    }/
}
"#;

    let debug = RowanPipeline::default()
        .execute_with_debug(source)
        .expect("ラムダ置換を含む RegexCommand が構文解析できること");

    assert!(
        debug.parser_diagnostics().is_empty(),
        "parser diagnostics: {:?}",
        debug.parser_diagnostics()
    );

    let artifacts = debug.into_artifacts();
    let program = artifacts.program;

    let body_expr = program
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionDeclaration { name, body, .. } if name == "main" => Some(body),
            _ => None,
        })
        .expect("main 関数が存在すること");

    let Expression::Block { statements, .. } = body_expr.as_ref() else {
        panic!("関数本体はブロック式の想定です: {body_expr:?}");
    };

    let mut commands = Vec::new();
    for statement in statements {
        if let Statement::ValDeclaration {
            initializer, name, ..
        } = statement
        {
            if let Expression::RegexCommand(command) = initializer {
                commands.push((name.as_str(), command.as_ref()));
            }
        }
    }

    assert_eq!(
        commands.len(),
        2,
        "RegexCommand が2件とも検出される想定です: {commands:?}"
    );

    let (first_name, first_command) = &commands[0];
    assert_eq!(*first_name, "masked");
    assert!(matches!(first_command.mode, RegexCommandMode::All));

    let (second_name, second_command) = &commands[1];
    assert_eq!(*second_name, "firstTagged");
    assert!(matches!(second_command.mode, RegexCommandMode::First));
    let replacement = second_command
        .replacement
        .as_ref()
        .expect("ラムダ置換が保持されること");
    assert!(matches!(replacement, jv_ast::RegexReplacement::Lambda(_)));
}

#[test]
fn concise_sample_parses() {
    let source = r#"
fun main(): Unit {
    val csv = "alice@example.com,bob@example.org,carol@example.net"

    val replacedAll = a/csv/'@example\.(com|org|net)'/'@example.dev'/
    val firstUpper = f/csv/'\w+'/$ { it.group().toUpperCase() }/
    val onlyEmails = m/csv/'^([\w.]+@[\w.]+,?)+$'/
    val parts = s/csv/'[,]\s*'/
    val matches = i/csv/'([\w.]+)@([\w.]+)'/ as List
}
"#;

    let debug = RowanPipeline::default()
        .execute_with_debug(source)
        .expect("concise regex sample parses");

    assert!(
        debug.parser_diagnostics().is_empty(),
        "parser diagnostics: {:?}",
        debug.parser_diagnostics()
    );

    let program = debug.into_artifacts().program;
    let body_expr = program
        .statements
        .iter()
        .find_map(|statement| match statement {
            Statement::FunctionDeclaration { name, body, .. } if name == "main" => Some(body),
            _ => None,
        })
        .expect("main 関数が存在すること");

    let Expression::Block { statements, .. } = body_expr.as_ref() else {
        panic!("関数本体はブロック式である必要があります: {body_expr:?}");
    };

    fn extract_regex_command(expr: &Expression) -> Option<&RegexCommand> {
        match expr {
            Expression::RegexCommand(command) => Some(command.as_ref()),
            Expression::TypeCast { expr, .. } => extract_regex_command(expr),
            _ => None,
        }
    }

    let mut regex_vars = Vec::new();
    for statement in statements {
        if let Statement::ValDeclaration {
            name, initializer, ..
        } = statement
        {
            if let Some(command) = extract_regex_command(initializer) {
                regex_vars.push((name.clone(), command.mode));
            }
        }
    }

    assert_eq!(
        regex_vars.len(),
        5,
        "5 つの RegexCommand が抽出される想定です: {:?}",
        regex_vars
    );
}
