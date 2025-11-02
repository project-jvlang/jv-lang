//! RegexCommand の IR から生成される Java コードを検証するテスト。

use super::*;
use jv_ast::{RegexCommandMode, RegexFlag, RegexGuardStrategy};
use jv_ir::{
    IrRegexCommand, IrRegexLiteralReplacement, IrRegexReplacement, IrRegexTemplateSegment,
};
use jv_pm::JavaTarget;

fn render_java(program: &IrProgram) -> String {
    let config = JavaCodeGenConfig::for_target(JavaTarget::Java25);
    let mut generator = JavaCodeGenerator::with_config(config.clone());
    let unit = generator
        .generate_compilation_unit(program)
        .expect("RegexCommand のコード生成が成功すること");
    unit.to_source(&config)
}

fn regex_command_program(command: IrRegexCommand) -> IrProgram {
    let span = command.span.clone();
    let return_type = command.java_type.clone();
    let block = IrExpression::Block {
        statements: vec![IrStatement::Return {
            value: Some(IrExpression::RegexCommand(command)),
            span: span.clone(),
        }],
        java_type: return_type.clone(),
        span: span.clone(),
    };

    let method = IrStatement::MethodDeclaration {
        name: "process".to_string(),
        java_name: None,
        type_parameters: vec![],
        parameters: vec![IrParameter {
            name: "text".to_string(),
            java_type: string_type(),
            modifiers: IrModifiers::default(),
            span: span.clone(),
        }],
        primitive_return: None,
        return_type: return_type.clone(),
        body: Some(block),
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            is_static: true,
            ..IrModifiers::default()
        },
        throws: vec![],
        span: span.clone(),
    };

    let class = IrStatement::ClassDeclaration {
        name: "RegexSamples".to_string(),
        type_parameters: vec![],
        superclass: None,
        interfaces: vec![],
        fields: vec![],
        methods: vec![method],
        nested_classes: vec![],
        modifiers: IrModifiers {
            visibility: IrVisibility::Public,
            ..IrModifiers::default()
        },
        span: span.clone(),
    };

    IrProgram {
        package: Some("regex.samples".to_string()),
        imports: vec![],
        type_declarations: vec![class],
        generic_metadata: Default::default(),
        conversion_metadata: Vec::new(),
        span,
    }
}

fn literal_replacement(text: &str, span: &Span) -> IrRegexReplacement {
    IrRegexReplacement::Literal(IrRegexLiteralReplacement {
        raw: text.to_string(),
        normalized: text.to_string(),
        segments: vec![IrRegexTemplateSegment::Text(text.to_string())],
        span: span.clone(),
    })
}

fn string_array_type() -> JavaType {
    JavaType::Array {
        element_type: Box::new(JavaType::string()),
        dimensions: 1,
    }
}

fn stream_type() -> JavaType {
    JavaType::Reference {
        name: "java.util.stream.Stream".to_string(),
        generic_args: vec![JavaType::Reference {
            name: "java.util.regex.MatchResult".to_string(),
            generic_args: vec![],
        }],
    }
}

fn match_boolean_type() -> JavaType {
    JavaType::Reference {
        name: "java.lang.Boolean".to_string(),
        generic_args: vec![],
    }
}

fn base_command(mode: RegexCommandMode) -> IrRegexCommand {
    let span = dummy_span();
    IrRegexCommand {
        mode,
        subject: Box::new(ir_identifier("text", &string_type())),
        pattern: Box::new(IrExpression::RegexPattern {
            pattern: "\\d+".to_string(),
            java_type: JavaType::pattern(),
            span: span.clone(),
        }),
        replacement: IrRegexReplacement::None,
        flags: Vec::new(),
        raw_flags: None,
        guard_strategy: RegexGuardStrategy::None,
        java_type: string_type(),
        requires_stream_materialization: false,
        span,
    }
}

#[test]
fn renders_replace_all_for_all_mode() {
    let mut command = base_command(RegexCommandMode::All);
    command.replacement = literal_replacement("NUM", &command.span);
    command.flags = vec![RegexFlag::CaseInsensitive];
    command.raw_flags = Some("i".to_string());

    let java = render_java(&regex_command_program(command));
    assert!(
        java.contains(".replaceAll("),
        "All モードでは replaceAll 呼び出しを生成する想定です:\n{java}"
    );
    assert!(
        java.contains("Pattern.compile"),
        "Pattern.compile 呼び出しが生成される想定です:\n{java}"
    );
}

#[test]
fn renders_additional_flag_mask() {
    let mut command = base_command(RegexCommandMode::All);
    command.replacement = literal_replacement("X", &command.span);
    command.flags = vec![
        RegexFlag::UnicodeCase,
        RegexFlag::UnixLines,
        RegexFlag::Comments,
        RegexFlag::Literal,
        RegexFlag::CanonEq,
    ];
    command.raw_flags = Some("udxLc".to_string());

    let java = render_java(&regex_command_program(command));
    assert!(
        java.contains(r#"Pattern.compile("\\d+", "#),
        "Pattern.compile 呼び出しに追加フラグが含まれる想定です:\n{java}"
    );
    for constant in [
        "Pattern.UNICODE_CASE",
        "Pattern.UNIX_LINES",
        "Pattern.COMMENTS",
        "Pattern.LITERAL",
        "Pattern.CANON_EQ",
    ] {
        assert!(
            java.contains(constant),
            "{constant} が Pattern.compile のビットマスクに含まれる想定です:\n{java}"
        );
    }
}

#[test]
fn renders_replace_first_for_first_mode() {
    let mut command = base_command(RegexCommandMode::First);
    command.replacement = literal_replacement("FOUND", &command.span);

    let java = render_java(&regex_command_program(command));
    assert!(
        java.contains(".replaceFirst("),
        "First モードでは replaceFirst 呼び出しを生成する想定です:\n{java}"
    );
}

#[test]
fn renders_matches_for_match_mode() {
    let mut command = base_command(RegexCommandMode::Match);
    command.java_type = match_boolean_type();
    command.flags = vec![RegexFlag::Multiline];
    command.raw_flags = Some("m".to_string());

    let java = render_java(&regex_command_program(command));
    assert!(
        java.contains(".matches()"),
        "Match モードでは matcher(...).matches() を生成する想定です:\n{java}"
    );
}

#[test]
fn renders_split_for_split_mode() {
    let mut command = base_command(RegexCommandMode::Split);
    command.java_type = string_array_type();
    command.pattern = Box::new(IrExpression::RegexPattern {
        pattern: ",\\s*".to_string(),
        java_type: JavaType::pattern(),
        span: command.span.clone(),
    });

    let java = render_java(&regex_command_program(command));
    assert!(
        java.contains(".split(") && java.contains(", -1"),
        "Split モードでは Pattern.split(subject, -1) を生成する想定です:\n{java}"
    );
}

#[test]
fn renders_results_for_iterate_stream_mode() {
    let mut command = base_command(RegexCommandMode::Iterate);
    command.java_type = stream_type();
    command.requires_stream_materialization = true;

    let java = render_java(&regex_command_program(command));
    assert!(
        java.contains(".results()"),
        "Iterate (Stream) モードでは matcher(...).results() を生成する想定です:\n{java}"
    );
    assert!(
        java.contains("import java.util.stream.Stream"),
        "Stream 利用時には java.util.stream.Stream の import が追加される想定です:\n{java}"
    );
}

#[test]
fn iterate_with_replacement_delegates_to_replace_all() {
    let mut command = base_command(RegexCommandMode::Iterate);
    command.replacement = literal_replacement("X", &command.span);
    command.java_type = string_type();

    let java = render_java(&regex_command_program(command));
    assert!(
        java.contains(".replaceAll("),
        "置換付き Iterate は replaceAll へフォールバックする想定です:\n{java}"
    );
    assert!(
        !java.contains(".results()"),
        "置換付き Iterate では matcher(...).results() が生成されない想定です:\n{java}"
    );
}

#[test]
fn adds_char_sequence_guard_for_object_subjects() {
    let mut command = base_command(RegexCommandMode::All);
    command.subject = Box::new(ir_identifier("text", &JavaType::object()));
    command.replacement = literal_replacement("MASK", &command.span);
    command.guard_strategy = RegexGuardStrategy::CaptureAndGuard {
        temp_name: Some("__guard".to_string()),
    };

    let mut program = regex_command_program(command);
    if let IrStatement::ClassDeclaration { methods, .. } = program
        .type_declarations
        .first_mut()
        .expect("クラス宣言が存在すること")
    {
        if let IrStatement::MethodDeclaration { parameters, .. } =
            methods.first_mut().expect("メソッド宣言が存在すること")
        {
            parameters[0].java_type = JavaType::object();
        }
    }

    let java = render_java(&program);
    assert!(
        java.contains("instanceof java.lang.CharSequence"),
        "Object 型の subject には CharSequence ガードを挿入する想定です:\n{java}"
    );
}
