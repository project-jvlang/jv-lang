use super::*;
use jv_ast::RegexLiteral;
use jv_ast::{RegexCommandMode, RegexCommandModeOrigin, RegexFlag};
use jv_ir::{IrRegexLiteralReplacement, IrRegexReplacement, IrRegexTemplateSegment};

fn regex_literal(pattern: &str) -> RegexLiteral {
    RegexLiteral {
        pattern: pattern.to_string(),
        raw: format!("/{pattern}/"),
        span: dummy_span(),
        origin: None,
        const_key: None,
        template_segments: Vec::new(),
    }
}

fn regex_command_expression(
    mode: RegexCommandMode,
    replacement: Option<IrRegexReplacement>,
    flags: Vec<RegexFlag>,
) -> IrExpression {
    let java_type = match mode {
        RegexCommandMode::All | RegexCommandMode::First => JavaType::string(),
        RegexCommandMode::Match => JavaType::boolean(),
        RegexCommandMode::Split => JavaType::Array {
            element_type: Box::new(JavaType::string()),
            dimensions: 1,
        },
        RegexCommandMode::Iterate => {
            if replacement.is_some() {
                JavaType::string()
            } else {
                JavaType::Reference {
                    name: "java.util.stream.Stream".to_string(),
                    generic_args: vec![],
                }
            }
        }
    };

    IrExpression::RegexCommand {
        mode,
        mode_origin: RegexCommandModeOrigin::ExplicitToken,
        subject: Box::new(IrExpression::Literal(
            Literal::String("input".to_string()),
            dummy_span(),
        )),
        pattern: regex_literal("\\d+"),
        pattern_expr: None,
        replacement,
        flags,
        java_type,
        span: dummy_span(),
    }
}

fn literal_replacement(value: &str) -> IrRegexReplacement {
    IrRegexReplacement::Literal(IrRegexLiteralReplacement {
        normalized: value.to_string(),
        segments: vec![IrRegexTemplateSegment::Text(value.to_string())],
        span: dummy_span(),
    })
}

#[test]
fn regex_match_generates_matcher_call() {
    let mut generator = JavaCodeGenerator::new();
    let expr = regex_command_expression(RegexCommandMode::Match, None, Vec::new());

    let rendered = generator
        .generate_expression(&expr)
        .expect("regex match expression");

    assert_eq!(
        rendered,
        r#"Pattern.compile("\\d+").matcher("input").matches()"#
    );
}

#[test]
fn regex_replace_all_uses_replace_all() {
    let mut generator = JavaCodeGenerator::new();
    let expr = regex_command_expression(
        RegexCommandMode::All,
        Some(literal_replacement("value")),
        Vec::new(),
    );

    let rendered = generator
        .generate_expression(&expr)
        .expect("regex replace all expression");

    assert_eq!(
        rendered,
        r#"Pattern.compile("\\d+").matcher("input").replaceAll("value")"#
    );
}

#[test]
fn regex_split_invokes_pattern_split() {
    let mut generator = JavaCodeGenerator::new();
    let expr = regex_command_expression(RegexCommandMode::Split, None, Vec::new());

    let rendered = generator
        .generate_expression(&expr)
        .expect("regex split expression");

    assert_eq!(rendered, r#"Pattern.compile("\\d+").split("input")"#);
}

#[test]
fn regex_iterate_without_replacement_returns_results_stream() {
    let mut generator = JavaCodeGenerator::new();
    let expr = regex_command_expression(RegexCommandMode::Iterate, None, Vec::new());

    let rendered = generator
        .generate_expression(&expr)
        .expect("regex iterate expression");

    assert_eq!(
        rendered,
        r#"Pattern.compile("\\d+").matcher("input").results()"#
    );
}

#[test]
fn regex_flags_are_combined_when_present() {
    let mut generator = JavaCodeGenerator::new();
    let expr = regex_command_expression(
        RegexCommandMode::Match,
        None,
        vec![RegexFlag::CaseInsensitive, RegexFlag::Multiline],
    );

    let rendered = generator
        .generate_expression(&expr)
        .expect("regex match with flags");

    assert_eq!(
        rendered,
        r#"Pattern.compile("\\d+", Pattern.CASE_INSENSITIVE | Pattern.MULTILINE).matcher("input").matches()"#
    );
}
