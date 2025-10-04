use crate::{ParseError, Parser};

#[test]
fn parse_error_reports_span_for_unexpected_token() {
    let result = Parser::parse("val=");

    match result {
        Err(ParseError::Syntax { span, .. }) => {
            assert_eq!(span.start_line, 1);
            assert_eq!(span.end_line, 1);
            assert_eq!(span.start_column, 4);
            assert_eq!(span.end_column, 5);
        }
        other => panic!("expected syntax error with span, got {:?}", other),
    }
}

#[test]
fn parse_error_reports_span_for_unexpected_eof() {
    let result = Parser::parse("val x =");

    match result {
        Err(ParseError::UnexpectedEof { span }) => {
            assert_eq!(span.start_line, 1);
            assert_eq!(span.end_line, 1);
            assert_eq!(span.start_column, 8);
            assert_eq!(span.end_column, 8);
        }
        other => panic!("expected unexpected eof error with span, got {:?}", other),
    }
}

#[test]
fn comma_separated_array_emits_jv2101() {
    let result = Parser::parse("val numbers = [1, 2 3]");

    match result {
        Err(ParseError::Syntax { message, .. }) => {
            assert!(
                message.contains("JV2101"),
                "expected JV2101 diagnostic in message, got {}",
                message
            );
        }
        other => panic!(
            "expected syntax error for comma-separated array literal, got {:?}",
            other
        ),
    }
}

#[test]
fn whitespace_arguments_with_named_emit_jv1009() {
    let result = Parser::parse("val result = plot(x=1 2)");

    match result {
        Err(ParseError::Syntax { message, span }) => {
            assert!(
                message.contains("JV1009"),
                "expected JV1009 diagnostic in message, got {}",
                message
            );
            assert_eq!(span.start_line, 1);
            assert_eq!(span.end_line, 1);
            assert_eq!(span.start_column, 19);
            assert_eq!(span.end_column, 24);
        }
        other => panic!(
            "expected syntax error for whitespace named arguments, got {:?}",
            other
        ),
    }
}

#[test]
fn whitespace_arguments_with_comma_emit_jv2102() {
    let result = Parser::parse("val result = plot(1, 2 3)");

    match result {
        Err(ParseError::Syntax { message, .. }) => {
            assert!(
                message.contains("JV2102"),
                "expected JV2102 diagnostic in message, got {}",
                message
            );
        }
        other => panic!(
            "expected syntax error for mixed whitespace/comma arguments, got {:?}",
            other
        ),
    }
}

#[test]
fn legacy_while_loop_reports_e_loop_001() {
    let result = Parser::parse("while (true) { val x = 1 }");

    match result {
        Err(ParseError::Syntax { message, .. }) => {
            assert!(
                message.contains("E_LOOP_001"),
                "expected E_LOOP_001 diagnostic for legacy while loop, got {}",
                message
            );
        }
        other => panic!("expected syntax error for while loop, got {:?}", other),
    }
}

#[test]
fn legacy_do_while_loop_reports_e_loop_001() {
    let result = Parser::parse("do { val x = 1 } while (x < 10)");

    match result {
        Err(ParseError::Syntax { message, .. }) => {
            assert!(
                message.contains("E_LOOP_001"),
                "expected E_LOOP_001 diagnostic for legacy do-while loop, got {}",
                message
            );
        }
        other => panic!("expected syntax error for do-while loop, got {:?}", other),
    }
}

#[test]
fn legacy_if_expression_reports_jv3103() {
    let result = Parser::parse("val result = if (true) 1 else 0");

    match result {
        Err(ParseError::Syntax { message, .. }) => {
            assert!(
                message.contains("JV3103"),
                "expected JV3103 diagnostic for if expression, got {}",
                message
            );
            assert!(
                message.contains("when"),
                "expected guidance to use when expression, got {}",
                message
            );
            assert!(
                message.contains("Quick Fix"),
                "expected Quick Fix hint in diagnostic message, got {}",
                message
            );
        }
        other => panic!("expected syntax error for if expression, got {:?}", other),
    }
}
