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
fn mixed_array_delimiters_emit_jv1007() {
    let result = Parser::parse("val numbers = [1, 2 3]");

    match result {
        Err(ParseError::Syntax { message, .. }) => {
            assert!(
                message.contains("JV1007"),
                "expected JV1007 diagnostic in message, got {}",
                message
            );
        }
        other => panic!(
            "expected syntax error for mixed delimiters, got {:?}",
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
fn whitespace_arguments_with_comma_emit_jv1009() {
    let result = Parser::parse("val result = plot(1, 2 3)");

    match result {
        Err(ParseError::Syntax { message, .. }) => {
            assert!(
                message.contains("JV1009"),
                "expected JV1009 diagnostic in message, got {}",
                message
            );
        }
        other => panic!(
            "expected syntax error for mixed whitespace/comma arguments, got {:?}",
            other
        ),
    }
}
