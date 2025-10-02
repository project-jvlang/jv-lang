use super::support::parse_program;
use jv_ast::{CallArgumentStyle, Expression, SequenceDelimiter, Statement};

#[test]
fn whitespace_array_does_not_leak_into_comma_array() {
    let program = parse_program("val layout_first = [1 2 3]\nval comma_second = [4, 5, 6]");

    let first = program
        .statements
        .get(0)
        .expect("expected first declaration");
    let second = program
        .statements
        .get(1)
        .expect("expected second declaration");

    match first {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Array { delimiter, .. } => {
                assert_eq!(*delimiter, SequenceDelimiter::Whitespace);
            }
            other => panic!("expected array initializer, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }

    match second {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Array { delimiter, .. } => {
                assert_eq!(*delimiter, SequenceDelimiter::Comma);
            }
            other => panic!("expected array initializer, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}

#[test]
fn whitespace_call_does_not_force_following_calls_to_layout_style() {
    let program = parse_program("val layout_call = plot(1 2 3)\nval comma_call = plot(4, 5, 6)");

    let first = program
        .statements
        .get(0)
        .expect("expected first declaration");
    let second = program
        .statements
        .get(1)
        .expect("expected second declaration");

    match first {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call {
                argument_metadata, ..
            } => {
                assert_eq!(argument_metadata.style, CallArgumentStyle::Whitespace);
            }
            other => panic!("expected call initializer, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }

    match second {
        Statement::ValDeclaration { initializer, .. } => match initializer {
            Expression::Call {
                argument_metadata, ..
            } => {
                assert_eq!(argument_metadata.style, CallArgumentStyle::Comma);
            }
            other => panic!("expected call initializer, found {:?}", other),
        },
        other => panic!("expected val declaration, found {:?}", other),
    }
}
