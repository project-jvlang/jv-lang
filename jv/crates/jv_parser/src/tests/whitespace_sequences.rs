use super::support::parse_program;
use jv_ast::{
    Argument, BinaryOp, CallArgumentStyle, Expression, SequenceDelimiter, Statement, UnaryOp,
};

fn assert_offset_array_structure(elements: &[Expression]) {
    assert_eq!(
        elements.len(),
        7,
        "layout-delimited array should surface seven expression nodes"
    );

    match &elements[0] {
        Expression::Identifier(name, _) => assert_eq!(name, "i"),
        other => panic!("expected leading identifier element, found {:?}", other),
    }

    match &elements[1] {
        Expression::Binary { op, .. } if *op == BinaryOp::Add => {}
        other => panic!(
            "expected parenthesized addition element at position 1, found {:?}",
            other
        ),
    }

    match &elements[2] {
        Expression::Identifier(name, _) => assert_eq!(name, "i"),
        other => panic!("expected identifier before unary plus, found {:?}", other),
    }

    match &elements[3] {
        Expression::Unary { op, .. } if *op == UnaryOp::Plus => {}
        other => panic!("expected unary plus offset element, found {:?}", other),
    }

    match &elements[4] {
        Expression::Binary { op, .. } if *op == BinaryOp::Add => {}
        other => panic!(
            "expected tight addition element at position 4, found {:?}",
            other
        ),
    }

    match &elements[5] {
        Expression::Identifier(name, _) => assert_eq!(name, "i"),
        other => panic!(
            "expected trailing identifier before last unary plus, found {:?}",
            other
        ),
    }

    match &elements[6] {
        Expression::Unary { op, .. } if *op == UnaryOp::Plus => {}
        other => panic!(
            "expected final unary plus offset element, found {:?}",
            other
        ),
    }
}

#[test]
fn whitespace_array_does_not_leak_into_comma_array() {
    let program = parse_program("val layout_first = [1 2 3]\nval single_second = [4]");

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
fn whitespace_array_with_expression_elements_preserves_structure() {
    let program = parse_program("val offsets = [i (i +1) i +2 i+3 i +4]");
    let statement = program
        .statements
        .get(0)
        .expect("expected offsets declaration");

    let Statement::ValDeclaration { initializer, .. } = statement else {
        panic!("expected val declaration for offsets");
    };

    let Expression::Array {
        elements,
        delimiter,
        ..
    } = initializer
    else {
        panic!("expected array initializer for offsets");
    };

    assert_eq!(*delimiter, SequenceDelimiter::Whitespace);
    assert_offset_array_structure(elements);
}

#[test]
fn whitespace_array_inside_lambda_body_preserves_delimiter() {
    let program = parse_program("val mapped = numbers.map { i -> [i (i +1) i +2 i+3 i +4] }");
    let statement = program
        .statements
        .get(0)
        .expect("expected mapped declaration");

    let Statement::ValDeclaration { initializer, .. } = statement else {
        panic!("expected val declaration for mapped");
    };

    let Expression::Call { args, .. } = initializer else {
        panic!("expected call expression for mapped initializer");
    };

    assert_eq!(args.len(), 1, "expected single lambda argument to map");

    let Argument::Positional(Expression::Lambda { body, .. }) = &args[0] else {
        panic!("expected lambda argument to map call");
    };

    let Expression::Array {
        elements,
        delimiter,
        ..
    } = body.as_ref()
    else {
        panic!("expected lambda body to be an array literal");
    };

    assert_eq!(*delimiter, SequenceDelimiter::Whitespace);
    assert_offset_array_structure(elements);
}

#[test]
fn whitespace_array_lambda_allows_binary_operator_elements() {
    let program =
        parse_program("val flattened = values.flatMap { value -> [value value + 1] }");
    let statement = program
        .statements
        .get(0)
        .expect("expected flattened declaration");

    let Statement::ValDeclaration { initializer, .. } = statement else {
        panic!("expected val declaration for flattened");
    };

    let Expression::Call { args, .. } = initializer else {
        panic!("expected call expression for flattened initializer");
    };

    assert_eq!(args.len(), 1, "expected single lambda argument to flatMap");

    let Argument::Positional(Expression::Lambda { body, .. }) = &args[0] else {
        panic!("expected lambda argument to flatMap call");
    };

    let Expression::Array {
        elements,
        delimiter,
        ..
    } = body.as_ref()
    else {
        panic!("expected lambda body to be an array literal");
    };

    assert_eq!(*delimiter, SequenceDelimiter::Whitespace);
    assert_eq!(elements.len(), 2, "expected two array elements");

    match &elements[0] {
        Expression::Identifier(name, _) => assert_eq!(name, "value"),
        other => panic!("expected identifier element, found {:?}", other),
    }

    match &elements[1] {
        Expression::Binary { op, .. } => {
            assert_eq!(*op, BinaryOp::Add, "expected addition in second element");
        }
        other => panic!("expected binary addition element, found {:?}", other),
    }
}

#[test]
fn whitespace_call_does_not_force_following_calls_to_layout_style() {
    let program = parse_program("val layout_call = plot(1 2 3)\nval regular_call = plot(value)");

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

#[test]
fn whitespace_call_supports_parenthesized_static_access() {
    let program = parse_program(
        "fun builder() {
    return StreamSupport.stream(
        values()
        false
    )
}",
    );

    let statement = program
        .statements
        .iter()
        .find_map(|stmt| match stmt {
            Statement::FunctionDeclaration { body, .. } => Some(body),
            _ => None,
        })
        .expect("expected function declaration");

    let Expression::Block { statements, .. } = statement.as_ref() else {
        panic!("expected block body");
    };

    let return_expr = match statements.first() {
        Some(Statement::Return { value, .. }) => value.as_ref().expect("return expression"),
        other => panic!("expected return statement, found {:?}", other),
    };

    let Expression::Call {
        args,
        argument_metadata,
        ..
    } = return_expr
    else {
        panic!("expected return call expression");
    };

    assert_eq!(argument_metadata.style, CallArgumentStyle::Whitespace);
    assert_eq!(
        args.len(),
        2,
        "expected two positional arguments parsed via whitespace"
    );
}
