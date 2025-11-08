use super::lowering_cases::lower_source;
use jv_ast::expression::{Argument, CallArgumentStyle};
use jv_ast::{BinaryOp, Expression, Literal, SequenceDelimiter, Statement, UnaryOp};

fn lower_statements(source: &str) -> Vec<Statement> {
    let result = lower_source(source);
    assert!(
        result.diagnostics.is_empty(),
        "予期しないローワリング診断が発生しました: {:?}",
        result.diagnostics
    );
    result.statements
}

fn assert_offset_array_structure(elements: &[Expression]) {
    assert_eq!(
        elements.len(),
        7,
        "オフセット配列は7要素で構築されるはずです"
    );

    match &elements[0] {
        Expression::Identifier(name, _) => assert_eq!(name, "i"),
        other => panic!("先頭要素が識別子ではありません: {:?}", other),
    }

    match &elements[1] {
        Expression::Binary { op, .. } if *op == BinaryOp::Add => {}
        other => panic!("括弧付き加算式が期待されます: {:?}", other),
    }

    match &elements[2] {
        Expression::Identifier(name, _) => assert_eq!(name, "i"),
        other => panic!("2番目の識別子が保持されていません: {:?}", other),
    }

    match &elements[3] {
        Expression::Unary { op, .. } if *op == UnaryOp::Plus => {}
        other => panic!("単項プラスの要素が欠落しています: {:?}", other),
    }

    match &elements[4] {
        Expression::Binary { op, .. } if *op == BinaryOp::Add => {}
        other => panic!("4番目の加算要素が期待と異なります: {:?}", other),
    }

    match &elements[5] {
        Expression::Identifier(name, _) => assert_eq!(name, "i"),
        other => panic!("終端直前の識別子が失われています: {:?}", other),
    }

    match &elements[6] {
        Expression::Unary { op, .. } if *op == UnaryOp::Plus => {}
        other => panic!("末尾の単項プラスが保持されていません: {:?}", other),
    }
}

#[test]
fn 配列のホワイトスペース区切りがカンマ配列と区別される() {
    let statements = lower_statements("val layout_first = [1 2 3]\nval single_second = [4]");

    let first = statements.get(0).expect("1番目の宣言が存在するはずです");
    let Statement::ValDeclaration {
        initializer: first_init,
        ..
    } = first
    else {
        panic!("val宣言が生成されませんでした: {:?}", first);
    };
    let Expression::Array {
        delimiter: first_delimiter,
        ..
    } = first_init
    else {
        panic!("配列リテラルを期待しました: {:?}", first_init);
    };
    assert_eq!(
        *first_delimiter,
        SequenceDelimiter::Whitespace,
        "空白区切りの配列リテラルはSequenceDelimiter::Whitespaceを保持する必要があります"
    );

    let second = statements.get(1).expect("2番目の宣言が存在するはずです");
    let Statement::ValDeclaration {
        initializer: second_init,
        ..
    } = second
    else {
        panic!("2番目もval宣言のはずです: {:?}", second);
    };
    let Expression::Array {
        delimiter: second_delimiter,
        ..
    } = second_init
    else {
        panic!("単一要素配列を期待しました: {:?}", second_init);
    };
    assert_eq!(
        *second_delimiter,
        SequenceDelimiter::Comma,
        "カンマ配列はSequenceDelimiter::Commaのまま維持される必要があります"
    );
}

#[test]
fn ラムダ引数のホワイトスペース区切りが保持される() {
    let statements = lower_statements("val sum = numbers.reduce { acc x -> acc + x }");
    let first = statements
        .first()
        .expect("reduce呼び出しの宣言が生成されるはずです");

    let Statement::ValDeclaration { initializer, .. } = first else {
        panic!("val宣言が得られませんでした: {:?}", first);
    };

    let Expression::Call { args, .. } = initializer else {
        panic!("reduce呼び出しの初期化式を期待しました: {:?}", initializer);
    };
    assert_eq!(args.len(), 1, "reduce呼び出しはラムダ1件を受け取るはずです");

    let Argument::Positional(Expression::Lambda { parameters, .. }) = &args[0] else {
        panic!(
            "ラムダ引数が位置引数として保持されるはずです: {:?}",
            args[0]
        );
    };
    assert_eq!(
        parameters.len(),
        2,
        "ホワイトスペース区切りのパラメータが2件取得されるはずです"
    );
    assert_eq!(parameters[0].name, "acc", "最初のパラメータ名が不一致です");
    assert_eq!(parameters[1].name, "x", "2番目のパラメータ名が不一致です");
}

#[test]
fn ラムダ内部の配列でも空白区切りメタデータを保持する() {
    let statements = lower_statements("val mapped = numbers.map { i -> [i (i +1) i +2 i+3 i +4] }");
    let first = statements
        .first()
        .expect("map呼び出しの宣言が生成されるはずです");

    let Statement::ValDeclaration { initializer, .. } = first else {
        panic!("val宣言が得られませんでした: {:?}", first);
    };

    let Expression::Call { args, .. } = initializer else {
        panic!("map呼び出しの初期化式を期待しました: {:?}", initializer);
    };
    assert_eq!(args.len(), 1, "map呼び出しは単一のラムダ引数を取るはずです");

    let Argument::Positional(Expression::Lambda { body, .. }) = &args[0] else {
        panic!("ラムダ引数が保持されていません: {:?}", args[0]);
    };

    let Expression::Array {
        elements,
        delimiter,
        ..
    } = body.as_ref()
    else {
        panic!("ラムダ本体が配列リテラルであるはずです: {:?}", body);
    };
    assert_eq!(
        *delimiter,
        SequenceDelimiter::Whitespace,
        "ラムダ本体の配列もSequenceDelimiter::Whitespaceを保持する必要があります"
    );
    assert_offset_array_structure(elements);
}

#[test]
fn ホワイトスペース引数呼び出しはメタデータを保持する() {
    let statements = lower_statements("val result = plot(1 2 3)");
    let first = statements
        .first()
        .expect("plot呼び出しの宣言が生成されるはずです");

    let Statement::ValDeclaration { initializer, .. } = first else {
        panic!("val宣言が得られませんでした: {:?}", first);
    };

    let Expression::Call {
        args,
        argument_metadata,
        ..
    } = initializer
    else {
        panic!("呼び出し式が期待されます: {:?}", initializer);
    };

    assert_eq!(args.len(), 3, "位置引数が3件維持されるはずです");
    for (index, argument) in args.iter().enumerate() {
        match argument {
            Argument::Positional(Expression::Literal(Literal::Number(value), _)) => {
                let expected = match index {
                    0 => "1",
                    1 => "2",
                    2 => "3",
                    other => panic!("想定外の引数位置です: {other}"),
                };
                assert_eq!(value, expected, "引数{index}の数値が不一致です");
            }
            other => panic!("数値リテラル以外の引数が混入しました: {:?}", other),
        }
    }

    assert_eq!(
        argument_metadata.style,
        CallArgumentStyle::Whitespace,
        "ホワイトスペース引数スタイルがCallArgumentStyle::Whitespaceとして記録される必要があります"
    );
    assert!(
        argument_metadata.separator_diagnostics.is_empty(),
        "ホワイトスペース引数では区切り診断が発生しないはずです"
    );
    assert!(
        !argument_metadata.used_commas,
        "空白レイアウトではused_commasがfalseのまま維持される必要があります"
    );
}
