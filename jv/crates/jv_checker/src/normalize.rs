use jv_ast::{Expression, Modifiers, Program, Statement, ValBindingOrigin};

/// parser2入力で失われがちな暗黙代入を`val`宣言へ正規化する。
/// - `x = expr` を `val x = expr` に変換し、`origin` を `Implicit` で付与する。
/// - それ以外のステートメントはそのまま。
pub fn normalize_implicit_assignments(program: &Program) -> Program {
    let mut cloned = program.clone();
    cloned.statements = cloned
        .statements
        .iter()
        .cloned()
        .map(|statement| match statement {
            Statement::Assignment {
                target: Expression::Identifier(name, _),
                value,
                span,
                ..
            } => Statement::ValDeclaration {
                name,
                binding: None,
                type_annotation: None,
                initializer: value,
                modifiers: Modifiers::default(),
                origin: ValBindingOrigin::Implicit,
                span,
            },
            other => other,
        })
        .collect();
    cloned
}
