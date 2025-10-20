use jv_ast::{Program, Span, Statement};

/// `Program` の読み取り専用ビュー。
///
/// 直接`Program`構造体に依存せず、必要なプロパティのみを公開する。
#[derive(Debug, Clone, Copy)]
pub struct ProgramView<'a> {
    program: &'a Program,
}

impl<'a> ProgramView<'a> {
    pub(crate) fn new(program: &'a Program) -> Self {
        Self { program }
    }

    /// `package`宣言を取得する。
    pub fn package(&self) -> Option<&'a str> {
        self.program.package.as_deref()
    }

    /// `import`文の一覧を返す。
    pub fn imports(&self) -> &'a [Statement] {
        &self.program.imports
    }

    /// トップレベルステートメントの一覧を返す。
    pub fn statements(&self) -> &'a [Statement] {
        &self.program.statements
    }

    /// プログラム全体のスパンを取得する。
    pub fn span(&self) -> &'a Span {
        &self.program.span
    }
}
