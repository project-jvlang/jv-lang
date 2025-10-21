use core::mem;

use jv_lexer::{Token, TokenType};

/// Rowan 構文木で使用するノードおよびトークン種別。
#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SyntaxKind {
    /// 未知のノード/トークン。
    Unknown = 0,
    /// ソースファイルのルート。
    Root,
    /// `package` 宣言。
    PackageDeclaration,
    /// package 名。
    PackageName,
    /// `import` 宣言。
    ImportDeclaration,
    /// import のパス。
    ImportPath,
    /// import 宣言末尾の付加情報（ワイルドカードや別名）。
    ImportClause,
    /// import の `as` 句（将来拡張用）。
    ImportAlias,
    /// import のワイルドカード指定。
    ImportWildcard,
    /// `val` 宣言。
    ValDeclaration,
    /// `var` 宣言。
    VarDeclaration,
    /// バインディングパターン。
    BindingPattern,
    /// 型注釈。
    TypeAnnotation,
    /// 初期化子。
    InitializerClause,
    /// アノテーションの並び。
    AnnotationList,
    /// 単一のアノテーション。
    Annotation,
    /// アノテーションの引数リスト。
    AnnotationArgumentList,
    /// アノテーション引数。
    AnnotationArgument,
    /// ステートメントレベル修飾子のリスト。
    ModifierList,
    /// 単一の修飾子。
    Modifier,
    /// 一般的な式プレースホルダー。
    Expression,
    /// 修飾名。
    QualifiedName,
    /// 修飾名セグメント。
    QualifiedNameSegment,
    /// エラーノード。
    Error,
    /// 関数宣言。
    FunctionDeclaration,
    /// 型パラメータリスト。
    TypeParameterList,
    /// 単一の型パラメータ。
    TypeParameter,
    /// 関数パラメータリスト。
    FunctionParameterList,
    /// 関数パラメータ。
    FunctionParameter,
    /// 関数戻り値型注釈。
    FunctionReturnType,
    /// `where` 句。
    WhereClause,
    /// `where` 句の述語。
    WherePredicate,
    /// クラス宣言。
    ClassDeclaration,
    /// クラスボディ。
    ClassBody,
    /// ブロック文。
    Block,
    /// ブロック内部のステートメント列。
    StatementList,
    /// if 文。
    IfStatement,
    /// else 節。
    ElseClause,
    /// when 文。
    WhenStatement,
    /// when の分岐。
    WhenBranch,
    /// for 文。
    ForStatement,
    /// while 文。
    WhileStatement,
    /// do-while 文。
    DoWhileStatement,
    /// return 文。
    ReturnStatement,
    /// throw 文。
    ThrowStatement,
    /// break 文。
    BreakStatement,
    /// continue 文。
    ContinueStatement,
    /// パラメータ修飾子リスト。
    ParameterModifierList,
    /// パラメータ修飾子。
    ParameterModifier,
    /// ブロック内のエラーノード。
    BlockError,
    /// `package` キーワード。
    PackageKw,
    /// `import` キーワード。
    ImportKw,
    /// `val` キーワード。
    ValKw,
    /// `var` キーワード。
    VarKw,
    /// `null` キーワード。
    NullKw,
    /// `fun` キーワード。
    FunKw,
    /// `class` キーワード。
    ClassKw,
    /// `data` キーワード。
    DataKw,
    /// `when` キーワード。
    WhenKw,
    /// `where` キーワード。
    WhereKw,
    /// `if` キーワード。
    IfKw,
    /// `else` キーワード。
    ElseKw,
    /// `for` キーワード。
    ForKw,
    /// `in` キーワード。
    InKw,
    /// `while` キーワード。
    WhileKw,
    /// `do` キーワード。
    DoKw,
    /// `return` キーワード。
    ReturnKw,
    /// `throw` キーワード。
    ThrowKw,
    /// `break` キーワード。
    BreakKw,
    /// `continue` キーワード。
    ContinueKw,
    /// `true` キーワード。
    TrueKw,
    /// `false` キーワード。
    FalseKw,
    /// 真偽値リテラル。
    BooleanLiteral,
    /// 数値リテラル。
    NumberLiteral,
    /// 文字列リテラル。
    StringLiteral,
    /// 正規表現リテラル。
    RegexLiteral,
    /// 文字リテラル。
    CharacterLiteral,
    /// 識別子。
    Identifier,
    /// コロン。
    Colon,
    /// セミコロン。
    Semicolon,
    /// 代入演算子 `=`
    Assign,
    /// ドット。
    Dot,
    /// カンマ。
    Comma,
    /// レイアウトカンマ（レイアウトシーケンス用）。
    LayoutComma,
    /// 左丸括弧。
    LeftParen,
    /// 右丸括弧。
    RightParen,
    /// 左波括弧。
    LeftBrace,
    /// 右波括弧。
    RightBrace,
    /// 左角括弧。
    LeftBracket,
    /// 右角括弧。
    RightBracket,
    /// クエスチョン。
    Question,
    /// アットマーク。
    At,
    /// `->` 演算子。
    Arrow,
    /// `=>` 演算子。
    FatArrow,
    /// `::` 演算子。
    DoubleColon,
    /// `?.` 演算子。
    NullSafe,
    /// `?:` 演算子。
    Elvis,
    /// `..` 演算子。
    RangeExclusive,
    /// `..=` 演算子。
    RangeInclusive,
    /// 加算演算子。
    Plus,
    /// 減算演算子。
    Minus,
    /// 乗算演算子。
    Star,
    /// 除算演算子。
    Slash,
    /// 剰余演算子。
    Percent,
    /// 等価演算子。
    EqualEqual,
    /// 不等価演算子。
    NotEqual,
    /// 小なり演算子。
    Less,
    /// 以下演算子。
    LessEqual,
    /// 大なり演算子。
    Greater,
    /// 以上演算子。
    GreaterEqual,
    /// 論理積演算子。
    AndAnd,
    /// 論理和演算子。
    OrOr,
    /// 否定演算子。
    Bang,
    /// 文字列補間開始。
    StringStart,
    /// 文字列補間中間。
    StringMid,
    /// 文字列補間終了。
    StringEnd,
    /// ホワイトスペース。
    Whitespace,
    /// 改行。
    Newline,
    /// 行コメント。
    LineComment,
    /// ブロックコメント。
    BlockComment,
    /// JavaDocコメント。
    DocComment,
    /// ファイル終端。
    Eof,
}

impl SyntaxKind {
    /// `rowan::SyntaxKind` から安全に変換する。
    pub fn from_raw(raw: rowan::SyntaxKind) -> Self {
        Self::from_u16(raw.0)
    }

    /// `u16` から安全に変換する。
    pub fn from_u16(raw: u16) -> Self {
        if raw <= SyntaxKind::Eof as u16 {
            // SAFETY: すべての列挙子は `repr(u16)` で連続しており、有効範囲内のみを transmute する。
            unsafe { mem::transmute::<u16, SyntaxKind>(raw) }
        } else {
            SyntaxKind::Unknown
        }
    }

    /// トークンかどうかを判定する。
    pub const fn is_token(self) -> bool {
        (self as u16) >= SyntaxKind::PackageKw as u16
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        rowan::SyntaxKind(kind as u16)
    }
}

/// レキサートークンを Rowan トークンへ写像するための分類。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// 未知のトークン。
    Unknown,
    /// `package` キーワード。
    PackageKw,
    /// `import` キーワード。
    ImportKw,
    /// `val` キーワード。
    ValKw,
    /// `var` キーワード。
    VarKw,
    /// `null` キーワード。
    NullKw,
    /// `fun` キーワード。
    FunKw,
    /// `class` キーワード。
    ClassKw,
    /// `data` キーワード。
    DataKw,
    /// `when` キーワード。
    WhenKw,
    /// `where` キーワード。
    WhereKw,
    /// `if` キーワード。
    IfKw,
    /// `else` キーワード。
    ElseKw,
    /// `for` キーワード。
    ForKw,
    /// `in` キーワード。
    InKw,
    /// `while` キーワード。
    WhileKw,
    /// `do` キーワード。
    DoKw,
    /// `return` キーワード。
    ReturnKw,
    /// `throw` キーワード。
    ThrowKw,
    /// `break` キーワード。
    BreakKw,
    /// `continue` キーワード。
    ContinueKw,
    /// `true` キーワード。
    TrueKw,
    /// `false` キーワード。
    FalseKw,
    /// 真偽値リテラル。
    BooleanLiteral,
    /// 数値リテラル。
    NumberLiteral,
    /// 文字列リテラル。
    StringLiteral,
    /// 正規表現リテラル。
    RegexLiteral,
    /// 文字リテラル。
    CharacterLiteral,
    /// 識別子。
    Identifier,
    /// コロン。
    Colon,
    /// セミコロン。
    Semicolon,
    /// 代入演算子。
    Assign,
    /// ドット。
    Dot,
    /// カンマ。
    Comma,
    /// レイアウトカンマ。
    LayoutComma,
    /// 左丸括弧。
    LeftParen,
    /// 右丸括弧。
    RightParen,
    /// 左波括弧。
    LeftBrace,
    /// 右波括弧。
    RightBrace,
    /// 左角括弧。
    LeftBracket,
    /// 右角括弧。
    RightBracket,
    /// クエスチョン。
    Question,
    /// アットマーク。
    At,
    /// `->` 演算子。
    Arrow,
    /// `=>` 演算子。
    FatArrow,
    /// `::` 演算子。
    DoubleColon,
    /// `?.` 演算子。
    NullSafe,
    /// `?:` 演算子。
    Elvis,
    /// `..` 演算子。
    RangeExclusive,
    /// `..=` 演算子。
    RangeInclusive,
    /// 加算演算子。
    Plus,
    /// 減算演算子。
    Minus,
    /// 乗算演算子。
    Star,
    /// 除算演算子。
    Slash,
    /// 剰余演算子。
    Percent,
    /// 等価演算子。
    EqualEqual,
    /// 不等価演算子。
    NotEqual,
    /// 小なり演算子。
    Less,
    /// 以下演算子。
    LessEqual,
    /// 大なり演算子。
    Greater,
    /// 以上演算子。
    GreaterEqual,
    /// 論理積演算子。
    AndAnd,
    /// 論理和演算子。
    OrOr,
    /// 否定演算子。
    Bang,
    /// 文字列補間開始。
    StringStart,
    /// 文字列補間中間。
    StringMid,
    /// 文字列補間終了。
    StringEnd,
    /// ホワイトスペース。
    Whitespace,
    /// 改行。
    Newline,
    /// 行コメント。
    LineComment,
    /// ブロックコメント。
    BlockComment,
    /// JavaDoc コメント。
    DocComment,
    /// ファイル終端。
    Eof,
}

impl TokenKind {
    /// 対応する `SyntaxKind` を返す。
    pub const fn to_syntax(self) -> SyntaxKind {
        match self {
            TokenKind::Unknown => SyntaxKind::Unknown,
            TokenKind::PackageKw => SyntaxKind::PackageKw,
            TokenKind::ImportKw => SyntaxKind::ImportKw,
            TokenKind::ValKw => SyntaxKind::ValKw,
            TokenKind::VarKw => SyntaxKind::VarKw,
            TokenKind::NullKw => SyntaxKind::NullKw,
            TokenKind::FunKw => SyntaxKind::FunKw,
            TokenKind::ClassKw => SyntaxKind::ClassKw,
            TokenKind::DataKw => SyntaxKind::DataKw,
            TokenKind::WhenKw => SyntaxKind::WhenKw,
            TokenKind::WhereKw => SyntaxKind::WhereKw,
            TokenKind::IfKw => SyntaxKind::IfKw,
            TokenKind::ElseKw => SyntaxKind::ElseKw,
            TokenKind::ForKw => SyntaxKind::ForKw,
            TokenKind::InKw => SyntaxKind::InKw,
            TokenKind::WhileKw => SyntaxKind::WhileKw,
            TokenKind::DoKw => SyntaxKind::DoKw,
            TokenKind::ReturnKw => SyntaxKind::ReturnKw,
            TokenKind::ThrowKw => SyntaxKind::ThrowKw,
            TokenKind::BreakKw => SyntaxKind::BreakKw,
            TokenKind::ContinueKw => SyntaxKind::ContinueKw,
            TokenKind::TrueKw => SyntaxKind::TrueKw,
            TokenKind::FalseKw => SyntaxKind::FalseKw,
            TokenKind::BooleanLiteral => SyntaxKind::BooleanLiteral,
            TokenKind::NumberLiteral => SyntaxKind::NumberLiteral,
            TokenKind::StringLiteral => SyntaxKind::StringLiteral,
            TokenKind::RegexLiteral => SyntaxKind::RegexLiteral,
            TokenKind::CharacterLiteral => SyntaxKind::CharacterLiteral,
            TokenKind::Identifier => SyntaxKind::Identifier,
            TokenKind::Colon => SyntaxKind::Colon,
            TokenKind::Semicolon => SyntaxKind::Semicolon,
            TokenKind::Assign => SyntaxKind::Assign,
            TokenKind::Dot => SyntaxKind::Dot,
            TokenKind::Comma => SyntaxKind::Comma,
            TokenKind::LayoutComma => SyntaxKind::LayoutComma,
            TokenKind::LeftParen => SyntaxKind::LeftParen,
            TokenKind::RightParen => SyntaxKind::RightParen,
            TokenKind::LeftBrace => SyntaxKind::LeftBrace,
            TokenKind::RightBrace => SyntaxKind::RightBrace,
            TokenKind::LeftBracket => SyntaxKind::LeftBracket,
            TokenKind::RightBracket => SyntaxKind::RightBracket,
            TokenKind::Question => SyntaxKind::Question,
            TokenKind::At => SyntaxKind::At,
            TokenKind::Arrow => SyntaxKind::Arrow,
            TokenKind::FatArrow => SyntaxKind::FatArrow,
            TokenKind::DoubleColon => SyntaxKind::DoubleColon,
            TokenKind::NullSafe => SyntaxKind::NullSafe,
            TokenKind::Elvis => SyntaxKind::Elvis,
            TokenKind::RangeExclusive => SyntaxKind::RangeExclusive,
            TokenKind::RangeInclusive => SyntaxKind::RangeInclusive,
            TokenKind::Plus => SyntaxKind::Plus,
            TokenKind::Minus => SyntaxKind::Minus,
            TokenKind::Star => SyntaxKind::Star,
            TokenKind::Slash => SyntaxKind::Slash,
            TokenKind::Percent => SyntaxKind::Percent,
            TokenKind::EqualEqual => SyntaxKind::EqualEqual,
            TokenKind::NotEqual => SyntaxKind::NotEqual,
            TokenKind::Less => SyntaxKind::Less,
            TokenKind::LessEqual => SyntaxKind::LessEqual,
            TokenKind::Greater => SyntaxKind::Greater,
            TokenKind::GreaterEqual => SyntaxKind::GreaterEqual,
            TokenKind::AndAnd => SyntaxKind::AndAnd,
            TokenKind::OrOr => SyntaxKind::OrOr,
            TokenKind::Bang => SyntaxKind::Bang,
            TokenKind::StringStart => SyntaxKind::StringStart,
            TokenKind::StringMid => SyntaxKind::StringMid,
            TokenKind::StringEnd => SyntaxKind::StringEnd,
            TokenKind::Whitespace => SyntaxKind::Whitespace,
            TokenKind::Newline => SyntaxKind::Newline,
            TokenKind::LineComment => SyntaxKind::LineComment,
            TokenKind::BlockComment => SyntaxKind::BlockComment,
            TokenKind::DocComment => SyntaxKind::DocComment,
            TokenKind::Eof => SyntaxKind::Eof,
        }
    }

    /// `jv_lexer::Token` から `TokenKind` を導出する。
    pub fn from_token(token: &Token) -> Self {
        Self::from_token_type(&token.token_type)
    }

    /// `jv_lexer::TokenType` から `TokenKind` を導出する。
    pub fn from_token_type(token_type: &TokenType) -> Self {
        match token_type {
            TokenType::Package => TokenKind::PackageKw,
            TokenType::Import => TokenKind::ImportKw,
            TokenType::Val => TokenKind::ValKw,
            TokenType::Var => TokenKind::VarKw,
            TokenType::Null => TokenKind::NullKw,
            TokenType::Fun => TokenKind::FunKw,
            TokenType::Class => TokenKind::ClassKw,
            TokenType::Data => TokenKind::DataKw,
            TokenType::When => TokenKind::WhenKw,
            TokenType::Where => TokenKind::WhereKw,
            TokenType::If => TokenKind::IfKw,
            TokenType::Else => TokenKind::ElseKw,
            TokenType::For => TokenKind::ForKw,
            TokenType::In => TokenKind::InKw,
            TokenType::While => TokenKind::WhileKw,
            TokenType::Do => TokenKind::DoKw,
            TokenType::Return => TokenKind::ReturnKw,
            TokenType::Throw => TokenKind::ThrowKw,
            TokenType::Break => TokenKind::BreakKw,
            TokenType::Continue => TokenKind::ContinueKw,
            TokenType::True => TokenKind::TrueKw,
            TokenType::False => TokenKind::FalseKw,
            TokenType::Boolean(_) => TokenKind::BooleanLiteral,
            TokenType::Number(_) => TokenKind::NumberLiteral,
            TokenType::String(_) | TokenType::StringInterpolation(_) => TokenKind::StringLiteral,
            TokenType::RegexLiteral(_) => TokenKind::RegexLiteral,
            TokenType::Character(_) => TokenKind::CharacterLiteral,
            TokenType::Identifier(_) => TokenKind::Identifier,
            TokenType::Colon => TokenKind::Colon,
            TokenType::Semicolon => TokenKind::Semicolon,
            TokenType::Assign => TokenKind::Assign,
            TokenType::Dot => TokenKind::Dot,
            TokenType::Comma => TokenKind::Comma,
            TokenType::LayoutComma => TokenKind::LayoutComma,
            TokenType::LeftParen => TokenKind::LeftParen,
            TokenType::RightParen => TokenKind::RightParen,
            TokenType::LeftBrace => TokenKind::LeftBrace,
            TokenType::RightBrace => TokenKind::RightBrace,
            TokenType::LeftBracket => TokenKind::LeftBracket,
            TokenType::RightBracket => TokenKind::RightBracket,
            TokenType::Question => TokenKind::Question,
            TokenType::At => TokenKind::At,
            TokenType::Arrow => TokenKind::Arrow,
            TokenType::FatArrow => TokenKind::FatArrow,
            TokenType::DoubleColon => TokenKind::DoubleColon,
            TokenType::NullSafe => TokenKind::NullSafe,
            TokenType::Elvis => TokenKind::Elvis,
            TokenType::RangeExclusive => TokenKind::RangeExclusive,
            TokenType::RangeInclusive => TokenKind::RangeInclusive,
            TokenType::Plus => TokenKind::Plus,
            TokenType::Minus => TokenKind::Minus,
            TokenType::Multiply => TokenKind::Star,
            TokenType::Divide => TokenKind::Slash,
            TokenType::Modulo => TokenKind::Percent,
            TokenType::Equal => TokenKind::EqualEqual,
            TokenType::NotEqual => TokenKind::NotEqual,
            TokenType::Less => TokenKind::Less,
            TokenType::LessEqual => TokenKind::LessEqual,
            TokenType::Greater => TokenKind::Greater,
            TokenType::GreaterEqual => TokenKind::GreaterEqual,
            TokenType::And => TokenKind::AndAnd,
            TokenType::Or => TokenKind::OrOr,
            TokenType::Not => TokenKind::Bang,
            TokenType::StringStart => TokenKind::StringStart,
            TokenType::StringMid => TokenKind::StringMid,
            TokenType::StringEnd => TokenKind::StringEnd,
            TokenType::Whitespace(_) => TokenKind::Whitespace,
            TokenType::Newline => TokenKind::Newline,
            TokenType::LineComment(_) => TokenKind::LineComment,
            TokenType::BlockComment(_) => TokenKind::BlockComment,
            TokenType::JavaDocComment(_) => TokenKind::DocComment,
            TokenType::Eof => TokenKind::Eof,
            TokenType::Invalid(_) => TokenKind::Unknown,
        }
    }

    /// トークンがトリビア（空白・コメント等）か判定する。
    pub const fn is_trivia(self) -> bool {
        matches!(
            self,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LineComment
                | TokenKind::BlockComment
                | TokenKind::DocComment
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::STATEMENT_GRAMMAR;
    use std::collections::{BTreeSet, HashMap};
    use ungrammar::Grammar;

    const NODE_KIND_MAP: &[(&str, SyntaxKind)] = &[
        ("Root", SyntaxKind::Root),
        ("PackageDeclaration", SyntaxKind::PackageDeclaration),
        ("PackageName", SyntaxKind::PackageName),
        ("ImportDeclaration", SyntaxKind::ImportDeclaration),
        ("ImportPath", SyntaxKind::ImportPath),
        ("ImportClause", SyntaxKind::ImportClause),
        ("ImportWildcard", SyntaxKind::ImportWildcard),
        ("StatementList", SyntaxKind::StatementList),
        ("ValDeclaration", SyntaxKind::ValDeclaration),
        ("VarDeclaration", SyntaxKind::VarDeclaration),
        ("BindingPattern", SyntaxKind::BindingPattern),
        ("TypeAnnotation", SyntaxKind::TypeAnnotation),
        ("InitializerClause", SyntaxKind::InitializerClause),
        ("AnnotationList", SyntaxKind::AnnotationList),
        ("Annotation", SyntaxKind::Annotation),
        ("AnnotationArgumentList", SyntaxKind::AnnotationArgumentList),
        ("AnnotationArgument", SyntaxKind::AnnotationArgument),
        ("ModifierList", SyntaxKind::ModifierList),
        ("Modifier", SyntaxKind::Modifier),
        ("FunctionDeclaration", SyntaxKind::FunctionDeclaration),
        ("FunctionParameterList", SyntaxKind::FunctionParameterList),
        ("FunctionParameter", SyntaxKind::FunctionParameter),
        ("ParameterModifierList", SyntaxKind::ParameterModifierList),
        ("ParameterModifier", SyntaxKind::ParameterModifier),
        ("FunctionReturnType", SyntaxKind::FunctionReturnType),
        ("TypeParameterList", SyntaxKind::TypeParameterList),
        ("TypeParameter", SyntaxKind::TypeParameter),
        ("WhereClause", SyntaxKind::WhereClause),
        ("WherePredicate", SyntaxKind::WherePredicate),
        ("ClassDeclaration", SyntaxKind::ClassDeclaration),
        ("ClassBody", SyntaxKind::ClassBody),
        ("Block", SyntaxKind::Block),
        ("IfStatement", SyntaxKind::IfStatement),
        ("ElseClause", SyntaxKind::ElseClause),
        ("WhenStatement", SyntaxKind::WhenStatement),
        ("WhenBranch", SyntaxKind::WhenBranch),
        ("ForStatement", SyntaxKind::ForStatement),
        ("WhileStatement", SyntaxKind::WhileStatement),
        ("DoWhileStatement", SyntaxKind::DoWhileStatement),
        ("ReturnStatement", SyntaxKind::ReturnStatement),
        ("ThrowStatement", SyntaxKind::ThrowStatement),
        ("BreakStatement", SyntaxKind::BreakStatement),
        ("ContinueStatement", SyntaxKind::ContinueStatement),
        ("QualifiedName", SyntaxKind::QualifiedName),
        ("QualifiedNameSegment", SyntaxKind::QualifiedNameSegment),
        ("Expression", SyntaxKind::Expression),
        ("PackageKw", SyntaxKind::PackageKw),
        ("ImportKw", SyntaxKind::ImportKw),
        ("ValKw", SyntaxKind::ValKw),
        ("VarKw", SyntaxKind::VarKw),
        ("FunKw", SyntaxKind::FunKw),
        ("ClassKw", SyntaxKind::ClassKw),
        ("DataKw", SyntaxKind::DataKw),
        ("ReturnKw", SyntaxKind::ReturnKw),
        ("ThrowKw", SyntaxKind::ThrowKw),
        ("BreakKw", SyntaxKind::BreakKw),
        ("ContinueKw", SyntaxKind::ContinueKw),
        ("IfKw", SyntaxKind::IfKw),
        ("ElseKw", SyntaxKind::ElseKw),
        ("WhenKw", SyntaxKind::WhenKw),
        ("ForKw", SyntaxKind::ForKw),
        ("InKw", SyntaxKind::InKw),
        ("WhileKw", SyntaxKind::WhileKw),
        ("DoKw", SyntaxKind::DoKw),
        ("WhereKw", SyntaxKind::WhereKw),
        ("At", SyntaxKind::At),
        ("Dot", SyntaxKind::Dot),
        ("Star", SyntaxKind::Star),
        ("Assign", SyntaxKind::Assign),
        ("Colon", SyntaxKind::Colon),
        ("Comma", SyntaxKind::Comma),
        ("LeftParen", SyntaxKind::LeftParen),
        ("RightParen", SyntaxKind::RightParen),
        ("LeftBrace", SyntaxKind::LeftBrace),
        ("RightBrace", SyntaxKind::RightBrace),
        ("Less", SyntaxKind::Less),
        ("Greater", SyntaxKind::Greater),
        ("FatArrow", SyntaxKind::FatArrow),
        ("Identifier", SyntaxKind::Identifier),
        ("ExpressionToken", SyntaxKind::Expression),
    ];

    #[test]
    fn grammar_nodes_have_matching_syntax_kinds() {
        let grammar: Grammar = STATEMENT_GRAMMAR.parse().expect("statement grammar parses");

        let mapping: HashMap<&str, SyntaxKind> = NODE_KIND_MAP.iter().copied().collect();

        let mut missing = Vec::new();
        for node in grammar.iter() {
            let name = &grammar[node].name;
            if !mapping.contains_key(name.as_str()) {
                missing.push(name.clone());
            }
        }

        assert!(
            missing.is_empty(),
            "SyntaxKind mapping missing entries for grammar nodes: {:?}",
            missing
        );

        let defined: BTreeSet<&str> = mapping.keys().copied().collect();
        let actual: BTreeSet<String> = grammar
            .iter()
            .map(|node| grammar[node].name.clone())
            .collect();

        for name in defined {
            assert!(
                actual.contains(name),
                "Mapping contains `{}` which is absent from grammar",
                name
            );
        }
    }
}

impl From<TokenKind> for SyntaxKind {
    fn from(kind: TokenKind) -> Self {
        kind.to_syntax()
    }
}

impl From<TokenKind> for rowan::SyntaxKind {
    fn from(kind: TokenKind) -> Self {
        rowan::SyntaxKind(kind.to_syntax() as u16)
    }
}
