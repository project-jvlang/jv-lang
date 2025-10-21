use jv_lexer::{Token, TokenType};

/// Rowan 構文木で使用するノード・トークン種別。
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
    /// import の `as` 句（将来拡張用）。
    ImportAlias,
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
    /// 一般的な式プレースホルダー。
    Expression,
    /// 修飾名。
    QualifiedName,
    /// 修飾名セグメント。
    QualifiedNameSegment,
    /// エラーノード。
    Error,
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
    /// 真偽値リテラル。
    BooleanLiteral,
    /// 数値リテラル。
    NumberLiteral,
    /// 文字列リテラル。
    StringLiteral,
    /// 正規表現リテラル。
    RegexLiteral,
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
    pub const fn from_u16(raw: u16) -> Self {
        match raw {
            x if x == SyntaxKind::Unknown as u16 => SyntaxKind::Unknown,
            x if x == SyntaxKind::Root as u16 => SyntaxKind::Root,
            x if x == SyntaxKind::PackageDeclaration as u16 => SyntaxKind::PackageDeclaration,
            x if x == SyntaxKind::PackageName as u16 => SyntaxKind::PackageName,
            x if x == SyntaxKind::ImportDeclaration as u16 => SyntaxKind::ImportDeclaration,
            x if x == SyntaxKind::ImportPath as u16 => SyntaxKind::ImportPath,
            x if x == SyntaxKind::ImportAlias as u16 => SyntaxKind::ImportAlias,
            x if x == SyntaxKind::ValDeclaration as u16 => SyntaxKind::ValDeclaration,
            x if x == SyntaxKind::VarDeclaration as u16 => SyntaxKind::VarDeclaration,
            x if x == SyntaxKind::BindingPattern as u16 => SyntaxKind::BindingPattern,
            x if x == SyntaxKind::TypeAnnotation as u16 => SyntaxKind::TypeAnnotation,
            x if x == SyntaxKind::InitializerClause as u16 => SyntaxKind::InitializerClause,
            x if x == SyntaxKind::Expression as u16 => SyntaxKind::Expression,
            x if x == SyntaxKind::QualifiedName as u16 => SyntaxKind::QualifiedName,
            x if x == SyntaxKind::QualifiedNameSegment as u16 => SyntaxKind::QualifiedNameSegment,
            x if x == SyntaxKind::Error as u16 => SyntaxKind::Error,
            x if x == SyntaxKind::PackageKw as u16 => SyntaxKind::PackageKw,
            x if x == SyntaxKind::ImportKw as u16 => SyntaxKind::ImportKw,
            x if x == SyntaxKind::ValKw as u16 => SyntaxKind::ValKw,
            x if x == SyntaxKind::VarKw as u16 => SyntaxKind::VarKw,
            x if x == SyntaxKind::NullKw as u16 => SyntaxKind::NullKw,
            x if x == SyntaxKind::BooleanLiteral as u16 => SyntaxKind::BooleanLiteral,
            x if x == SyntaxKind::NumberLiteral as u16 => SyntaxKind::NumberLiteral,
            x if x == SyntaxKind::StringLiteral as u16 => SyntaxKind::StringLiteral,
            x if x == SyntaxKind::RegexLiteral as u16 => SyntaxKind::RegexLiteral,
            x if x == SyntaxKind::Identifier as u16 => SyntaxKind::Identifier,
            x if x == SyntaxKind::Colon as u16 => SyntaxKind::Colon,
            x if x == SyntaxKind::Semicolon as u16 => SyntaxKind::Semicolon,
            x if x == SyntaxKind::Assign as u16 => SyntaxKind::Assign,
            x if x == SyntaxKind::Dot as u16 => SyntaxKind::Dot,
            x if x == SyntaxKind::Comma as u16 => SyntaxKind::Comma,
            x if x == SyntaxKind::LayoutComma as u16 => SyntaxKind::LayoutComma,
            x if x == SyntaxKind::LeftParen as u16 => SyntaxKind::LeftParen,
            x if x == SyntaxKind::RightParen as u16 => SyntaxKind::RightParen,
            x if x == SyntaxKind::LeftBrace as u16 => SyntaxKind::LeftBrace,
            x if x == SyntaxKind::RightBrace as u16 => SyntaxKind::RightBrace,
            x if x == SyntaxKind::LeftBracket as u16 => SyntaxKind::LeftBracket,
            x if x == SyntaxKind::RightBracket as u16 => SyntaxKind::RightBracket,
            x if x == SyntaxKind::Question as u16 => SyntaxKind::Question,
            x if x == SyntaxKind::At as u16 => SyntaxKind::At,
            x if x == SyntaxKind::Whitespace as u16 => SyntaxKind::Whitespace,
            x if x == SyntaxKind::Newline as u16 => SyntaxKind::Newline,
            x if x == SyntaxKind::LineComment as u16 => SyntaxKind::LineComment,
            x if x == SyntaxKind::BlockComment as u16 => SyntaxKind::BlockComment,
            x if x == SyntaxKind::DocComment as u16 => SyntaxKind::DocComment,
            x if x == SyntaxKind::Eof as u16 => SyntaxKind::Eof,
            _ => SyntaxKind::Unknown,
        }
    }

    /// トークンかどうかを判定する。
    pub const fn is_token(self) -> bool {
        matches!(
            self,
            SyntaxKind::PackageKw
                | SyntaxKind::ImportKw
                | SyntaxKind::ValKw
                | SyntaxKind::VarKw
                | SyntaxKind::NullKw
                | SyntaxKind::BooleanLiteral
                | SyntaxKind::NumberLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::RegexLiteral
                | SyntaxKind::Identifier
                | SyntaxKind::Colon
                | SyntaxKind::Semicolon
                | SyntaxKind::Assign
                | SyntaxKind::Dot
                | SyntaxKind::Comma
                | SyntaxKind::LayoutComma
                | SyntaxKind::LeftParen
                | SyntaxKind::RightParen
                | SyntaxKind::LeftBrace
                | SyntaxKind::RightBrace
                | SyntaxKind::LeftBracket
                | SyntaxKind::RightBracket
                | SyntaxKind::Question
                | SyntaxKind::At
                | SyntaxKind::Whitespace
                | SyntaxKind::Newline
                | SyntaxKind::LineComment
                | SyntaxKind::BlockComment
                | SyntaxKind::DocComment
                | SyntaxKind::Eof
        )
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
    /// 真偽値リテラル。
    BooleanLiteral,
    /// 数値リテラル。
    NumberLiteral,
    /// 文字列リテラル。
    StringLiteral,
    /// 正規表現リテラル。
    RegexLiteral,
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
            TokenKind::BooleanLiteral => SyntaxKind::BooleanLiteral,
            TokenKind::NumberLiteral => SyntaxKind::NumberLiteral,
            TokenKind::StringLiteral => SyntaxKind::StringLiteral,
            TokenKind::RegexLiteral => SyntaxKind::RegexLiteral,
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
            TokenType::Boolean(_) => TokenKind::BooleanLiteral,
            TokenType::Number(_) => TokenKind::NumberLiteral,
            TokenType::String(_) | TokenType::StringInterpolation(_) => TokenKind::StringLiteral,
            TokenType::RegexLiteral(_) => TokenKind::RegexLiteral,
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
            TokenType::Whitespace(_) => TokenKind::Whitespace,
            TokenType::Newline => TokenKind::Newline,
            TokenType::LineComment(_) => TokenKind::LineComment,
            TokenType::BlockComment(_) => TokenKind::BlockComment,
            TokenType::JavaDocComment(_) => TokenKind::DocComment,
            TokenType::Eof => TokenKind::Eof,
            _ => TokenKind::Unknown,
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
