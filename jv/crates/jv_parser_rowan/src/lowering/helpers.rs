use crate::syntax::SyntaxKind;
use crate::JvLanguage;
use jv_ast::Span;
use jv_lexer::Token;
use jv_parser_syntax_support::support::spans::{merge_spans, span_from_token};
use rowan::{SyntaxElement, SyntaxNode, SyntaxToken, TextRange, TextSize};

/// 型付きRowanノードのエイリアス。
pub type JvSyntaxNode = SyntaxNode<JvLanguage>;
/// 型付きRowanトークンのエイリアス。
pub type JvSyntaxToken = SyntaxToken<JvLanguage>;

/// Rowanトークンと元トークンを紐付けるためのストア。
struct TokenStore<'a> {
    entries: Vec<TokenEntry<'a>>,
}

struct TokenEntry<'a> {
    range: TextRange,
    token: &'a Token,
}

impl<'a> TokenStore<'a> {
    /// トークン列からストアを構築する。
    pub fn new(tokens: &'a [Token]) -> Self {
        let mut entries = Vec::with_capacity(tokens.len());
        let mut offset: u32 = 0;
        for token in tokens {
            let length = token.lexeme.len() as u32;
            let start = TextSize::from(offset);
            let end = TextSize::from(offset + length);
            entries.push(TokenEntry {
                range: TextRange::new(start, end),
                token,
            });
            offset = offset.saturating_add(length);
        }
        Self { entries }
    }

    /// Rowanトークンからオリジナルの`Token`を取得する。
    pub fn find(&self, token: &JvSyntaxToken) -> Option<&'a Token> {
        let target = token.text_range();
        self.entries
            .iter()
            .find(|entry| entry.range == target)
            .map(|entry| entry.token)
    }
}

/// ノード配下のトークンを抽出し、辞書順で返す。
fn collect_tokens<'store, 'token>(
    node: &JvSyntaxNode,
    store: &'store TokenStore<'token>,
) -> Vec<&'token Token> {
    let mut collected = Vec::new();
    for element in node.descendants_with_tokens() {
        if let SyntaxElement::Token(token) = element {
            if let Some(original) = store.find(&token) {
                collected.push(original);
            }
        }
    }
    collected
}

/// トークン列から統合スパンを生成する。
pub fn merged_span(tokens: &[&Token]) -> Option<Span> {
    let first = tokens.first()?;
    let start_span = span_from_token(first);
    let last = tokens.last().unwrap_or(first);
    let end_span = span_from_token(last);
    Some(merge_spans(&start_span, &end_span))
}

/// 最初に出現する識別子トークンを抽出する。
pub fn first_identifier_text(node: &JvSyntaxNode) -> Option<String> {
    node.descendants_with_tokens().find_map(|element| {
        if let SyntaxElement::Token(token) = element {
            if token.kind() == SyntaxKind::Identifier {
                return Some(token.text().to_string());
            }
        }
        None
    })
}

/// アノテーションノードのテキスト一覧を抽出する。
pub fn collect_annotation_texts(node: &JvSyntaxNode) -> Vec<String> {
    node.children()
        .filter(|child| child.kind() == SyntaxKind::Annotation)
        .map(|ann| ann.text().to_string())
        .collect()
}

/// Rowanローワリング用の共通コンテキスト。
#[allow(dead_code)]
pub struct LoweringContext<'a> {
    tokens: &'a [Token],
    store: TokenStore<'a>,
}

impl<'a> LoweringContext<'a> {
    /// 新しいローワリングコンテキストを生成する。
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            store: TokenStore::new(tokens),
        }
    }

    /// 解析対象の元トークン列を取得する。
    #[allow(dead_code)]
    pub fn tokens(&self) -> &'a [Token] {
        self.tokens
    }

    /// Rowanトークンに対応する元トークンを取得する。
    #[allow(dead_code)]
    pub fn original_token(&self, token: &JvSyntaxToken) -> Option<&'a Token> {
        self.store.find(token)
    }

    /// ノードに含まれる元トークン列を収集する。
    pub fn tokens_for(&self, node: &JvSyntaxNode) -> Vec<&'a Token> {
        collect_tokens(node, &self.store)
    }

    /// ノード全体のソーススパンを計算する。
    pub fn span_for(&self, node: &JvSyntaxNode) -> Option<Span> {
        let tokens = self.tokens_for(node);
        merged_span(&tokens)
    }
}
