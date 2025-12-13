use super::{OwnedToken, SyntaxKind};
use crate::lexer::TokenTrivia;

/// CST ノード。
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CstNode {
    pub kind: SyntaxKind,
    pub children: Vec<CstElement>,
}

impl CstNode {
    fn new(kind: SyntaxKind) -> Self {
        Self {
            kind,
            children: Vec::new(),
        }
    }
}

/// CST 要素（子ノードまたはトークン）。
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CstElement {
    Node(CstNode),
    Token(OwnedToken),
}

/// ロスレス構文木ビルダー。
pub struct CstBuilder {
    stack: Vec<CstNode>,
    root: Option<CstNode>,
}

impl Default for CstBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl CstBuilder {
    /// 新しいビルダーを生成する。
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            root: None,
        }
    }

    /// ノード開始。
    pub fn start_node(&mut self, kind: SyntaxKind) {
        self.stack.push(CstNode::new(kind));
    }

    /// ノード終了。
    pub fn finish_node(&mut self) {
        if let Some(node) = self.stack.pop() {
            if let Some(parent) = self.stack.last_mut() {
                parent.children.push(CstElement::Node(node));
            } else {
                self.root = Some(node);
            }
        }
    }

    /// トークンを追加する。
    pub fn push_token(&mut self, token: OwnedToken) {
        if let Some(parent) = self.stack.last_mut() {
            parent.children.push(CstElement::Token(token));
        }
    }

    /// トリビアをトークンへ付与する（salsa 版ではトークンに既に保持されているため no-op）。
    pub fn attach_trivia(&mut self, _leading: &TokenTrivia, _trailing: &TokenTrivia) {}

    /// 完成した CST を返す。
    pub fn build(self) -> Option<CstNode> {
        if self.root.is_some() {
            return self.root;
        }
        self.stack.into_iter().last()
    }
}
