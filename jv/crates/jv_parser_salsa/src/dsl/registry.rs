use crate::lexer::TokenKind;
use std::collections::HashMap;

/// DSL キーワードとハンドラを保持するレジストリ。
#[derive(Default)]
pub struct PluginRegistry {
    keywords: HashMap<&'static str, TokenKind>,
    block_handlers: HashMap<TokenKind, Box<dyn BlockHandler>>,
    operator_handlers: HashMap<TokenKind, Box<dyn OperatorHandler>>,
}

pub trait BlockHandler: Sync {
    fn boundary_tokens(&self) -> (TokenKind, TokenKind);
}

pub trait OperatorHandler: Sync {
    fn handles(&self) -> TokenKind;
}

impl PluginRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// キーワード→TokenKind のマッピングを登録する。
    pub fn register_keyword(&mut self, keyword: &'static str, kind: TokenKind) {
        self.keywords.insert(keyword, kind);
    }

    /// ブロックハンドラを登録する。
    pub fn register_block_handler(&mut self, kind: TokenKind, handler: Box<dyn BlockHandler>) {
        self.block_handlers.insert(kind, handler);
    }

    /// 演算子ハンドラを登録する。
    pub fn register_operator_handler(
        &mut self,
        kind: TokenKind,
        handler: Box<dyn OperatorHandler>,
    ) {
        self.operator_handlers.insert(kind, handler);
    }

    pub fn keyword_kind(&self, keyword: &str) -> Option<TokenKind> {
        self.keywords.get(keyword).copied()
    }

    pub fn block_handler(&self, kind: &TokenKind) -> Option<&dyn BlockHandler> {
        self.block_handlers.get(kind).map(|h| h.as_ref())
    }

    pub fn operator_handler(&self, kind: &TokenKind) -> Option<&dyn OperatorHandler> {
        self.operator_handlers.get(kind).map(|h| h.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct BraceHandler;
    impl BlockHandler for BraceHandler {
        fn boundary_tokens(&self) -> (TokenKind, TokenKind) {
            (TokenKind::LeftBrace, TokenKind::RightBrace)
        }
    }

    struct DemoOp;
    impl OperatorHandler for DemoOp {
        fn handles(&self) -> TokenKind {
            TokenKind::Plus
        }
    }

    #[test]
    fn registers_keywords_and_handlers() {
        let mut registry = PluginRegistry::new();
        registry.register_keyword("LOG", TokenKind::Log);
        registry.register_block_handler(TokenKind::Log, Box::new(BraceHandler));
        registry.register_operator_handler(TokenKind::Plus, Box::new(DemoOp));

        assert_eq!(registry.keyword_kind("LOG"), Some(TokenKind::Log));
        let (start, end) = registry
            .block_handler(&TokenKind::Log)
            .unwrap()
            .boundary_tokens();
        assert_eq!((start, end), (TokenKind::LeftBrace, TokenKind::RightBrace));
        assert!(matches!(
            registry
                .operator_handler(&TokenKind::Plus)
                .unwrap()
                .handles(),
            TokenKind::Plus
        ));
    }
}
