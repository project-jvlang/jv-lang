use std::collections::{HashMap, HashSet};

use jv_dsl_api::{BlockPlugin, DslBlock, DslError, DslTokenKind, GlobalOperatorPlugin, TokenPlugin};

#[cfg(feature = "dsl-assert")]
use jv_dsl_builtin::AssertPlugin;
#[cfg(feature = "dsl-cron")]
use jv_dsl_builtin::CronPlugin;
#[cfg(feature = "dsl-io")]
use jv_dsl_builtin::IoPlugin;
#[cfg(feature = "dsl-lock")]
use jv_dsl_builtin::LockPlugin;
#[cfg(feature = "dsl-log")]
use jv_dsl_builtin::LogPlugin;

const DSL_TOKEN_START: DslTokenKind = 120;
const DSL_TOKEN_LIMIT: DslTokenKind = 200;

/// DSLキーワードキャッシュ（高速ルックアップ用）。
pub struct DslKeywordCache {
    pub(crate) keyword_to_token: HashMap<&'static str, DslTokenKind>,
    pub(crate) first_char_index: [Vec<&'static str>; 128],
}

impl DslKeywordCache {
    pub fn empty() -> Self {
        Self {
            keyword_to_token: HashMap::new(),
            first_char_index: std::array::from_fn(|_| Vec::new()),
        }
    }
}

/// レジストリへ登録するプラグインセット。
pub struct PluginEntry {
    pub token_plugin: Option<Box<dyn TokenPlugin>>,
    pub block_plugin: Option<Box<dyn BlockPlugin>>,
    pub global_operator_plugin: Option<Box<dyn GlobalOperatorPlugin>>,
}

impl PluginEntry {
    pub fn block<P>(plugin: P) -> Self
    where
        P: BlockPlugin + 'static,
    {
        Self {
            token_plugin: None,
            block_plugin: Some(Box::new(plugin)),
            global_operator_plugin: None,
        }
    }

    pub fn token_and_block<P>(plugin: P) -> Self
    where
        P: TokenPlugin + BlockPlugin + Clone + 'static,
    {
        Self {
            token_plugin: Some(Box::new(plugin.clone())),
            block_plugin: Some(Box::new(plugin)),
            global_operator_plugin: None,
        }
    }

    pub fn block_and_global<P>(plugin: P) -> Self
    where
        P: BlockPlugin + GlobalOperatorPlugin + Clone + 'static,
    {
        Self {
            token_plugin: None,
            block_plugin: Some(Box::new(plugin.clone())),
            global_operator_plugin: Some(Box::new(plugin)),
        }
    }
}

/// DSLプラグイン登録とキーワードキャッシュ管理を行う。
pub struct PluginRegistry {
    token_plugins: Vec<Box<dyn TokenPlugin>>,
    block_plugins: Vec<Box<dyn BlockPlugin>>,
    global_operator_plugins: Vec<Box<dyn GlobalOperatorPlugin>>,
    keyword_cache: DslKeywordCache,
}

impl PluginRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            token_plugins: Vec::new(),
            block_plugins: Vec::new(),
            global_operator_plugins: Vec::new(),
            keyword_cache: DslKeywordCache::empty(),
        };

        #[cfg(feature = "dsl-log")]
        registry.register(PluginEntry::token_and_block(LogPlugin));
        #[cfg(feature = "dsl-io")]
        registry.register(PluginEntry::block(IoPlugin));
        #[cfg(feature = "dsl-lock")]
        registry.register(PluginEntry::block(LockPlugin));
        #[cfg(feature = "dsl-cron")]
        registry.register(PluginEntry::block_and_global(CronPlugin));
        #[cfg(feature = "dsl-assert")]
        registry.register(PluginEntry::token_and_block(AssertPlugin));

        registry.rebuild_keyword_cache();
        registry
    }

    /// プラグインを登録する。必要に応じて複数カテゴリへ追加される。
    pub fn register(&mut self, entry: PluginEntry) {
        if let Some(token_plugin) = entry.token_plugin {
            self.token_plugins.push(token_plugin);
        }
        if let Some(block_plugin) = entry.block_plugin {
            self.block_plugins.push(block_plugin);
        }
        if let Some(operator_plugin) = entry.global_operator_plugin {
            self.global_operator_plugins.push(operator_plugin);
        }
        self.rebuild_keyword_cache();
    }

    /// 先頭文字で候補を絞り込みつつキーワードを高速に解決する。
    #[inline]
    pub fn lookup_keyword(&self, text: &str) -> Option<DslTokenKind> {
        self.keyword_cache.keyword_to_token.get(text).copied()
    }

    /// DSLブロックキーワードに対応するプラグインを返す。
    pub fn find_block_plugin(&self, keyword: &str) -> Option<&dyn BlockPlugin> {
        let normalized = keyword.trim();
        self.block_plugins.iter().find_map(|plugin| {
            plugin
                .registered_keywords()
                .iter()
                .copied()
                .find(|kw| *kw == normalized)
                .map(|_| plugin.as_ref() as &dyn BlockPlugin)
        })
    }

    /// 登録済みのグローバル演算子プラグインを列挙する。
    pub fn global_operators(&self) -> impl Iterator<Item = &dyn GlobalOperatorPlugin> {
        self.global_operator_plugins
            .iter()
            .map(|p| p.as_ref() as &dyn GlobalOperatorPlugin)
    }

    /// DSLブロックを直接解析する簡易ヘルパ。
    pub fn parse_block(&self, keyword: &str, body: &str) -> Result<Option<DslBlock>, DslError> {
        if let Some(plugin) = self.find_block_plugin(keyword) {
            plugin.parse_block_body(body).map(Some)
        } else {
            Ok(None)
        }
    }

    /// すべての登録済みプラグインからキーワードを収集してキャッシュを構築する。
    pub fn rebuild_keyword_cache(&mut self) {
        let mut keyword_to_token = HashMap::new();
        let mut first_char_index: [Vec<&'static str>; 128] = std::array::from_fn(|_| Vec::new());
        let mut used_tokens = HashSet::new();
        let mut next_token: DslTokenKind = DSL_TOKEN_START;

        for plugin in &self.token_plugins {
            for keyword in plugin.registered_keywords() {
                let token_id = assign_token(
                    keyword,
                    plugin.keyword_to_token(keyword),
                    &mut keyword_to_token,
                    &mut used_tokens,
                    &mut next_token,
                );
                push_first_char(keyword, token_id, &mut keyword_to_token, &mut first_char_index);
            }
        }

        for plugin in &self.block_plugins {
            for keyword in plugin.registered_keywords() {
                let token_id = assign_token(
                    keyword,
                    None,
                    &mut keyword_to_token,
                    &mut used_tokens,
                    &mut next_token,
                );
                push_first_char(keyword, token_id, &mut keyword_to_token, &mut first_char_index);
            }
        }

        assert!(
            next_token < DSL_TOKEN_LIMIT,
            "DSL keyword count {} exceeds TokenKind limit 200",
            next_token.saturating_sub(DSL_TOKEN_START)
        );

        self.keyword_cache = DslKeywordCache {
            keyword_to_token,
            first_char_index,
        };
    }

    /// 先頭文字に紐づく候補キーワードを返す。
    #[inline]
    pub fn keyword_candidates(&self, first_char: u8) -> &[&'static str] {
        if first_char < 128 {
            &self.keyword_cache.first_char_index[first_char as usize]
        } else {
            &[]
        }
    }
}

fn push_first_char(
    keyword: &'static str,
    token_id: DslTokenKind,
    keyword_to_token: &mut HashMap<&'static str, DslTokenKind>,
    first_char_index: &mut [Vec<&'static str>; 128],
) {
    keyword_to_token.entry(keyword).or_insert(token_id);
    if let Some(&first_byte) = keyword.as_bytes().first() {
        if first_byte < 128 {
            first_char_index[first_byte as usize].push(keyword);
        }
    }
}

fn allocate_token(
    keyword: &'static str,
    requested: Option<DslTokenKind>,
    keyword_to_token: &mut HashMap<&'static str, DslTokenKind>,
    used_tokens: &mut HashSet<DslTokenKind>,
    next_token: &mut DslTokenKind,
) -> DslTokenKind {
    if let Some(explicit) = requested {
        assert!(
            explicit >= DSL_TOKEN_START && explicit < DSL_TOKEN_LIMIT,
            "DSL token id {} for keyword {} is out of range [{}, {})",
            explicit,
            keyword,
            DSL_TOKEN_START,
            DSL_TOKEN_LIMIT
        );
        if let Some(existing) = keyword_to_token.get(keyword) {
            assert_eq!(
                *existing, explicit,
                "Keyword {} already mapped to {}, cannot remap to {}",
                keyword, existing, explicit
            );
            return explicit;
        }
        assert!(
            !used_tokens.contains(&explicit),
            "DSL token id {} for keyword {} collides with another keyword",
            explicit,
            keyword
        );
        used_tokens.insert(explicit);
        keyword_to_token.insert(keyword, explicit);
        return explicit;
    }
    let token = *next_token;
    *next_token = next_token.saturating_add(1);
    assert!(
        token < DSL_TOKEN_LIMIT,
        "DSL keyword count {} exceeds TokenKind limit 200",
        token.saturating_sub(DSL_TOKEN_START)
    );
    assert!(
        !used_tokens.contains(&token),
        "Allocated DSL token id {} for keyword {} collides with another keyword",
        token,
        keyword
    );
    used_tokens.insert(token);
    keyword_to_token.insert(keyword, token);
    token
}

fn assign_token(
    keyword: &'static str,
    requested: Option<DslTokenKind>,
    keyword_to_token: &mut HashMap<&'static str, DslTokenKind>,
    used_tokens: &mut HashSet<DslTokenKind>,
    next_token: &mut DslTokenKind,
) -> DslTokenKind {
    if let Some(existing) = keyword_to_token.get(keyword) {
        return *existing;
    }
    allocate_token(keyword, requested, keyword_to_token, used_tokens, next_token)
}

#[cfg(test)]
mod tests {
    use super::*;
    use jv_dsl_api::{
        Associativity, BlockPlugin, DslPlugin, GlobalOperator, GlobalOperatorPlugin, TokenPlugin,
    };
    use serde_json::json;

    #[derive(Clone)]
    struct DummyGlobal;

    impl DslPlugin for DummyGlobal {
        fn name(&self) -> &'static str {
            "dummy-global"
        }

        fn registered_keywords(&self) -> &'static [&'static str] {
            &["DUMMY"]
        }
    }

    impl BlockPlugin for DummyGlobal {
        fn keyword(&self) -> &'static str {
            "DUMMY"
        }

        fn parse_block_body(&self, _body: &str) -> Result<DslBlock, DslError> {
            Ok(DslBlock::new("DUMMY", json!({}), true, 1))
        }
    }

    impl GlobalOperatorPlugin for DummyGlobal {
        fn register_operators(&self) -> &'static [GlobalOperator] {
            const OPS: &[GlobalOperator] = &[GlobalOperator {
                symbol: "##",
                precedence: 10,
                associativity: Associativity::Left,
            }];
            OPS
        }
    }

    #[test]
    fn exposes_global_operator_plugins() {
        let mut registry = PluginRegistry::new();
        registry.register(PluginEntry::block_and_global(DummyGlobal));
        let count = registry.global_operators().count();
        assert!(count >= 1);
    }

    #[test]
    #[should_panic(expected = "out of range")]
    fn rejects_explicit_token_out_of_range() {
        #[derive(Clone)]
        struct BadToken;
        impl DslPlugin for BadToken {
            fn name(&self) -> &'static str {
                "bad"
            }
            fn registered_keywords(&self) -> &'static [&'static str] {
                &["BAD"]
            }
        }
        impl TokenPlugin for BadToken {
            fn keyword_to_token(&self, _keyword: &str) -> Option<DslTokenKind> {
                Some(1)
            }
        }
        impl BlockPlugin for BadToken {
            fn keyword(&self) -> &'static str {
                "BAD"
            }
            fn parse_block_body(&self, _body: &str) -> Result<DslBlock, DslError> {
                Ok(DslBlock::new("BAD", json!({}), true, 1))
            }
        }

        let mut registry = PluginRegistry::new();
        registry.register(PluginEntry::token_and_block(BadToken));
    }

    #[test]
    #[should_panic(expected = "collides")]
    fn rejects_duplicate_token_id() {
        #[derive(Clone)]
        struct TokenOne(&'static str, &'static [&'static str]);
        const FIRST_KW: [&str; 1] = ["FIRST"];
        const SECOND_KW: [&str; 1] = ["SECOND"];
        impl DslPlugin for TokenOne {
            fn name(&self) -> &'static str {
                self.0
            }
            fn registered_keywords(&self) -> &'static [&'static str] {
                self.1
            }
        }
        impl TokenPlugin for TokenOne {
            fn keyword_to_token(&self, _keyword: &str) -> Option<DslTokenKind> {
                Some(DSL_TOKEN_START)
            }
        }
        impl BlockPlugin for TokenOne {
            fn keyword(&self) -> &'static str {
                self.1[0]
            }
            fn parse_block_body(&self, _body: &str) -> Result<DslBlock, DslError> {
                Ok(DslBlock::new(self.1[0], json!({}), true, 1))
            }
        }

        let mut registry = PluginRegistry::new();
        registry.register(PluginEntry::token_and_block(TokenOne("first", &FIRST_KW)));
        registry.register(PluginEntry::token_and_block(TokenOne("second", &SECOND_KW)));
    }
}
