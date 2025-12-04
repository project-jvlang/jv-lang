use std::collections::HashMap;

use jv_parser_frontend::{FrontendOutput, ParseError, Parser2Pipeline, ParserPipeline};
use jv_parser2::{
    Arena, Lexer, Parser, Source,
    parser::incremental::{IncrementalCache, TextChange, parse_incremental},
    span::compute_line_starts,
};

/// jv_parser2 を用いたパーサアダプタ。インクリメンタルパースのキャッシュを
/// ドキュメントごとに保持する。
#[derive(Default)]
pub struct ParserAdapter {
    caches: HashMap<String, IncrementalCache>,
}

impl ParserAdapter {
    pub fn new() -> Self {
        Self {
            caches: HashMap::new(),
        }
    }

    pub fn parse_full(&mut self, uri: &str, source: &str) -> Result<FrontendOutput, ParseError> {
        self.caches.entry(uri.to_string()).or_default();
        Parser2Pipeline::default().parse(source)
    }

    pub fn parse_incremental(
        &mut self,
        uri: &str,
        old_text: &str,
        new_text: &str,
    ) -> Result<FrontendOutput, ParseError> {
        let cache = self.caches.entry(uri.to_string()).or_default();
        {
            let arena = Arena::new();
            let parser = Parser::new(Lexer::new(Source::from_str(new_text)), &arena);
            let change = TextChange {
                start: 0,
                old_end: old_text.len(),
                new_text: new_text.to_string(),
                start_line: 1,
                end_line: compute_line_starts(old_text).len() as u32,
            };
            // parse_incremental は現状フルパースへフォールバックするが、API を通すことで将来の高速化に備える。
            let _ = parse_incremental(parser, &change, cache);
        }
        Parser2Pipeline::default().parse(new_text)
    }

    pub fn drop_cache(&mut self, uri: &str) {
        self.caches.remove(uri);
    }
}
