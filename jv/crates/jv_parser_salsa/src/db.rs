use std::sync::Arc;

use crate::hir::HirFile;
use crate::lexer::Lexer;
use crate::lower;
use crate::parser::{self, ParseOutput, ParseResult};

#[salsa::input]
pub struct FileInput {
    #[id]
    pub path: Arc<str>,
    #[return_ref]
    pub text: Arc<str>,
}

#[salsa::tracked]
pub fn lower_to_hir(db: &dyn ParserDatabase, file: FileInput) -> Arc<HirFile> {
    let parse_result = parse(db, file);
    let source = file.text(db);
    let lowering = lower::lower(source.as_ref(), &parse_result);
    Arc::new(HirFile::new(
        lowering.statements,
        lowering.diagnostics,
        lowering.token_spans,
    ))
}

#[salsa::tracked]
pub fn parse(db: &dyn ParserDatabase, file: FileInput) -> Arc<ParseResult> {
    let lexer = Lexer::new(file.text(db).as_ref());
    match lexer {
        Ok(lexer) => Arc::new(parser::parse(lexer.collect_owned_tokens())),
        Err(err) => Arc::new(ParseResult {
            tokens: Vec::new(),
            output: ParseOutput {
                events: Vec::new(),
                diagnostics: Vec::new(),
                recovered: false,
            },
            errors: vec![err.to_string()],
        }),
    }
}

#[salsa::db]
pub trait ParserDatabase: salsa::Database {}

#[salsa::db]
#[derive(Clone, Default)]
pub struct Database {
    storage: salsa::Storage<Database>,
}

#[salsa::db]
impl salsa::Database for Database {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

#[salsa::db]
impl ParserDatabase for Database {}

impl Database {
    pub fn new() -> Self {
        Self::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use salsa::Setter;

    #[test]
    fn initializes_database_and_reads_source_text() {
        let db = Database::new();
        let file = FileInput::new(
            &db,
            Arc::from("main.jv"),
            Arc::from("package main\nfun main() {}"),
        );

        assert_eq!(file.path(&db).as_ref(), "main.jv");
        assert_eq!(file.text(&db).as_ref(), "package main\nfun main() {}");
    }

    #[test]
    fn parse_query_caches_results() {
        let db = Database::new();
        let file = FileInput::new(
            &db,
            Arc::from("sample.jv"),
            Arc::from("package main\nval x = 1"),
        );

        let first = parse(&db, file);
        let second = parse(&db, file);
        assert!(
            Arc::ptr_eq(&first, &second),
            "salsa cache should reuse results"
        );
        assert!(first.errors.is_empty());
        assert!(
            !first.tokens.is_empty(),
            "token stream should be produced for valid input"
        );
    }

    #[test]
    fn parse_query_recomputes_on_change() {
        let mut db = Database::new();
        let file = FileInput::new(
            &db,
            Arc::from("mutable.jv"),
            Arc::from("package main\nval x = 1"),
        );

        let initial = parse(&db, file);
        assert!(initial.errors.is_empty());

        file.set_text(&mut db)
            .to(Arc::from("package main\nval x = 2 + 3"));
        let updated = parse(&db, file);

        assert!(
            !Arc::ptr_eq(&initial, &updated),
            "changing input should invalidate cached value"
        );
        assert!(updated.errors.is_empty());
        assert!(
            updated.tokens.len() >= initial.tokens.len(),
            "updated source should produce tokens"
        );
    }
}
