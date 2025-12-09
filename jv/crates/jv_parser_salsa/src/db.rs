use std::sync::Arc;

/// salsa クエリを集約する Jar。
#[salsa::jar(db = SalsaDatabase)]
pub struct ParserJar(FileId, CrateId, source_text);

/// ファイル単位の入力 ID。
#[salsa::input]
pub struct FileId {
    /// 論理パス（LSP 連携や診断用）。
    #[id]
    pub path: Arc<str>,
    /// ファイル内容。
    #[return_ref]
    pub text: Arc<str>,
}

/// クレート単位の入力 ID。
#[salsa::input]
pub struct CrateId {
    /// 論理クレート名。
    #[id]
    pub name: Arc<str>,
}

/// 入力ソーステキストを返すクエリ。
#[salsa::tracked]
pub fn source_text(db: &dyn SalsaDatabase, file: FileId) -> Arc<str> {
    file.text(db).clone()
}

/// パーサー用 salsa DB 境界。
#[salsa::db(ParserJar)]
pub trait SalsaDatabase: salsa::Database {}

/// テストおよび実行用のデフォルト実装。
#[derive(Default)]
pub struct Database {
    storage: salsa::Storage<Self>,
}

impl Database {
    /// 空のデータベースを生成する。
    pub fn new() -> Self {
        Self::default()
    }
}

impl salsa::Database for Database {
    fn salsa_event(&self, _event: salsa::Event) {}
}

impl SalsaDatabase for Database {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initializes_database_and_reads_source_text() {
        let db = Database::new();
        let file = FileId::new(
            &db,
            Arc::from("main.jv"),
            Arc::from("package main\nfun main() {}"),
        );
        let crate_id = CrateId::new(&db, Arc::from("stdlib"));

        assert_eq!(file.path(&db).as_ref(), "main.jv");
        assert_eq!(crate_id.name(&db).as_ref(), "stdlib");

        let text = source_text(&db, file);
        assert_eq!(text.as_ref(), "package main\nfun main() {}");
    }
}
