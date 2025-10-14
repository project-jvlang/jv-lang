use crate::span_to_range;
use jv_ir::types::{IrImport, IrImportDetail};
use jv_support::i18n::{catalog, LocaleCode};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tower_lsp::lsp_types::{Position, Range, TextDocumentIdentifier};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ImportsParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentIdentifier,
}

#[derive(Debug, Serialize)]
pub struct ImportsResponse {
    pub imports: Vec<ImportItem>,
}

#[derive(Debug, Serialize)]
pub struct ImportItem {
    pub statement: String,
    pub summary: BilingualText,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub details: Vec<BilingualText>,
    pub kind: ImportKind,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub module_dependency: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub range: Option<Range>,
}

#[derive(Debug, Serialize)]
pub struct BilingualText {
    pub ja: String,
    pub en: String,
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ImportKind {
    Type { fqcn: String },
    Package { name: String },
    Static { owner: String, member: String },
    Module { name: String },
}

pub fn build_imports_response(plan: &[IrImport]) -> ImportsResponse {
    let imports = plan
        .iter()
        .map(|import| {
            let internal_range = span_to_range(&import.span);
            ImportItem {
            statement: render_statement(import),
            summary: summary_text(import),
            details: detail_texts(import),
            kind: import_kind(import),
            module_dependency: import.module_dependency.clone(),
            range: Some(Range {
                start: Position {
                    line: internal_range.start.line,
                    character: internal_range.start.character,
                },
                end: Position {
                    line: internal_range.end.line,
                    character: internal_range.end.character,
                },
            }),
        }
        })
        .collect();
    ImportsResponse { imports }
}

fn summary_text(import: &IrImport) -> BilingualText {
    match &import.detail {
        IrImportDetail::Type { fqcn } => {
            let mut args = HashMap::new();
            args.insert("fqcn", fqcn.clone());
            bilingual_or(
                "imports.plan.type.summary",
                &args,
                format!("型 import: {fqcn}"),
                format!("Type import: {fqcn}"),
            )
        }
        IrImportDetail::Package { name } => {
            let mut args = HashMap::new();
            args.insert("name", name.clone());
            bilingual_or(
                "imports.plan.package.summary",
                &args,
                format!("パッケージ import: {name}.*"),
                format!("Package import: {name}.*"),
            )
        }
        IrImportDetail::Static { owner, member } => {
            let mut args = HashMap::new();
            args.insert("owner", owner.clone());
            args.insert("member", member.clone());
            bilingual_or(
                "imports.plan.static.summary",
                &args,
                format!("静的 import: {owner}.{member}"),
                format!("Static import: {owner}.{member}"),
            )
        }
        IrImportDetail::Module { name } => {
            let mut args = HashMap::new();
            args.insert("name", name.clone());
            bilingual_or(
                "imports.plan.module.summary",
                &args,
                format!("モジュール import: {name}"),
                format!("Module import: {name}"),
            )
        }
    }
}

fn detail_texts(import: &IrImport) -> Vec<BilingualText> {
    let mut entries = Vec::new();

    if let Some(alias) = import.alias.as_ref() {
        let mut args = HashMap::new();
        args.insert("alias", alias.clone());
        entries.push(bilingual_or(
            "imports.plan.alias.summary",
            &args,
            format!("別名: {alias}"),
            format!("Alias: {alias}"),
        ));
    }

    if let Some(module) = import.module_dependency.as_ref() {
        let mut args = HashMap::new();
        args.insert("module", module.clone());
        entries.push(bilingual_or(
            "imports.plan.module_dependency.summary",
            &args,
            format!("モジュール依存: {module}"),
            format!("Module dependency: {module}"),
        ));
    }

    entries
}

fn import_kind(import: &IrImport) -> ImportKind {
    match &import.detail {
        IrImportDetail::Type { fqcn } => ImportKind::Type {
            fqcn: fqcn.clone(),
        },
        IrImportDetail::Package { name } => ImportKind::Package {
            name: name.clone(),
        },
        IrImportDetail::Static { owner, member } => ImportKind::Static {
            owner: owner.clone(),
            member: member.clone(),
        },
        IrImportDetail::Module { name } => ImportKind::Module {
            name: name.clone(),
        },
    }
}

fn render_statement(import: &IrImport) -> String {
    match &import.detail {
        IrImportDetail::Type { fqcn } => format!("import {fqcn}"),
        IrImportDetail::Package { name } => format!("import {name}.*"),
        IrImportDetail::Static { owner, member } => {
            format!("import static {owner}.{member}")
        }
        IrImportDetail::Module { name } => format!("import module {name}"),
    }
}

fn bilingual_or(
    key: &str,
    args: &HashMap<&str, String>,
    fallback_ja: String,
    fallback_en: String,
) -> BilingualText {
    render_bilingual(key, args).unwrap_or(BilingualText {
        ja: fallback_ja,
        en: fallback_en,
    })
}

fn render_bilingual(key: &str, args: &HashMap<&str, String>) -> Option<BilingualText> {
    let ja = catalog(LocaleCode::Ja).render(key, args)?;
    let en = catalog(LocaleCode::En).render(key, args)?;
    Some(BilingualText { ja, en })
}
