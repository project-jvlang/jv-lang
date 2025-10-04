use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LocaleCode {
    En,
    Ja,
}

impl LocaleCode {
    pub fn as_str(&self) -> &'static str {
        match self {
            LocaleCode::En => "en",
            LocaleCode::Ja => "ja",
        }
    }
}

impl Default for LocaleCode {
    fn default() -> Self {
        LocaleCode::Ja
    }
}

#[derive(Debug, Clone)]
pub struct TemplateCatalog {
    entries: HashMap<String, String>,
}

impl TemplateCatalog {
    fn new(entries: HashMap<String, String>) -> Self {
        Self { entries }
    }

    pub fn render(&self, key: &str, args: &HashMap<&str, String>) -> Option<String> {
        let template = self.entries.get(key)?;
        let mut rendered = template.clone();
        for (name, value) in args {
            let placeholder = format!("{{{}}}", name);
            rendered = rendered.replace(&placeholder, value);
        }
        Some(rendered)
    }
}

fn parse_templates(input: &str) -> HashMap<String, String> {
    let value: toml::Value = input
        .parse()
        .expect("diagnostic templates must be valid TOML");
    let mut entries = HashMap::new();
    if let toml::Value::Table(table) = value {
        flatten_table("", &table, &mut entries);
    }
    entries
}

fn flatten_table(prefix: &str, table: &toml::value::Table, entries: &mut HashMap<String, String>) {
    for (key, value) in table {
        let full_key = if prefix.is_empty() {
            key.clone()
        } else {
            format!("{prefix}.{key}")
        };
        match value {
            toml::Value::String(s) => {
                entries.insert(full_key, s.clone());
            }
            toml::Value::Table(sub) => flatten_table(&full_key, sub, entries),
            _ => {}
        }
    }
}

static CATALOG_JA: Lazy<TemplateCatalog> = Lazy::new(|| {
    TemplateCatalog::new(parse_templates(include_str!(
        "../../../resources/diagnostics_ja.toml"
    )))
});

static CATALOG_EN: Lazy<TemplateCatalog> = Lazy::new(|| {
    TemplateCatalog::new(parse_templates(include_str!(
        "../../../resources/diagnostics_en.toml"
    )))
});

pub fn catalog(locale: LocaleCode) -> &'static TemplateCatalog {
    match locale {
        LocaleCode::Ja => &CATALOG_JA,
        LocaleCode::En => &CATALOG_EN,
    }
}
