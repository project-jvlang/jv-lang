// jv_mapper - Source mapping (.jv ↔ .java)
pub mod types;

pub use types::lower_generic_signature;

use jv_ast::Span;
use jv_ir::ConversionMetadata;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MappingError {
    #[error("Invalid source position: {0}")]
    InvalidPosition(String),
    #[error("Source map generation error: {0}")]
    GenerationError(String),
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct JavaPosition {
    pub line: usize,
    pub column: usize,
}

impl JavaPosition {
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct JavaSpan {
    pub start: JavaPosition,
    pub end: JavaPosition,
}

impl JavaSpan {
    pub fn new(start: JavaPosition, end: JavaPosition) -> Result<Self, MappingError> {
        let span = Self { start, end };
        span.validate()?;
        Ok(span)
    }

    pub fn contains(&self, line: usize, column: usize) -> bool {
        if line < self.start.line || line > self.end.line {
            return false;
        }
        if line == self.start.line && column < self.start.column {
            return false;
        }
        if line == self.end.line && column > self.end.column {
            return false;
        }
        true
    }

    pub fn validate(&self) -> Result<(), MappingError> {
        if self.end.line < self.start.line
            || (self.end.line == self.start.line && self.end.column < self.start.column)
        {
            return Err(MappingError::InvalidPosition(
                "Java span end precedes start".to_string(),
            ));
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "category", content = "detail")]
pub enum MappingCategory {
    #[serde(rename = "package")]
    Package,
    #[serde(rename = "import")]
    Import,
    #[serde(rename = "type_declaration")]
    TypeDeclaration,
    #[serde(rename = "method")]
    Method,
    #[serde(rename = "field")]
    Field,
    #[serde(rename = "statement")]
    Statement,
    #[serde(rename = "expression")]
    Expression,
    #[serde(rename = "custom")]
    Custom(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MappingEntry {
    pub ir_span: Span,
    pub java_span: JavaSpan,
    pub category: MappingCategory,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ir_node: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conversions: Option<Vec<ConversionMetadata>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SourceMap {
    pub version: u8,
    pub source_file: String,
    pub generated_file: String,
    #[serde(default)]
    pub entries: Vec<MappingEntry>,
}

impl SourceMap {
    pub const VERSION: u8 = 1;

    /// Create an empty source map that associates the given source and generated files.
    ///
    /// ```
    /// use jv_mapper::{SourceMap, MappingEntry, MappingCategory, JavaSpan, JavaPosition};
    /// use jv_ast::Span;
    ///
    /// let mut map = SourceMap::new("example.jv", "Example.java");
    /// map.entries.push(MappingEntry {
    ///     ir_span: Span::new(1, 0, 1, 5),
    ///     java_span: JavaSpan::new(JavaPosition::new(1, 0), JavaPosition::new(1, 5)).unwrap(),
    ///     category: MappingCategory::Statement,
    ///     ir_node: Some("println".to_string()),
    /// });
    ///
    /// assert!(map.find_by_java_position(1, 2).is_some());
    /// ```
    pub fn new(source_file: impl Into<String>, generated_file: impl Into<String>) -> Self {
        Self {
            version: Self::VERSION,
            source_file: source_file.into(),
            generated_file: generated_file.into(),
            entries: Vec::new(),
        }
    }

    pub fn to_json_pretty(&self) -> Result<String, MappingError> {
        serde_json::to_string_pretty(self).map_err(MappingError::from)
    }

    pub fn from_json(json: &str) -> Result<Self, MappingError> {
        let mut map: SourceMap = serde_json::from_str(json)?;
        map.version = Self::VERSION;
        Ok(map)
    }

    pub fn find_by_ir_position(&self, line: usize, column: usize) -> Option<&MappingEntry> {
        self.entries
            .iter()
            .find(|entry| span_contains(&entry.ir_span, line, column))
    }

    pub fn find_by_java_position(&self, line: usize, column: usize) -> Option<&MappingEntry> {
        self.entries
            .iter()
            .find(|entry| entry.java_span.contains(line, column))
    }
}

#[derive(Debug)]
pub struct SourceMapBuilder {
    source_file: String,
    generated_file: String,
    entries: Vec<MappingEntry>,
    current_java: JavaPosition,
}

impl SourceMapBuilder {
    pub fn new(source_file: impl Into<String>, generated_file: impl Into<String>) -> Self {
        Self {
            source_file: source_file.into(),
            generated_file: generated_file.into(),
            entries: Vec::new(),
            current_java: JavaPosition::new(1, 0),
        }
    }

    pub fn current_java(&self) -> JavaPosition {
        self.current_java
    }

    pub fn set_java_position(&mut self, position: JavaPosition) {
        self.current_java = position;
    }

    pub fn record_mapping(
        &mut self,
        ir_span: Span,
        java_span: JavaSpan,
        category: MappingCategory,
        ir_node: Option<String>,
        conversions: Option<Vec<ConversionMetadata>>,
    ) -> Result<(), MappingError> {
        validate_ir_span(&ir_span)?;
        java_span.validate()?;

        if !self.entries.iter().any(|entry| {
            entry.ir_span == ir_span
                && entry.java_span == java_span
                && entry.category == category
                && entry.ir_node == ir_node
                && entry.conversions == conversions
        }) {
            self.entries.push(MappingEntry {
                ir_span,
                java_span,
                category,
                ir_node,
                conversions,
            });
        }

        Ok(())
    }

    pub fn record_generated_segment(
        &mut self,
        ir_span: Span,
        segment: &str,
        category: MappingCategory,
        ir_node: Option<String>,
    ) -> Result<JavaSpan, MappingError> {
        validate_ir_span(&ir_span)?;
        let java_span = self.consume_java(segment);
        self.entries.push(MappingEntry {
            ir_span,
            java_span: java_span.clone(),
            category,
            ir_node,
            conversions: None,
        });
        Ok(java_span)
    }

    pub fn advance_unmapped(&mut self, segment: &str) -> JavaSpan {
        self.consume_java(segment)
    }

    pub fn build(mut self) -> SourceMap {
        self.entries.sort_by(|a, b| {
            a.java_span
                .start
                .line
                .cmp(&b.java_span.start.line)
                .then(a.java_span.start.column.cmp(&b.java_span.start.column))
                .then(a.ir_span.start_line.cmp(&b.ir_span.start_line))
                .then(a.ir_span.start_column.cmp(&b.ir_span.start_column))
        });

        SourceMap {
            version: SourceMap::VERSION,
            source_file: self.source_file,
            generated_file: self.generated_file,
            entries: self.entries,
        }
    }

    fn consume_java(&mut self, segment: &str) -> JavaSpan {
        let start = self.current_java;
        advance_java_position(&mut self.current_java, segment);
        JavaSpan {
            start,
            end: self.current_java,
        }
    }
}

#[derive(Debug, Default)]
pub struct SourceMapManager {
    source_maps: HashMap<String, SourceMap>,
}

impl SourceMapManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_source_map(&mut self, source_file: String, source_map: SourceMap) {
        if source_map.source_file != source_file {
            // Keep the provided key authoritative to preserve backward compatibility.
            let mut map = source_map;
            map.source_file = source_file.clone();
            self.source_maps.insert(source_file, map);
        } else {
            self.source_maps.insert(source_file, source_map);
        }
    }

    pub fn add(&mut self, source_map: SourceMap) {
        let key = source_map.source_file.clone();
        self.source_maps.insert(key, source_map);
    }

    pub fn get_source_map(&self, source_file: &str) -> Option<&SourceMap> {
        self.source_maps.get(source_file)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &SourceMap)> {
        self.source_maps.iter()
    }

    pub fn save_all(&self, output_dir: &str) -> Result<(), MappingError> {
        fs::create_dir_all(output_dir)?;

        for source_map in self.source_maps.values() {
            let file_name = map_file_name(&source_map.generated_file, &source_map.source_file);
            let map_path = Path::new(output_dir).join(file_name);
            let json = source_map.to_json_pretty()?;
            fs::write(map_path, json)?;
        }

        Ok(())
    }
}

fn validate_ir_span(span: &Span) -> Result<(), MappingError> {
    if span.end_line < span.start_line
        || (span.end_line == span.start_line && span.end_column < span.start_column)
    {
        return Err(MappingError::InvalidPosition(
            "IR span end precedes start".to_string(),
        ));
    }
    Ok(())
}

fn span_contains(span: &Span, line: usize, column: usize) -> bool {
    if line < span.start_line || line > span.end_line {
        return false;
    }
    if line == span.start_line && column < span.start_column {
        return false;
    }
    if line == span.end_line && column > span.end_column {
        return false;
    }
    true
}

fn advance_java_position(position: &mut JavaPosition, segment: &str) {
    let mut chars = segment.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '\r' => {
                if let Some(&'\n') = chars.peek() {
                    chars.next();
                }
                position.line += 1;
                position.column = 0;
            }
            '\n' => {
                position.line += 1;
                position.column = 0;
            }
            _ => {
                position.column += 1;
            }
        }
    }
}

fn map_file_name(generated_file: &str, source_file: &str) -> String {
    let generated = Path::new(generated_file)
        .file_name()
        .map(|name| name.to_string_lossy().to_string());

    let base = generated.unwrap_or_else(|| {
        Path::new(source_file)
            .file_name()
            .map(|name| name.to_string_lossy().to_string())
            .unwrap_or_else(|| "source".to_string())
    });

    format!("{}.map.json", base)
}

#[cfg(test)]
mod tests;
