// jv_mapper - Source mapping (.jv ↔ .java)
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MappingError {
    #[error("Invalid source position: {0}")]
    InvalidPosition(String),
    #[error("Source map generation error: {0}")]
    GenerationError(String),
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SourceMap {
    pub source_file: String,
    pub generated_file: String,
    pub mappings: Vec<Mapping>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Mapping {
    pub jv_line: usize,
    pub java_line: usize,
    pub jv_column: usize,
    pub java_column: usize,
    pub original_text: String,
    pub generated_text: String,
}

impl SourceMap {
    pub fn new(source_file: String, generated_file: String) -> Self {
        Self {
            source_file,
            generated_file,
            mappings: Vec::new(),
        }
    }

    pub fn add_mapping(&mut self, mapping: Mapping) {
        self.mappings.push(mapping);
    }

    /// Find Java line for a given .jv line
    pub fn find_java_line(&self, jv_line: usize) -> Option<usize> {
        self.mappings
            .iter()
            .find(|m| m.jv_line == jv_line)
            .map(|m| m.java_line)
    }

    /// Find .jv line for a given Java line
    pub fn find_jv_line(&self, java_line: usize) -> Option<usize> {
        self.mappings
            .iter()
            .find(|m| m.java_line == java_line)
            .map(|m| m.jv_line)
    }
}

impl Mapping {
    pub fn new(
        jv_line: usize,
        java_line: usize,
        jv_column: usize,
        java_column: usize,
        original_text: String,
        generated_text: String,
    ) -> Self {
        Self {
            jv_line,
            java_line,
            jv_column,
            java_column,
            original_text,
            generated_text,
        }
    }

    pub fn simple_line_mapping(
        jv_line: usize,
        java_line: usize,
        original_text: String,
        generated_text: String,
    ) -> Self {
        Self {
            jv_line,
            java_line,
            jv_column: 0,
            java_column: 0,
            original_text,
            generated_text,
        }
    }
}

/// Source map builder for incremental construction
pub struct SourceMapBuilder {
    source_file: String,
    generated_file: String,
    mappings: Vec<Mapping>,
}

impl SourceMapBuilder {
    pub fn new(source_file: String, generated_file: String) -> Self {
        Self {
            source_file,
            generated_file,
            mappings: Vec::new(),
        }
    }

    /// Add a simple line-to-line mapping
    pub fn add_line_mapping(
        &mut self,
        jv_line: usize,
        java_line: usize,
        original_text: String,
        generated_text: String,
    ) {
        let mapping =
            Mapping::simple_line_mapping(jv_line, java_line, original_text, generated_text);
        self.mappings.push(mapping);
    }

    /// Build the final source map
    pub fn build(self) -> SourceMap {
        SourceMap {
            source_file: self.source_file,
            generated_file: self.generated_file,
            mappings: self.mappings,
        }
    }
}

/// Source map manager for handling multiple compilation units
pub struct SourceMapManager {
    source_maps: HashMap<String, SourceMap>,
}

impl SourceMapManager {
    pub fn new() -> Self {
        Self {
            source_maps: HashMap::new(),
        }
    }

    /// Add a source map for a compilation unit
    pub fn add_source_map(&mut self, source_file: String, source_map: SourceMap) {
        self.source_maps.insert(source_file, source_map);
    }

    /// Get source map for a file
    pub fn get_source_map(&self, source_file: &str) -> Option<&SourceMap> {
        self.source_maps.get(source_file)
    }

    /// Save all source maps to disk
    pub fn save_all(&self, output_dir: &str) -> Result<(), MappingError> {
        std::fs::create_dir_all(output_dir)?;

        for (source_file, source_map) in &self.source_maps {
            let map_filename = format!(
                "{}.map",
                std::path::Path::new(source_file)
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
            );
            let map_path = std::path::Path::new(output_dir).join(map_filename);

            let json = serde_json::to_string_pretty(source_map)
                .map_err(|e| MappingError::GenerationError(e.to_string()))?;
            std::fs::write(map_path, json)?;
        }

        Ok(())
    }
}

impl Default for SourceMapManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_map_creation() {
        let mut source_map = SourceMap::new("test.jv".to_string(), "Test.java".to_string());

        let mapping = Mapping::simple_line_mapping(
            0,
            2,
            "val x = 42".to_string(),
            "final int x = 42;".to_string(),
        );
        source_map.add_mapping(mapping);

        assert_eq!(source_map.mappings.len(), 1);
        assert_eq!(source_map.find_java_line(0), Some(2));
        assert_eq!(source_map.find_jv_line(2), Some(0));
    }

    #[test]
    fn test_source_map_builder() {
        let mut builder = SourceMapBuilder::new("test.jv".to_string(), "Test.java".to_string());
        builder.add_line_mapping(
            0,
            2,
            "val x = 42".to_string(),
            "final int x = 42;".to_string(),
        );

        let source_map = builder.build();

        assert_eq!(source_map.mappings.len(), 1);
        assert_eq!(source_map.source_file, "test.jv");
        assert_eq!(source_map.generated_file, "Test.java");
    }

    #[test]
    fn test_source_map_manager() {
        let mut manager = SourceMapManager::new();
        let source_map = SourceMap::new("test.jv".to_string(), "Test.java".to_string());

        manager.add_source_map("test.jv".to_string(), source_map);

        assert!(manager.get_source_map("test.jv").is_some());
        assert!(manager.get_source_map("nonexistent.jv").is_none());
    }

    #[test]
    fn test_mapping_creation() {
        let mapping = Mapping::new(
            5,
            10,
            15,
            20,
            "original".to_string(),
            "generated".to_string(),
        );

        assert_eq!(mapping.jv_line, 5);
        assert_eq!(mapping.java_line, 10);
        assert_eq!(mapping.jv_column, 15);
        assert_eq!(mapping.java_column, 20);
        assert_eq!(mapping.original_text, "original");
        assert_eq!(mapping.generated_text, "generated");
    }

    #[test]
    fn test_simple_line_mapping() {
        let mapping =
            Mapping::simple_line_mapping(3, 7, "jv code".to_string(), "java code".to_string());

        assert_eq!(mapping.jv_line, 3);
        assert_eq!(mapping.java_line, 7);
        assert_eq!(mapping.jv_column, 0);
        assert_eq!(mapping.java_column, 0);
        assert_eq!(mapping.original_text, "jv code");
        assert_eq!(mapping.generated_text, "java code");
    }

    #[test]
    fn test_multiple_mappings() {
        let mut source_map = SourceMap::new("test.jv".to_string(), "Test.java".to_string());

        // Add multiple mappings
        source_map.add_mapping(Mapping::simple_line_mapping(
            0,
            2,
            "val x = 1".to_string(),
            "final int x = 1;".to_string(),
        ));
        source_map.add_mapping(Mapping::simple_line_mapping(
            1,
            3,
            "val y = 2".to_string(),
            "final int y = 2;".to_string(),
        ));
        source_map.add_mapping(Mapping::simple_line_mapping(
            2,
            4,
            "val z = x + y".to_string(),
            "final int z = x + y;".to_string(),
        ));

        assert_eq!(source_map.mappings.len(), 3);

        // Test lookups
        assert_eq!(source_map.find_java_line(0), Some(2));
        assert_eq!(source_map.find_java_line(1), Some(3));
        assert_eq!(source_map.find_java_line(2), Some(4));
        assert_eq!(source_map.find_java_line(999), None);

        assert_eq!(source_map.find_jv_line(2), Some(0));
        assert_eq!(source_map.find_jv_line(3), Some(1));
        assert_eq!(source_map.find_jv_line(4), Some(2));
        assert_eq!(source_map.find_jv_line(999), None);
    }

    #[test]
    fn test_source_map_serialization() {
        let mut source_map = SourceMap::new("test.jv".to_string(), "Test.java".to_string());
        source_map.add_mapping(Mapping::simple_line_mapping(
            0,
            1,
            "jv".to_string(),
            "java".to_string(),
        ));

        // Test serialization
        let json = serde_json::to_string(&source_map).unwrap();
        assert!(json.contains("test.jv"));
        assert!(json.contains("Test.java"));

        // Test deserialization
        let deserialized: SourceMap = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.source_file, source_map.source_file);
        assert_eq!(deserialized.generated_file, source_map.generated_file);
        assert_eq!(deserialized.mappings.len(), source_map.mappings.len());
    }

    #[test]
    fn test_complex_jv_to_java_mappings() {
        let mut source_map = SourceMap::new("complex.jv".to_string(), "Complex.java".to_string());

        // Test various jv language construct mappings
        source_map.add_mapping(Mapping::new(
            0,
            5,
            0,
            4,
            "val name = \"John\"".to_string(),
            "final String name = \"John\";".to_string(),
        ));

        source_map.add_mapping(Mapping::new(
            1,
            6,
            0,
            4,
            "fun greet(name: String) = println(\"Hello, $name\")".to_string(),
            "public static void greet(String name) { System.out.println(\"Hello, \" + name); }"
                .to_string(),
        ));

        source_map.add_mapping(Mapping::new(
            2,
            8,
            0,
            4,
            "when (x) { 1 -> \"one\" else -> \"other\" }".to_string(),
            "switch (x) { case 1: return \"one\"; default: return \"other\"; }".to_string(),
        ));

        // Test complex lookups
        assert_eq!(source_map.find_java_line(0), Some(5));
        assert_eq!(source_map.find_java_line(1), Some(6));
        assert_eq!(source_map.find_java_line(2), Some(8));

        assert_eq!(source_map.find_jv_line(5), Some(0));
        assert_eq!(source_map.find_jv_line(6), Some(1));
        assert_eq!(source_map.find_jv_line(8), Some(2));
    }

    #[test]
    fn test_edge_cases() {
        let mut source_map = SourceMap::new("edge.jv".to_string(), "Edge.java".to_string());

        // Empty strings
        source_map.add_mapping(Mapping::simple_line_mapping(
            0,
            0,
            "".to_string(),
            "".to_string(),
        ));

        // Very long lines
        let long_original = "x".repeat(1000);
        let long_generated = "y".repeat(1500);
        source_map.add_mapping(Mapping::simple_line_mapping(
            1,
            1,
            long_original.clone(),
            long_generated.clone(),
        ));

        // Unicode content
        source_map.add_mapping(Mapping::simple_line_mapping(
            2,
            2,
            "変数 = 値".to_string(),
            "variable = value".to_string(),
        ));

        assert_eq!(source_map.mappings.len(), 3);
        assert_eq!(source_map.mappings[1].original_text, long_original);
        assert_eq!(source_map.mappings[1].generated_text, long_generated);
        assert_eq!(source_map.mappings[2].original_text, "変数 = 値");
    }

    #[test]
    fn test_error_handling() {
        use std::io::{Error, ErrorKind};

        // Test MappingError variants
        let pos_error = MappingError::InvalidPosition("test position".to_string());
        let gen_error = MappingError::GenerationError("test generation".to_string());
        let io_error = MappingError::IoError(Error::new(ErrorKind::NotFound, "test io"));

        assert!(pos_error.to_string().contains("Invalid source position"));
        assert!(gen_error
            .to_string()
            .contains("Source map generation error"));
        assert!(io_error.to_string().contains("I/O error"));
    }
}
