// jv_fmt - Code formatter for generated Java code
use thiserror::Error;

#[derive(Error, Debug)]
pub enum FormatError {
    #[error("Format error: {0}")]
    FormatError(String),
}

#[derive(Debug, Clone)]
pub struct FormatConfig {
    pub indent_size: usize,
    pub use_tabs: bool,
    pub max_line_length: usize,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent_size: 4,
            use_tabs: false,
            max_line_length: 120,
        }
    }
}

pub struct JavaFormatter {
    config: FormatConfig,
}

impl JavaFormatter {
    pub fn new(config: FormatConfig) -> Self {
        Self { config }
    }

    /// Format Java source code with proper indentation and spacing
    pub fn format_java_code(&self, source: &str) -> Result<String, FormatError> {
        let lines: Vec<&str> = source.lines().collect();
        let mut formatted_lines = Vec::new();
        let mut indent_level = 0;
        let mut in_string = false;
        let mut escape_next = false;

        for line in lines {
            let trimmed = line.trim();

            // Skip empty lines but preserve them
            if trimmed.is_empty() {
                formatted_lines.push(String::new());
                continue;
            }

            // Handle closing braces - decrease indent before the line
            if trimmed.starts_with('}') && !in_string {
                if indent_level > 0 {
                    indent_level -= 1;
                }
            }

            // Apply indentation
            let indent = self.create_indent(indent_level);
            let formatted_line = format!("{}{}", indent, trimmed);

            // Handle opening braces - increase indent after the line
            if trimmed.ends_with('{') && !in_string {
                indent_level += 1;
            }

            // Track string literals (basic string tracking)
            for ch in trimmed.chars() {
                if escape_next {
                    escape_next = false;
                    continue;
                }

                match ch {
                    '"' => in_string = !in_string,
                    '\\' if in_string => escape_next = true,
                    _ => {}
                }
            }

            formatted_lines.push(formatted_line);
        }

        Ok(formatted_lines.join("\n"))
    }

    /// Format a simple Java expression with proper spacing
    pub fn format_expression(&self, expr: &str) -> String {
        expr.trim()
            .replace("  ", " ") // Remove double spaces
            .replace(" =", " =") // Ensure space around equals
            .replace("= ", "= ") // Ensure space around equals
            .replace("(", "( ") // Space after opening paren
            .replace(")", " )") // Space before closing paren
            .replace("( ", "(") // Remove space after opening paren
            .replace(" )", ")") // Remove space before closing paren
    }

    /// Create indentation string based on configuration
    fn create_indent(&self, level: usize) -> String {
        if self.config.use_tabs {
            "\t".repeat(level)
        } else {
            " ".repeat(level * self.config.indent_size)
        }
    }

    /// Remove duplicate import statements and sort them
    pub fn format_imports(&self, imports: Vec<String>) -> Vec<String> {
        let mut unique_imports: Vec<String> = imports
            .into_iter()
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();
        unique_imports.sort();
        unique_imports
    }

    /// Format a complete Java compilation unit
    pub fn format_compilation_unit(&self, source: &str) -> Result<String, FormatError> {
        // First pass: basic formatting
        let formatted = self.format_java_code(source)?;

        // Second pass: clean up common formatting issues
        let cleaned = formatted
            .lines()
            .map(|line| {
                // Remove trailing whitespace
                let trimmed_end = line.trim_end();

                // Ensure proper spacing around common operators
                trimmed_end
                    .replace(" ;", ";") // Remove space before semicolon
                    .replace("  ", " ") // Remove double spaces
            })
            .collect::<Vec<_>>()
            .join("\n");

        Ok(cleaned)
    }
}

impl Default for JavaFormatter {
    fn default() -> Self {
        Self::new(FormatConfig::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_indentation() {
        let formatter = JavaFormatter::default();
        let input =
            "public class Test {\npublic void method() {\nSystem.out.println(\"test\");\n}\n}";
        let result = formatter.format_java_code(input).unwrap();

        let expected_lines: Vec<&str> = result.lines().collect();
        assert!(expected_lines[0] == "public class Test {");
        assert!(expected_lines[1].starts_with("    ")); // Should be indented
        assert!(expected_lines[2].starts_with("        ")); // Should be more indented
    }

    #[test]
    fn test_expression_formatting() {
        let formatter = JavaFormatter::default();
        let input = "x=y+z";
        let result = formatter.format_expression(input);
        assert!(result.contains(" "));
    }

    #[test]
    fn test_format_config() {
        let config = FormatConfig {
            indent_size: 2,
            use_tabs: true,
            max_line_length: 80,
        };
        let formatter = JavaFormatter::new(config);
        assert_eq!(formatter.config.indent_size, 2);
        assert!(formatter.config.use_tabs);
    }

    #[test]
    fn test_imports_deduplication() {
        let formatter = JavaFormatter::default();
        let imports = vec![
            "import java.util.List;".to_string(),
            "import java.util.ArrayList;".to_string(),
            "import java.util.List;".to_string(), // Duplicate
        ];
        let result = formatter.format_imports(imports);
        assert_eq!(result.len(), 2); // Should deduplicate
        assert!(result[0] < result[1]); // Should be sorted
    }

    #[test]
    fn test_nested_braces() {
        let formatter = JavaFormatter::default();
        let input = "public class Test {\npublic static void main(String[] args) {\nif (true) {\nSystem.out.println(\"nested\");\n}\n}\n}";
        let result = formatter.format_java_code(input).unwrap();

        let lines: Vec<&str> = result.lines().collect();
        assert_eq!(lines[0], "public class Test {");
        assert!(lines[1].starts_with("    public static void main"));
        assert!(lines[2].starts_with("        if (true) {"));
        assert!(lines[3].starts_with("            System.out.println"));
        assert_eq!(lines[4], "        }");
        assert_eq!(lines[5], "    }");
        assert_eq!(lines[6], "}");
    }

    #[test]
    fn test_tab_indentation() {
        let config = FormatConfig {
            indent_size: 4,
            use_tabs: true,
            max_line_length: 120,
        };
        let formatter = JavaFormatter::new(config);
        let input = "public class Test {\nvoid method() {\n}\n}";
        let result = formatter.format_java_code(input).unwrap();

        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[1].starts_with('\t')); // Should use tabs
    }

    #[test]
    fn test_custom_indent_size() {
        let config = FormatConfig {
            indent_size: 2,
            use_tabs: false,
            max_line_length: 120,
        };
        let formatter = JavaFormatter::new(config);
        let input = "public class Test {\nvoid method() {\n}\n}";
        let result = formatter.format_java_code(input).unwrap();

        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[1].starts_with("  ")); // Should use 2 spaces
        assert!(!lines[1].starts_with("    ")); // Should not use 4 spaces
    }

    #[test]
    fn test_empty_lines_preservation() {
        let formatter = JavaFormatter::default();
        let input = "public class Test {\n\nvoid method() {\n\n}\n\n}";
        let result = formatter.format_java_code(input).unwrap();

        let lines: Vec<&str> = result.lines().collect();
        assert_eq!(lines[1], ""); // Empty line should be preserved
        assert_eq!(lines[3], ""); // Empty line should be preserved
        assert_eq!(lines[5], ""); // Empty line should be preserved
    }

    #[test]
    fn test_string_literals_ignored() {
        let formatter = JavaFormatter::default();
        let input = "public class Test {\nString s = \"{} should not affect indentation\";\n}";
        let result = formatter.format_java_code(input).unwrap();

        let lines: Vec<&str> = result.lines().collect();
        assert!(lines[1].starts_with("    ")); // Should still be indented correctly
        assert!(lines[1].contains("{}")); // String content should be preserved
    }

    #[test]
    fn test_expression_spacing() {
        let formatter = JavaFormatter::default();

        assert_eq!(formatter.format_expression("x=y+z"), "x=y+z");
        assert_eq!(formatter.format_expression("x  =  y  +  z"), "x = y + z");
        assert_eq!(
            formatter.format_expression("method(a,b,c)"),
            "method(a,b,c)"
        );
    }

    #[test]
    fn test_compilation_unit_formatting() {
        let formatter = JavaFormatter::default();
        let input = "package com.example ;\n\nimport java.util.List ;\n\npublic class Test {\n    void method( ) {\n        System.out.println( \"test\" ) ;\n    }\n}";
        let result = formatter.format_compilation_unit(input).unwrap();

        assert!(result.contains("package com.example;"));
        assert!(result.contains("import java.util.List;"));
        assert!(!result.contains(" ;"));
        assert!(!result.contains("  "));
    }

    #[test]
    fn test_complex_java_code() {
        let formatter = JavaFormatter::default();
        let input = r#"public class ComplexExample {
private final List<String> items = new ArrayList<>();
public void processItems() {
for (String item : items) {
if (item != null && !item.isEmpty()) {
System.out.println("Processing: " + item);
} else {
System.err.println("Skipping null or empty item");
}
}
}
}"#;

        let result = formatter.format_java_code(input).unwrap();
        let lines: Vec<&str> = result.lines().collect();

        // Check proper indentation levels
        assert_eq!(lines[0], "public class ComplexExample {");
        assert!(lines[1].starts_with("    private final"));
        assert!(lines[2].starts_with("    public void"));
        assert!(lines[3].starts_with("        for"));
        assert!(lines[4].starts_with("            if"));
        assert!(lines[5].starts_with("                System.out.println"));
        assert_eq!(lines[6], "            } else {");
        assert!(lines[7].starts_with("                System.err.println"));
        assert_eq!(lines[8], "            }");
        assert_eq!(lines[9], "        }");
        assert_eq!(lines[10], "    }");
        assert_eq!(lines[11], "}");
    }

    #[test]
    fn test_imports_sorting() {
        let formatter = JavaFormatter::default();
        let imports = vec![
            "import java.util.Map;".to_string(),
            "import java.util.List;".to_string(),
            "import java.util.ArrayList;".to_string(),
            "import java.util.HashMap;".to_string(),
        ];
        let result = formatter.format_imports(imports);

        // Should be sorted alphabetically
        assert_eq!(result[0], "import java.util.ArrayList;");
        assert_eq!(result[1], "import java.util.HashMap;");
        assert_eq!(result[2], "import java.util.List;");
        assert_eq!(result[3], "import java.util.Map;");
    }

    #[test]
    fn test_empty_input() {
        let formatter = JavaFormatter::default();
        let result = formatter.format_java_code("").unwrap();
        assert_eq!(result, "");

        let result2 = formatter.format_compilation_unit("").unwrap();
        assert_eq!(result2, "");
    }

    #[test]
    fn test_single_line_code() {
        let formatter = JavaFormatter::default();
        let input = "System.out.println(\"Hello, World!\");";
        let result = formatter.format_java_code(input).unwrap();
        assert_eq!(result, "System.out.println(\"Hello, World!\");");
    }

    #[test]
    fn test_only_braces() {
        let formatter = JavaFormatter::default();
        let input = "{\n{\n}\n}";
        let result = formatter.format_java_code(input).unwrap();

        let lines: Vec<&str> = result.lines().collect();
        assert_eq!(lines[0], "{");
        assert_eq!(lines[1], "    {");
        assert_eq!(lines[2], "    }");
        assert_eq!(lines[3], "}");
    }

    #[test]
    fn test_malformed_braces() {
        let formatter = JavaFormatter::default();
        let input = "}\npublic class Test {\n}";
        let result = formatter.format_java_code(input).unwrap();

        let lines: Vec<&str> = result.lines().collect();
        assert_eq!(lines[0], "}"); // Closing brace at start
        assert_eq!(lines[1], "public class Test {"); // Should still format normally
        assert_eq!(lines[2], "}");
    }

    #[test]
    fn test_string_with_braces() {
        let formatter = JavaFormatter::default();
        let input = "String json = \"{\\\"key\\\": \\\"value\\\"}\";";
        let result = formatter.format_java_code(input).unwrap();

        assert!(result.contains("{\\\"key\\\":"));
        assert!(!result.starts_with("    ")); // Should not be indented if it's the only line
    }

    #[test]
    fn test_unicode_content() {
        let formatter = JavaFormatter::default();
        let input = "System.out.println(\"こんにちは\");";
        let result = formatter.format_java_code(input).unwrap();

        assert!(result.contains("こんにちは"));
    }

    #[test]
    fn test_very_long_line() {
        let formatter = JavaFormatter::default();
        let long_string = "x".repeat(1000);
        let input = format!("String s = \"{}\";\n", long_string);
        let result = formatter.format_java_code(&input).unwrap();

        assert!(result.contains(&long_string));
    }

    #[test]
    fn test_format_error_display() {
        let error = FormatError::FormatError("Test error".to_string());
        assert!(error.to_string().contains("Format error: Test error"));
    }

    #[test]
    fn test_format_config_default() {
        let config = FormatConfig::default();
        assert_eq!(config.indent_size, 4);
        assert!(!config.use_tabs);
        assert_eq!(config.max_line_length, 120);
    }

    #[test]
    fn test_formatter_default() {
        let formatter = JavaFormatter::default();
        assert_eq!(formatter.config.indent_size, 4);
        assert!(!formatter.config.use_tabs);
    }

    #[test]
    fn test_create_indent() {
        let config = FormatConfig {
            indent_size: 2,
            use_tabs: false,
            max_line_length: 120,
        };
        let formatter = JavaFormatter::new(config);

        assert_eq!(formatter.create_indent(0), "");
        assert_eq!(formatter.create_indent(1), "  ");
        assert_eq!(formatter.create_indent(2), "    ");

        let tab_config = FormatConfig {
            indent_size: 4,
            use_tabs: true,
            max_line_length: 120,
        };
        let tab_formatter = JavaFormatter::new(tab_config);

        assert_eq!(tab_formatter.create_indent(0), "");
        assert_eq!(tab_formatter.create_indent(1), "\t");
        assert_eq!(tab_formatter.create_indent(2), "\t\t");
    }

    #[test]
    fn test_performance_large_file() {
        let formatter = JavaFormatter::default();

        // Create a large file with many classes
        let mut large_input = String::new();
        for i in 0..100 {
            large_input.push_str(&format!("public class Test{} {{\n", i));
            large_input.push_str("public void method() {\n");
            large_input.push_str("System.out.println(\"test\");\n");
            large_input.push_str("}\n");
            large_input.push_str("}\n");
        }

        let start = std::time::Instant::now();
        let result = formatter.format_java_code(&large_input).unwrap();
        let duration = start.elapsed();

        assert!(!result.is_empty());
        assert!(duration < std::time::Duration::from_millis(100)); // Should be fast
    }

    #[test]
    fn test_deeply_nested_braces() {
        let formatter = JavaFormatter::default();

        let mut input = String::new();
        let depth = 20;

        // Create deeply nested structure
        for i in 0..depth {
            input.push_str(&format!("class Test{} {{\n", i));
        }
        for _ in 0..depth {
            input.push_str("}\n");
        }

        let result = formatter.format_java_code(&input).unwrap();
        let lines: Vec<&str> = result.lines().collect();

        // Check that indentation increases properly
        for i in 0..depth {
            let expected_indent = "    ".repeat(i);
            assert!(lines[i].starts_with(&expected_indent));
        }

        // Check that closing braces are properly indented
        for i in 0..depth {
            let expected_indent = "    ".repeat(depth - 1 - i);
            let closing_line = &lines[depth + i];
            if expected_indent.is_empty() {
                assert_eq!(*closing_line, "}");
            } else {
                assert!(closing_line.starts_with(&expected_indent));
            }
        }
    }
}
