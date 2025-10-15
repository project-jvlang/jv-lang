// jv_fmt - Code formatter for generated Java code
use jv_checker::diagnostics;
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

/// 新しい診断コードの補足テキストを取得するユーティリティ。
pub fn diagnostic_note(code: &str) -> Option<&'static str> {
    diagnostics::lookup(code).map(|descriptor| descriptor.help)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diagnostic_note_returns_help_for_jv2101() {
        let help = diagnostic_note("JV2101").expect("expected help text for JV2101");
        assert!(help.contains("要素"));
    }

    #[test]
    fn diagnostic_note_returns_none_for_unknown_code() {
        assert!(diagnostic_note("JV9999").is_none());
    }
}
