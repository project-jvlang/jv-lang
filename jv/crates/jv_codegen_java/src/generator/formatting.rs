use super::*;
use jv_ast::types::{RawTypeContinuation, RawTypeDirective};

impl JavaCodeGenerator {
    /// Escape special characters in Java string literals.
    ///
    /// Handles: backslash, double quotes, newline, carriage return, tab.
    pub(super) fn escape_string(value: &str) -> String {
        value
            .replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
    }

    /// Push multiple lines to JavaSourceBuilder.
    ///
    /// Splits text by newlines and pushes each line individually.
    pub(super) fn push_lines(builder: &mut JavaSourceBuilder, text: &str) {
        for line in text.lines() {
            builder.push_line(line);
        }
    }

    /// Append inline comment to generated code.
    ///
    /// If code is empty, returns comment only.
    /// Otherwise, appends comment to the last line with a space separator.
    pub(super) fn append_inline_comment(mut code: String, comment: &str) -> String {
        if code.is_empty() {
            return comment.to_string();
        }

        let mut lines: Vec<String> = code.lines().map(|line| line.to_string()).collect();

        if let Some(last_line) = lines.last_mut() {
            if !last_line.trim_end().is_empty() {
                last_line.push(' ');
            }
            last_line.push_str(comment);
        } else {
            lines.push(comment.to_string());
        }

        code.clear();
        for (index, line) in lines.iter().enumerate() {
            if index > 0 {
                code.push('\n');
            }
            code.push_str(line);
        }

        code
    }

    pub(super) fn render_raw_type_comment(directive: &RawTypeDirective) -> String {
        let prefix = match directive.mode {
            RawTypeContinuation::AllowWithComment => "jv:raw-allow",
            RawTypeContinuation::DefaultPolicy => "jv:raw-default",
        };

        format!("// {} {}", prefix, directive.owner.qualified())
    }

    pub(super) fn parse_raw_type_comment(comment: &str) -> Option<(RawTypeContinuation, String)> {
        let trimmed = comment.trim();
        let content = if let Some(rest) = trimmed.strip_prefix("//") {
            rest.trim_start_matches('*').trim()
        } else if let Some(rest) = trimmed.strip_prefix("/*") {
            rest.trim_end_matches("*/").trim()
        } else {
            trimmed
        };

        let (mode, payload) = if let Some(rest) = content.strip_prefix("jv:raw-allow") {
            (RawTypeContinuation::AllowWithComment, rest.trim())
        } else if let Some(rest) = content.strip_prefix("jv:raw-default") {
            (RawTypeContinuation::DefaultPolicy, rest.trim())
        } else {
            return None;
        };

        let normalized_owner = payload
            .split('.')
            .map(|segment| segment.trim())
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>()
            .join(".");

        if normalized_owner.is_empty() {
            return None;
        }

        Some((mode, normalized_owner))
    }
}
