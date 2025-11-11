use crate::expression::StringPart;
use crate::types::Span;
use serde::{Deserialize, Serialize};

/// Kinds of multiline string delimiters supported by the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MultilineKind {
    /// Triple double quote delimiter (""" ... """).
    TripleQuote,
    /// Backtick-based delimiter (``` ... ```).
    Backtick,
    /// Raw single quote delimiter (' ... ').
    RawSingle,
    /// Raw triple single quote delimiter (''' ... ''').
    RawTriple,
}

/// Flavor information for raw string literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RawStringFlavor {
    /// Raw strings declared with single quotes.
    SingleLine,
    /// Raw strings declared with triple single quotes.
    MultiLine,
}

/// Indentation metadata calculated for multiline string literals.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IndentMetadata {
    /// Baseline indentation width that was removed during normalization.
    pub base_indent: usize,
    /// Whether trailing blank lines were trimmed as part of normalization.
    #[serde(default)]
    pub trimmed_trailing: bool,
}

impl IndentMetadata {
    pub fn new(base_indent: usize, trimmed_trailing: bool) -> Self {
        Self {
            base_indent,
            trimmed_trailing,
        }
    }
}

/// Representation of a multiline string literal, including optional interpolation parts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MultilineStringLiteral {
    pub kind: MultilineKind,
    pub normalized: String,
    pub raw: String,
    #[serde(default)]
    pub parts: Vec<StringPart>,
    #[serde(default)]
    pub indent: Option<IndentMetadata>,
    #[serde(default)]
    pub raw_flavor: Option<RawStringFlavor>,
    pub span: Span,
}
