use crate::{Literal, Span};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Structured representation of an annotation applied to declarations.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Annotation {
    pub name: AnnotationName,
    #[serde(default)]
    pub arguments: Vec<AnnotationArgument>,
    pub span: Span,
}

impl Annotation {
    pub fn qualified_name(&self) -> String {
        self.name.qualified_name()
    }
}

/// Qualified annotation name broken into package segments.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AnnotationName {
    pub segments: Vec<String>,
    pub span: Span,
}

impl AnnotationName {
    pub fn new(segments: Vec<String>, span: Span) -> Self {
        Self { segments, span }
    }

    pub fn simple_name(&self) -> &str {
        self.segments.last().map(String::as_str).unwrap_or("")
    }

    pub fn qualified_name(&self) -> String {
        self.segments.join(".")
    }

    pub fn split_package(&self) -> (Option<String>, String) {
        if self.segments.len() <= 1 {
            return (None, self.simple_name().to_string());
        }

        let package = self.segments[..self.segments.len() - 1].join(".");
        (Some(package), self.simple_name().to_string())
    }
}

/// Annotation argument storing positional or named value forms.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AnnotationArgument {
    Positional {
        value: AnnotationValue,
        span: Span,
    },
    Named {
        name: String,
        value: AnnotationValue,
        span: Span,
    },
}

impl AnnotationArgument {
    pub fn span(&self) -> &Span {
        match self {
            AnnotationArgument::Positional { span, .. }
            | AnnotationArgument::Named { span, .. } => span,
        }
    }
}

/// Supported annotation value variants mirroring Java semantics.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AnnotationValue {
    Literal(Literal),
    EnumConstant {
        type_path: Vec<String>,
        constant: String,
    },
    Array(Vec<AnnotationValue>),
    ClassLiteral {
        type_path: Vec<String>,
    },
    NestedAnnotation(Box<Annotation>),
}

/// Metadata describing reserved jv annotations and their conflict rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ReservedAnnotation {
    pub name: &'static str,
    pub purpose: &'static str,
    pub conflicts_with: &'static [&'static str],
}

pub const JV_RESERVED_ANNOTATIONS: &[ReservedAnnotation] = &[
    ReservedAnnotation {
        name: "Sample",
        purpose: "サンプルデータ駆動開発",
        conflicts_with: &["PactSample"],
    },
    ReservedAnnotation {
        name: "UnitSystem",
        purpose: "カスタム単位系合成",
        conflicts_with: &[],
    },
    ReservedAnnotation {
        name: "PactSample",
        purpose: "Pact契約テスト",
        conflicts_with: &["Sample"],
    },
    ReservedAnnotation {
        name: "Mock",
        purpose: "テストモック指定",
        conflicts_with: &[],
    },
];

/// Conflict kind encountered when combining reserved annotations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReservedConflictKind {
    DuplicateUsage,
    MutualExclusion { other: &'static str },
    NameShadowing { reserved: &'static str },
}

/// Conflict payload linking the offending annotation(s) and rationale.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ReservedAnnotationConflict<'a> {
    pub primary: &'a Annotation,
    pub secondary: Option<&'a Annotation>,
    pub kind: ReservedConflictKind,
}

pub fn reserved_annotation(simple_name: &str) -> Option<&'static ReservedAnnotation> {
    JV_RESERVED_ANNOTATIONS
        .iter()
        .find(|reserved| reserved.name == simple_name)
}

pub fn is_jv_reserved(simple_name: &str) -> bool {
    reserved_annotation(simple_name).is_some()
}

pub fn detect_reserved_conflicts<'a>(
    annotations: &'a [Annotation],
) -> Vec<ReservedAnnotationConflict<'a>> {
    let mut conflicts = Vec::new();
    let mut grouped: HashMap<String, Vec<&Annotation>> = HashMap::new();

    for annotation in annotations {
        let key = annotation.name.simple_name().to_string();
        grouped.entry(key).or_default().push(annotation);
    }

    for (simple, annotations_with_name) in &grouped {
        if let Some(reserved) = reserved_annotation(simple) {
            let mut reserved_uses = Vec::new();
            let mut shadowing = Vec::new();
            for annotation in annotations_with_name {
                if annotation.name.segments.len() == 1 {
                    reserved_uses.push(*annotation);
                } else {
                    shadowing.push(*annotation);
                }
            }

            if reserved_uses.len() > 1 {
                if let Some(first) = reserved_uses.first() {
                    for duplicate in reserved_uses.iter().skip(1) {
                        conflicts.push(ReservedAnnotationConflict {
                            primary: *first,
                            secondary: Some(*duplicate),
                            kind: ReservedConflictKind::DuplicateUsage,
                        });
                    }
                }
            }

            for annotation in shadowing {
                conflicts.push(ReservedAnnotationConflict {
                    primary: annotation,
                    secondary: None,
                    kind: ReservedConflictKind::NameShadowing {
                        reserved: reserved.name,
                    },
                });
            }

            if !reserved_uses.is_empty() {
                for other in reserved.conflicts_with {
                    if let Some(other_group) = grouped.get(*other) {
                        let other_reserved: Vec<_> = other_group
                            .iter()
                            .filter_map(|ann| {
                                if ann.name.segments.len() == 1 {
                                    Some(*ann)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        if !other_reserved.is_empty() && simple.as_str() <= *other {
                            conflicts.push(ReservedAnnotationConflict {
                                primary: *reserved_uses.first().unwrap(),
                                secondary: Some(other_reserved[0]),
                                kind: ReservedConflictKind::MutualExclusion { other },
                            });
                        }
                    }
                }
            }
        }
    }

    conflicts
}
