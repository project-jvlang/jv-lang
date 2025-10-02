use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum JavaNullabilityHint {
    Nullable,
    NonNull,
    NullMarked,
}

const ENTRIES: &[(&[&str], JavaNullabilityHint, u8)] = &[
    // JetBrains annotations (highest precedence)
    (
        &["org.jetbrains.annotations.notnull", "notnull"],
        JavaNullabilityHint::NonNull,
        100,
    ),
    (
        &["org.jetbrains.annotations.nullable", "nullable"],
        JavaNullabilityHint::Nullable,
        100,
    ),
    // JSR-305 / Jakarta / javax variants
    (
        &[
            "javax.annotation.nonnull",
            "jakarta.annotation.nonnull",
            "edu.umd.cs.findbugs.annotations.nonnull",
            "non-null",
            "nonnull",
        ],
        JavaNullabilityHint::NonNull,
        80,
    ),
    (
        &[
            "javax.annotation.nullable",
            "jakarta.annotation.nullable",
            "javax.annotation.checkfornull",
            "checkfornull",
        ],
        JavaNullabilityHint::Nullable,
        80,
    ),
    // Checker Framework
    (
        &["org.checkerframework.checker.nullness.qual.nonnull"],
        JavaNullabilityHint::NonNull,
        60,
    ),
    (
        &["org.checkerframework.checker.nullness.qual.nullable"],
        JavaNullabilityHint::Nullable,
        60,
    ),
    // Spring
    (
        &["org.springframework.lang.nonnull"],
        JavaNullabilityHint::NonNull,
        40,
    ),
    (
        &["org.springframework.lang.nullable"],
        JavaNullabilityHint::Nullable,
        40,
    ),
];

static PRECEDENCE: OnceLock<HashMap<&'static str, (JavaNullabilityHint, u8)>> = OnceLock::new();

fn precedence_map() -> &'static HashMap<&'static str, (JavaNullabilityHint, u8)> {
    PRECEDENCE.get_or_init(|| {
        let mut map = HashMap::new();
        for (names, hint, priority) in ENTRIES {
            for name in *names {
                map.insert(*name, (*hint, *priority));
            }
        }
        map
    })
}

pub fn lookup_nullability_hint(annotation: &str) -> Option<(JavaNullabilityHint, u8)> {
    let normalized = annotation
        .trim()
        .trim_start_matches('@')
        .to_ascii_lowercase();
    precedence_map().get(normalized.as_str()).copied()
}
