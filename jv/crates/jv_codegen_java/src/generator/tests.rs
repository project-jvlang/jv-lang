use super::*;

pub(super) fn register_test_class_imports(
    generator: &mut JavaCodeGenerator,
    statement: &IrStatement,
) {
    if let Some(features) = TestClassFeatures::from_class(statement) {
        features.apply(generator);
    }
}

#[derive(Default, Clone, Copy)]
struct TestClassFeatures {
    uses_junit_api: bool,
    uses_parameterized: bool,
    uses_method_source: bool,
    needs_stream_api: bool,
}

impl TestClassFeatures {
    fn from_class(statement: &IrStatement) -> Option<Self> {
        let IrStatement::ClassDeclaration { methods, .. } = statement else {
            return None;
        };

        let mut features = TestClassFeatures::default();
        let mut is_test_suite = false;

        for method in methods {
            if let Some(method_features) = Self::from_method(method) {
                features.merge(&method_features.features);
                if method_features.marked_test_suite {
                    is_test_suite = true;
                }
            }
        }

        if !is_test_suite {
            return None;
        }

        if features.uses_method_source {
            features.needs_stream_api = true;
        }

        Some(features)
    }

    fn from_method(statement: &IrStatement) -> Option<MethodFeatures> {
        let method = extract_method(statement)?;

        let IrStatement::MethodDeclaration {
            modifiers,
            assertion_patterns,
            return_type,
            ..
        } = method
        else {
            return None;
        };

        let mut features = TestClassFeatures::default();
        let mut marked = false;

        if !assertion_patterns.is_empty() {
            features.uses_junit_api = true;
            marked = true;
        }

        if returns_stream_type(return_type) {
            features.needs_stream_api = true;
        }

        for annotation in &modifiers.annotations {
            let simple = annotation.name.simple_name();
            if is_junit_api_annotation(simple) {
                features.uses_junit_api = true;
                marked = true;
            }
            if is_parameterized_annotation(simple) {
                features.uses_junit_api = true;
                features.uses_parameterized = true;
                marked = true;
            }
            if is_method_source_annotation(simple) {
                features.uses_junit_api = true;
                features.uses_parameterized = true;
                features.uses_method_source = true;
                marked = true;
            }
        }

        Some(MethodFeatures {
            features,
            marked_test_suite: marked,
        })
    }

    fn merge(&mut self, other: &TestClassFeatures) {
        self.uses_junit_api |= other.uses_junit_api;
        self.uses_parameterized |= other.uses_parameterized;
        self.uses_method_source |= other.uses_method_source;
        self.needs_stream_api |= other.needs_stream_api;
    }

    fn apply(self, generator: &mut JavaCodeGenerator) {
        if self.uses_junit_api {
            generator.add_import("org.junit.jupiter.api.*");
        }
        if self.uses_parameterized {
            generator.add_import("org.junit.jupiter.params.*");
        }
        if self.uses_method_source {
            generator.add_import("org.junit.jupiter.params.provider.*");
        }
        if self.needs_stream_api {
            generator.add_import("java.util.stream.Stream");
        }
    }
}

struct MethodFeatures {
    features: TestClassFeatures,
    marked_test_suite: bool,
}

fn extract_method<'a>(statement: &'a IrStatement) -> Option<&'a IrStatement> {
    match statement {
        IrStatement::Commented { statement, .. } => extract_method(statement),
        IrStatement::MethodDeclaration { .. } => Some(statement),
        _ => None,
    }
}

fn returns_stream_type(java_type: &JavaType) -> bool {
    match java_type {
        JavaType::Reference { name, .. } => name == "java.util.stream.Stream" || name == "Stream",
        _ => false,
    }
}

fn is_junit_api_annotation(name: &str) -> bool {
    matches!(
        name,
        "Test"
            | "DisplayName"
            | "BeforeEach"
            | "AfterEach"
            | "BeforeAll"
            | "AfterAll"
            | "Nested"
            | "Disabled"
    )
}

fn is_parameterized_annotation(name: &str) -> bool {
    matches!(name, "ParameterizedTest")
}

fn is_method_source_annotation(name: &str) -> bool {
    matches!(name, "MethodSource")
}
