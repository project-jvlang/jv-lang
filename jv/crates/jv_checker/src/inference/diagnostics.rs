use crate::diagnostics::{DiagnosticDescriptor, DiagnosticSeverity, EnhancedDiagnostic};
use crate::inference::conversions::{AppliedConversion, ConversionKind};

/// Diagnostic descriptors surfaced for implicit conversions.
pub const CONVERSION_DIAGNOSTICS: &[DiagnosticDescriptor] = &[
    DiagnosticDescriptor {
        code: "JV_TYPE_001",
        title: "Implicit conversion applied",
        help: "The compiler applied an implicit conversion. Review the generated Java if you prefer an explicit transform.",
        severity: DiagnosticSeverity::Information,
    },
    DiagnosticDescriptor {
        code: "JV_TYPE_002",
        title: "Primitive widening conversion applied",
        help: "A primitive value was widened to a larger type. Consider using an explicit cast if this is unintended.",
        severity: DiagnosticSeverity::Information,
    },
    DiagnosticDescriptor {
        code: "JV_TYPE_003",
        title: "Conversion helper method invoked",
        help: "A helper method is required for this conversion. Confirm that the runtime semantics match expectations.",
        severity: DiagnosticSeverity::Warning,
    },
    DiagnosticDescriptor {
        code: "JV_TYPE_004",
        title: "Narrowing conversion requires explicit cast",
        help: "Provide an explicit cast to acknowledge the narrowing conversion and avoid potential data loss.",
        severity: DiagnosticSeverity::Error,
    },
];

/// Build a diagnostic describing the supplied conversion, if applicable.
pub fn conversion_diagnostic(conversion: &AppliedConversion) -> Option<EnhancedDiagnostic> {
    let descriptor = match conversion.kind {
        ConversionKind::Identity => return None,
        ConversionKind::Boxing | ConversionKind::Unboxing => Some(&CONVERSION_DIAGNOSTICS[0]),
        ConversionKind::WideningPrimitive => Some(&CONVERSION_DIAGNOSTICS[1]),
        ConversionKind::StringConversion | ConversionKind::MethodInvocation => {
            Some(&CONVERSION_DIAGNOSTICS[2])
        }
    }?;

    let message = match conversion.kind {
        ConversionKind::Boxing => format!(
            "Implicitly boxed `{}` to `{}`",
            conversion.from_type.describe(),
            conversion.to_type.describe()
        ),
        ConversionKind::Unboxing => format!(
            "Implicitly unboxed `{}` to `{}`",
            conversion.from_type.describe(),
            conversion.to_type.describe()
        ),
        ConversionKind::WideningPrimitive => format!(
            "Widened `{}` to `{}`",
            conversion.from_type.describe(),
            conversion.to_type.describe()
        ),
        ConversionKind::StringConversion | ConversionKind::MethodInvocation => {
            if let Some(helper) = conversion.helper_method.as_ref() {
                format!(
                    "Invoking {}::{} while converting `{}` to `{}`",
                    helper.owner,
                    helper.method,
                    conversion.from_type.describe(),
                    conversion.to_type.describe()
                )
            } else {
                format!(
                    "Applied helper conversion from `{}` to `{}`",
                    conversion.from_type.describe(),
                    conversion.to_type.describe()
                )
            }
        }
        ConversionKind::Identity => unreachable!(),
    };

    Some(EnhancedDiagnostic::new(
        descriptor,
        message,
        conversion.source_span.clone(),
    ))
}
