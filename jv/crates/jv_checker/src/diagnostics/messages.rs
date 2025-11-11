use crate::inference::conversions::HelperSpec;
use jv_ir::error::TestLoweringDiagnostic;

/// Returns a human friendly label for a helper method.
pub fn helper_label(helper: &HelperSpec) -> String {
    if helper.is_static {
        format!("{}::{}", helper.owner, helper.method)
    } else {
        format!("{}#{}", helper.owner, helper.method)
    }
}

/// Formats a recommendation message describing how often a helper was applied implicitly.
pub fn helper_recommendation(helper: &HelperSpec, count: usize) -> String {
    let label = helper_label(helper);
    let times = match count {
        0 => "no times".to_string(),
        1 => "once".to_string(),
        2 => "twice".to_string(),
        value => format!("{value} times"),
    };

    let invocation_hint = if helper.is_static {
        "Invoke it directly as a static helper when you prefer explicit conversions."
    } else {
        "Call it on the value explicitly when readability matters."
    };

    format!("{label} was applied implicitly {times}. {invocation_hint}")
}

pub fn format_test_lowering_message(
    code: &str,
    fallback: &str,
    details: Option<&TestLoweringDiagnostic>,
) -> (String, Vec<String>) {
    match details {
        Some(TestLoweringDiagnostic::DatasetColumnMismatch {
            parameter_count,
            column_count,
        }) => dataset_column_mismatch_message(code, *parameter_count, *column_count),
        Some(TestLoweringDiagnostic::AssertionRewriteRequired) => assertion_rewrite_message(code),
        None => (format!("{code}: {fallback}"), Vec::new()),
    }
}

fn dataset_column_mismatch_message(
    code: &str,
    parameter_count: usize,
    column_count: usize,
) -> (String, Vec<String>) {
    let message = format!(
        "{code}: 宣言されたテストパラメータは{parameter_count}個ですが、データセット行は{column_count}列です。列数を揃えてください。\n{code}: Declared {parameter_count} parameters but dataset rows provide {column_count} columns; align the column count with the parameter list.",
    );
    let suggestions = vec![
        format!(
            "Quick Fix: tests.dataset.align-columns -> データ行を {parameter_count} 列に揃える"
        ),
        format!(
            "Quick Fix: tests.dataset.align-columns -> Align dataset rows to {parameter_count} columns"
        ),
    ];
    (message, suggestions)
}

fn assertion_rewrite_message(code: &str) -> (String, Vec<String>) {
    let message = format!(
        "{code}: boolean 以外の式は自動的に JUnit アサーションへ変換できません。Assertions.assertEquals(...) などの明示的な呼び出しに書き換えてください。\n{code}: Non-boolean expressions cannot be converted into JUnit assertions automatically. Rewrite the expression using an explicit Assertions.assert* call.",
    );
    let suggestions = vec![
        "Quick Fix: tests.assertion.rewrite -> Assertions.assertEquals(expected, actual)"
            .to_string(),
        "Quick Fix: tests.assertion.rewrite -> Wrap expression with Assertions.assert*".to_string(),
    ];
    (message, suggestions)
}
