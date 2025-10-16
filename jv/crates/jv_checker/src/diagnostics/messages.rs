use crate::inference::conversions::HelperSpec;

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
