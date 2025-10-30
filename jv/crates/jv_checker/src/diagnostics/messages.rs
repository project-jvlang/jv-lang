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

/// 多言語対応メッセージを生成するユーティリティ。
pub fn bilingual_message(code: &str, ja: impl Into<String>, en: impl Into<String>) -> String {
    let ja = ja.into();
    let en = en.into();
    format!("{code}: {ja}\n{code} (EN): {en}")
}

/// Optional 左辺に対する正規表現 `is` 警告メッセージ。
pub fn regex_optional_warning_message() -> String {
    bilingual_message(
        "JV_REGEX_W001",
        "Optional 型の左辺ではコンパイラが `value != null && matcher(value)` を自動挿入します。明示的な null ガードを追加して意図を読みやすくすることも検討してください。",
        "When the left-hand side is Optional, the compiler expands it to `value != null && matcher(value)`. Consider adding an explicit null guard to make the intent clearer.",
    )
}

/// 左辺型が正規表現互換でない場合のエラーメッセージ。
pub fn regex_subject_error_message(ty_desc: &str) -> String {
    bilingual_message(
        "JV_REGEX_E002",
        format!(
            "`is /pattern/` の左辺型 `{}` は CharSequence 互換である必要があります。`toString()` で文字列化するか、CharSequence を実装した型を使用してください。",
            ty_desc
        ),
        format!(
            "The left-hand side type `{}` must be compatible with CharSequence when using `is /pattern/`. Convert it with `toString()` or provide a CharSequence implementation.",
            ty_desc
        ),
    )
}

/// 右辺型が `java.util.regex.Pattern` でない場合のエラーメッセージ。
pub fn regex_pattern_error_message(ty_desc: &str) -> String {
    bilingual_message(
        "JV_REGEX_E003",
        format!(
            "`is` の右辺は `java.util.regex.Pattern` 型でなければなりません。現在の型は `{}` です。`Pattern.compile(...)` で生成するか、Pattern 型の値を渡してください。",
            ty_desc
        ),
        format!(
            "The right-hand side of `is` must be a `java.util.regex.Pattern`, but the current type is `{}`. Construct a Pattern with `Pattern.compile(...)` or pass an existing Pattern.",
            ty_desc
        ),
    )
}
