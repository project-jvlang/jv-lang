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

/// Java公式正規表現仕様への参照URL。
pub const JAVA_REGEX_DOC_URL: &str =
    "https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/regex/Pattern.html";

fn regex_message(code: &str, ja: impl Into<String>, en: impl Into<String>) -> String {
    let ja = ja.into();
    let en = en.into();
    let ja = format!("{ja}\n参考資料: {JAVA_REGEX_DOC_URL}");
    let en = format!("{en}\nReference: {JAVA_REGEX_DOC_URL}");
    bilingual_message(code, ja, en)
}

/// 生文字列リテラルが未終端の場合のエラーメッセージ。
pub fn raw_string_unterminated_message(line: usize, column: usize) -> String {
    bilingual_message(
        "JV4300",
        format!(
            "生文字列リテラルが閉じられていません（{}行{}列）。' または ''' で閉じてください。参考: docs/language/raw-strings.md",
            line, column
        ),
        format!(
            "Raw string literal is unterminated at line {}, column {}. Close it with ' or '''. See: docs/language/raw-strings.md",
            line, column
        ),
    )
}

/// Optional 左辺に対する正規表現 `is` 警告メッセージ。
pub fn regex_optional_warning_message() -> String {
    regex_message(
        "JV_REGEX_W001",
        "Optional 型の左辺ではコンパイラが `value != null && matcher(value)` を自動挿入します。明示的な null ガードを追加して意図を読みやすくすることも検討してください。",
        "When the left-hand side is Optional, the compiler expands it to `value != null && matcher(value)`. Consider adding an explicit null guard to make the intent clearer.",
    )
}

/// 左辺型が正規表現互換でない場合のエラーメッセージ。
pub fn regex_subject_error_message(ty_desc: &str) -> String {
    regex_message(
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
    regex_message(
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

/// モード `m` とフラグ `m` の混同に関する情報メッセージ。
pub fn regex_mode_flag_confusion_message() -> String {
    regex_message(
        "JV_REGEX_I001",
        "正規表現リテラルでモード `[match]` とフラグ `m` (MULTILINE) が混同されています。モード指定を省略するか `[match]` を明示して区別してください。",
        "Regex literal mode `[match]` is being confused with flag `m` (MULTILINE). Omit the mode or spell out `[match]` to keep them distinct.",
    )
}

/// 置換目的が不明な構文に関する情報メッセージ。
pub fn regex_ambiguous_mode_message() -> String {
    regex_message(
        "JV_REGEX_I002",
        "正規表現リテラルのモードがあいまいです。判定目的なら `[match]` を明示し、置換目的なら `//` を追加してください。",
        "Regex literal mode is ambiguous. Add `[match]` for a predicate or append `//` for a replacement.",
    )
}

/// 未知フラグ検出時のエラーメッセージ。
pub fn regex_unknown_flag_message(flag: char) -> String {
    regex_message(
        "JV_REGEX_E101",
        format!(
            "正規表現リテラルに未知のフラグ `{}` が含まれています。利用可能なフラグは `i`, `m`, `s`, `u`, `d`, `x`, `l`, `c` です。",
            flag
        ),
        format!(
            "Regex literal flag `{}` is unknown. Valid flags are `i`, `m`, `s`, `u`, `d`, `x`, `l`, `c`.",
            flag
        ),
    )
}

/// 排他的なフラグ組み合わせに関するエラーメッセージ。
pub fn regex_flag_conflict_message(primary: char, secondary: char) -> String {
    regex_message(
        "JV_REGEX_E103",
        format!(
            "正規表現リテラルではフラグ `{}` と `{}` を同時に使用できません。どちらか一方のみを残してください。",
            primary, secondary
        ),
        format!(
            "Regex literal flags `{}` and `{}` cannot be combined. Keep only one of them.",
            primary, secondary
        ),
    )
}

/// ラムダ置換の戻り値型が String でない場合のエラーメッセージ。
pub fn regex_lambda_return_mismatch_message(actual: &str) -> String {
    regex_message(
        "JV_REGEX_E102",
        format!(
            "正規表現リテラルのラムダ置換は `String` 型を返す必要があります（現在の型: `{}`）。`toString()` で文字列化するか `String.valueOf(...)` を利用してください。",
            actual
        ),
        format!(
            "Regex literal replacement lambdas must return `String` (current type: `{}`). Call `toString()` or `String.valueOf(...)` to convert the value.",
            actual
        ),
    )
}

/// 正規表現リテラルが終端スラッシュで閉じられていない場合のエラーメッセージ。
pub fn regex_unterminated_literal_message() -> String {
    regex_message(
        "JV_REGEX_E201",
        "正規表現リテラルが `/` で閉じられていません。末尾に `/` を追加してリテラルを確定してください。",
        "The regex literal is missing its closing `/`. Add the trailing slash to terminate the literal.",
    )
}

/// 括弧やブラケットの対応が崩れている場合のエラーメッセージ。
pub fn regex_group_balance_message(expected: Option<char>, found: Option<char>) -> String {
    let (ja, en) = match (expected, found) {
        (Some(expected), Some(found)) => (
            format!(
                "開き括弧と閉じ括弧が対応していません。`{found}` を `{expected}` へ置き換えて対応関係を修正してください。"
            ),
            format!(
                "Opening and closing brackets are mispaired. Replace `{found}` with the closing `{expected}`."
            ),
        ),
        (Some(expected), None) => (
            format!(
                "開き括弧に対応する閉じ括弧 `{expected}` が不足しています。対応する記号を追加してグループを閉じてください。"
            ),
            format!(
                "The regex is missing its closing `{expected}`. Insert the matching symbol to balance the group."
            ),
        ),
        (None, Some(found)) => (
            format!(
                "対応する開き括弧がない閉じ括弧 `{found}` が含まれています。不要であれば削除し、必要であれば対応する開き括弧を追加してください。"
            ),
            format!(
                "The regex contains an unmatched closing `{found}`. Remove it or add the corresponding opening bracket."
            ),
        ),
        (None, None) => (
            "正規表現リテラルの括弧構造が不正です。".to_string(),
            "The regex literal contains an invalid grouping structure.".to_string(),
        ),
    };
    regex_message("JV_REGEX_E202", ja, en)
}

/// バックスラッシュで終わる正規表現エスケープのエラーメッセージ。
pub fn regex_trailing_escape_message() -> String {
    regex_message(
        "JV_REGEX_E203",
        "バックスラッシュで終わるエスケープは無効です。次の文字をエスケープするか、バックスラッシュを削除してください。",
        "A trailing backslash escape is invalid. Escape the following character or remove the backslash.",
    )
}

/// 非対応のエスケープシーケンスを検出した場合のエラーメッセージ。
pub fn regex_invalid_escape_sequence_message(sequence: &str) -> String {
    regex_message(
        "JV_REGEX_E203",
        format!(
            "サポートされないエスケープシーケンス `{sequence}` が含まれています。Java互換のエスケープ（例: `\\n`, `\\t`, `\\\\`）へ置き換えてください。"
        ),
        format!(
            "Unsupported escape sequence `{sequence}` detected. Use a Java-compatible escape such as `\\n`, `\\t`, or `\\\\`."
        ),
    )
}

/// 無効な文字が含まれる場合のエラーメッセージ。
pub fn regex_invalid_character_message(character: char) -> String {
    let display = character.escape_default().collect::<String>();
    regex_message(
        "JV_REGEX_E204",
        format!(
            "正規表現リテラルに許可されていない文字 `{display}` が含まれています。制御文字は削除するか適切にエスケープしてください。"
        ),
        format!(
            "Regex literal contains a disallowed character `{display}`. Remove control characters or escape it appropriately."
        ),
    )
}

/// 定数化された正規表現の静的検証失敗メッセージ。
pub fn regex_const_validation_failure_message(trigger_code: &str) -> String {
    format!(
        "定数化された正規表現の静的検証が `{trigger_code}` を原因として失敗しました。`Pattern.compile` 相当のチェックを通過できるようパターンを修正してください。\n参考資料: {JAVA_REGEX_DOC_URL}\nConstant regex static validation failed because `{trigger_code}` was reported. Update the pattern so it passes the equivalent of `Pattern.compile`.\nReference: {JAVA_REGEX_DOC_URL}"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn regex_messages_include_reference_url() {
        let message = regex_unknown_flag_message('z');
        assert!(
            message.contains(JAVA_REGEX_DOC_URL),
            "メッセージに公式ドキュメントURLが含まれている必要があります: {message}"
        );
        assert!(
            message.contains("JV_REGEX_E101"),
            "メッセージに診断コードが含まれている必要があります: {message}"
        );
    }

    #[test]
    fn group_balance_message_handles_variants() {
        let missing = regex_group_balance_message(Some(')'), None);
        assert!(
            missing.contains("`)`"),
            "不足メッセージは対象記号を含む必要があります: {missing}"
        );
        assert!(missing.contains("JV_REGEX_E202"));

        let stray = regex_group_balance_message(None, Some(']'));
        assert!(
            stray.contains("`]`"),
            "余分な閉じ括弧メッセージは対象記号を含む必要があります: {stray}"
        );
        assert!(stray.contains("JV_REGEX_E202"));
    }

    #[test]
    fn invalid_escape_message_mentions_sequence() {
        let message = regex_invalid_escape_sequence_message("\\q");
        assert!(
            message.contains("\\q"),
            "エスケープ文字列が含まれている必要があります: {message}"
        );
        assert!(message.contains("JV_REGEX_E203"));
    }
}
