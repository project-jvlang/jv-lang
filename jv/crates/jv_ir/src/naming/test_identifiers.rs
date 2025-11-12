use crate::transform::utils::identifier_components;
use ahash::RandomState;
use jv_ast::Span;
use std::hash::{BuildHasher, Hash, Hasher};
use unicode_normalization::UnicodeNormalization;

const HASH_KEY1: u64 = 0x6a09e667f3bcc908;
const HASH_KEY2: u64 = 0xbb67ae8584caa73b;
const HASH_KEY3: u64 = !HASH_KEY1;
const HASH_KEY4: u64 = !HASH_KEY2;

/// テスト識別子の正規化結果を保持する構造体。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedName {
    base_identifier: String,
    hash_suffix: String,
    identifier: String,
}

impl NormalizedName {
    /// 衝突検出前の基底識別子を取得する。
    pub fn base(&self) -> &str {
        &self.base_identifier
    }

    /// ハッシュに基づくサフィックスを取得する。先頭の`_`を含む。
    pub fn hash_suffix(&self) -> &str {
        &self.hash_suffix
    }

    /// ハッシュサフィックスを付与した最終的な識別子を取得する。
    pub fn identifier(&self) -> &str {
        &self.identifier
    }

    /// 所有権付きで最終的な識別子を返す。
    pub fn into_identifier(self) -> String {
        self.identifier
    }
}

/// テストメソッド名を正規化する。
pub fn normalize_method(display_name: &str, source_span: &Span) -> NormalizedName {
    let core = sanitize_display_name(display_name);
    let base = format!("test_{}", core);
    build_normalized_name(display_name, source_span, base)
}

/// `@MethodSource` 向けのデータセット識別子を正規化する。
pub fn normalize_dataset(display_name: &str, source_span: &Span) -> NormalizedName {
    let core = sanitize_display_name(display_name);
    let base = format!("test_{}_source", core);
    build_normalized_name(display_name, source_span, base)
}

fn sanitize_display_name(display_name: &str) -> String {
    let decomposed = display_name.nfkd().collect::<String>();
    let ascii_only = decomposed
        .chars()
        .filter(|ch| ch.is_ascii())
        .collect::<String>();
    let ascii_lower = ascii_only.to_ascii_lowercase();

    let components = identifier_components(&ascii_lower);
    let mut core = join_components(&components);

    if core.is_empty() {
        core.push_str("case");
    }

    if core.chars().next().is_some_and(|ch| ch.is_ascii_digit()) {
        core.insert(0, '_');
    }

    core
}

fn join_components(components: &[String]) -> String {
    if components.is_empty() {
        return String::new();
    }
    components.join("_")
}

fn build_normalized_name(display_name: &str, source_span: &Span, base: String) -> NormalizedName {
    let hash_suffix = compute_hash_suffix(display_name, source_span);
    let identifier = format!("{}{}", base, hash_suffix);

    NormalizedName {
        base_identifier: base,
        hash_suffix,
        identifier,
    }
}

fn compute_hash_suffix(display_name: &str, span: &Span) -> String {
    let state = RandomState::with_seeds(HASH_KEY1, HASH_KEY2, HASH_KEY3, HASH_KEY4);
    let mut hasher = state.build_hasher();
    display_name.hash(&mut hasher);
    SpanKey::from(span).hash(&mut hasher);
    let hash = hasher.finish() as u32;
    format!("_{hash:08x}")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct SpanKey {
    start_line: usize,
    start_column: usize,
    end_line: usize,
    end_column: usize,
}

impl From<&Span> for SpanKey {
    fn from(span: &Span) -> Self {
        Self {
            start_line: span.start_line,
            start_column: span.start_column,
            end_line: span.end_line,
            end_column: span.end_column,
        }
    }
}
