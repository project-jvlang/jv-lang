use jv_ast::{PatternConstKey, PatternOrigin, PatternOriginKind, RegexFlag, RegexLiteral};
use sha2::{Digest, Sha256};

/// 正規表現パターンが定数として扱えるかどうかの分類。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternConstKind {
    /// リテラル基盤で静的にコンパイル可能なパターン。
    Static,
    /// 実行時に `Pattern.compile` が必要なパターン。
    Dynamic,
}

/// 正規表現定数解析を担当するアナライザ。
#[derive(Debug, Default)]
pub struct PatternConstAnalyzer;

impl PatternConstAnalyzer {
    /// 正規表現リテラルとその生成元に基づいて定数性を分類する。
    pub fn classify(literal: &RegexLiteral, origin: &PatternOrigin) -> PatternConstKind {
        match origin.kind {
            PatternOriginKind::RegexLiteral | PatternOriginKind::RegexCommand => {
                if literal.pattern.is_empty() {
                    PatternConstKind::Dynamic
                } else {
                    PatternConstKind::Static
                }
            }
        }
    }

    /// パターン文字列とフラグの組み合わせから定数キーを生成する。
    pub fn build_key(literal: &RegexLiteral, flags: &[RegexFlag]) -> PatternConstKey {
        let mut hasher = Sha256::new();
        hasher.update(literal.pattern.as_bytes());

        let mut flag_chars: Vec<char> = flags.iter().map(Self::flag_to_char).collect();
        flag_chars.sort_unstable();
        for ch in &flag_chars {
            hasher.update(&[*ch as u8]);
        }

        let digest = hasher.finalize();
        let mut truncated = [0u8; 16];
        truncated.copy_from_slice(&digest[..16]);

        let preview: String = literal.pattern.chars().take(32).collect();
        PatternConstKey::new(truncated, preview)
    }

    fn flag_to_char(flag: &RegexFlag) -> char {
        match flag {
            RegexFlag::CaseInsensitive => 'i',
            RegexFlag::Multiline => 'm',
            RegexFlag::DotAll => 's',
            RegexFlag::UnicodeCase => 'u',
            RegexFlag::UnixLines => 'd',
            RegexFlag::Comments => 'x',
            RegexFlag::Literal => 'l',
            RegexFlag::CanonEq => 'c',
        }
    }
}
