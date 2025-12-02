//! ソース位置管理（Span / SourceLocation）。

/// バイトオフセットで表す閉区間半開区間のスパン。
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// 新しいスパンを作成する。
    pub const fn new(start: u32, end: u32) -> Self {
        debug_assert!(start <= end, "start <= end");
        Self { start, end }
    }

    /// スパン長（バイト数）。
    pub const fn len(self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    /// 空スパンかどうか。
    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    /// 指定オフセットを包含するか。
    pub const fn contains(self, offset: u32) -> bool {
        offset >= self.start && offset < self.end
    }

    /// 2つのスパンをマージする。
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// 全長を超えないようにクランプする。
    pub fn clamp(self, full_len: u32) -> Span {
        Span {
            start: self.start.min(full_len),
            end: self.end.min(full_len),
        }
    }
}

/// スパンが8バイトであることをコンパイル時に検証。
const _: [(); 8] = [(); core::mem::size_of::<Span>()];

/// 行・桁で表すソース位置（1-origin）。
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    pub line: u32,
    pub column: u32,
}

impl SourceLocation {
    pub const fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

/// 入力文字列から行頭オフセット一覧を生成する。
pub fn compute_line_starts(text: &str) -> Vec<u32> {
    let mut starts = Vec::with_capacity(32);
    starts.push(0);
    for (idx, byte) in text.as_bytes().iter().enumerate() {
        if *byte == b'\n' {
            starts.push((idx + 1) as u32);
        }
    }
    starts
}

/// バイトオフセットから行・桁を計算する。
pub fn offset_to_location(line_starts: &[u32], offset: u32) -> SourceLocation {
    if line_starts.is_empty() {
        return SourceLocation::new(1, offset + 1);
    }

    let idx = line_starts.partition_point(|start| *start <= offset);
    let line_idx = idx.saturating_sub(1);
    let line_start = line_starts[line_idx];
    let column = offset.saturating_sub(line_start) + 1;
    SourceLocation::new((line_idx as u32) + 1, column)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_merge_and_contains() {
        let a = Span::new(0, 5);
        let b = Span::new(3, 8);
        let merged = a.merge(b);
        assert_eq!(merged.start, 0);
        assert_eq!(merged.end, 8);
        assert!(merged.contains(4));
        assert!(!merged.contains(9));
    }

    #[test]
    fn line_offsets_compute_location() {
        let text = "line1\nline2\nlast";
        let starts = compute_line_starts(text);
        let loc = offset_to_location(&starts, 7);
        assert_eq!(loc.line, 2);
        assert_eq!(loc.column, 2);
    }
}
