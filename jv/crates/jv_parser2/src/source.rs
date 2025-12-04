//! 入力ソースをゼロコピーで走査するカーソル。

use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Source<'src> {
    bytes: &'src [u8],
    len: usize,
    cursor: usize,
}

impl<'src> Source<'src> {
    /// UTF-8検証付きでソースを生成する。
    pub fn new(text: &'src [u8]) -> Result<Self, std::str::Utf8Error> {
        std::str::from_utf8(text)?; // 検証のみ
        Ok(Self {
            bytes: text,
            len: text.len(),
            cursor: 0,
        })
    }

    /// 既存の文字列から生成するヘルパ。
    pub fn from_str(text: &'src str) -> Self {
        Self {
            bytes: text.as_bytes(),
            len: text.len(),
            cursor: 0,
        }
    }

    /// 現在位置のバイトを返す。
    pub fn peek(&self) -> Option<u8> {
        self.bytes.get(self.cursor).copied()
    }

    /// 次の2バイトを返す。
    pub fn peek2(&self) -> Option<(u8, u8)> {
        if self.cursor + 1 < self.len {
            Some((self.bytes[self.cursor], self.bytes[self.cursor + 1]))
        } else {
            None
        }
    }

    /// 現在位置を1バイト進め、進めたバイトを返す。
    pub fn advance(&mut self) -> Option<u8> {
        let byte = self.peek()?;
        self.cursor += 1;
        Some(byte)
    }

    /// 任意バイト数進める。
    pub fn advance_by(&mut self, count: usize) {
        self.cursor = (self.cursor + count).min(self.len);
    }

    /// 現在位置からバイトスライスを取得する。
    pub fn slice_from(&self, start: usize) -> &'src [u8] {
        &self.bytes[start.min(self.len)..]
    }

    /// Span から UTF-8 文字列スライスを取得する。
    pub fn slice_span(&self, span: Span) -> Option<&'src str> {
        let start = span.start.min(self.len as u32) as usize;
        let end = span.end.min(self.len as u32) as usize;
        std::str::from_utf8(&self.bytes[start..end]).ok()
    }

    /// 現在オフセット（バイト単位）。
    pub fn offset(&self) -> usize {
        self.cursor
    }

    /// オフセットを設定する（入力長にクランプ）。
    pub fn set_offset(&mut self, offset: usize) {
        self.cursor = offset.min(self.len);
    }

    /// ファイル末尾に達したか。
    pub fn is_eof(&self) -> bool {
        self.cursor >= self.len
    }

    /// 全体長。
    pub fn len(&self) -> usize {
        self.len
    }

    /// 任意の開始・終了からSpanを生成する（入力長にクランプ）。
    pub fn span_from(&self, start: usize, end: usize) -> Span {
        let start_u32 = start.min(self.len) as u32;
        let end_u32 = end.min(self.len) as u32;
        Span::new(start_u32, end_u32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_peek_and_advance() {
        let mut src = Source::from_str("ab");
        assert_eq!(src.peek(), Some(b'a'));
        assert_eq!(src.peek2(), Some((b'a', b'b')));
        assert_eq!(src.advance(), Some(b'a'));
        assert_eq!(src.offset(), 1);
        assert_eq!(src.advance(), Some(b'b'));
        assert!(src.is_eof());
    }

    #[test]
    fn span_is_clamped() {
        let src = Source::from_str("xyz");
        let span = src.span_from(0, 10);
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 3);
    }
}
