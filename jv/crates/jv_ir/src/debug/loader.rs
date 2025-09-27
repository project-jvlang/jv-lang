use std::fmt;
use std::fs::File;
use std::io::{self, Read, StdinLock};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};

use crate::types::IrProgram;

/// IRアーティファクトの入力元を表す抽象化。
pub enum IrArtifactSource<'a> {
    /// 既に復元済みの `IrProgram`。
    Program(&'a IrProgram),
    /// UTF-8 文字列にエンコードされた JSON。
    Json(&'a str),
    /// JSON バイト列。
    Bytes(&'a [u8]),
    /// 任意の `Read` ストリーム。
    Reader(Box<dyn Read + Send + 'a>),
    /// ファイルパス。
    Path(PathBuf),
    /// 呼び出し時に標準入力から読み込む。
    Stdin,
}

impl<'a> IrArtifactSource<'a> {
    pub fn from_program(program: &'a IrProgram) -> Self {
        Self::Program(program)
    }

    pub fn from_json(json: &'a str) -> Self {
        Self::Json(json)
    }

    pub fn from_bytes(bytes: &'a [u8]) -> Self {
        Self::Bytes(bytes)
    }

    pub fn from_reader<R>(reader: R) -> Self
    where
        R: Read + Send + 'a,
    {
        Self::Reader(Box::new(reader))
    }

    pub fn from_path(path: impl AsRef<Path>) -> Self {
        Self::Path(path.as_ref().to_path_buf())
    }

    pub fn stdin() -> Self {
        Self::Stdin
    }
}

impl fmt::Debug for IrArtifactSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Program(_) => f.write_str("IrArtifactSource::Program"),
            Self::Json(_) => f.write_str("IrArtifactSource::Json"),
            Self::Bytes(bytes) => f
                .debug_tuple("IrArtifactSource::Bytes")
                .field(&bytes.len())
                .finish(),
            Self::Reader(_) => f.write_str("IrArtifactSource::Reader"),
            Self::Path(path) => f.debug_tuple("IrArtifactSource::Path").field(path).finish(),
            Self::Stdin => f.write_str("IrArtifactSource::Stdin"),
        }
    }
}

/// 指定されたソースから `IrProgram` を読み込む。
///
/// 各入出力エラーには `anyhow::Context` を介して情報を付与する。
pub fn load_ir_program(source: IrArtifactSource<'_>) -> Result<IrProgram> {
    match source {
        IrArtifactSource::Program(program) => Ok(program.clone()),
        IrArtifactSource::Json(json) => {
            serde_json::from_str(json).context("IR JSON文字列の解析に失敗しました")
        }
        IrArtifactSource::Bytes(bytes) => {
            serde_json::from_slice(bytes).context("IR JSONバイト列の解析に失敗しました")
        }
        IrArtifactSource::Reader(mut reader) => {
            deserialize_reader(&mut *reader).context("IRストリームの解析に失敗しました")
        }
        IrArtifactSource::Path(path) => load_from_path(&path),
        IrArtifactSource::Stdin => load_from_stdin(io::stdin().lock()),
    }
}

fn load_from_path(path: &Path) -> Result<IrProgram> {
    let file = File::open(path).with_context(|| {
        format!(
            "IRアーティファクトの読み込みに失敗しました: {}",
            path.display()
        )
    })?;
    let mut reader = io::BufReader::new(file);
    deserialize_reader(&mut reader).with_context(|| {
        format!(
            "IRアーティファクトをJSONとして解析できませんでした: {}",
            path.display()
        )
    })
}

fn load_from_stdin(stdin: StdinLock<'_>) -> Result<IrProgram> {
    deserialize_reader(stdin).context("標準入力からIRを解析できませんでした")
}

fn deserialize_reader<R>(reader: R) -> Result<IrProgram>
where
    R: Read,
{
    serde_json::from_reader(reader).context("IR JSONのデシリアライズに失敗しました")
}
