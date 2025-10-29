use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;

use walkdir::WalkDir;
use zip::write::FileOptions;
use zip::CompressionMethod;

fn main() -> anyhow::Result<()> {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?);
    let stdlib_dir = manifest_dir.join("../../stdlib");
    let out_dir = PathBuf::from(env::var("OUT_DIR")?);

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", stdlib_dir.display());

    if !stdlib_dir.exists() {
        anyhow::bail!(
            "Unable to locate stdlib directory at {}",
            stdlib_dir.display()
        );
    }

    let zip_path = out_dir.join("embedded_stdlib.zip");
    let mut zip_file = File::create(&zip_path)?;
    let mut zip = zip::ZipWriter::new(&mut zip_file);

    for entry in WalkDir::new(&stdlib_dir) {
        let entry = entry?;
        let path = entry.path();

        println!("cargo:rerun-if-changed={}", path.display());

        if path.is_dir() {
            continue;
        }

        let relative = path.strip_prefix(&stdlib_dir)?;
        let normalized = relative
            .to_string_lossy()
            .replace(std::path::MAIN_SEPARATOR, "/");
        let metadata = fs::metadata(path)?;

        #[cfg(unix)]
        let perms = {
            use std::os::unix::fs::PermissionsExt;
            metadata.permissions().mode()
        };
        #[cfg(not(unix))]
        let perms = 0o644;

        let options = FileOptions::default()
            .compression_method(CompressionMethod::Deflated)
            .unix_permissions(perms);
        zip.start_file(&normalized, options)?;
        let data = fs::read(path)?;
        zip.write_all(&data)?;
    }

    zip.finish()?;

    let bundle_rs_path = out_dir.join("embedded_stdlib_data.rs");
    let bundle_rs = r#"pub static STDLIB_ZIP: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/embedded_stdlib.zip"));"#;
    fs::write(bundle_rs_path, bundle_rs)?;

    Ok(())
}
