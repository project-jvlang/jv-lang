use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-env-changed=JVPM_DEFAULT_REPOSITORIES");
    println!("cargo:rerun-if-env-changed=JVPM_DEFAULT_REPOSITORIES_FILE");

    let manifest_dir =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR is set"));
    let workspace_root = manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root resolution");

    let config_source = resolve_config_source(&workspace_root);
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR is set"));
    let target = out_dir.join("embedded_default_repositories.toml");

    fs::write(&target, config_source).expect("write embedded repositories");
}

fn resolve_config_source(workspace_root: &std::path::Path) -> String {
    if let Some(path) = env::var_os("JVPM_DEFAULT_REPOSITORIES_FILE") {
        let path = PathBuf::from(path);
        println!("cargo:rerun-if-changed={}", path.display());
        return fs::read_to_string(&path).unwrap_or_else(|error| {
            panic!(
                "failed to read JVPM_DEFAULT_REPOSITORIES_FILE ({}): {error}",
                path.display()
            )
        });
    }

    if let Some(raw) = env::var_os("JVPM_DEFAULT_REPOSITORIES") {
        return raw.into_string().unwrap_or_else(|_| "[]".to_string());
    }

    let fallback = workspace_root.join("build-config/global-repositories.toml");
    println!("cargo:rerun-if-changed={}", fallback.display());
    fs::read_to_string(&fallback).unwrap_or_else(|error| {
        panic!(
            "failed to read build-config/global-repositories.toml ({}): {error}",
            fallback.display()
        )
    })
}
