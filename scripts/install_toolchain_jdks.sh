#!/usr/bin/env bash
set -euo pipefail

# Allow overriding repo root via first argument (useful when invoked from Nix shellHook)
if [[ $# -gt 0 ]]; then
  REPO_ROOT=$(cd -- "$1" && pwd)
else
  REPO_ROOT=$(cd -- "$(dirname "$0")/.." && pwd)
fi
TOOLCHAIN_DIR="$REPO_ROOT/toolchains"
mkdir -p "$TOOLCHAIN_DIR"

download_and_install() {
  local version="$1"
  local url="$2"
  local target_dir="$TOOLCHAIN_DIR/$3"

  if [[ -x "$target_dir/bin/java" ]]; then
    echo "[jdk$version] Already installed at $target_dir" >&2
    return
  fi

  echo "[jdk$version] Downloading from $url" >&2
  local tmp_tar tmp_dir extracted
  tmp_tar=$(mktemp)
  tmp_dir=$(mktemp -d)
  trap 'rm -f "$tmp_tar"; rm -rf "$tmp_dir"' RETURN

  curl -L "$url" -o "$tmp_tar"
  tar -xzf "$tmp_tar" -C "$tmp_dir"
  extracted=$(find "$tmp_dir" -maxdepth 1 -mindepth 1 -type d | head -n1)
  if [[ -z "$extracted" ]]; then
    echo "Failed to extract JDK $version" >&2
    exit 1
  fi

  rm -rf "$target_dir"
  mv "$extracted" "$target_dir"
  echo "[jdk$version] Installed to $target_dir" >&2
}

download_and_install "21" "https://download.oracle.com/java/21/latest/jdk-21_linux-x64_bin.tar.gz" "jdk21"
download_and_install "25" "https://download.oracle.com/java/25/latest/jdk-25_linux-x64_bin.tar.gz" "jdk25"
