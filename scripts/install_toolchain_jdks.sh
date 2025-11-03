#!/usr/bin/env bash
set -euo pipefail

# Detect OS and Architecture
detect_platform() {
  local os arch

  case "$(uname -s)" in
    Linux*)  os="linux" ;;
    Darwin*) os="macos" ;;
    *)       echo "Unsupported OS: $(uname -s)" >&2; exit 1 ;;
  esac

  case "$(uname -m)" in
    x86_64)  arch="x64" ;;
    aarch64|arm64) arch="aarch64" ;;
    *)       echo "Unsupported arch: $(uname -m)" >&2; exit 1 ;;
  esac

  echo "${os}-${arch}"
}

PLATFORM=$(detect_platform)
echo "Detected platform: $PLATFORM" >&2

# Allow overriding repo root via first argument (useful when invoked from Nix shellHook)
if [[ $# -gt 0 ]]; then
  REPO_ROOT=$(cd -- "$1" && pwd)
else
  REPO_ROOT=$(cd -- "$(dirname "$0")/.." && pwd)
fi
TOOLCHAIN_DIR="$REPO_ROOT/toolchains"
mkdir -p "$TOOLCHAIN_DIR"

download_and_install() {
  local name="$1"
  local url="$2"
  local target_dir="$TOOLCHAIN_DIR/$3"
  local binary_path="$4"

  if [[ -x "$target_dir/$binary_path" ]]; then
    echo "[$name] Already installed at $target_dir" >&2
    return
  fi

  echo "[$name] Downloading from $url" >&2
  local tmp_tar tmp_dir extracted
  tmp_tar=$(mktemp)
  tmp_dir=$(mktemp -d)
  trap 'rm -f "${tmp_tar:-}"; rm -rf "${tmp_dir:-}"' RETURN

  curl -fL "$url" -o "$tmp_tar"
  tar -xzf "$tmp_tar" -C "$tmp_dir"
  extracted=$(find "$tmp_dir" -maxdepth 1 -mindepth 1 -type d | head -n1)
  if [[ -z "$extracted" ]]; then
    echo "Failed to extract $name" >&2
    exit 1
  fi

  rm -rf "$target_dir"
  mv "$extracted" "$target_dir"
  echo "[$name] Installed to $target_dir" >&2
}

download_and_install "jdk-graalvm-21" "https://download.oracle.com/graalvm/21/latest/graalvm-jdk-21_${PLATFORM}_bin.tar.gz" "graalvm21" "bin/java"
download_and_install "jdk-graalvm-25" "https://download.oracle.com/graalvm/25/latest/graalvm-jdk-25_${PLATFORM}_bin.tar.gz" "graalvm25" "bin/java"

# Maven installation (latest 3.9.x release)
install_maven() {
  local base_url="https://dlcdn.apache.org/maven/maven-3"
  local desired_series="3.9"
  local fallback_version="3.9.6"
  local version="${MAVEN_VERSION:-}"

  if [[ -z "$version" ]]; then
    version=$(curl -fsSL "$base_url/" | grep -oE "${desired_series//./\\.}\\.[0-9]+" | sort -V | tail -n1 || true)
  fi
  if [[ -z "$version" ]]; then
    echo "[maven] Failed to detect latest ${desired_series}.x release, falling back to ${fallback_version}" >&2
    version="$fallback_version"
  fi

  local tarball="apache-maven-${version}-bin.tar.gz"
  local url="${base_url}/${version}/binaries/${tarball}"
  download_and_install "maven-${version}" "$url" "maven" "bin/mvn"
}

install_maven
