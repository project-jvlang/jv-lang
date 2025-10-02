#!/usr/bin/env bash
# jv-lang installer script
# Installs the latest version of jv CLI from GitHub releases

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
GITHUB_REPO="project-jvlang/jv-lang"
INSTALL_DIR="${HOME}/.jv/bin"
BIN_NAME="jv"

# Detect OS and architecture
detect_platform() {
    local os=""
    local arch=""

    case "$(uname -s)" in
        Linux*)     os="unknown-linux-gnu" ;;
        Darwin*)    os="apple-darwin" ;;
        MINGW*|MSYS*|CYGWIN*) os="pc-windows-msvc" ;;
        *)
            echo -e "${RED}Error: Unsupported operating system$(uname -s)${NC}" >&2
            exit 1
            ;;
    esac

    case "$(uname -m)" in
        x86_64|amd64)   arch="x86_64" ;;
        aarch64|arm64)  arch="aarch64" ;;
        *)
            echo -e "${RED}Error: Unsupported architecture $(uname -m)${NC}" >&2
            exit 1
            ;;
    esac

    echo "${arch}-${os}"
}

# Download and extract binary
install_jv() {
    local platform
    platform=$(detect_platform)

    echo -e "${GREEN}Installing jv for ${platform}...${NC}"

    # Create install directory
    mkdir -p "${INSTALL_DIR}"

    # Determine file extension
    local ext="tar.gz"
    if [[ "$platform" == *"windows"* ]]; then
        ext="zip"
    fi

    # Download URL
    local download_url="https://github.com/${GITHUB_REPO}/releases/latest/download/jv-${platform}.${ext}"
    local temp_file="/tmp/jv.${ext}"

    echo -e "${YELLOW}Downloading from ${download_url}...${NC}"

    if command -v curl &> /dev/null; then
        if ! curl -fsSL "${download_url}" -o "${temp_file}"; then
            echo "" >&2
            echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}" >&2
            echo -e "${RED}  Error: Failed to download jv for ${platform}${NC}" >&2
            echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}" >&2
            echo "" >&2
            echo -e "${YELLOW}The binary may not be available for your platform yet.${NC}" >&2
            echo "" >&2
            echo -e "${YELLOW}Currently supported platforms:${NC}" >&2
            echo -e "  • x86_64-unknown-linux-gnu    (Linux x86_64)" >&2
            echo -e "  • aarch64-unknown-linux-gnu   (Linux ARM64)" >&2
            echo -e "  • x86_64-apple-darwin         (macOS Intel)" >&2
            echo -e "  • aarch64-apple-darwin        (macOS Apple Silicon)" >&2
            echo -e "  • x86_64-pc-windows-msvc      (Windows x86_64)" >&2
            echo "" >&2
            echo -e "${GREEN}Alternative installation methods:${NC}" >&2
            echo -e "  ${YELLOW}# Install via Cargo (requires Rust toolchain)${NC}" >&2
            echo -e "  cargo install jv-cli" >&2
            echo "" >&2
            echo -e "  ${YELLOW}# Build from source${NC}" >&2
            echo -e "  git clone https://github.com/${GITHUB_REPO}.git" >&2
            echo -e "  cd jv-lang/jv" >&2
            echo -e "  cargo build --release" >&2
            echo "" >&2
            echo -e "For more information, visit:" >&2
            echo -e "  ${YELLOW}https://github.com/${GITHUB_REPO}#installation${NC}" >&2
            echo "" >&2
            exit 1
        fi
    elif command -v wget &> /dev/null; then
        if ! wget -q "${download_url}" -O "${temp_file}"; then
            echo "" >&2
            echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}" >&2
            echo -e "${RED}  Error: Failed to download jv for ${platform}${NC}" >&2
            echo -e "${RED}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}" >&2
            echo "" >&2
            echo -e "${YELLOW}The binary may not be available for your platform yet.${NC}" >&2
            echo "" >&2
            echo -e "${YELLOW}Currently supported platforms:${NC}" >&2
            echo -e "  • x86_64-unknown-linux-gnu    (Linux x86_64)" >&2
            echo -e "  • aarch64-unknown-linux-gnu   (Linux ARM64)" >&2
            echo -e "  • x86_64-apple-darwin         (macOS Intel)" >&2
            echo -e "  • aarch64-apple-darwin        (macOS Apple Silicon)" >&2
            echo -e "  • x86_64-pc-windows-msvc      (Windows x86_64)" >&2
            echo "" >&2
            echo -e "${GREEN}Alternative installation methods:${NC}" >&2
            echo -e "  ${YELLOW}# Install via Cargo (requires Rust toolchain)${NC}" >&2
            echo -e "  cargo install jv-cli" >&2
            echo "" >&2
            echo -e "  ${YELLOW}# Build from source${NC}" >&2
            echo -e "  git clone https://github.com/${GITHUB_REPO}.git" >&2
            echo -e "  cd jv-lang/jv" >&2
            echo -e "  cargo build --release" >&2
            echo "" >&2
            echo -e "For more information, visit:" >&2
            echo -e "  ${YELLOW}https://github.com/${GITHUB_REPO}#installation${NC}" >&2
            echo "" >&2
            exit 1
        fi
    else
        echo -e "${RED}Error: curl or wget is required${NC}" >&2
        exit 1
    fi

    # Extract binary
    echo -e "${YELLOW}Extracting...${NC}"

    if [[ "$ext" == "tar.gz" ]]; then
        tar xzf "${temp_file}" -C "${INSTALL_DIR}"
    elif [[ "$ext" == "zip" ]]; then
        if command -v unzip &> /dev/null; then
            unzip -q "${temp_file}" -d "${INSTALL_DIR}"
        else
            echo -e "${RED}Error: unzip is required for Windows installation${NC}" >&2
            exit 1
        fi
    fi

    # Make binary executable (Unix only)
    if [[ "$platform" != *"windows"* ]]; then
        chmod +x "${INSTALL_DIR}/${BIN_NAME}"
    fi

    # Clean up
    rm -f "${temp_file}"

    echo -e "${GREEN}✓ jv installed to ${INSTALL_DIR}/${BIN_NAME}${NC}"
}

# Configure shell PATH
configure_shell() {
    local shell_config=""
    local shell_name=""

    # Detect shell
    if [[ -n "${BASH_VERSION:-}" ]]; then
        shell_name="bash"
        shell_config="${HOME}/.bashrc"
    elif [[ -n "${ZSH_VERSION:-}" ]]; then
        shell_name="zsh"
        shell_config="${HOME}/.zshrc"
    elif [[ -n "${FISH_VERSION:-}" ]]; then
        shell_name="fish"
        shell_config="${HOME}/.config/fish/config.fish"
    else
        shell_name="sh"
        shell_config="${HOME}/.profile"
    fi

    # Check if PATH already configured
    if echo "${PATH}" | grep -q "${INSTALL_DIR}"; then
        echo -e "${GREEN}✓ PATH already configured${NC}"
        return
    fi

    # Add to PATH in shell config
    local path_line=""
    if [[ "$shell_name" == "fish" ]]; then
        path_line="set -gx PATH ${INSTALL_DIR} \$PATH"
    else
        path_line="export PATH=\"${INSTALL_DIR}:\$PATH\""
    fi

    if [[ -f "$shell_config" ]]; then
        if ! grep -q "${INSTALL_DIR}" "$shell_config" 2>/dev/null; then
            echo "" >> "$shell_config"
            echo "# jv-lang" >> "$shell_config"
            echo "$path_line" >> "$shell_config"
            echo -e "${GREEN}✓ Added ${INSTALL_DIR} to PATH in ${shell_config}${NC}"
        fi
    else
        echo -e "${YELLOW}! Could not find ${shell_config}${NC}"
        echo -e "${YELLOW}! Please manually add this to your shell configuration:${NC}"
        echo -e "${YELLOW}  ${path_line}${NC}"
    fi

    # Show instructions
    echo ""
    echo -e "${GREEN}Installation complete!${NC}"
    echo ""
    echo -e "To use jv in the current session, run:"
    if [[ "$shell_name" == "fish" ]]; then
        echo -e "  ${YELLOW}set -gx PATH ${INSTALL_DIR} \$PATH${NC}"
    else
        echo -e "  ${YELLOW}export PATH=\"${INSTALL_DIR}:\$PATH\"${NC}"
    fi
    echo ""
    echo -e "Or start a new terminal session."
    echo ""
    echo -e "Verify installation with:"
    echo -e "  ${YELLOW}jv --version${NC}"
}

# Verify installation
verify_installation() {
    if [[ -x "${INSTALL_DIR}/${BIN_NAME}" ]] || command -v jv &> /dev/null; then
        echo ""
        echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo -e "${GREEN}  jv successfully installed! 🎉${NC}"
        echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo ""
        echo -e "Get started with:"
        echo -e "  ${YELLOW}jv init my-project${NC}"
        echo ""
        echo -e "Learn more:"
        echo -e "  ${YELLOW}jv --help${NC}"
        echo -e "  ${YELLOW}https://github.com/${GITHUB_REPO}${NC}"
        echo ""
    else
        echo -e "${RED}Warning: Installation completed but binary not found in PATH${NC}" >&2
    fi
}

# Main installation flow
main() {
    echo ""
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${GREEN}  jv-lang Installer${NC}"
    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""

    install_jv
    configure_shell
    verify_installation
}

main "$@"
