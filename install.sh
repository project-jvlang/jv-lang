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

# Install GraalVM JDK 25
install_graalvm() {
    echo ""
    echo -e "${GREEN}Installing GraalVM JDK 25...${NC}"

    local platform
    local arch
    local os_name

    # Detect platform
    case "$(uname -s)" in
        Linux*)     os_name="linux" ;;
        Darwin*)    os_name="macos" ;;
        *)
            echo -e "${RED}Error: Unsupported OS for automatic JDK installation${NC}" >&2
            return 1
            ;;
    esac

    case "$(uname -m)" in
        x86_64|amd64)   arch="x64" ;;
        aarch64|arm64)  arch="aarch64" ;;
        *)
            echo -e "${RED}Error: Unsupported architecture for automatic JDK installation${NC}" >&2
            return 1
            ;;
    esac

    # GraalVM download URL
    local graalvm_version="25.0.0"
    local graalvm_build="11"
    local graalvm_url="https://download.oracle.com/graalvm/${graalvm_version%%.*}/archive/graalvm-jdk-${graalvm_version}_${os_name}-${arch}_bin.tar.gz"

    # Installation directory
    local jdk_install_dir="${HOME}/.jv/jdk"
    local graalvm_dir="${jdk_install_dir}/graalvm-jdk-${graalvm_version}"

    # Check if already installed
    if [[ -d "$graalvm_dir" ]]; then
        echo -e "${GREEN}✓ GraalVM JDK 25 already installed at ${graalvm_dir}${NC}"

        # Set JAVA_HOME
        export JAVA_HOME="$graalvm_dir"
        export PATH="$JAVA_HOME/bin:$PATH"

        # Update shell config
        update_java_home_in_shell "$graalvm_dir"
        return 0
    fi

    # Create installation directory
    mkdir -p "$jdk_install_dir"

    # Download GraalVM
    local temp_file="/tmp/graalvm-jdk-${graalvm_version}.tar.gz"
    echo -e "${YELLOW}Downloading GraalVM JDK 25 from ${graalvm_url}...${NC}"

    if command -v curl &> /dev/null; then
        if ! curl -fSL "${graalvm_url}" -o "${temp_file}"; then
            echo -e "${RED}Error: Failed to download GraalVM JDK${NC}" >&2
            rm -f "${temp_file}"
            return 1
        fi
    elif command -v wget &> /dev/null; then
        if ! wget -q "${graalvm_url}" -O "${temp_file}"; then
            echo -e "${RED}Error: Failed to download GraalVM JDK${NC}" >&2
            rm -f "${temp_file}"
            return 1
        fi
    else
        echo -e "${RED}Error: curl or wget is required${NC}" >&2
        return 1
    fi

    # Extract
    echo -e "${YELLOW}Extracting GraalVM JDK...${NC}"
    tar xzf "${temp_file}" -C "${jdk_install_dir}"
    rm -f "${temp_file}"

    # Rename directory to standard name
    local extracted_dir=$(find "${jdk_install_dir}" -maxdepth 1 -type d -name "graalvm-jdk-${graalvm_version}*" | head -n1)
    if [[ -n "$extracted_dir" ]] && [[ "$extracted_dir" != "$graalvm_dir" ]]; then
        mv "$extracted_dir" "$graalvm_dir"
    fi

    # For macOS, handle Contents/Home structure
    if [[ "$os_name" == "macos" ]] && [[ -d "${graalvm_dir}/Contents/Home" ]]; then
        local temp_home="${jdk_install_dir}/graalvm-temp"
        mv "${graalvm_dir}/Contents/Home" "$temp_home"
        rm -rf "$graalvm_dir"
        mv "$temp_home" "$graalvm_dir"
    fi

    echo -e "${GREEN}✓ GraalVM JDK 25 installed to ${graalvm_dir}${NC}"

    # Set JAVA_HOME
    export JAVA_HOME="$graalvm_dir"
    export PATH="$JAVA_HOME/bin:$PATH"

    # Update shell config
    update_java_home_in_shell "$graalvm_dir"

    return 0
}

# Update JAVA_HOME in shell configuration
update_java_home_in_shell() {
    local java_home_path="$1"
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

    # Add JAVA_HOME to shell config
    if [[ -f "$shell_config" ]]; then
        if ! grep -q "JAVA_HOME.*jv/jdk" "$shell_config" 2>/dev/null; then
            echo "" >> "$shell_config"
            echo "# GraalVM JDK (jv-lang)" >> "$shell_config"
            if [[ "$shell_name" == "fish" ]]; then
                echo "set -gx JAVA_HOME \"${java_home_path}\"" >> "$shell_config"
                echo "set -gx PATH \$JAVA_HOME/bin \$PATH" >> "$shell_config"
            else
                echo "export JAVA_HOME=\"${java_home_path}\"" >> "$shell_config"
                echo "export PATH=\"\$JAVA_HOME/bin:\$PATH\"" >> "$shell_config"
            fi
            echo -e "${GREEN}✓ Added JAVA_HOME to ${shell_config}${NC}"
        fi
    fi
}

# Check Java environment and install if needed
check_java_environment() {
    local has_javac=false
    local has_maven=false
    local javac_version=""
    local javac_version_num=0
    local maven_version=""

    # Check for javac
    if command -v javac &> /dev/null; then
        javac_version=$(javac -version 2>&1 | head -n1)
        # Extract version number (e.g., "21" from "javac 21.0.1")
        javac_version_num=$(echo "$javac_version" | grep -oE '[0-9]+' | head -n1)
        if [[ -n "$javac_version_num" ]] && [[ "$javac_version_num" -ge 21 ]]; then
            has_javac=true
        fi
    fi

    # Check for Maven
    if command -v mvn &> /dev/null; then
        maven_version=$(mvn -version 2>&1 | head -n1)
        has_maven=true
    fi

    echo ""
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${YELLOW}  Java Environment Check${NC}"
    echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""

    if [[ "$has_javac" == true ]]; then
        echo -e "${GREEN}✓ Java Compiler: ${javac_version}${NC}"
    elif [[ -n "$javac_version_num" ]] && [[ "$javac_version_num" -lt 21 ]]; then
        echo -e "${RED}✗ Java Compiler: ${javac_version} (version < 21)${NC}"
    else
        echo -e "${RED}✗ Java Compiler: Not found${NC}"
    fi

    if [[ "$has_maven" == true ]]; then
        echo -e "${GREEN}✓ Maven: ${maven_version}${NC}"
    else
        echo -e "${RED}✗ Maven: Not found${NC}"
    fi

    echo ""

    # Handle JDK installation
    if [[ "$has_javac" == false ]]; then
        if [[ -n "$javac_version_num" ]] && [[ "$javac_version_num" -lt 21 ]]; then
            # JDK exists but version is too old
            echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
            echo -e "${YELLOW}  JDK Update Required${NC}"
            echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
            echo ""
            echo -e "Your current JDK version is ${javac_version_num}, but jv requires JDK 21 or higher."
            echo ""
            echo -e "${GREEN}Recommended: Install GraalVM JDK 25${NC}"
            echo ""
            read -p "Do you want to install GraalVM JDK 25 now? [Y/n] " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
                if install_graalvm; then
                    has_javac=true
                    echo ""
                    echo -e "${GREEN}✓ GraalVM JDK 25 installed successfully${NC}"
                    echo ""
                    echo -e "${YELLOW}Note: Please restart your terminal or run:${NC}"
                    echo -e "  ${YELLOW}source ~/.bashrc${NC}  # or your shell's config file"
                    echo ""
                else
                    echo ""
                    echo -e "${YELLOW}Automatic installation failed. Please install manually:${NC}"
                    echo -e "  • GraalVM JDK 25: ${YELLOW}https://www.graalvm.org/downloads/${NC}"
                    echo -e "  • Or JDK 21+: ${YELLOW}https://adoptium.net/${NC}"
                    echo ""
                fi
            else
                echo ""
                echo -e "${YELLOW}Manual installation:${NC}"
                echo -e "  • GraalVM JDK 25: ${YELLOW}https://www.graalvm.org/downloads/${NC}"
                echo -e "  • Or JDK 21+: ${YELLOW}https://adoptium.net/${NC}"
                echo ""
            fi
        else
            # JDK not found
            echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
            echo -e "${YELLOW}  JDK Installation Required${NC}"
            echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
            echo ""
            echo -e "jv requires JDK 21 or higher to compile jv code to Java."
            echo ""
            echo -e "${GREEN}Recommended: Install GraalVM JDK 25${NC}"
            echo ""
            read -p "Do you want to install GraalVM JDK 25 now? [Y/n] " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
                if install_graalvm; then
                    has_javac=true
                    echo ""
                    echo -e "${GREEN}✓ GraalVM JDK 25 installed successfully${NC}"
                    echo ""
                    echo -e "${YELLOW}Note: Please restart your terminal or run:${NC}"
                    echo -e "  ${YELLOW}source ~/.bashrc${NC}  # or your shell's config file"
                    echo ""
                else
                    echo ""
                    echo -e "${YELLOW}Automatic installation failed. Please install manually:${NC}"
                    echo -e "  • GraalVM JDK 25: ${YELLOW}https://www.graalvm.org/downloads/${NC}"
                    echo -e "  • Or JDK 21+: ${YELLOW}https://adoptium.net/${NC}"
                    echo ""
                fi
            else
                echo ""
                echo -e "${YELLOW}Manual installation:${NC}"
                echo -e "  • GraalVM JDK 25: ${YELLOW}https://www.graalvm.org/downloads/${NC}"
                echo -e "  • Or JDK 21+: ${YELLOW}https://adoptium.net/${NC}"
                echo ""
            fi
        fi
    fi

    # If Java tools are missing, guide to jv init or manual installation
    if [[ "$has_javac" == false ]] || [[ "$has_maven" == false ]]; then
        echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo -e "${YELLOW}  Setup Required${NC}"
        echo -e "${YELLOW}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo ""

        if [[ "$has_javac" == false ]]; then
            echo -e "jv requires Java 21+ to compile jv code to Java."
            echo ""
        fi

        if [[ "$has_maven" == false ]]; then
            echo -e "jv requires Maven for dependency management."
            echo ""
            echo -e "${GREEN}Install Maven:${NC}"
            echo -e "  • ${YELLOW}https://maven.apache.org/download.cgi${NC}"
            echo ""
        fi

        echo -e "${GREEN}Automatic setup (recommended):${NC}"
        echo -e "  ${YELLOW}jv init${NC}"
        echo ""
        echo -e "This will configure your development environment."
        echo ""
        return 1
    fi

    return 0
}

# Verify installation
verify_installation() {
    if [[ -x "${INSTALL_DIR}/${BIN_NAME}" ]] || command -v jv &> /dev/null; then
        echo ""
        echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo -e "${GREEN}  jv successfully installed! 🎉${NC}"
        echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
        echo ""

        # Check Java environment
        if check_java_environment; then
            echo -e "${GREEN}✓ Java environment ready${NC}"
            echo ""
            echo -e "Get started with:"
            echo -e "  ${YELLOW}jv init my-project${NC}"
            echo ""
        fi

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
