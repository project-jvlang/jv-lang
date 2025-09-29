# jv Language Project Makefile
# Rust workspace build automation for jv transpiler and toolchain

.PHONY: help build check test clean fmt lint install dev release docs setup
.DEFAULT_GOAL := help

# Build configuration
MANIFEST_PATH := jv/Cargo.toml
CARGO_FLAGS := --manifest-path $(MANIFEST_PATH)
TEST_FLAGS := $(CARGO_FLAGS) --lib
RELEASE_FLAGS := $(CARGO_FLAGS) --release
# Reduce debug info size to avoid linker overflows during tests
export RUSTFLAGS := -C debuginfo=0 -C strip=debuginfo

help: ## Show this help message
	@echo "jv Language Project Build Commands"
	@echo "=================================="
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

# Development commands
build: ## Build all crates in debug mode
	cargo build $(CARGO_FLAGS)

check: ## Check all crates without building
	cargo check $(CARGO_FLAGS)

test: ## Run library tests for all crates
	cargo test $(TEST_FLAGS)

test-parser: ## Run tests for jv_parser specifically
	cargo test --lib -p jv_parser $(CARGO_FLAGS)

test-all: ## Run all tests including integration tests
	cargo test $(CARGO_FLAGS)

clean: ## Clean build artifacts
	cargo clean $(CARGO_FLAGS)

fmt: ## Format source code
	cargo fmt $(CARGO_FLAGS)

lint: ## Run clippy linter
	cargo clippy $(CARGO_FLAGS) -- -D warnings

# Release commands
release: ## Build optimized release binaries
	cargo build $(RELEASE_FLAGS)

release-binaries: ## Build all release binaries (jv, jvx, jv-lsp)
	cargo build --bin jv $(RELEASE_FLAGS)
	cargo build --bin jvx $(RELEASE_FLAGS)
	cargo build --bin jv-lsp $(RELEASE_FLAGS)

release-jv: ## Build jv release binary only
	cargo build --bin jv $(RELEASE_FLAGS)

release-jvx: ## Build jvx release binary only
	cargo build --bin jvx $(RELEASE_FLAGS)

release-jv-lsp: ## Build jv-lsp release binary only
	cargo build --bin jv-lsp $(RELEASE_FLAGS)

install: ## Install all jv tools locally (jv, jvx, jv-lsp)
	cargo install --path jv/crates/jv_cli
	cargo install --path jv/crates/jv_lsp

install-cli: ## Install jv CLI tools only (jv, jvx)
	cargo install --path jv/crates/jv_cli

install-lsp: ## Install jv-lsp Language Server only
	cargo install --path jv/crates/jv_lsp

# Documentation
docs: ## Generate documentation
	cargo doc $(CARGO_FLAGS) --no-deps --open

# Development setup
setup: ## Set up development environment
	@echo "Setting up jv development environment..."
	rustup component add rustfmt clippy
	@echo "✅ Development environment ready"

dev: check fmt lint ## Quick development cycle (check + format + lint)

# Specific crate builds (useful for debugging compilation issues)
build-lexer: ## Build lexer crate only
	cargo build -p jv_lexer $(CARGO_FLAGS)

build-parser: ## Build parser crate only
	cargo build -p jv_parser $(CARGO_FLAGS)

build-ast: ## Build AST crate only
	cargo build -p jv_ast $(CARGO_FLAGS)

build-ir: ## Build IR crate only
	cargo build -p jv_ir $(CARGO_FLAGS)

build-codegen: ## Build code generator crate only
	cargo build -p jv_codegen_java $(CARGO_FLAGS)

build-cli: ## Build CLI crate only
	cargo build -p jv_cli $(CARGO_FLAGS)

build-lsp: ## Build LSP server crate only
	cargo build -p jv_lsp $(CARGO_FLAGS)

# Binary-specific builds
build-jv: ## Build jv binary only
	cargo build --bin jv $(CARGO_FLAGS)

build-jvx: ## Build jvx binary only
	cargo build --bin jvx $(CARGO_FLAGS)

build-jv-lsp: ## Build jv-lsp binary only
	cargo build --bin jv-lsp $(CARGO_FLAGS)

build-binaries: ## Build all binaries (jv, jvx, jv-lsp)
	cargo build --bin jv $(CARGO_FLAGS)
	cargo build --bin jvx $(CARGO_FLAGS)
	cargo build --bin jv-lsp $(CARGO_FLAGS)

# Testing individual crates
test-lexer: ## Test lexer crate only
	cargo test --lib -p jv_lexer $(CARGO_FLAGS)

test-ast: ## Test AST crate only
	cargo test --lib -p jv_ast $(CARGO_FLAGS)

test-ir: ## Test IR crate only
	cargo test --lib -p jv_ir $(CARGO_FLAGS)

test-support: ## Test support crate only
	cargo test --lib -p jv_support $(CARGO_FLAGS)

test-codegen: ## Test code generator crate only
	cargo test --lib -p jv_codegen_java $(CARGO_FLAGS)

test-checker: ## Test checker crate only
	cargo test --lib -p jv_checker $(CARGO_FLAGS)

test-fmt: ## Test formatter crate only
	cargo test --lib -p jv_fmt $(CARGO_FLAGS)

test-inference: ## Test inference crate only
	cargo test --lib -p jv_inference $(CARGO_FLAGS)

test-cli: ## Test CLI crate only
	cargo test --lib -p jv_cli $(CARGO_FLAGS)

test-lsp: ## Test LSP server crate only
	cargo test --lib -p jv_lsp $(CARGO_FLAGS)

# Binary testing commands
test-jv-bin: ## Test jv binary integration
	cargo test --bin jv $(CARGO_FLAGS)

test-jvx-bin: ## Test jvx binary integration
	cargo test --bin jvx $(CARGO_FLAGS)

test-jv-lsp-bin: ## Test jv-lsp binary integration
	cargo test --bin jv-lsp $(CARGO_FLAGS)

test-all-binaries: ## Test all binaries
	cargo test --bin jv $(CARGO_FLAGS)
	cargo test --bin jvx $(CARGO_FLAGS)
	cargo test --bin jv-lsp $(CARGO_FLAGS)

# CI/CD commands
ci: check test lint ## Run CI checks (check + test + lint)

# Memory-optimized builds (for low-memory environments)
build-lowmem: ## Build with reduced parallelism for low-memory systems
	cargo build $(CARGO_FLAGS) -j 2

test-lowmem: ## Test with reduced parallelism for low-memory systems
	CARGO_INCREMENTAL=0 cargo test $(CARGO_FLAGS) --workspace --exclude jv_cli --exclude jv_lsp --lib -j 2
	CARGO_INCREMENTAL=0 cargo test $(CARGO_FLAGS) --lib -p jv_cli -j 1
	CARGO_INCREMENTAL=0 cargo test $(CARGO_FLAGS) --lib -p jv_lsp -j 1

# Ultra low-memory builds (for constrained CI environments)
build-minimal: ## Build with single thread for minimal memory usage
	CARGO_INCREMENTAL=0 cargo build $(CARGO_FLAGS) -j 1

test-minimal: ## Test with single thread for minimal memory usage
	CARGO_INCREMENTAL=0 cargo test $(TEST_FLAGS) -j 1

# Performance testing with memory constraints
perf-phase1: clean ## Run phase1 performance tests with memory optimization
	@echo "Running AST→IR performance tests with memory constraints..."
	CARGO_INCREMENTAL=0 cargo test $(CARGO_FLAGS) --package jv_ir -- --ignored perf_phase1

perf-build: ## Run performance build with memory optimization
	@echo "Running jv build with performance profiling..."
	CARGO_INCREMENTAL=0 cargo run $(CARGO_FLAGS) --bin jv -- \
		build jv/tests/performance/phase1.jv --java-only --perf

# Watch commands (requires cargo-watch)
watch: ## Watch for changes and rebuild
	cargo watch -x 'check $(CARGO_FLAGS)'

watch-test: ## Watch for changes and run tests
	cargo watch -x 'test $(TEST_FLAGS)'

# Utility commands
tree: ## Show dependency tree
	cargo tree $(CARGO_FLAGS)

# Verification commands
verify-binaries: ## Verify all binaries can be built
	@echo "Verifying binary builds..."
	cargo build --bin jv $(CARGO_FLAGS)
	cargo build --bin jvx $(CARGO_FLAGS)
	cargo build --bin jv-lsp $(CARGO_FLAGS)
	@echo "✅ All binaries built successfully"

verify-structure: ## Verify project structure and files
	@echo "Verifying project structure..."
	@test -f jv/crates/jv_cli/src/main.rs || (echo "❌ Missing jv CLI main.rs" && exit 1)
	@test -f jv/crates/jv_cli/src/bin/jvx.rs || (echo "❌ Missing jvx binary" && exit 1)
	@test -f jv/crates/jv_lsp/src/main.rs || (echo "❌ Missing jv-lsp main.rs" && exit 1)
	@test -f jv/crates/jv_lsp/src/lib.rs || (echo "❌ Missing jv-lsp lib.rs" && exit 1)
	@echo "✅ Project structure verified"

verify-all: verify-structure verify-binaries ## Run all verification checks
	@echo "✅ All verification checks passed"

outdated: ## Check for outdated dependencies (requires cargo-outdated)
	cargo outdated $(CARGO_FLAGS)

audit: ## Security audit (requires cargo-audit)
	cargo audit $(CARGO_FLAGS)

# Project information
info: ## Show project information
	@echo "jv Language Project Information"
	@echo "==============================="
	@echo "Workspace: $(shell pwd)/jv"
	@echo "Crates:"
	@find jv/crates -name Cargo.toml -exec dirname {} \; | sed 's|jv/crates/|  - |'
	@echo ""
	@echo "Binaries:"
	@echo "  - jv       (Main CLI - project management, build, run)"
	@echo "  - jvx      (Quick execution - snippets, testing)"
	@echo "  - jv-lsp   (Language Server - editor integration)"
	@echo ""
	@echo "Build Configuration:"
	@echo "  Debug codegen-units: 16 (memory optimized)"
	@echo "  Release codegen-units: 1 (size optimized)"
	@echo "  LTO: thin (debug), fat (release)"
