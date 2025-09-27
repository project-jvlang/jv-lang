# Build Configuration

## Development Setup

This project uses specific Cargo configuration to prevent SIGKILL issues during compilation.

### Compilation Issues

The project uses the `chumsky` parser library which can cause excessive memory consumption (30GB+) during compilation with default optimization settings. This has been resolved with a specialized `.cargo/config.toml` configuration.

### Current Configuration

The `.cargo/config.toml` file contains aggressive optimization disabling to prevent memory exhaustion:

```toml
# Emergency chumsky SIGKILL fix configuration
[build]
rustflags = [
    "-C", "opt-level=0",         # Disable optimizations
    "-C", "debuginfo=0",         # Disable debug info
    "-C", "codegen-units=1",     # Single codegen unit
    "-C", "incremental=false",   # Disable incremental compilation
    "-C", "target-cpu=generic"   # Use generic CPU target
]

[profile.test]
opt-level = 0
lto = false
codegen-units = 1
incremental = false
```

### Build Commands

Standard Rust commands work as expected:

```bash
# Clean build
cargo clean

# Check compilation
cargo check

# Build the project
cargo build

# Run tests
cargo test

# Run specific crate tests
cargo test --lib -p jv_lsp
cargo test --lib -p jv_parser
```

### Performance Impact

The aggressive optimization disabling results in:
- **Benefit**: Prevents SIGKILL/OOM during compilation
- **Cost**: Slightly slower runtime performance in debug builds
- **Build Time**: jv_parser ~30s, jv_lsp ~43s (acceptable)

This configuration is necessary for stable development and should not be modified unless the chumsky dependency is updated or replaced.

### Troubleshooting

If you encounter SIGKILL errors during compilation:

1. Verify `.cargo/config.toml` contains the optimization-disabled configuration
2. Run `cargo clean` to clear build cache
3. Ensure sufficient system memory (>8GB recommended)
4. Check WSL2 memory limits if using Windows Subsystem for Linux

### Alternative Solutions

If compilation issues persist, consider:
- Increasing WSL2 memory allocation
- Using a different parser library instead of chumsky
- Building on a machine with more RAM