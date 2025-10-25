{
  description = "jvlang dev environment (Rust 1.90 + dual JDK setup)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        rustToolchain = pkgs.rust-bin.stable."1.90.0".default.override {
          extensions = [ "rust-src" "clippy" "rustfmt" "rust-analyzer" ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            rustToolchain

            # Utilities needed for tooling/bootstrap scripts
            pkgs.git
            pkgs.pkg-config
            pkgs.curl
          ];

          shellHook = ''
            repo_root=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
            installer="$repo_root/scripts/install_toolchain_jdks.sh"
            if [ -x "$installer" ]; then
              "$installer" "$repo_root"
            else
              echo "[devShell] Missing $installer; JDK toolchains not installed" >&2
            fi

            export JAVA21_HOME="$repo_root/toolchains/jdk21"
            export JAVA25_HOME="$repo_root/toolchains/jdk25"
            export PATH="$JAVA25_HOME/bin:$JAVA21_HOME/bin:$PATH"

            cat <<'INSTRUCTIONS'
JDK setup:
  * toolchains/jdk21 and toolchains/jdk25 are auto-synced when entering nix develop.
  * Default PATH prefers Java 25, override per-run via:
      JAVA_HOME=$JAVA25_HOME cargo test        # Java 25 compliance
      JAVA_HOME=$JAVA21_HOME cargo test        # Java 21 compatibility
  * Delete toolchains/ to force a re-download on next shell entry.
INSTRUCTIONS
          '';
        };

        # Convenience app: runs the CLI via cargo
        apps.jv = {
          type = "app";
          program = (
            pkgs.writeShellScriptBin "jv" ''
              exec ${pkgs.cargo}/bin/cargo run -p jv_cli -- "$@"
            ''
          ).outPath + "/bin/jv";
        };
      }
    );
}
