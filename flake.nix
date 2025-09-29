{
  description = "jvlang dev environment (Rust 1.90 + JDK)";

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
        jdk = pkgs.jdk; # System default JDK (provides javac/jar); adjust if JDK 25 available
        rustToolchain = pkgs.rust-bin.stable."1.90.0".default.override {
          extensions = [ "rust-src" "clippy" "rustfmt" "rust-analyzer" ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            # Rust 1.90.0 toolchain
            rustToolchain

            # Java toolchain
            jdk

            # Utilities
            pkgs.git
            pkgs.pkg-config
          ];
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

