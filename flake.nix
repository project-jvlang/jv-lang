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
        defaultJdk = pkgs.jdk;
        jdk25 =
          if builtins.hasAttr "temurin-bin-25" pkgs then pkgs."temurin-bin-25"
          else if builtins.hasAttr "jdk25" pkgs then pkgs.jdk25
          else defaultJdk;
        jdk21 =
          if builtins.hasAttr "temurin-bin-21" pkgs then pkgs."temurin-bin-21"
          else if builtins.hasAttr "jdk21" pkgs then pkgs.jdk21
          else defaultJdk;
        rustToolchain = pkgs.rust-bin.stable."1.90.0".default.override {
          extensions = [ "rust-src" "clippy" "rustfmt" "rust-analyzer" ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            # Rust 1.90.0 toolchain
            rustToolchain

            # Java toolchains for compatibility testing
            jdk25
            jdk21

            # Utilities
            pkgs.git
            pkgs.pkg-config
          ];

          shellHook = ''
            export JAVA25_HOME=${jdk25}
            export JAVA21_HOME=${jdk21}
            export PATH="$JAVA25_HOME/bin:$JAVA21_HOME/bin:$PATH"

            cat <<'INSTRUCTIONS'
JDK setup:
  * Default PATH points to JDK 25 (java/javac from $JAVA25_HOME/bin).
  * Use JAVA_HOME to pick a toolchain per test run, e.g.:
      JAVA_HOME=$JAVA25_HOME cargo test        # Java 25 compliance
      JAVA_HOME=$JAVA21_HOME cargo test        # Java 21 compatibility
  * Both JDKs ship fully, so integration tests can spawn either version.
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
