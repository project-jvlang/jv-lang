{
  description = "jvlang dev environment (Rust + JDK)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        jdk = pkgs.jdk; # System default JDK (provides javac/jar); adjust if JDK 25 available
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            # Rust toolchain
            pkgs.rustc
            pkgs.cargo
            pkgs.clippy
            pkgs.rustfmt
            pkgs.rust-analyzer

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

