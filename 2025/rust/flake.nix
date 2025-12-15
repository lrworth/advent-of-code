{
  description = "AOC 2025 (Rust)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
  };

  outputs =
    { nixpkgs, ... }:
    let
      lib = nixpkgs.lib;
      forAllSystems =
        fn:
        lib.genAttrs [
          "aarch64-darwin"
        ] (system: fn nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = [
            pkgs.rustc
            pkgs.rustfmt
            pkgs.rust-analyzer
            pkgs.cargo
          ];
        };
      });
    };
}
