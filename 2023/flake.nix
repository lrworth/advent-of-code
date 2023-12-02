{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellCompiler = "ghc981";
      in {
        devShells = rec {
          default = pkgs.mkShell {
            packages = with pkgs; [
              cabal-install
              haskellPackages.cabal-fmt
              haskellPackages.ormolu
              haskell.compiler.${haskellCompiler}
              fstar
            ];
          };
        };
      }
    );
}
