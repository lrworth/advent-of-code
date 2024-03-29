{
  description = "Advent of Code 2022";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            cabal-install
            haskell.compiler.ghc924
            haskell.packages.ghc924.cabal-fmt
            haskell.packages.ghc924.haskell-language-server
            ormolu
            idris2
            rlwrap
          ];
        };
      }
    );
}
