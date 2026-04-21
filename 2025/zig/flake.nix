{
  description = "AOC 2025 (Zig)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    nixpkgs-master.url = "github:NixOS/nixpkgs?rev=87735154131376b0ed230289c8c9a73de37bad6c";
  };

  outputs =
    { nixpkgs, nixpkgs-master, ... }:
    let
      lib = nixpkgs.lib;
      forAllSystems =
        fn:
        lib.genAttrs [
          "aarch64-darwin"
        ] (system: fn nixpkgs.legacyPackages.${system} nixpkgs-master.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (
        pkgs: pkgs-master: {
          default = pkgs.mkShellNoCC {
            packages = [
              pkgs.lldb
              pkgs.samply
              pkgs-master.zig
              pkgs-master.zls
            ];
          };
        }
      );
    };
}
