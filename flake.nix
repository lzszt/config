{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs =
    { self, nixpkgs, ... }:
    let
      pkgs = import ./nix/pkgs.nix {
        inherit nixpkgs;
        system = "x86_64-linux";
      };
      packageName = "config";
    in
    {
      packages.x86_64-linux.${packageName} = pkgs.haskellPackages.config;
      defaultPackage.x86_64-linux = self.packages.x86_64-linux.${packageName};
      devShells.x86_64-linux = {
        default = pkgs.haskellPackages.shellFor {
          packages = p: [ p.config ];
          nativeBuildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghc
            haskellPackages.hlint
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.fourmolu
          ];
        };
      };
    };
}
