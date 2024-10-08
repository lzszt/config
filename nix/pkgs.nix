{ nixpkgs, system }:

let
  overlays = import ./overlays.nix;
  config = {
    allowBroken = false;
  };
in
import nixpkgs { inherit config overlays system; }
