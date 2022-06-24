let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper:
        let
          dontCheck = super.haskell.lib.dontCheck;
          dontHaddock = super.haskell.lib.dontHaddock;

          # Disable Haddock generation and profiling by default. The former
          # can be done via cabal, while the latter should be enabled on
          # demand.
          defaultMod = drv:
            super.haskell.lib.dontHaddock
            (super.haskell.lib.disableLibraryProfiling drv);

          config-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          config = hself.callCabal2nix "config" config-src { };
        in { inherit config; };
    };
  };
in [ customHaskellPackages ]
