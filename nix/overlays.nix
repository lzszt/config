let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides =
        hself: hsuper:
        let
          config-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          config = hself.callCabal2nix "config" config-src { };
        in
        {
          inherit config;
        };
    };
  };
in
[ customHaskellPackages ]
