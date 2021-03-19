with (import ./default.nix {});

hsPkgs.shellFor {
  packages = _: [ kvs ];
  withHoogle = false;
  buildInputs = with pkgs; [
    cabal-install
    cabal2nix

    hsPkgs.ghcid
    hsPkgs.fast-tags
  ];
}
