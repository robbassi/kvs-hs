{ pkgs ? import <nixpkgs> {} }:
rec {
  inherit pkgs;
  hsPkgs = pkgs.haskell.packages.ghc8102;
  gitignore = pkgs.nix-gitignore.gitignoreSource [] ./.;
  kvs = hsPkgs.callCabal2nix "kvs" gitignore {};
}
