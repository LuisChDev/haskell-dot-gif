{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {config = { allowBroken = true; }; } # Yampa, fixed already on unstable
, hoo ? false }:

## override permite alterar atributos de la derivación
# 'haskellPackages'

let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  lib = pkgs.haskell.lib;

in (pkgs.haskellPackages.override (old: {
  overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
    (self: super: {
      Yampa = lib.dontCheck super.Yampa;
      ghc = super.ghc
        // (if hoo then { withPackages = super.ghc.withHoogle; } else { });
      ghcWithPackages = self.ghc.withPackages;
    });
})).callCabal2nix "haskell-dot-gif" src {}
