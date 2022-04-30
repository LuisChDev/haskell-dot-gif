{
  description = "haskell proofs of concept";
  nixConfig.bash-prompt = "\\e[1;34m\n\[haskell-dot-gif@\$HOSTNAME:\$PWD\]$ \\e[0m";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs?rev=6bebc91e2882e0b4917567d0c025bbfafb01be69";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        packageName = "haskell-dot-gif";
        pkgs = import nixpkgs {
          inherit system;
          config = { allowBroken = true; };
        };
        lib = pkgs.haskell.lib;
        haskellPackages = pkgs.haskellPackages.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
          # (self: super: {
          #   Yampa = lib.dontCheck super.Yampa;
          # });
            (self: super: { });
        });

      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self { };
        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.${packageName} ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskellPackages.haskell-language-server
            cabal-install
            cabal2nix
            heroku
          ];
        };
      });
}
