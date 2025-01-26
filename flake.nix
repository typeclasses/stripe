{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import inputs.nixpkgs { inherit system; };
      in (let
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        hls = pkgs.haskell-language-server.override {
          supportedGhcVersions = [ "98" ];
        };

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {

        packages = let
          makeTestConfiguration = let defaultPkgs = pkgs;
          in { ghcVersion, pkgs ? defaultPkgs, overrides ? new: old: { } }:
          let
            inherit (pkgs.haskell.lib) packageSourceOverrides;
            haskellPackages = (pkgs.haskell.packages.${ghcVersion}.override
              (old: {
                overrides = combineOverrides old [
                  (packageSourceOverrides {
                    stripe-concepts = ./stripe-concepts;
                    stripe-signature = ./stripe-signature;
                    stripe-wreq = ./stripe-wreq;
                  })
                  overrides
                ];
              }));
          in pkgs.symlinkJoin {
            name = "stripe-${ghcVersion}";
            paths = [
              haskellPackages.stripe-concepts
              haskellPackages.stripe-signature
              haskellPackages.stripe-wreq
            ];
          };
        in rec {
          ghc-9-2 = makeTestConfiguration { ghcVersion = "ghc92"; };
          ghc-9-4 = makeTestConfiguration { ghcVersion = "ghc94"; };
          ghc-9-6 = makeTestConfiguration { ghcVersion = "ghc96"; };
          ghc-9-8 = makeTestConfiguration { ghcVersion = "ghc98"; };
          all = pkgs.symlinkJoin {
            name = "stripe-all";
            paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ghc-9-8 ];
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.haskell.compiler.ghc98 pkgs.cabal-install ];
        };

      }));
}
