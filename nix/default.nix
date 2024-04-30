{ pkgs, unstable }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  hls = pkgs.haskell-language-server.override {
    supportedGhcVersions = [ "96" ];
  };

  combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

  testConfigurations =
    let
      makeTestConfiguration = let defaultPkgs = pkgs; in { ghcVersion, pkgs ? defaultPkgs, overrides ? new: old: { } }:
        let
          inherit (pkgs.haskell.lib) packageSourceOverrides;
          haskellPackages = (pkgs.haskell.packages.${ghcVersion}.override (old: {
            overrides =
              combineOverrides old [
                (packageSourceOverrides {
                  stripe-concepts = ../stripe-concepts;
                  stripe-signature = ../stripe-signature;
                  stripe-wreq = ../stripe-wreq;
                })
                overrides
              ];
          }));
        in
        pkgs.symlinkJoin {
          name = "stripe-${ghcVersion}";
          paths = [
            haskellPackages.stripe-concepts
            haskellPackages.stripe-signature
            haskellPackages.stripe-wreq
          ];
        };
    in
    rec {
      ghc-9-2 = makeTestConfiguration {
        ghcVersion = "ghc92";
      };
      ghc-9-4 = makeTestConfiguration {
        ghcVersion = "ghc94";
      };
      ghc-9-6 = makeTestConfiguration {
        ghcVersion = "ghc96";
        overrides = new: old: {
          # x = new.callPackage ./haskell/x.nix { };
        };
      };
      ghc-9-8 = makeTestConfiguration {
        ghcVersion = "ghc98";
        pkgs = unstable;
      };
      all = pkgs.symlinkJoin {
        name = "stripe-tests";
        paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ghc-9-8 ];
      };
    };

in
{

  packages = { inherit testConfigurations; };

  devShells.default = pkgs.mkShell {
    buildInputs = [ hls pkgs.haskell.compiler.ghc94 pkgs.cabal-install ];
  };

}
