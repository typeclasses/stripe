let

sources = import ./nix/sources.nix;
nixos-22-05 = import sources."nixos-22.05" {};
nixos-22-11 = import sources."nixos-22.11" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    stripe-concepts = ./stripe-concepts;
    stripe-scotty = ./stripe-scotty;
    stripe-signature = ./stripe-signature;
    stripe-wreq = ./stripe-wreq;
};

depOverrides = new: old: {
    # x = new.callPackage ./nix/x-1.0.1.0.nix {};
};

ghc."8.10" = nixos-22-05.haskell.packages.ghc8107.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

ghc."9.0" = nixos-22-11.haskell.packages.ghc90.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides ];
});

ghc."9.4" = nixos-22-11.haskell.packages.ghc94.override (old: {
    overrides = combineOverrides old [ sourceOverrides depOverrides
      (
        new: old: {
            scotty = new.callPackage ./nix/scotty-0.12.1.nix {};
        }
      )
    ];
});

in

symlinkJoin {
    name = "multiple-stripe-libraries";
    paths = concatMap (x: [x.stripe-concepts x.stripe-scotty x.stripe-signature x.stripe-wreq]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
