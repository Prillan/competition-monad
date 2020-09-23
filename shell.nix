let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.haskell-competition-monad.components.all
