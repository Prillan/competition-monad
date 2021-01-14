{ pkgs ? import <nixpkgs> {}}:
let compilerSet = pkgs.haskell.packages.ghc8102;
    dependencies = p: with p; [
      containers
      # free
      probability
      MonadRandom
      set-monad
      transformers
    ];
in
{
  shell = pkgs.mkShell {
    buildInputs = [
      (compilerSet.ghcWithPackages dependencies)
      pkgs.cabal-install
    ];
  };
}
