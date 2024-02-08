{ nixpkgs ? import <nixpkgs> {} }:
let
  nixpkgs_source = fetchTarball https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz;

   overlays = [];
   config = {
     allowUnfree = true;
     cudaSupport = true;
   };
   myNix = import nixpkgs_source {inherit overlays; inherit config;};
in
with myNix.pkgs; 
let hp9 = haskell.packages.ghc901; # needs latest hackage as well
    hp8 = haskellPackages.override{
      overrides = self: super: {
      };};
    hp = hp8;
    ghc = hp.ghcWithPackages (ps: with ps; ([ cabal-install base bibtex mtl ConfigFile directory process filepath parsek parsec containers split text bytestring optparse-applicative groom ]));
in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ glibcLocales ghc ];
  shellHook = ''
    export LANG=en_US.UTF-8
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}


